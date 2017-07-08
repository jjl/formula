(ns irresponsible.formula
  (:require [clojure.spec.alpha :as s]
            [clojure.core.match :refer [match]]
            [irresponsible.spectra :as ss]
            [irresponsible.formula.conform :as c]))

;; TODO: move these into spectra?
(def invalid? c/invalid?)
(def valid? (complement invalid?))

(defn get-spec! [s]
  (or (cond (s/spec? s) s
            (s/valid? ::nskw s) (s/get-spec s))
      ::s/invalid))

(s/def ::nskw (s/and keyword? namespace))
(s/def ::spec (s/conformer get-spec!))
(s/def ::specs (s/coll-of ::spec))
(s/def ::specs+ (s/coll-of ::spec :min-count 1))
(s/def ::copy ::nskw)
(s/def ::error some?)
(s/def ::conform ifn?)
(s/def ::unform ifn?)
(s/def ::gen ifn?)
(s/def ::key (complement nil?))

(defn truly
  "Can't use any? as it's 1.9 only"
  [_] true)

(def nnil?
  "Can't use some? as it's 1.9 only"
  (complement nil?))

(defrecord Field [field key conform unform gen error raw-opts]
  s/Specize
  (specize* [f] f)
  (specize* [f _] f)
  s/Spec
  (conform*  [f form]
    (let [r (conform (form field))]
      (if (invalid? r)
        ::s/invalid
        {key r})))
  (unform*   [f form] {field (unform (form key))})
  (with-gen* [f gfn]  (assoc f :gen gfn))
  (describe* [f]      `(field ~field ~@raw-opts))
  (gen*      [f overrides path rmap] gen)
  (explain*  [f path via in x]
    (when (c/invalid? (s/conform f x))
      [{:path (conj path field) :val (x field) :in in :error error}])))

(s/def ::field-opts
  (s/conformer
   (s/keys* :req-un [(or ::error ::copy)] :opt-un [::conform ::unform ::gen ::key])
   (fn [{:keys [copy] :as opts}]
     (if copy
       (if-let [parent (s/get-spec copy)]
         (if (instance? Field parent)
           (into parent opts)
           (throw (ex-info (str "Spec must be a field: " copy) {:got copy})))
         (throw (ex-info (str "Could not find spec to copy: " copy) {:got copy})))
       opts))))

(defn field-impl
  [field opts raw-opts]
;; TODO: alpha18
  (let [all-fields (some-fn :error :copy)
        {:keys [conform unform gen error key]
         :or {conform identity unform identity key field}}
        (ss/conform! (s/and ::field-opts all-fields) opts)]
    (->Field field key conform unform gen error raw-opts)))

(defmacro field [field & opts]
  `(field-impl ~field [~@opts] '~opts))

(defrecord And [fields gen raw-opts]
  s/Specize
  (specize* [f] f)
  (specize* [f _] f)
  s/Spec
  (conform* [f form]
    (let [rs (map #(s/conform % form) fields)]
      (if (some invalid? rs)
        ::s/invalid
        (apply merge rs))))
  (unform* [f form]
    (apply merge (map #(s/unform* % form) fields)))
  (explain* [f path via in x]
    (when (= ::s/invalid (s/conform f x))
      (mapcat #(s/explain* % path via in x) fields)))
  (gen* [f overrides path rmap] gen)
  (with-gen* [f gfn] (assoc f :gen gfn))
  (describe* [f]   `(and ~@raw-opts)))

(defn and-impl [fields raw-opts]
  (->And (ss/conform! ::specs+ fields) nil raw-opts))

(defrecord Or [fields gen raw-opts]
  s/Specize
  (specize* [f] f)
  (specize* [f _] f)
  s/Spec
  (conform* [f form]
    (-> #(let [r (s/conform % form)]
            (when-not (c/invalid? r)
              r))
        (some fields)
        (or ::s/invalid)))
  (unform* [f form]
    (some #(s/unform* % form) fields))
  (explain* [f path via in x]
    (when (= ::s/invalid (s/conform f x))
      (mapcat #(s/explain* % path via in x) fields)))
  (gen* [f overrides path rmap]   gen)
  (with-gen* [f gfn] (assoc f :gen gfn))
  (describe* [f]     `(or ~@raw-opts)))

(defn or-impl [fields raw-opts]
  (if (seq fields)
    (let [r (->Or (ss/conform! ::specs+ fields) nil raw-opts)]
      r)
    (throw (ex-info "or requires at least one field" {}))))

(defrecord Compound [fields key conform unform gen error raw-opts]
  s/Specize
  (specize* [f] f)
  (specize* [f _] f)
  s/Spec
  (conform* [f form]
    (let [rs (mapv #(s/conform % form) fields)]
      (if (some invalid? rs)
        ::s/invalid
        (let [r (conform (apply merge rs))]
          (if (invalid? r)
            ::s/invalid
            {key r})))))
  (unform* [f form]
    (unform (apply merge (map #(s/unform* % form) fields))))
  (explain* [f path via in x]
    (when (= ::s/invalid (s/conform f x))
      (mapcat #(s/explain* % path via in x) fields)))
  (gen* [f overrides path rmap] gen)
  (with-gen* [f gfn] (assoc f :gen gfn))
  (describe* [f]     `(compound ~@raw-opts)))

(s/def ::fields ::specs+)

(s/def ::compound-opts
  (s/keys* :req-un [::fields ::error ::key] :opt-un [::conform ::unform ::gen]))

(defn compound-impl
  [opts raw-opts]
;; TODO: alpha18
  (let [all-fields (every-pred :conform :unform :error :fields)
        {:keys [conform unform gen error fields key]
         :or {}}
        (ss/conform! (s/and ::compound-opts all-fields) opts)]
    (->Compound fields key conform unform gen error raw-opts)))

  ;; (explain* [f path via in x]
  ;;   (mapcat #(explain* % (conj path %) via in (form field))))
  ;; (gen* [f overrides path rmap]
  ;;   123);;(gen* spec override (conj path field) rmap))
  ;; (with-gen* [f gfn]
  ;;   (assoc f :gen gfn))
  ;; (describe* [f]
  ;;   `(field ~spec ~field)))

(defmacro compound [& opts]
  `(compound-impl [~@opts] '~opts))
;; (s/def ::form-opts (s/keys :opt-un [conform unform req opt]))

;;; do req and opt handling

(defrecord Form [conform unform gen req opt error raw-opts]
  s/Specize
  (specize* [f] f)
  (specize* [f _] f)
  s/Spec
  (conform* [f form]
    (let [req2 (mapv #(s/conform % form) req)
          opt2 (into [] (comp (map #(s/conform % form))
                              (filter valid?)) opt)]
      (if (some invalid? req2)
        ::s/invalid
        (conform (apply merge (apply merge req2) opt2)))))
  (unform* [f form]
    (let [req2 (map #(unform (form %)) req)
          opt2 (map #(unform (form %)) opt)]
      (merge (apply merge opt2) (apply merge req2))))
  (explain* [f path via in x]
    (when (= ::s/invalid (s/conform* f x))
      (mapcat #(s/explain* % path via in x) req)))
  (gen* [f overrides path rmap]   gen)
  (with-gen* [f gfn] (assoc f :gen gfn))
  (describe* [f]     `(formula ~@raw-opts)))

(s/def ::req ::specs)
(s/def ::opt ::specs)

(s/def ::form-opts (s/keys* :req-un [(or ::req ::opt)] :opt-un [::req ::opt ::conform ::unform ::gen ::error]))

(defn form-impl
  [opts raw-opts]
;; TODO: alpha18
  (let [all-fields (every-pred (some-fn (comp seq :req) (comp seq :opt)))
        {:keys [req opt conform unform gen error]
         :or {conform identity unform identity}}
        (ss/conform! (s/and ::form-opts all-fields) opts)]
    (->Form conform unform gen req opt error raw-opts)))

(defmacro form [& opts]
  `(form-impl [~@opts] '~opts))

(defmacro or  [& opts]
  `(or-impl [~@opts] '~opts))
(defmacro and [& opts]
  `(and-impl [~@opts] '~opts))
