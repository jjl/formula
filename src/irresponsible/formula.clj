(ns irresponsible.formula
  (:require [clojure.spec.alpha :as s]
            [clojure.core.match :refer [match]]
            [irresponsible.spectra :as ss]
            [irresponsible.formula.conform :as c]
            [com.rpl.specter :as sp]
            [com.rpl.specter.impl :as spi]
            [flatland.ordered.map :refer [ordered-map]]))

;; TODO: move these into spectra?
(def invalid? c/invalid?)

(s/def ::nskw (s/and keyword? namespace))
(s/def ::spec (s/or :nskw ::nskw :spec s/spec?))
(s/def ::copy ::nskw)
(s/def ::error some?)
(s/def ::conform fn?)
(s/def ::unform fn?)
(s/def ::gen fn?)

(s/def ::field-map (s/and (s/map-of any? s/spec?) (complement record?)))

(s/def ::to-field-map
  (s/or :nskw ::nskw
        :map  ::field-map))

(defn field-map [i]
  (match (ss/conform! ::to-field-map i)
    [:nskw n] (if-let [s (s/get-spec i)]
                       {i s}
                       (throw (ex-info (str "Could not find spec " i) {:got i})))
    [:map m]  m))

(defrecord Field [field conform unform gen error raw-opts]
  s/Specize
  (specize* [f] f)
  (specize* [f _] f)
  s/Spec
  (conform*  [f form] (conform (form field)))
  (unform*   [f form] {field (unform (form field))})
  (with-gen* [f gfn]  (assoc f :gen gfn))
  (describe* [f]      `(field ~field ~@raw-opts))
  (gen*      [f overrides path rmap] gen)
  (explain*  [f path via in x]
    (when (= ::s/invalid (s/conform f x))
      [{:path (conj path field) :val (x field) :in in :error error}])))

(s/def ::field-opts
  (s/conformer
   (s/keys* :req-un [(or ::error ::copy)] :opt-un [::conform ::unform ::gen])
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
        {:keys [conform unform gen error]
         :or {conform identity unform identity}}
        (ss/conform! (s/and ::field-opts all-fields) opts)]
    (->Field field conform unform gen error raw-opts)))

(defmacro field [field & opts]
  `(field-impl ~field [~@opts] '~opts))

(defrecord And [fields gen raw-opts]
  s/Specize
  (specize* [f] f)
  (specize* [f _] f)
  s/Spec
  (conform* [f form]
    (let [r (sp/transform [sp/MAP-VALS] #(s/conform % form) fields)]
      (if (not= ::spi/NONE (sp/select-any [sp/MAP-VALS invalid?] r))
        ::s/invalid
        r)))
  (unform* [f form]
    (apply merge (map #(s/unform* % form) (sp/select [sp/MAP-VALS] fields))))
  (explain* [f path via in x]
    (when (= ::s/invalid (s/conform f x))
      (mapcat #(s/explain* % path via in x) fields)))
  (gen* [f overrides path rmap]   gen)
  (with-gen* [f gfn] (assoc f :gen gfn))
  (describe* [f]   `(and ~@raw-opts)))

(let [form {:foo :foo :bar :bar}
      fields {:foo (field :foo :error "oops"
                          :conform (c/pred-conformer #(= :foo %)))
              :bar (field :bar :error "oops"
                          :conform  (c/pred-conformer #(= :bar %)))}]
  (sp/transform [sp/MAP-VALS] #(s/conform % form) fields))


(defn and-impl [fields raw-opts]
  (if (seq fields)
    (->And (apply merge (map field-map fields)) nil raw-opts)
    (throw (ex-info "and requires at least one field" {}))))

(defrecord Or [fields gen raw-opts]
  s/Specize
  (specize* [f] f)
  (specize* [f _] f)
  s/Spec
  (conform* [f form]
    (-> (fn [[k v]]
          (let [r (s/conform v form)]
            (when (not= ::s/invalid r)
              {k r})))
        (some fields)
        (or ::s/invalid)))
  (unform* [f form]
    (some (fn [[k v]]
            (s/unform* v form))))
  (explain* [f path via in x]
    (when (= ::s/invalid (s/conform f x))
      (mapcat #(s/explain* % path via in x) fields)))
  (gen* [f overrides path rmap]   gen)
  (with-gen* [f gfn] (assoc f :gen gfn))
  (describe* [f]     `(or ~@raw-opts)))

(defn or-impl [fields raw-opts]
  (if (seq fields)
    (let [r (->Or (reduce into (ordered-map) (map field-map fields)) nil raw-opts)]
      r)
    (throw (ex-info "or requires at least one field" {}))))

(defrecord Compound [fields conform unform gen error raw-opts]
  s/Specize
  (specize* [f] f)
  (specize* [f _] f)
  s/Spec
  (conform* [f form]
    (let [r (-> (fn [acc k v]
                  (let [r (s/conform v form)]
                    (if (invalid? r)
                      (reduced ::s/invalid)
                      (assoc! acc k r))))
                (reduce-kv (transient {}) fields))]
      (if (invalid? r)
        ::s/invalid
        (conform (persistent! r)))))
  (unform* [f form]
    (reduce-kv (fn [acc k v]
                 (merge acc (s/unform* v form)))
               {}
               fields))
  (explain* [f path via in x]
    (when (= ::s/invalid (s/conform f x))
      (mapcat #(s/explain* % path via in x) fields)))
  (gen* [f overrides path rmap] gen)
  (with-gen* [f gfn] (assoc f :gen gfn))
  (describe* [f]     `(compound ~@raw-opts)))

(s/def ::compound-opts
  (s/keys* :req-un [::fields ::error] :opt-un [::conform ::unform ::gen]))

(defn compound-impl
  [opts raw-opts]
;; TODO: alpha18
  (let [all-fields (every-pred :conform :unform :error :fields)
        {:keys [conform unform gen error fields]
         :or {}}
        (ss/conform! (s/and ::compound-opts all-fields) opts)]
    (->Compound (apply merge (map field-map fields)) conform unform gen error raw-opts)))

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
    (let [req2 (sp/transform [sp/MAP-VALS] #(s/conform % form) req)]
      (if (not= ::spi/NONE (sp/select-any [sp/MAP-VALS invalid?] req2))
        ::s/invalid
        (let [opt2 (sp/transform [sp/MAP-VALS] #(s/conform % form) opt)]
          (conform (merge opt2 req2))))))
  (unform* [f form]
    (let [req2 (map #(unform (form %)) (vals req))
          opt2 (map #(unform (form %)) (vals opt))]
      (merge (apply merge opt2) (apply merge req2))))
  (explain* [f path via in x]
    (when (= ::s/invalid (s/conform* f x))
      (mapcat #(s/explain* % path via in x) req)))
  (gen* [f overrides path rmap]   gen)
  (with-gen* [f gfn] (assoc f :gen gfn))
  (describe* [f]     `(formula ~@raw-opts)))

(s/def ::form-field
  (s/or :nskw ::nskw
        :map (s/map-of ::nskw s/spec? :min-count 1)))

(s/def ::form-fields (s/coll-of ::form-field))

(s/def :irresponsible.formula.form/req ::form-fields)
(s/def :irresponsible.formula.form/opt ::form-fields)

(s/def ::form-opts (s/keys* :req-un [(or :irresponsible.formula.form/req :irresponsible.formula.form/opt)] :opt-un [:irresponsible.formula.form/req :irresponsible.formula.form/opt ::conform ::unform ::gen ::error]))

(defn form-impl
  [opts raw-opts]
;; TODO: alpha18
  (let [all-fields (every-pred :error (some-fn (comp seq :req) (comp seq :opt)))
        {:keys [req opt conform unform gen error]
         :or {conform identity unform identity}}
        (ss/conform! (s/and ::form-opts all-fields) opts)
        req2 (reduce into (ordered-map) (map field-map (s/unform ::form-fields req)))
        opt2 (reduce into (ordered-map) (map field-map (s/unform ::form-fields opt)))]
    (->Form conform unform gen req2 opt2 error raw-opts)))

(defmacro form [& opts]
  `(form-impl [~@opts] '~opts))

(defmacro or  [& opts]
  `(or-impl [~@opts] '~opts))
(defmacro and [& opts]
  `(and-impl [~@opts] '~opts))
