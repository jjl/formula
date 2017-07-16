(ns irresponsible.formula
  (:require [clojure.core.match :refer [match]]
            [flatland.ordered.map :refer [ordered-map]]
            [irresponsible.formula.conform :as c]))

(defn blank?
  "true if a value is either nil or the empty string
   args: [val]
   returns: boolean"
  [v]
  (or (nil? v) (= "" v)))

(defn invalid?
  "true if there was at least one error
   args: [ret]
   returns: boolean"
  [{:keys [error]}]
  (boolean (seq error)))

(def valid?
  "true if there were no errors
   args: [ret]
   returns: boolean"
  (complement invalid?))

(defn truly
  "Can't use any? as it's 1.9 only"
  [_] true)

(def nnil?
  "Can't use some? as it's 1.9 only"
  (complement nil?))

(defprotocol Formula
  (deform* [self form-data path])
  (reform* [self form-data]))

(defn formula?
  "true if the provided value is satisfies Formula
   args: [val]
   returns: boolean"
  [v]
  (satisfies? Formula v))

(defn deform
  "Given a form and some form data, validates and canonicalises the data
   args: [form data] [form data path]
   returns: map with keys:
     :form - map of canonicalised form data
     :data - map of logical data
     :error - map of path vector to vector of error strings"
  ([form data]      (deform* form data []))
  ([form data path] (deform* form data path)))

(defn reform
  "Turns deformed form data into raw form data
   args: [form data] [form data path]
   returns: map of form data"
  [form data]
  (reform* form data))

(defn merge-results
  "Merges a seq of results into one result
   args: [results]
   returns: result"
  [& [f & fs]]
  (reduce (fn [{f1 :form e1 :error d1 :data} {f2 :form e2 :error d2 :data}]
              {:form (merge f1 f2)
               :error (merge-with into e1 e2)
               :data (merge d1 d2)}) f fs))

(defrecord Field [in-key out-key test canon deform reform error]
  Formula
  (deform* [self form path]
    (let [in (get form in-key)]
      (try (or (test in) (throw (ex-info "" {})))
           (let [f (canon in)
                 d (deform in)]
             {:form {in-key f}
              :data {out-key d}})
           (catch Throwable e
             ;; if canon throws, we shall assume we aren't meant to keep the field value
             (try {:form {in-key (canon in)} :error {(into path in-key) [error]}}
                  (catch Throwable e
                    {:error {(into path in-key) [error]}}))))))
  (reform* [self form]
    {:form {in-key (reform (get form out-key))}}))

(defn field
  "Creates a field with the given parameters
   args: [opts] ; map with keys:
     :in-key  the key in the form data map
     :out-key the key to output the data under
     :test    an optional predicate
     :canon   an optional reformatter for the form value
     :deform  an optional transform fn used after deformation
     :reform  an optional transform fn used after reformation
     :error   something that isn't nil (a string? a keyword?)
   returns: Field"
  [{:keys [in-key out-key test canon deform reform error]
    :or {test truly canon identity deform identity reform identity}}]
  {:pre [(ifn? test)
         (ifn? canon)
         (ifn? deform)
         (ifn? reform)
         (not (nil? error))
         (not (nil? in-key))
         (not (nil? out-key))]}
    (->Field in-key out-key test canon deform reform error))

(defrecord And [fields deform reform]
  Formula
  (deform* [self form path]
    ;; don't throw, because it's not clear how we should handle throws
    (->> fields
         (into [] (mapcat #(deform* % form path)))
         (apply merge-results)
         deform))
  (reform* [self form]
    (reform (apply merge (map #(reform* % form) fields)))))

(defn and*
  "Creates a field where all subfields must succeed
   Designed for use inside of `or*` as forms naturally have an 'and' nature
   args: [opts] ; map with keys:
     :fields  a sequence of fields
     :deform  an optional transform fn used after deformation
     :reform  an optional transform fn used after reformation
   returns: Field"
  [{:keys [fields deform reform] :or {deform identity reform identity}}]
  {:pre [(seq fields)
         (every? formula? fields)
         (ifn? deform)
         (ifn? reform)]}
  (->And fields deform reform))

(defrecord Or [fields deform reform tag default]
  Formula
  (deform* [self form path]
    ;; don't throw, because it's not clear how we should handle throws
    (let [rs (map #(deform* % form path) fields)]
      (or (some valid? rs)
          (deform (apply merge-results rs)))))
  (reform* [self form]
    (let [t (get form tag default)]
      (reform (reform* (get fields t) form)))))

(defn or*
  "Creates a field where subfields will be tried in order until one succeeds
   Note that the inbuilt deformation of this
   args: [opts] ; map with keys:
     :fields  a sequence of [key field] pairs (or an ordered-map)
     :deform  an optional transform fn used after deformation
     :reform  an optional transform fn used after reformation
     :tag     a non-nil key where the 
     :default a value which is used for the tag if none is found (default: nil)
   returns: Field"
  [{:keys [fields deform reform tag default] :or {deform identity reform identity}}]
  {:pre [(seq fields)
         (every? formula? fields)
         (not (nil? tag))
         (ifn? deform)
         (ifn? reform)]}
  (->Or fields deform reform tag default))

(defrecord Form [req opt deform reform]
  Formula
  (deform* [self form path]
    (let [req2 (mapv #(deform* % form []) req)
          help #(let [v (deform* % form [])]
                  (if (valid? v)
                    v
                    (dissoc :error v)))
          opt2 (into [] (map help opt))]
      (deform (apply merge-results (into req2 opt2)))))
  (reform* [self form]
    (let [req2 (map #(reform % form) req)
          opt2 (map #(reform % form) opt)]
      (apply merge (apply merge opt2) req2))))

(defn form
  "Creates a field where subfields will be tried in order until one succeeds
   Note that the inbuilt deformation of this
   args: [opts] ; map with keys:
     :req     a sequence of fields
     :opt     a sequence of fields which may fail (errors are removed)
     :deform  an optional transform fn used after deformation
     :reform  an optional transform fn used after reformation
   returns: Field"
  [{:keys [req opt deform reform] :or {deform identity reform identity}}]
  {:pre [(every? formula? req)
         (every? formula? opt)
         (ifn? deform)
         (ifn? reform)]}
  (->Form req opt deform reform))

(defmacro or  [& opts]
  `(or* ~@opts))
(defmacro and [& opts]
  `(or* ~@opts))
