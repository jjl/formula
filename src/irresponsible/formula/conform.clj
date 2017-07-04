(ns irresponsible.formula.conform
  (:require [clojure.spec.alpha :as s]
            [irresponsible.spectra :as ss]))

(defn invalid? [v] (= ::s/invalid v))

(defmacro try-conform [& exprs]
  `(try ~@exprs
        (catch Throwable ~'_ ::s/invalid)))

(defmacro try-conformer [& exprs]
  `(fn [& ~'_]
     (try-conform ~@exprs)))

(defn keep-conformer [c]
  #(let [r (c %)]
     (if (nil? r) ::s/invalid r)))

(defn pred-conformer
  ([pred]
   (pred-conformer pred identity))
  ([pred transform]
   #(if (pred %) (transform %) ::s/invalid)))

(defn conp [& fs]
  (ss/assert! seq fs)
  (let [[f & fs] (reverse fs)]
    (reduce (fn [acc f]
              (fn [v]
                (let [r (acc v)]
                  (if (invalid? r)
                    ::s/invalid
                    (f r)))))
            f fs)))

(defn min-len [l]
  (pred-conformer (every-pred string? #(>= (count %) l))))

(defn min [i]
  (pred-conformer (every-pred number? #(>= % i))))

(defn parse-long [l]
  (try-conform (Long/parseLong l)))

