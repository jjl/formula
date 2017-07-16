(ns irresponsible.formula-test
  (:require [clojure.test :as t]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as tt]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.spec.alpha :as s]
            [clojure.math.numeric-tower :as math]
            [irresponsible.formula :as f]
            [irresponsible.formula.conform :as c]
            [irresponsible.spectra :as ss]))

;; generation utilities

(def nils          (gen/return nil))
(def some*         (gen/such-that (complement nil?) gen/any))
(defn or-nil [gen] (gen/frequency [[1 (gen/return nil)] [10 gen]]))

;; basic functions

(def blank (gen/one-of [nils (gen/return "")]))
(def nonempty-string   (gen/such-that seq gen/string))

(tt/defspec blank?-test
  (prop/for-all [b blank
                 v nonempty-string]
    (and (true?  (f/blank? b))
         (false? (f/blank? v)))))

(def form-fields        (gen/map some* some*))
(def canon-fields       (gen/map some* some*))
(def error-path         (gen/vector some*))
(def errors             (gen/vector some*))
(def error-fields       (gen/map error-path errors {:min-elements 1 :max-elements 3}))
(def base-result        (gen/hash-map :form form-fields :canon canon-fields))
(def invalid-result     (gen/let [r base-result
                                  e error-fields]
                          (assoc r :error e)))
(def dirty-valid-result (gen/fmap #(assoc % :errors []) base-result))
(def valid-result       (gen/frequency [[10 base-result] [1 dirty-valid-result]]))
(def any-result         (gen/one-of [invalid-result dirty-valid-result dirty-valid-result]))

(tt/defspec invalid?-test
  (prop/for-all [i invalid-result
                 v valid-result]
    (and (f/invalid? i)
         (not (f/valid? i))
         (f/valid? v)
         (not (f/invalid? v)))))

(tt/defspec truly-test
  (prop/for-all [x gen/any]
    (true? (f/truly x))))

(tt/defspec nnil?-test
  (prop/for-all [x some*]
    (and (true? (f/nnil? x))
         (false? (f/nnil? nil)))))

(tt/defspec formula?-test
  (prop/for-all [x (gen/such-that #(not (satisfies? f/Formula %)) gen/any)]
    (and (false? (f/formula? x))
         (true? (f/formula? (reify f/Formula))))))

(tt/defspec deform-test
  (prop/for-all [x some*
                 y some*
                 z some*]
    (with-redefs [f/deform* vector]
      (and (= [x y []] (f/deform x y))
           (= [x y z]  (f/deform x y z))))))

(tt/defspec reform-test
  (prop/for-all [x some*
                 y some*]
    (with-redefs [f/reform* vector]
      (= [x y] (f/reform x y)))))

(defn starts? [pre all]
  (= pre (subvec all 0 (count pre))))

(defn ends? [post all]
  (= post (subvec all (- (count all) (count post)))))

(tt/defspec merge-results-test
  (prop/for-all [{fx :form ex :error dx :data :as x} any-result
                 {fy :form ey :error dy :data :as y} any-result]
    (let [{:keys [form error data] :as z} (f/merge-results x y)]
      (t/testing "original keys are retained"
        (t/is (every? #(contains? form %)  (keys fx)))
        (t/is (every? #(contains? data %) (keys dx))))
      (t/testing "secondary keys overwrite"
        (t/is (every? #(= (fy %) (form %)) (keys fy)))
        (t/is (every? #(= (dy %) (data %)) (keys dy))))
      (t/testing "error keys are merged"
        (t/is (every? #(starts? (dx %) (data %)) (keys dx)))
        (t/is (every? #(ends?   (dy %) (data %)) (keys dy)))
        ))))
                  
(tt/defspec field-test
  (prop/for-all [in-key  some*
                 out-key some*
                 error   some*
                 pass    gen/large-integer
                 fail    gen/keyword]
    (let [f1 (f/field {:in-key in-key :out-key out-key :error error
                       :test   integer?
                       :canon  inc
                       :deform #(+ 2 %)
                       :reform #(+ 3 %)})
          pass1 (f/deform f1 {in-key pass})]
          ;; fail1 (f/deform f1 {in-key fail})]
      (t/is (f/valid? pass1))
      (t/is (= (+ 1 pass) (get-in pass1 [:form in-key])))
      (t/is (= (+ 2 pass) (get-in pass1 [:data out-key])))
      ;; (t/is (f/invalid? fail1))
      )))



;; (let [f1 (f/field {:in-key :foo :out-key :bar :error :baz
;;                    :test   integer?
;;                    :canon  inc
;;                    :deform #(+ 2 %)
;;                    :reform #(+ 3 %)})]
;;           (f/deform f1 {:foo :k}))

;; (field-test)

;; TODO: and* or form

;;;;;;;;;;;;;; OLD TESTS BELOW THIS LINE


;; (s/def ::foo (f/field :foo :key :foofoo :conform (c/pred-conformer #{:foo}) :error "ugh"))
;; (s/def ::bar (f/field :bar :key :barbar :conform (c/pred-conformer #{:bar}) :error "eek"))

;; (t/deftest field-test
;;   (t/testing :mandatory ;; todo: more
;;     (t/is (= ::sentinel
;;              (try (f/field :foo)
;;                   (catch Throwable e ::sentinel)))))
;;   (let [t1 (f/field "foo" :error "oops")
;;         t2 (f/field "foo" :error "oops" :conform #(* 2 %))
;;         t3 (f/field "foo" :error "oops" :unform #(* 2 %))
;;         t4 (f/field "foo" :error "oops" :conform #(* 2 %) :unform #(* 2 %))
;;         t5 (f/field "foo" :error "oops" :key "bar")]
;;     (t/testing :conform
;;       (t/is (= {"foo" 1} (s/conform t1 {"foo" 1})))
;;       (t/is (= {"foo" 2} (s/conform t2 {"foo" 1})))
;;       (t/is (= {"foo" 1} (s/conform t3 {"foo" 1})))
;;       (t/is (= {"foo" 2} (s/conform t4 {"foo" 1})))
;;       (t/is (= {"bar" 1} (s/conform t5 {"foo" 1}))))
;;     (t/testing :unform
;;       (t/is (= {"foo" 1} (s/unform t1 {"foo" 1})))
;;       (t/is (= {"foo" 1} (s/unform t2 {"foo" 1})))
;;       (t/is (= {"foo" 2} (s/unform t3 {"foo" 1})))
;;       (t/is (= {"foo" 2} (s/unform t4 {"foo" 1})))
;;       (t/is (= {"foo" 1} (s/unform t5 {"bar" 1}))))
;;     (t/testing :describe
;;       (t/is (= '(field "foo" :error "oops") (s/describe t1)))))
;;   (t/testing :explain
;;     (let [t1 (f/field "foo" :error "oops" :conform c/parse-long)]
;;       (t/is (nil? (s/explain-data t1 {"foo" "123"})))
;;       (t/is (s/explain-data t1 {"foo" "abc"}))
;;       (doseq [in [123 "abc"]]
;;         (t/is (= [{:path ["foo"] :val in :in [] :error "oops"}]
;;                (::s/problems (s/explain-data t1 {"foo" in})))))))
;;   (t/testing :gen)
;;   (t/testing :with-gen))

;; (t/deftest and-test
;;   (t/testing ::mandatory
;;     (t/is (= ::sentinel
;;              (try (f/and)
;;                   (catch Throwable e ::sentinel)))))
;;   (let [f1 (f/field "baz" :error "oops"
;;              :conform (c/pred-conformer #(= :baz %)))
;;         f2 (f/field "quux" :error "oops"
;;              :conform  (c/pred-conformer #(= :quux %)))
;;         t1 (f/and ::foo)
;;         t2 (f/and ::foo f1 f2)]
;;     (t/testing :conform
;;       (t/is (c/invalid? (s/conform t1 {})))
;;       (t/is (c/invalid? (s/conform t1 {:foo :baz})))
;;       (t/is (= {:foofoo :foo} (s/conform t1 {:foo :foo})))
;;       (t/is (c/invalid? (s/conform t2 {})))
;;       (t/is (c/invalid? (s/conform t2 {:foo :foo})))
;;       (t/is (c/invalid? (s/conform t2 {"baz" :baz})))
;;       (t/is (c/invalid? (s/conform t2 {"quux" :quux})))
;;       (t/is (c/invalid? (s/conform t2 {:foo :foo "baz" :baz})))
;;       (t/is (c/invalid? (s/conform t2 {:foo :foo "quux" :quux})))
;;       (t/is (c/invalid? (s/conform t2 {"baz" :baz "quux" :quux})))
;;       (t/is (= {:foofoo :foo "baz" :baz "quux" :quux} (s/conform t2 {:foo :foo "baz" :baz "quux" :quux})))
;;       )
;;     (t/testing :unform)
;;     (t/testing :explain)
;;     (t/testing :describe)
;;     (t/testing :gen)
;;     (t/testing :with-gen)
;;     ))

;; (t/deftest or-test
;;   (t/testing ::mandatory
;;     (t/is (= ::sentinel
;;              (try (f/or)
;;                   (catch Throwable e ::sentinel)))))
;;   (let [f1 (f/field "bar" :error "oops"
;;              :conform (c/pred-conformer #(= :bar %)))
;;         f2 (f/field "baz" :error "oops"
;;              :conform  (c/pred-conformer #(= :baz %)))
;;         t3 (f/or ::foo)
;;         t4 (f/or ::foo f1 f2)]
;;     (t/testing :conform
;;       (t/is (c/invalid? (s/conform t3 {})))
;;       (t/is (c/invalid? (s/conform t3 {:foo :bar})))
;;       (t/is (= {:foofoo :foo} (s/conform t3 {:foo :foo})))
;;       (t/is (c/invalid? (s/conform t4 {})))
;;       (t/is (= {:foofoo :foo} (s/conform t4 {:foo :foo})))
;;       (t/is (= {"bar" :bar} (s/conform t4 {"bar" :bar})))
;;       (t/is (= {"baz" :baz} (s/conform t4 {"baz" :baz})))
;;       )))
;;     ;; (t/testing :unform)
;;     ;; (t/testing :explain)
;;     ;; (t/testing :describe)
;;     ;; (t/testing :gen)
;;     ;; (t/testing :with-gen)
;;     ;; ))

;; (t/deftest compound-test
;;   (t/testing ::mandatory
;;     (t/is (= ::sentinel
;;              (try (f/compound)
;;                   (catch Throwable e ::sentinel)))))
;;   (let [f1 (f/field "bar" :error "oops"
;;              :conform (c/pred-conformer #(= :bar %)))
;;         f2 (f/field "baz" :error "oops"
;;              :conform  (c/pred-conformer #(= :baz %)))
;;         t1 (f/compound :fields [f1] :error "oops" :conform #(% "bar") :unform #(assoc % :unformed? true) :key :quux)
;;         t2 (f/compound :fields [f1 f2] :error "oops" :conform (juxt #(% "bar") #(% "baz")) :unform (juxt #(% "bar") #(% "baz")) :key :quux)
;;         t3 (f/compound :fields [::foo] :error "oops" :conform :foofoo :unform :foofoo :key :quux)
;;         t4 (f/compound :fields [::foo ::bar] :error "oops" :conform (juxt :foofoo :barbar) :unform (juxt :foofoo :barbar) :key :quux)]
;;     (t/testing :conform
;;       (t/is (c/invalid? (s/conform t1 {"bar" :foo})))
;;       (t/is (c/invalid? (s/conform t1 {})))
;;       (t/is (= {:quux :bar} (s/conform t1 {"bar" :bar})))
;;       (t/is (c/invalid? (s/conform t2 {"bar" :bar})))
;;       (t/is (c/invalid? (s/conform t2 {"baz" :baz})))
;;       (t/is (c/invalid? (s/conform t2 {})))
;;       (t/is (= {:quux [:bar :baz]} (s/conform t2 {"bar" :bar "baz" :baz})))
;;       (t/is (c/invalid? (s/conform t3 {})))
;;       (t/is (c/invalid? (s/conform t3 {:foo :bar})))
;;       (t/is (= {:quux :foo} (s/conform t3 {:foo :foo})))
;;       (t/is (c/invalid? (s/conform t4 {})))
;;       (t/is (c/invalid? (s/conform t4 {:foo :foo})))
;;       (t/is (c/invalid? (s/conform t4 {:bar :bar})))
;;       (t/is (= {:quux [:foo :bar]} (s/conform t4 {:foo :foo :bar :bar})))
;;       )
;;     (t/testing :unform)
;;     (t/testing :explain)
;;     (t/testing :describe)
;;     (t/testing :gen)
;;     (t/testing :with-gen)
;;     ))


;; (t/deftest form-test
;;   (t/testing :mandatory
;;     (t/is (= ::sentinel
;;              (try (f/form)
;;                   (catch Throwable e ::sentinel)))))
;;   (let [f1 (f/form :opt [::foo] :conform #(assoc % :conformed? true) :unform #(assoc % :unformed? true) :error "oops")
;;         f2 (f/form :req [::foo] :conform #(assoc % :conformed? true) :unform #(assoc % :unformed? true) :error "oops")
;;         f3 (f/form :req [::foo] :opt [::bar] :conform #(assoc % :conformed? true) :unform #(assoc % :unformed? true) :error "oops")]
;;     (t/is (= {:conformed? true} (s/conform f1 {})))
;;     (t/is (= {:foofoo :foo :conformed? true} (s/conform f1 {:foo :foo})))
;;     (t/is (c/invalid? (s/conform f2 {})))
;;     (t/is (c/invalid? (s/conform f2 {:foo :bar})))
;;     (t/is (= {:foofoo :foo :conformed? true} (s/conform f2 {:foo :foo})))
;;     (t/is (c/invalid? (s/conform f3 {})))
;;     (t/is (c/invalid? (s/conform f3 {:foo :bar})))
;;     (t/is (= {:foofoo :foo :conformed? true} (s/conform f3 {:foo :foo})))
;;     (t/is (= {:foofoo :foo :conformed? true :barbar :bar} (s/conform f3 {:foo :foo :bar :bar})))
;;     ))

;; (t/run-tests)
