(ns irresponsible.formula-test
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [irresponsible.formula :as f]
            [irresponsible.formula.conform :as c]
            [irresponsible.spectra :as ss]))

(s/def ::foo (f/field :foo :key :foofoo :conform (c/pred-conformer #{:foo}) :error "ugh"))
(s/def ::bar (f/field :bar :key :barbar :conform (c/pred-conformer #{:bar}) :error "eek"))

(t/deftest invalid?-test
  (t/is (f/invalid? ::s/invalid))
  (doseq [i [1 1.2 \a "a" :a 'a [] {} ()]]
    (t/testing i
      (t/is (not (f/invalid? i))))))

(t/deftest try-conform-test
  (t/is (c/invalid? (c/try-conform (throw (ex-info "" {})))))
  (t/is (= ::sentinel  (c/try-conform ::sentinel))))

(t/deftest keep-conformer-test
  (t/is (c/invalid? ((c/keep-conformer (constantly nil)) 123)))
  (t/is (= 123 ((c/keep-conformer identity) 123))))

(t/deftest pred-conformer-test
  (let [t1 (c/pred-conformer integer?)
        t2 (c/pred-conformer integer? #(* 2 %))]
    (doseq [i ["" [] {} () :a 'a 1.23]]
      (t/is (c/invalid? (t1 i))))
    (t/is (= 2 (t1 2)))
    (t/is (= 4 (t2 2)))))

(t/deftest conp-test
  (let [t1 (c/conp (c/min 10) c/parse-long)]
    (t/is (= 123 (t1 "123")))
    (t/is (f/invalid? (t1 "abc")))
    (t/is (f/invalid? (t1 "09")))))

;; (t/deftest field-map-test
;;   (let [f1 (f/field "foo" :error "bar")]
;;     (t/is (= {:foo f1} (f/field-map {:foo f1})))
;;     (t/is (= {::foo (s/get-spec ::foo)} (f/field-map ::foo)))))

(t/deftest field-test
  (t/testing :mandatory ;; todo: more
    (t/is (= ::sentinel
             (try (f/field :foo)
                  (catch Throwable e ::sentinel)))))
  (let [t1 (f/field "foo" :error "oops")
        t2 (f/field "foo" :error "oops" :conform #(* 2 %))
        t3 (f/field "foo" :error "oops" :unform #(* 2 %))
        t4 (f/field "foo" :error "oops" :conform #(* 2 %) :unform #(* 2 %))
        t5 (f/field "foo" :error "oops" :key "bar")]
    (t/testing :conform
      (t/is (= {"foo" 1} (s/conform t1 {"foo" 1})))
      (t/is (= {"foo" 2} (s/conform t2 {"foo" 1})))
      (t/is (= {"foo" 1} (s/conform t3 {"foo" 1})))
      (t/is (= {"foo" 2} (s/conform t4 {"foo" 1})))
      (t/is (= {"bar" 1} (s/conform t5 {"foo" 1}))))
    (t/testing :unform
      (t/is (= {"foo" 1} (s/unform t1 {"foo" 1})))
      (t/is (= {"foo" 1} (s/unform t2 {"foo" 1})))
      (t/is (= {"foo" 2} (s/unform t3 {"foo" 1})))
      (t/is (= {"foo" 2} (s/unform t4 {"foo" 1})))
      (t/is (= {"foo" 1} (s/unform t5 {"bar" 1}))))
    (t/testing :describe
      (t/is (= '(field "foo" :error "oops") (s/describe t1)))))
  (t/testing :explain
    (let [t1 (f/field "foo" :error "oops" :conform c/parse-long)]
      (t/is (nil? (s/explain-data t1 {"foo" "123"})))
      (t/is (s/explain-data t1 {"foo" "abc"}))
      (doseq [in [123 "abc"]]
        (t/is (= [{:path ["foo"] :val in :in [] :error "oops"}]
               (::s/problems (s/explain-data t1 {"foo" in})))))))
  (t/testing :gen)
  (t/testing :with-gen))

(t/deftest and-test
  (t/testing ::mandatory
    (t/is (= ::sentinel
             (try (f/and)
                  (catch Throwable e ::sentinel)))))
  (let [f1 (f/field "baz" :error "oops"
             :conform (c/pred-conformer #(= :baz %)))
        f2 (f/field "quux" :error "oops"
             :conform  (c/pred-conformer #(= :quux %)))
        t1 (f/and ::foo)
        t2 (f/and ::foo f1 f2)]
    (t/testing :conform
      (t/is (c/invalid? (s/conform t1 {})))
      (t/is (c/invalid? (s/conform t1 {:foo :baz})))
      (t/is (= {:foofoo :foo} (s/conform t1 {:foo :foo})))
      (t/is (c/invalid? (s/conform t2 {})))
      (t/is (c/invalid? (s/conform t2 {:foo :foo})))
      (t/is (c/invalid? (s/conform t2 {"baz" :baz})))
      (t/is (c/invalid? (s/conform t2 {"quux" :quux})))
      (t/is (c/invalid? (s/conform t2 {:foo :foo "baz" :baz})))
      (t/is (c/invalid? (s/conform t2 {:foo :foo "quux" :quux})))
      (t/is (c/invalid? (s/conform t2 {"baz" :baz "quux" :quux})))
      (t/is (= {:foofoo :foo "baz" :baz "quux" :quux} (s/conform t2 {:foo :foo "baz" :baz "quux" :quux})))
      )
    (t/testing :unform)
    (t/testing :explain)
    (t/testing :describe)
    (t/testing :gen)
    (t/testing :with-gen)
    ))

(t/deftest or-test
  (t/testing ::mandatory
    (t/is (= ::sentinel
             (try (f/or)
                  (catch Throwable e ::sentinel)))))
  (let [f1 (f/field "bar" :error "oops"
             :conform (c/pred-conformer #(= :bar %)))
        f2 (f/field "baz" :error "oops"
             :conform  (c/pred-conformer #(= :baz %)))
        t3 (f/or ::foo)
        t4 (f/or ::foo f1 f2)]
    (t/testing :conform
      (t/is (c/invalid? (s/conform t3 {})))
      (t/is (c/invalid? (s/conform t3 {:foo :bar})))
      (t/is (= {:foofoo :foo} (s/conform t3 {:foo :foo})))
      (t/is (c/invalid? (s/conform t4 {})))
      (t/is (= {:foofoo :foo} (s/conform t4 {:foo :foo})))
      (t/is (= {"bar" :bar} (s/conform t4 {"bar" :bar})))
      (t/is (= {"baz" :baz} (s/conform t4 {"baz" :baz})))
      )))
    ;; (t/testing :unform)
    ;; (t/testing :explain)
    ;; (t/testing :describe)
    ;; (t/testing :gen)
    ;; (t/testing :with-gen)
    ;; ))

(t/deftest compound-test
  (t/testing ::mandatory
    (t/is (= ::sentinel
             (try (f/compound)
                  (catch Throwable e ::sentinel)))))
  (let [f1 (f/field "bar" :error "oops"
             :conform (c/pred-conformer #(= :bar %)))
        f2 (f/field "baz" :error "oops"
             :conform  (c/pred-conformer #(= :baz %)))
        t1 (f/compound :fields [f1] :error "oops" :conform #(% "bar") :unform #(assoc % :unformed? true) :key :quux)
        t2 (f/compound :fields [f1 f2] :error "oops" :conform (juxt #(% "bar") #(% "baz")) :unform (juxt #(% "bar") #(% "baz")) :key :quux)
        t3 (f/compound :fields [::foo] :error "oops" :conform :foofoo :unform :foofoo :key :quux)
        t4 (f/compound :fields [::foo ::bar] :error "oops" :conform (juxt :foofoo :barbar) :unform (juxt :foofoo :barbar) :key :quux)]
    (t/testing :conform
      (t/is (c/invalid? (s/conform t1 {"bar" :foo})))
      (t/is (c/invalid? (s/conform t1 {})))
      (t/is (= {:quux :bar} (s/conform t1 {"bar" :bar})))
      (t/is (c/invalid? (s/conform t2 {"bar" :bar})))
      (t/is (c/invalid? (s/conform t2 {"baz" :baz})))
      (t/is (c/invalid? (s/conform t2 {})))
      (t/is (= {:quux [:bar :baz]} (s/conform t2 {"bar" :bar "baz" :baz})))
      (t/is (c/invalid? (s/conform t3 {})))
      (t/is (c/invalid? (s/conform t3 {:foo :bar})))
      (t/is (= {:quux :foo} (s/conform t3 {:foo :foo})))
      (t/is (c/invalid? (s/conform t4 {})))
      (t/is (c/invalid? (s/conform t4 {:foo :foo})))
      (t/is (c/invalid? (s/conform t4 {:bar :bar})))
      (t/is (= {:quux [:foo :bar]} (s/conform t4 {:foo :foo :bar :bar})))
      )
    (t/testing :unform)
    (t/testing :explain)
    (t/testing :describe)
    (t/testing :gen)
    (t/testing :with-gen)
    ))


(t/deftest form-test
  (t/testing :mandatory
    (t/is (= ::sentinel
             (try (f/form)
                  (catch Throwable e ::sentinel)))))
  (let [f1 (f/form :opt [::foo] :conform #(assoc % :conformed? true) :unform #(assoc % :unformed? true) :error "oops")
        f2 (f/form :req [::foo] :conform #(assoc % :conformed? true) :unform #(assoc % :unformed? true) :error "oops")
        f3 (f/form :req [::foo] :opt [::bar] :conform #(assoc % :conformed? true) :unform #(assoc % :unformed? true) :error "oops")]
    (t/is (= {:conformed? true} (s/conform f1 {})))
    (t/is (= {:foofoo :foo :conformed? true} (s/conform f1 {:foo :foo})))
    (t/is (c/invalid? (s/conform f2 {})))
    (t/is (c/invalid? (s/conform f2 {:foo :bar})))
    (t/is (= {:foofoo :foo :conformed? true} (s/conform f2 {:foo :foo})))
    (t/is (c/invalid? (s/conform f3 {})))
    (t/is (c/invalid? (s/conform f3 {:foo :bar})))
    (t/is (= {:foofoo :foo :conformed? true} (s/conform f3 {:foo :foo})))
    (t/is (= {:foofoo :foo :conformed? true :barbar :bar} (s/conform f3 {:foo :foo :bar :bar})))
    ))

;; (t/run-tests)
