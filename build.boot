; vim: syntax=clojure
(set-env! :dependencies (cond
  (= "1.8.0" (System/getenv "BOOT_CLOJURE_VERSION"))
    '[[org.clojure/clojure "1.8.0" :scope "provided"]
      [clojure-future-spec "1.9.0-alpha17"]]
  :else
    '[[org.clojure/clojure "1.9.0-alpha17" :scope "provided"]]))

(set-env!
  :project 'irresponsible/formula
  :version "0.1.0"
  :resource-paths #{"src" "resources"}
  :source-paths #{"src"}
  :dependencies #(into % '[[org.clojure/core.match "0.3.0-alpha4"]
                  [irresponsible/spectra "0.1.0"]
                  [adzerk/boot-test          "1.2.0"  :scope "test"]
                  ;; [criterium                 "0.4.4"  :scope "test"]
                  ;; [binaryage/devtools        "0.8.2"  :scope "test"]
                  ;; [binaryage/dirac           "0.6.6"  :scope "test"]
                  ;; [org.clojure/tools.nrepl   "0.2.12" :scope "test"]
                  ;; [com.cemerick/piggieback   "0.2.1"  :scope "test"]
                  ;; [weasel                    "0.7.0"  :scope "test"]
                  ;; [adzerk/boot-cljs-repl     "0.3.3"  :scope "test"]
                  ;; [powerlaces/boot-figreload "0.1.0-SNAPSHOT" :scope "test"]
                  ;; [adzerk/boot-cljs          "2.0.0-SNAPSHOT" :scope "test"]
                  ]))

(require '[adzerk.boot-test :as t])
;;          '[adzerk.boot-cljs :refer [cljs]]
;;          '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
;;          '[dirac.nrepl]
;;          '[powerlaces.boot-figreload :refer [reload]])

(task-options!
; repl {:port 7001 :middleware '[dirac.nrepl/middleware]}
  pom  {:project (get-env :project)
        :version (get-env :version)
        :description "Webforms, done with spec"
        :url "https://github.com/irresponsible/formula"
        :scm {:url "https://github.com/irresponsible/formula"}
        :license {"MIT" "https://en.wikipedia.org/MIT_License"}}
  push {:tag true
        :ensure-branch "master"
        :ensure-release true
        :ensure-clean true
        :gpg-sign true
        :repo "clojars"}
  target {:dir #{"target"}})

(deftask testing []
  (set-env! :source-paths  #(conj % "test")
            :resource-paths #(conj % "test"))
  identity)
   
(deftask test []
  (comp (testing) (t/test)))
