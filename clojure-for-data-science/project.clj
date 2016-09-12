(defproject clojure-for-data-science "0.1.0-SNAPSHOT"
  :description "Source code for the book 'Clojure for Data Science'"
  :url "https://github.com/jumarko/clojure-for-data-science"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter/incanter "1.5.5"]]
  :main ^:skip-aot clojure-for-data-science.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :plugins [[lein-gorilla "0.3.6"]])
