(defproject data-generator "0.2.0-SNAPSHOT"
  :description "CSV trade file data generator."
  :url "https://github.com/chrishowejones/data-generator"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/test.check "0.6.2"]
                 [org.clojure/data.csv "0.1.2"]
                 [clj-time "0.9.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [com.taoensso/timbre "3.3.1"]]
  :main ^:skip-aot data-generator.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev
             {:dependencies [[midje "1.6.3"]]}}
  :uberjar-name "data-generator.jar")
