(defproject com.thelastcitadel/json "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[cheshire "5.1.1"]
                                  [org.clojure/test.generative "0.4.1-SNAPSHOT"]
                                  [org.clojure/data.generators "0.1.0"]]}})
