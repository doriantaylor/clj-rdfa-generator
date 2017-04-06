(defproject rdfa-generator "0.1.0-SNAPSHOT"
  :description "Generate (X)HTML+RDFa from RDF graphs"
  :url "https://github.com/doriantaylor/clj-rdfa-generator"
  :license {:name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  ;;:plugins [[lein-git-deps "0.0.2"]]
  ;;:git-dependencies [["https://github.com/shellac/java-rdfa.git"]]
  ;; :jvm-opts ["-Dcom.sun.management.jmxremote"
  ;;            "-Dcom.sun.management.jmxremote.ssl=false"
  ;;            "-Dcom.sun.management.jmxremote.authenticate=false"
  ;;            "-Dcom.sun.management.jmxremote.port=43210"]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.0.8"]
                 ;;[edu.ucdenver.ccp/kr-core "1.4.17"]
                 [org.apache.jena/jena-core "3.0.1"]
                 [org.apache.jena/jena-arq "3.0.1"]
                 [org.apache.jena/jena-tdb "3.0.1"]
                 [org.apache.jena/jena-iri "3.0.1"]
                 ;;[net.rootdev/java-rdfa "0.4.3-SNAPSHOT"]
                 ;;[http-kit "2.2.0"]
                 ])
