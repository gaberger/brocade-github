  (defproject brocade-github "0.1.0-SNAPSHOT"
  :description "Brocade Github IO Site Generator"
  :url "http://brocade.github.io"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.5.1"

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.8.51"]
                 [cljsjs/react "0.14.3-0"]
                 [reagent "0.6.0-alpha"]
                 [re-frame "0.7.0-alpha-2"]
                 [tentacles "0.5.1"]
                 [clj-time "0.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [cljs-ajax "0.5.3"]
                 [environ "1.0.2"]
                 [com.taoensso/timbre "4.3.1"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/core.async "0.2.374"
                  :exclusions [org.clojure/tools.reader]]]
  :plugins [[lein-figwheel "0.5.3-1"]
            [lein-cljsbuild "1.1.3" :exclusions [[org.clojure/clojure]]]]

  :source-paths ["src/clj"]
  :main ^:skip-aot github.core

  :clean-targets ^{:protect false} ["resources/public/js/lib" "target"]

  :cljsbuild {:builds {
               :dev {
                :source-paths ["src/cljs"]
                :figwheel     true
                :compiler     {
                               :main                 github.page
                               :asset-path           "js/lib"
                               :output-to            "resources/public/js/brocade_github.js"
                               :output-dir           "resources/public/js/lib"
                               :source-map-timestamp true}}
               :prod {
                :source-paths ["src/cljs"]
                :compiler     {:main                 github.page
                               :asset-path           "js/lib"
                               :output-to            "resources/public/js/brocade_github.js"
                               ;:output-dir           "resources/public/js/lib"
                               :pretty-print false}}}}

  :figwheel {;; :http-server-root "public" ;; default and assumes "resources"
             ;; :server-port 3449 ;; default
             ;; :server-ip "127.0.0.1"

             :css-dirs ["resources/public/css"]             ;; watch and update CSS

             ;; Start an nREPL server into the running figwheel process
             ;; :nrepl-port 7888

             ;; Server Ring Handler (optional)
             ;; if you want to embed a ring handler into the figwheel http-kit
             ;; server, this is for simple ring servers, if this
             ;; doesn't work for you just run your own server :)
             ;; :ring-handler hello_world.server/handler

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             ;; if you want to disable the REPL
             ;; :repl false

             ;; to configure a different figwheel logfile path
             ;; :server-logfile "tmp/logs/figwheel-logfile.log"
             }

    ;:profiles {:dev {:dependencies [[figwheel-sidecar "0.5.3-1"]
    ;                                [com.cemerick/piggieback "0.2.1"]]
    ;                 ;; need to add dev source path here to get user.clj loaded
    ;                 :source-paths ["src/cljs" "dev"]
    ;                 ;; for CIDER
    ;                 ;; :plugins [[cider/cider-nrepl "0.12.0"]]
    ;                 :repl-options {; for nREPL dev you really need to limit output
    ;                                :init (set! *print-length* 50)
    ;                                :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

    )
