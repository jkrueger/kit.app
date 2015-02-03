(ns kit.app.config
  (:refer-clojure :exclude (get))
  (:require
    [clojure.string :as str]))

(def config (js/require "nconf"))

(defn init
  ([path]
     (-> config
         (.argv)
         (.file path)))
  ([]
     (init "config.json")))

(defn- to-key [ks]
  (str/join ":" (map name ks)))

(defn get [& keys]
  (let [item (.get config (to-key keys))]
    (js->clj item :keywordize-keys true)))
