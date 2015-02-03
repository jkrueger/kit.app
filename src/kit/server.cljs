(ns kit.server
  (:require
    [kit.async :as a]
    [kit.server.config :as conf]))

(defn boot [opts f]
  (conf/init (:config opts "config.json"))
  (f))

(def <boot (partial a/lift boot))
