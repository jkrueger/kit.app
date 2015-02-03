(ns kit.server.log
  (:require-macros
    [kit.core :refer (! ?)]))

(def bunyan (js/require "bunyan"))

(def request-serializer
  (? bunyan :stdSerializers :req))

(def response-serializer
  (? bunyan :stdSerializers :res))

(def error-serializer
  (? bunyan :stdSerializers :err))

(defn- unknown-level [level]
  (js/Error. (str "Unknown log level: " level)))

(defn level-logger [level level-fn]
  (fn [logger msg & rest]
    (if-let [log-fn (level-fn logger)]
      (if-let [args (seq rest)]
        (let [meta (last args)
              meta (if (map? meta)
                     (clj->js meta)
                     meta)
              args (cons meta (cons msg (butlast args)))]
          (.apply log-fn logger (into-array args)))
        (.call log-fn logger msg))
      (throw (unknown-level level)))))

(def debug (level-logger "info"  #(.-debug %)))
(def info  (level-logger "debug" #(.-info %)))
(def warn  (level-logger "warn"  #(.-warn %)))
(def error (level-logger "error" #(.-error %)))

(defn info-fn [msg f]
  (fn [x]
    (info msg {:x x})
    (f x)))

(defn info-fn2 [msg f]
  (fn [err x]
    (info msg {:err err :x x})
    (f err x)))

(defn set-log-level [logger level]
  (.level logger level))

(defn make [name opts]
  (.createLogger bunyan (clj->js (assoc opts :name name))))
