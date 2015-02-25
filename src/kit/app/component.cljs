(ns kit.app.component
  (:require
    [cljs.core.async :as async :refer (<!)]
    [kit.algo.graph :as graph]
    [kit.async :refer (lift)])
  (:require-macros
    [cljs.core.async.macros :refer (go)]
    [kit.async :refer (<?)]))

(defprotocol Lifecycle
  (up [_ next])
  (down [_ next]))

(defn up* [x f]
  (up x #(f (or %1 x))))

(defn down* [x f]
  (down x #(f (or %1 x))))

(def <up   (partial lift up*))
(def <down (partial lift down*))

(defn- inject [component instances]
  (reduce
    (fn [component field]
      (let [x (get instances field)]
        (assoc component field x)))
    component
    (::dependencies (meta component))))

(defn to-dependencies [[k component]]
  (vector k (::dependencies (meta component))))

(defn- dependency-sort [components]
  (let [nodes (set (keys components))
        edges (into {} (map to-dependencies components))]
    (graph/topsort (graph/make nodes edges))))

(defn- <component-up [component system]
  (-> component
      (inject (:components system))
      (<up)))

(defn- <component-down [component system]
  ;; TODO: could check here if system was really started
  (<down component))

(defn component [system k]
  (get-in system [:components k]))

(defn set-component [system k component]
  (assoc-in system [:components k] component))

(defn- change-states [<f nodes system next]
  (go
    (try
      (next
        (loop [nodes  nodes
               system system]
          (if-let [node (first nodes)]
            (let [component (component system node)
                  started   (<? (<f component system))
                  system    (set-component system node started)]
              (recur (rest nodes) system))
            system)))
      (catch js/Error e
        (next e)))))

(def all-up   (partial change-states <component-up))
(def all-down (partial change-states <component-down))

(defrecord System [components]
  Lifecycle
  (up [this next]
    (let [sorted (reverse (dependency-sort components))]
      (all-up sorted this next)))
  (down [this next]
    (let [sorted (dependency-sort components)]
      (all-down sorted this next))))

(defn system [& kvs]
  (System. (apply hash-map kvs)))

(defn with [component & kvs]
  (let [m (set kvs)]
    (vary-meta component assoc ::dependencies m)))
