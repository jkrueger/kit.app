(ns kit.test.server.component
  (:require
    [cljs.core.async :as async :refer (<!)]
    [kit.algo.pred :refer (eq?)]
    [kit.server.component
     :refer (Lifecycle
             up down
             <up <down
             component
             system)]
    [latte.chai :refer (expect)]
    [the.parsatron :as p])
  (:require-macros
    [cljs.core.async.macros :refer (go)]
    [kit.async :refer (<?)]
    [latte.core :refer (describe it)]
    [the.parsatron :refer (defparser >>)]))

(def ^:dynamic *start-order* (atom []))
(def ^:dynamic *stop-order*  (atom []))

(defprotocol Flavor
  (flavor [_]))

(defrecord GrannySmith []
  Lifecycle
  (up [this next]
    (swap! *start-order* conj :apple)
    (next))
  (down [this next]
    (swap! *stop-order* conj :apple)
    (next))
  Flavor
  (flavor [_] "sour"))

(defrecord Valencia []
  Lifecycle
  (up [this next]
    (swap! *start-order* conj :orange)
    (next))
  (down [this next]
    (swap! *stop-order* conj :orange)
    (next))
  Flavor
  (flavor [_] "sweet"))

(defrecord Fruits [apple orange]
  Lifecycle
  (up [this next]
    (swap! *start-order* conj :fruits)
    (next))
  (down [this next]
    (swap! *stop-order* conj :fruits)
    (next)))

(defn- make-fruits [& opts]
  (with (map->Fruits (apply hash-map opts))
        :apple :orange))

(defn check-flavors [fruits] 
  (expect (flavor (:apple fruits))
          :to.equal
          "sour")
  (expect (flavor (:orange fruits))
          :to.equal
          "sweet"))

(def test-system-1
  (system
    :apple  (GrannySmith.)
    :orange (Valencia.)
    :fruits (make-fruits)))

(defparser start-order-checker []
  (>> (p/times 2
               (p/choice (p/token (eq? :apple))
                         (p/token (eq? :orange))))
      (p/token (eq? :fruits))))

(defparser stop-order-checker []
  (>> (p/token (eq? :fruits))
      (p/times 2
               (p/choice (p/token (eq? :apple))
                         (p/token (eq? :orange))))))

(describe "components"
  
  (it "can be started" [done]
    (up test-system-1
        (fn [system]
          (-> system
              (component :fruits)
              (check-flavors))
          (done))))
  
  (it "can be started in a go block" [done]
    (go
      (try
        (-> (<up test-system-1)
            (<?)
            (component :fruits)
            (check-flavors))
        (done)
        (catch js/Error e
          (done e)))))
  
  (it "respects dependencies" [done]
    (go
      (try
        (binding [*start-order* (atom [])
                  *stop-order*  (atom [])]
          (let [fruits (<? (<up test-system-1))
                _      (<? (<down fruits))]
            (p/run (start-order-checker) @*start-order*)
            (p/run (stop-order-checker) @*stop-order*)
            (done))
          (catch js/Error e
            (done e)))))))
