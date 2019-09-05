(ns io.dominic.needle-pilot.decomplect-identity-integrant
  "Integrant ties together identity (keywords) with implementations (symbols)
  this is an attempt at exploring what the alternative could look like."
  (:require
    [reitit.core :as reitit]
    [ring.adapter.jetty :as jetty]
    [weavejester.dependency :as dep]))

(defn db [] "I am the db")

(defn sse []
  "I am sse")

(def routes
  (reitit/router
    [
     ["/admin" {:name :admin
                :deps #{:db}
                ::handler (fn [{:keys [db]} req]
                            {:status 200
                             :body db})}]
     
     ["/" {:name :index
           :deps #{:sse}
           ::handler (fn [{:keys [sse db]} req]
                       {:status 200
                        :body (str sse "--[" db "]")})}]]))

(def handler
  {:init (fn [routes]
           {:routes @(requiring-resolve routes)})
   :deps (fn [{:keys [routes]}]
           (into #{} (mapcat (comp :deps second)
                             (reitit/routes routes))))
   :start (fn [{:keys [routes]} deps]
            (fn [req]
              (let [match (reitit/match-by-path routes (:uri req))
                    {::keys [handler]} (:data match)]
                (if match
                  (handler (select-keys deps (:deps (:data match))) req)
                  {:status 404
                   :body "<img src=\"https://http.cat/404\" />"}))))})

(comment
  (:deps (:data (reitit/match-by-path routes "/admin"))))

(comment
  ((:deps handler) ((:init handler) `routes)))

(defprotocol IStoppable
  :extend-via-metadata true
  (stop [this]))

(extend-protocol IStoppable
  java.lang.AutoCloseable
  (stop [this]
    (println "Closing" this)
    (.close this))
  java.lang.Object
  (stop [this])
  nil
  (stop [this]))

(extend-protocol IStoppable
  org.eclipse.jetty.util.component.AbstractLifeCycle
  (stop [this]
    (.stop this)
    (.join this)))

(defrecord Ref [target])
(defn ref?
  [x]
  (instance? Ref x))

(set! *data-readers* (merge *data-readers* {'dii/ref #'->Ref}))

(def system
  `{;; identitiese are local:
    :db
    ;; implementations are symbols, they are called like sexps
    (db)

    ;; dependencies are tagged literals by identity
    :http (jetty/run-jetty #dii/ref :handler {:join? false :port 0})


    ;; handler has localized dependencies, so ends up with dependencies not listed here:
    :handler (handler routes)

    ;;
    :sse (sse)
    :no-ref (sse)})

(defn system-graph
  [system]
  (reduce-kv
    (fn [g k v]
      (reduce #(dep/depend %1 k %2)
              g
              ((:deps v) (::init v))))
    (dep/graph)
    system))

(defn expand-system
  [system]
  (reduce-kv (fn [system k v]
               (assoc system k
                      (let [impl @(requiring-resolve (first v))]
                        (if (map? impl)
                          (merge {::args (next v)} impl)
                          {:init (fn [& args] args)
                           ::args (next v)
                           :deps (fn [_] (map :target (filter ref? v)))
                           :start (fn [args deps]
                                    (prn args deps)
                                    (apply impl (map
                                                  (fn [x]
                                                    (if (ref? x)
                                                      (get deps (:target x))
                                                      x))
                                                  args)))}))))
             {}
             system))

(defn init-system
  [system]
  (reduce-kv (fn [system k v]
               (assoc-in system [k ::init] (apply (:init v) (::args v))))
             system
             system))

(defn start-system
  [system-graph system]
  (reduce (fn [system k]
            (assoc-in system
                      [k ::start]
                      ((get-in system [k :start])
                       (get-in system [k ::init])
                       (zipmap (dep/immediate-dependencies system-graph k)
                               (map #(get-in system [% ::start])
                                    (dep/immediate-dependencies system-graph k))))))
          system
          (sort
            (dep/topo-comparator system-graph)
            (keys system))))

(defn stop-system
  [system-graph started-system]
  (run! stop
        (reverse
          (map val
               (sort-by
                 key
                 (dep/topo-comparator system-graph)
                 (reduce-kv (fn [system k v] (assoc system k (::start v)))
                            {}
                            started-system))))))

(comment
  (def initted (init-system (expand-system system)))
  (def started-system (start-system (system-graph initted) initted))

  (stop-system (system-graph initted) started-system)
  
  (str (.getURI (::start (:http started-system)))))
