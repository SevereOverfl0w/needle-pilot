(ns io.dominic.needle-pilot.service-locator
  "Using objection as a service locator-style pattern."
  (:require
    [reitit.core :as reitit]
    reitit.ring
    [ring.adapter.jetty :as jetty]
    [objection.core :as obj]))

(defn db
  [db-spec]
  (locking @#'obj/global-lock
    (or (obj/object [`db db-spec])
        (obj/register (do (println "Making a db") (Object.))
                      {:name (str `db)
                       :aliases [[`db db-spec]]
                       :data db-spec
                       :stopfn (fn [_] (println "Going to stop"))}))))

(obj/defsingleton ::sse
  (obj/register
    (do (println "Making a sse") (Object.))
    {:name "SSE"
     :stopfn (fn [_] (println "Going to stop sse"))}))

(defn sse
  []
  (obj/singleton ::sse))

(def routes
  [
   ["/admin" {:name :admin
              ;; config could be determined based on incoming vhost and cached.
              :handler (fn [{:keys [config] :as req}]
                         {:status 200
                          :body (str (db (:db config)))})}]

   ["/" {:name :index
         :handler (fn [req]
                    {:status 200
                     :body (str (sse))})}]])

(extend-protocol obj/IAutoStoppable
  org.eclipse.jetty.util.component.AbstractLifeCycle
  (stop [this]
    (.stop this)
    (.join this)))

(defn http
  [handler opts]
  (let [opts (merge {:join? false :port 0} opts)]
    (locking @#'obj/global-lock
      (or (obj/object [`http opts])
          (let [server (ring.adapter.jetty/run-jetty handler opts)]
            (obj/register server
                          {:name (str "HTTP server on " (.getURI server))
                           :aliases [[`http opts]]
                           :data {:opts opts
                                  :handler handler}
                           :stopfn #(.stop %)}))))))

(comment
  (http (let [handler (reitit.ring/ring-handler
                        (reitit.ring/router routes))]
          (fn [req]
            (handler (merge {:config {:db {:host "localhost"}} } req))))
        {})
  (obj/status)
  (obj/stop! [`http {:join? false :port 0}])
  (obj/stop-all!)
  )
