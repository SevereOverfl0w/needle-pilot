(ns io.dominic.needle-pilot.sub-system
  (:require
    [integrant.core :as ig]
    [ring.adapter.jetty :as jetty]))

(defmethod ig/init-key ::sub-system
  [_ sub-system]
  (ig/init sub-system))

(defmethod ig/halt-key! ::sub-system
  [_ system]
  (ig/halt! system))

(defmethod ig/suspend-key! ::sub-system
  [_ system]
  (ig/suspend! system))

(defmethod ig/resume-key ::sub-system
  [k value old-value old-impl]
  (if (= value old-value)
    (ig/resume old-impl)
    (ig/init-key k value)))

(defmethod ig/init-key ::http
  [_ {:keys [handler]}]
  (jetty/run-jetty handler {:port 0
                            :join? false}))

(defmethod ig/halt-key! ::http
  [_ srv]
  (.stop srv))

(defn foo
  [{:keys [sub-system] :as req}]
  (prn "Hello?" sub-system)
  {:status 200
   :body "Hello, world"})

(defn wrap-sub-system
  [handler sub-system]
  (fn [req]
    (handler (assoc req :sub-system sub-system))))

(defmethod ig/init-key ::handler
  [_ {:keys [sub-system handler]}]
  (wrap-sub-system (requiring-resolve handler) sub-system))

(defmethod ig/init-key ::db
  [_ opts]
  (do (println "Object<>") (Object.)))

(def system-config
  {;; consumer of memoized
   ::handler {:handler `foo
              :sub-system (ig/ref ::sub-system)}
   ;; places
   ::http {:handler (ig/ref ::handler)}
   ;; single per system, scoped
   ::sub-system {::db {:url "localhost"}}})

(comment
  (def system (ig/init system-config))
  (ig/halt! system)
  (str (.getURI (::http system)))
  )
