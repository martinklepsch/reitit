(ns reitit.perf-test
  (:require [criterium.core :as cc]
            [reitit.core :as reitit]
            [reitit.perf-utils :refer :all]

            [bidi.bidi :as bidi]
            [compojure.api.sweet :refer [api routes GET]]
            [compojure.api.routes :as routes]
            [ataraxy.core :as ataraxy]

            [io.pedestal.http.route.definition.table :as table]
            [io.pedestal.http.route.map-tree :as map-tree]
            [io.pedestal.http.route.router :as pedestal]
            [io.pedestal.http.route :as route]
            [reitit.core :as r]))

;;
;; start repl with `lein perf repl`
;; perf measured with the following setup:
;;
;; Model Name:            MacBook Pro
;; Model Identifier:      MacBookPro11,3
;; Processor Name:        Intel Core i7
;; Processor Speed:       2,5 GHz
;; Number of Processors:  1
;; Total Number of Cores: 4
;; L2 Cache (per Core):   256 KB
;; L3 Cache:              6 MB
;; Memory:                16 GB
;;

(def bidi-routes
  ["/" [["auth/login" :auth/login]
        [["auth/recovery/token/" :token] :auth/recovery]
        ["workspace/" [[[:project "/" :page] :workspace/page]]]]])

(def compojure-api-routes
  (routes
    (GET "/auth/login" [] {:name :auth/login} (constantly ""))
    (GET "/auth/recovery/token/:token" [] {:name :auth/recovery} (constantly ""))
    (GET "/workspace/:project/:page" [] {:name :workspace/page} (constantly ""))))

(def compojure-api-request
  {:compojure.api.request/lookup (routes/route-lookup-table (routes/get-routes (api compojure-api-routes)))})

(def ataraxy-routes
  (ataraxy/compile
    '{["/auth/login"] [:auth/login]
      ["/auth/recovery/token/" token] [:auth/recovery token]
      ["/workspace/" project "/" token] [:workspace/page project token]}))

(def pedestal-routes
  (table/table-routes
    [["/auth/login" :get (constantly "") :route-name :auth/login]
     ["/auth/recovery/token/:token" :get (constantly "") :route-name :auth/recovery]
     ["/workspace/:project/:page" :get (constantly "") :route-name :workspace/page]]))

(def pedestal-router
  (map-tree/router
    pedestal-routes))

(def pedestal-url-for (route/url-for-routes pedestal-routes))

(def reitit-routes
  (reitit/router
    [["/auth/login" :auth/login]
     ["/auth/recovery/token/:token" :auth/recovery]
     ["/workspace/:project/:page" :workspace/page]]))

(defn routing-test1 []

  (suite "static route")

  ;; 1800 µs
  (title "bidi")
  (let [call #(bidi/match-route bidi-routes "/auth/login")]
    (assert (call))
    (cc/quick-bench
      (dotimes [_ 1000]
        (call))))

  ;; 1400 µs
  (title "ataraxy")
  (let [call #(ataraxy/matches ataraxy-routes {:uri "/auth/login"})]
    (assert (call))
    (cc/quick-bench
      (dotimes [_ 1000]
        (call))))

  ;; 1200 µs
  (title "pedestal - map-tree => prefix-tree")
  (let [call #(pedestal/find-route pedestal-router {:path-info "/auth/login" :request-method :get})]
    (assert (call))
    (cc/quick-bench
      (dotimes [_ 1000]
        (call))))

  ;; 1400 µs
  (title "compojure-api")
  (let [call #(compojure-api-routes {:uri "/auth/login", :request-method :get})]
    (assert (call))
    (cc/quick-bench
      (dotimes [_ 1000]
        (call))))

  ;; 3.5 µs (300-500x)
  (title "reitit")
  (let [call #(reitit/match-by-path reitit-routes "/auth/login")]
    (assert (call))
    (cc/quick-bench
      (dotimes [_ 1000]
        (call)))))

(defn routing-test2 []

  (suite "wildcard route")

  ;; 12800 µs
  (title "bidi")
  (let [call #(bidi/match-route bidi-routes "/workspace/1/1")]
    (assert (call))
    (cc/quick-bench
      (dotimes [_ 1000]
        (call))))

  ;; 2800 µs
  (title "ataraxy")
  (let [call #(ataraxy/matches ataraxy-routes {:uri "/workspace/1/1"})]
    (assert (call))
    (cc/quick-bench
      (dotimes [_ 1000]
        (call))))

  ;; 2300 µs
  (title "pedestal - map-tree => prefix-tree")
  (let [call #(pedestal/find-route pedestal-router {:path-info "/workspace/1/1" :request-method :get})]
    (assert (call))
    (cc/quick-bench
      (dotimes [_ 1000]
        (call))))

  ;; 3800 µs
  (title "compojure-api")
  (let [call #(compojure-api-routes {:uri "/workspace/1/1", :request-method :get})]
    (assert (call))
    (cc/quick-bench
      (dotimes [_ 1000]
        (call))))

  ;; 710 µs (3-18x)
  ;; 540 µs (4-23x) -23% prefix-tree-router
  (title "reitit")
  (let [call #(reitit/match-by-path reitit-routes "/workspace/1/1")]
    (assert (call))
    (cc/quick-bench
      (dotimes [_ 1000]
        (call)))))

(defn reverse-routing-test []

  (suite "reverse routing")

  ;; 2.0µs (-59%)
  (title "bidi")
  (let [call #(bidi/path-for bidi-routes :workspace/page :project "1" :page "1")]
    (assert (= "/workspace/1/1" (call)))
    (cc/quick-bench
      (call)))

  (title "ataraxy doesn't support reverse routing :(")

  ;; 3.8µs (-22%)
  (title "pedestal - map-tree => prefix-tree")
  (let [call #(pedestal-url-for :workspace/page :path-params {:project "1" :page "1"})]
    (assert (= "/workspace/1/1" (call)))
    (cc/quick-bench
      (call)))

  ;; 4.9µs
  (title "compojure-api")
  (let [call #(routes/path-for* :workspace/page compojure-api-request {:project "1", :page "1"})]
    (assert (= "/workspace/1/1" (call)))
    (cc/quick-bench
      (call)))

  ;; 850ns (-83%)
  (title "reitit")
  (let [call #(reitit/match-by-name reitit-routes :workspace/page {:project "1", :page "1"})]
    (assert (call))
    (cc/quick-bench
      (call))))

(comment
  (routing-test1)
  (routing-test2)
  (reverse-routing-test))
