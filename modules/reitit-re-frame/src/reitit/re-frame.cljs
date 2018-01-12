(ns reitit.re-frame
  (:require [re-frame.core :as re :refer [dispatch]]
            [reitit.frontend :as reitit-frontend]
            [reitit.core :as reitit]))

(re/reg-event-db ::hash-change
  (fn [db _]
    (let [match (reitit-frontend/hash-change (:router (::routes db)) (reitit-frontend/get-hash))]
      (assoc db ::routes (assoc (::routes db)
                                :match match
                                :controllers (if (:enable-controllers? (::routes db))
                                               (reitit-frontend/apply-controllers (:controllers (::routes db)) match)
                                               (:controllers (::routes db))))))))

;; Enables controllers (when used the first time)
;; and applies the changes.
(re/reg-event-db :routes/apply-controllers
  (fn [db _]
    (assoc db ::routes (assoc (::routes db)
                              :controllers (reitit-frontend/apply-controllers (:controllers (::routes db)) (:match (::routes db)))
                              :enable-controllers? true))))

;; Options can enable-controllers right away. Should default be enabled or disabled?
(re/reg-event-fx :routes/init
  (fn [{:keys [db]} [_ router options]]
    ;; TODO: This is tied to onhashchange
    ;; What is user wants to use HTML5 History?
    (set! js/window.onhashchange #(dispatch [::hash-change]))
    {:db (assoc db ::routes (merge {:router router
                                    :match nil
                                    :controllers []
                                    :enable-controllers? false}
                                   options))
     :dispatch [::hash-change]}))

(re/reg-sub ::state
  (fn [db]
    (::routes db)))

(re/reg-sub :routes/match
  :<- [::state]
  (fn [state]
    (:match state)))

(re/reg-sub :routes/data
  :<- [:routes/match]
  (fn [match _]
    (:data match)))

(re/reg-sub :routes/router
  :<- [::state]
  (fn [state]
    (:router state)))

(re/reg-sub :routes/match-by-name
  :<- [:routes/router]
  (fn [router [_ k params]]
    (if router
      (reitit/match-by-name router k params)
      ::not-initialized)))

(defn routed-view
  "Renders the current view component.

  Current view component is the :view from last :controllers entry with :view set."
  []
  (let [view @(re/subscribe [:routes/match])
        ;; Select downmost controller that has view component
        controller (first (filter :view (reverse (:controllers (:data view)))))]
    (if-let [f (:view controller)]
      ;; NOTE: View component is passed the complete params map, not just the controllers params
      ;; :routes/match doesn't know about controller params
      [f (:params view)]
      [:div "No view component defined for route: " (:name (:data view))])))

;;
;; Utils to create hrefs and change URI
;;

(re/reg-sub :routes/href
  (fn [[_ name params :as p]]
    (re/subscribe [:routes/match-by-name name params]))
  (fn [match [_ name params]]
    ;; FIXME: query string
    ;; if last is map? -> append query string
    (if-let [path (:path match)]
      (str "#" path)
      (if (not= ::not-initialized match)
        (js/console.error "Can't create URL for route " (pr-str name) (pr-str params))))))

(defn href
  "Returns href (including #) for given route name and parameters."
  ([name] (href name nil))
  ([name params]
   @(re/subscribe [:routes/href name params])))

(defn update-uri!
  "Creates hash using given name and parameters and changes to browser hash."
  ([name]
   (update-uri! name nil))
  ([name params]
   (set! js/window.location.hash (href name params))))

(re/reg-fx :update-uri
  (fn [v]
    (if v
      (apply update-uri! v))))

(re/reg-event-fx :routes/update-uri
  (fn [_ [_ & v]]
    {:update-uri v}))
