(ns wordle.core
  (:require [reagent.core :as r]
            [goog.dom :as gdom]
            [reagent.dom :as rdom]))

(defn eval-char [goal x y]
  {:letter y
   :state (cond
            (= x y)          :hit
            (some #{y} goal) :miss
            :else            :not-valid)})

(defn eval-word [goal sample]
  (let [result (map #(eval-char goal %1 %2) goal sample)]
    {:evaluation result
     :matches? (every? #(= % :hit) (map :state result))}))


(defn px [a] (str a "px"))

(defn create-row [cell-n row-size word]
  (let [cell-size (/ row-size cell-n)]
    [:div.row.text-center {:style {:width (px row-size)}}
     (for [i (range cell-n)]
       ^{:key i}
       [:div.col.border.text {:style {:height (px cell-size)
                                      :font-size "3em"}} 
         (get word i)])]))

(map list [1 2 3 4 5] [1 2 4])


(defn app []
  [:div.container
   [:div.p-3 [:h1.text-center "Wordle Cljs"]]
    [create-row 5 500 "SABIN"]
   ])

(defn mount-app [] 
  (when-let [el (gdom/getElement "app")] 
    (rdom/render [app] el)))

(defn init []
  (println "----------Initialize App---------------")
  (mount-app))


(defn ^:dev/before-load stop []
  (js/console.log "stop"))

(defn ^:dev/after-load start []
  (js/console.log "start")
  (mount-app))

(comment
  (init)
  (js/alert "hello")
  (println "this is a comment")
  )
