(ns wordle.core
  (:require [reagent.core :as r]
            [goog.dom :as gdom]
            [reagent.dom :as rdom]))

(defn create-game [nround goal]
  {:goal goal
   :nround nround
   :round 1
   :past-rounds []
   :game-over? false
   :game-status nil})

(defn test-char [goal goal-char input-char]
  [input-char (cond (= goal-char input-char)  :hit
                    (some #{input-char} goal) :miss
                    :else                     :invalid)])

(defn test-word [goal sample]
  (let [result (map #(test-char goal %1 %2) goal sample)]
    {:result (vec result)
     :match? (every? #(= % :hit) (map last result))}))

(defn update-game-over [state match?]
  (let [{:keys [nround round]} state]
    (assoc state :game-over? (or match? (< nround round)))))

(defn update-game-win-status [state match?]
  (let [{:keys [game-over?]} state]
    (assoc state :game-status (cond (and game-over? match?) :win
                                    (and game-over? (not match?)) :lose
                                    :else nil))))

(defn next-state [state input]
  (let [{:keys [goal]} state
        {:keys [result match?]} (test-word goal input)]
    (-> state
        (update :round inc)
        (update :past-rounds conj result)
        (update-game-over match?)
        (update-game-win-status match?))))

(-> (create-game 3 ["s" "a" "b" "i" "n"])
    (next-state  ["s" "b" "b" "i" "n"])
    (next-state  ["s" "b" "b" "i" "n"])
    ; (next-state  ["s" "a" "b" "i" "n"])
    )

(update-game-over {:nround 1 :round 2} false)

; Views
; example game state
{:goal ["s" "a" "b" "i" "n"],
 :nround 6,
 :round 3,
 :past-rounds [[["s" :hit] ["b" :miss] ["b" :hit] ["i" :hit] ["n" :hit]]
               [["s" :hit] ["a" :hit] ["b" :hit] ["i" :hit] ["n" :hit]]],
 :game-over? true,
 :game-status :win}

(defn px [a] (str a "px"))
(defn css [& args] {:style (apply hash-map args)})


(defn evaluated-row [cell-n row-size words]
  (let [cell-size (-> (/ row-size cell-n) px)
        cell-color {:hit "green" :miss "orange" :invalid "grey"}
        render-cell (fn [i [letter state]]
                      ^{:key i}
                      [:div.col.border.text (css :height cell-size 
                                                 :font-size "3em" 
                                                 :background-color (state cell-color)) 
                       letter])]
    [:div.row.text-center (css :width (px row-size) :color "white")
     (map-indexed render-cell words)]))

(defn input-row 
  ([cell-n row-size] (input-row cell-n row-size ""))
  ([cell-n row-size word]
   (let [cell-size (/ row-size cell-n)]
     [:div.row.text-center (css :width (px row-size))
      (for [i (range cell-n)] ^{:key i}
        [:div.col.border.text (css :height (px cell-size) :font-size "3em") 
         (get word i)])])))


(defn key-board [n enter-cb change-cb]
  (let [word (r/atom "s")
        q "qwertyuiop"
        a "asdfghl"
        z "zxcvbnm"]
    (fn []
      (let [cur-word @word
            on-letter #(fn [_] (when (< (count cur-word) n)
                                 (-> (swap! word str %)
                                     change-cb)))
            on-backspace (fn [_] (->> (apply str (drop-last cur-word)) 
                                      (reset! word)
                                      change-cb))
            on-enter (fn [_] (when (= n (count cur-word)) (enter-cb cur-word)))]
        [:div [:h3 "KEYBOARD STATE : -> [" cur-word "]"]
         [:div.row (for [l q] ^{:key l} [:div.col.border {:on-click (on-letter l)} (str l)]) ]
         [:div.row (for [l a] ^{:key l} [:div.col.border {:on-click (on-letter l)} (str l)]) ]
         [:div.row (for [l z] ^{:key l} [:div.col.border {:on-click (on-letter l)} (str l)]) ]
         [:div.row 
          [:div.col.border {:on-click on-backspace} "backspace"]
          [:div.col.border {:on-click on-enter}"enter"]]]))))

(defn game []
  (let [word (r/atom "")]
    (fn []
      [:div

    [evaluated-row 5 500 [["s" :hit] ["g" :invalid] ["b" :hit] ["n" :miss] ["n" :hit]]]
       [input-row 5 500 @word]
       [key-board 5 js/alert #(reset! word %)]
       ]
      )
    )
  )

(defn app []
  [:div.container
   [:div.p-3 [:h1.text-center "Wordle Cljs"]]
   [game]

   ; [:div
   ;  [evaluated-row 5 500 [["s" :hit] ["g" :invalid] ["b" :hit] ["n" :miss] ["n" :hit]]]
   ;  [input-row 5 500 "sab"]
   ;  [input-row 5 500]
   ;  [input-row 5 500]
   ;  ]

   ; [key-board 5 js/alert #(println "changed :" %)]


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
