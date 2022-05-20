(ns wordle.core
  (:require [reagent.core :as r]
            [goog.dom :as gdom]
            [reagent.dom :as rdom]))

(. js/document addEventListener "keydown" #(. % preventDefault))
(defn key-listener [cb]
  (let [keymap {65 "a" 66 "b" 67 "c" 68 "d" 69 "e" 70 "f" 71 "g" 72 "h"
                73 "i" 74 "j" 75 "k" 76 "l" 77 "m" 78 "n" 79 "o" 80 "p"
                81 "q" 82 "r" 83 "s" 84 "t" 85 "u" 86 "v" 87 "w" 88 "x"
                89 "y" 90 "z" 8 "-" 13 "+"}
        cb2 (fn [ev] (cb (get keymap (.-which ev))))]
    (. js/document addEventListener "keyup" cb2)
    #(. js/document removeEventListener "keyup" cb2)))

; ========== game model ===============
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

(comment
  (update-game-over {:nround 1 :round 2} false)
  )

; ========== view ===============
(defn cls [cl] {:class cl})

(defn header [] [:div#header [:p "Wordle Cljs"]])

(defn keyboard [n enter-cb change-cb]
  (let [word (r/atom "")
        q "qwertyuiop"
        a "asdfghl"
        z "zxcvbnm"
        add-to-word (fn [w] (when (< (count @word) n)
                              (-> (swap! word str w) (change-cb))))
        undo-to-word (fn [] (->> (apply str (drop-last @word)) (reset! word) change-cb))
        select-word (fn [] (when (= n (count @word)) 
                                  (enter-cb @word)
                                  (reset! word "")
                                  (change-cb "")))
        keyEvent (key-listener (fn [w] (when w
                                         (case w
                                           "-" (undo-to-word)
                                           "+" (select-word)
                                           (add-to-word w)))))]
    (r/create-class
      {:reagent-render 
       (fn []
         (let [cur-word @word
               on-letter #(fn [_] (add-to-word %))]
           [:div#keyboard 
            [:div.key-row (for [l q] ^{:key l} [:div.key-item {:on-click (on-letter l)} (str l)]) ]
            [:div.key-row (for [l a] ^{:key l} [:div.key-item {:on-click (on-letter l)} (str l)]) ]
            [:div.key-row (for [l z] ^{:key l} [:div.key-item {:on-click (on-letter l)} (str l)]) ]
            [:div.key-row 
             [:div.key-item {:on-click #(undo-to-word)} "<-"]
             [:div.key-item {:on-click #(select-word)} "Enter"]]]))
       :component-will-unmount (fn [_] (keyEvent))})))

(defn input-row [cell-n word]
  [:div.row
     (for [i (range cell-n)] ^{:key i}
       [:div.cell (get word i)])])

(defn evaluated-row [words]
  (let [render-cell (fn [i [letter state]] ^{:key i}
                      [:div.cell (cls state) letter])]
    [:div.row.past-row (map-indexed render-cell words)]))

(defn table [word-size past-rounds current-word left-round]
  (println left-round)
     [:div#table
      (map-indexed (fn [i d] ^{:key i} [evaluated-row d]) past-rounds)
      (when-not (neg? left-round) [input-row word-size current-word])
      (for [i (range left-round)] ^{:key i} [input-row word-size ""])])

(defn app []
  (let [word (r/atom "")
        game (r/atom (create-game 5 ["s" "a" "b" "i" "n"]))
        adv-game (fn [wd] (reset! game (next-state @game wd)))
        create-new-game (fn [] (reset! game (create-game 5 ["s" "a" "b" "i" "n"]))) ]
    (fn []
      (let [state @game
            {:keys [past-rounds nround round game-over? game-status]} state]
        [:div
         [:div#main
          [:div.section [header]]
          [:div.section [table 5 past-rounds @word (- nround round)]]
          (if game-over?
            [:div.section.game-over 
             [:h1 "You " game-status]
             [:button.win-btn {:on-click create-new-game} "Play Again"]]
            [:div.section [keyboard 5 adv-game #(reset! word %)]])]]))))

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

(def test-game-state (-> (create-game 5 ["s" "a" "b" "i" "n"])
    (next-state  ["s" "b" "b" "i" "n"])
    (next-state  ["s" "b" "b" "i" "n"])
    ; (next-state  ["s" "b" "b" "i" "n"])
    (next-state  ["s" "b" "b" "i" "n"])
    ; (next-state  "sabin")
    ; (next-state  ["s" "b" "b" "i" "n"])
    ; (next-state  ["s" "a" "b" "i" "n"])
    )
  )

(comment
  (def hello 
    (key-listener println)
    )
  (hello)
  
  (init)
  (js/alert "hello")
  (println "this is a comment")
  )
