(ns scrabble.core
  (:require
   [uix.core :as uix :refer [defui $]]
   [scrabble.scrabble :as scrab]
   [uix.dom]
   [cljs.pprint :refer [pprint]]
   [clojure.string :as str]))

(defn replace-nth-char [s n new-char]
  (when s
    (let [before (subs s 0 n)
          after (subs s (inc n))]
      (str before new-char after))))

(def empty-board ["≡..2...≡...2..≡" ;1
                  ".=...3...3...=." ;2
                  "..=...2.2...=.." ;3
                  "2..=...2...=..2" ;4
                  "....=.....=...." ;5
                  ".3...3...3...3." ;6
                  "..2...2.2...2.." ;7
                  "≡..2...*...2..≡" ;8
                  "..2...2.2...2.." ;9
                  ".3...3...3...3." ;10
                  "....=.....=...." ;11
                  "2..=...2...=..2" ;12
                  "..=...2.2...=.." ;13
                  ".=...3...3...=." ;14
                  "≡..2...≡...2..≡" ;15
                  ])

(defui letter-tile [{:keys [l cnt]}]
  ($ :div.rel
     (when cnt
       ($ :div.fs10.abs.b.c-blu {:style {:left 3
                                         :top 3}}
          cnt))
     ($ :div.fs10.abs.b {:style {:left 38
                                 :top 33}}
        (get scrab/letter-scores (nth l 0)))
     ($ :div.fs36.b.c-g1 l)))

(defui row [{:keys [row row-idx state set-state]}]
  (let [{:keys [selected]} state]
    ($ :div.flexr.ais
       (map-indexed (fn [col-idx c]
                      (let [oc (get-in empty-board [row-idx col-idx])
                            [selected-col selected-row vert] selected]
                        ($ :div.w53.h53.b1.flexc.jcc.tac.pointer
                           {:key col-idx
                            :on-click (fn []
                                        (if (and (= row-idx
                                                    selected-row)
                                                 (= col-idx
                                                    selected-col))
                                          (set-state #(assoc % :selected [col-idx row-idx (not vert)]))
                                          (set-state #(assoc % :selected [col-idx row-idx vert]))))
                            :style {:border-color (cond (and (= row-idx
                                                                selected-row)
                                                             (= col-idx
                                                                selected-col))
                                                        "#00f"
                                                        
                                                        (or (and vert 
                                                                 (= col-idx
                                                                    selected-col))
                                                            (and (not vert)
                                                                 (= row-idx
                                                                    selected-row)))
                                                        "#bbf")
                                    }
                            :class (cond
                                     (= c \≡)  "bg-red-l"
                                     (= c \=)  "bg-pin-l"
                                     (= c \*)  "bg-pin-l"
                                     (= c \3)  "bg-blu2"
                                     (= c \2)  "bg-blu3"
                                     (= c \.)  "bg-gre-l"
                                     :else "bg-whi")}
                           (cond 
                             (= c \≡) ($ :span.fs10.b.c-g4 "TRIPLE WORD")
                             (= c \=) ($ :span.fs10.b.c-g4 "DOUBLE WORD")
                             (= c \*) ($ :span.fs24.b.c-g4 "★")
                             (= c \3) ($ :span.fs10.b.c-g4 "TRIPLE LETTER")
                             (= c \2) ($ :span.fs10.b.c-g4 "DOUBLE LETTER")
                             (= c \.) ($ :span.fs10.b "")
                             :else ($ letter-tile {:l c})
                             ))))
                    row))))

(defn remaining-letters [state]
  (let [board (get state :board)
        player-hands (apply str (map :hand (:players state)))]
    (->> (str/replace (apply str player-hands board) #"[^A-Za-z_]" "")
         (reduce (fn [bag l]
                   (str/replace-first bag (re-pattern l) ""))
                 (apply str scrab/full-bag)))))

(defui player [{:keys [state set-state player-idx]}]
  (let [[active set-active] (uix/use-state false)
        player (get-in state [:players player-idx])
        current-player (:current-player state)
        {:keys [selected]} player]
    (uix/use-effect (fn []
                      (let [handle-key (fn [evt]
                                         (when active
                                           (cond (= (.-key evt) "ArrowLeft")
                                                 (set-state #(update-in % [:players player-idx :selected]
                                                                        (fn [s] (dec s))))
                                                 (= (.-key evt) "ArrowRight")
                                                 (set-state #(update-in % [:players player-idx :selected]
                                                                        (fn [s] (inc s))))
                                                 (re-matches #"[A-Za-z ]" (.-key evt))
                                                 (set-state #(let [selected (get-in % [:players player-idx :selected])]
                                                               (-> (update-in % [:players player-idx :hand]
                                                                              (fn [r]
                                                                                (if (= " " (.-key evt))
                                                                                  (replace-nth-char r selected " ")
                                                                                  (replace-nth-char r selected (str/upper-case (.-key evt))))))
                                                                   (update-in [:players player-idx :selected]
                                                                              (fn [s] (inc s))))))
                                                 (= (.-key evt) "Backspace")
                                                 (set-state #(let [selected (get-in % [:players player-idx :selected])]
                                                               (-> (update-in % [:players player-idx :hand]
                                                                              (fn [r]
                                                                                (replace-nth-char r (dec selected) " ")))
                                                                   (update-in [:players player-idx :selected]
                                                                              (fn [s] (dec s)))))))
                                           (.preventDefault evt)))]
                        (when active (js/document.addEventListener "keydown" handle-key))
                        (fn [] (js/document.removeEventListener "keydown" handle-key))))
                    [set-state active player-idx])
    ($ :div.p6.mb10 {:tab-index 0
                     :on-focus #(set-active true)
                     :on-blur #(set-active false)
                     :style {:border (if active
                                       "5px solid #88f"
                                       "5px solid #bbb")}}
       ($ :div.flexr.jcsb.p5
          ($ :div.pb3 (:name player))
          ($ :input.w20.h20.pb3 {:type "checkbox"
                             :on-click #(set-state (fn [s] (assoc s :current-player player-idx)))
                             :on-change #()
                             :checked (= current-player player-idx)}))
       ($ :div.flexr.ais
          (for [i (range 7)]
            ($ :div.w53.h53.b1.flexc.jcc.tac.pointer.bg-whi
               {:key i
                :on-click (fn []
                            (set-state (fn [s] (assoc-in s [:players player-idx :selected] i)))
                            )
                :style {:border-color (when (= selected i)
                                        "#00F")}}
               ($ letter-tile {:l (nth (:hand player) i)})))))))

(defui board [{:keys [state set-state tab] :as props}]
  (let [[active set-active] (uix/use-state false)]
    (uix/use-effect (fn []
                     (let [handle-key (fn [evt]
                                        (when active
                                          (cond (= (.-key evt) "ArrowLeft")
                                                (set-state #(update % :selected
                                                                    (fn [s] (let [[sel-col sel-row vert] s]
                                                                              [(dec sel-col) sel-row vert]))))
                                                (= (.-key evt) "ArrowRight")
                                                (set-state #(update % :selected
                                                                    (fn [s] (let [[sel-col sel-row vert] s]
                                                                              [(inc sel-col) sel-row vert]))))
                                                (= (.-key evt) "ArrowUp")
                                                (set-state #(update % :selected
                                                                    (fn [s] (let [[sel-col sel-row vert] s]
                                                                              [sel-col (dec sel-row) vert]))))
                                                (= (.-key evt) "ArrowDown")
                                                (set-state #(update % :selected
                                                                    (fn [s] (let [[sel-col sel-row vert] s]
                                                                              [sel-col (inc sel-row) vert]))))
                                                (re-matches #"[A-Za-z ]" (.-key evt))
                                                (set-state #(let [k (str/upper-case (.-key evt))
                                                                  [col-idx row-idx vert] (:selected %)
                                                                  p-idx (:current-player %)
                                                                  source (if (= p-idx -1)
                                                                           (remaining-letters %)
                                                                           (get-in % [:players p-idx :hand]))]
                                                              (if (or (str/includes? source k) (neg? p-idx))
                                                                (-> (if (neg? p-idx)
                                                                      %
                                                                      (update-in % [:players p-idx :hand]
                                                                                 (fn [h] 
                                                                                   (str/replace-first h (re-pattern k) " "))))
                                                                    (update-in [:board row-idx]
                                                                               (fn [r]
                                                                                 (if (= " " k)
                                                                                   (replace-nth-char r col-idx (get-in empty-board [col-idx row-idx]))
                                                                                   (replace-nth-char r col-idx (str/upper-case (.-key evt))))))
                                                                    (update :selected (fn [s] (let [[sel-col sel-row vert] s]
                                                                                                (if vert
                                                                                                  [sel-col (inc sel-row) vert]
                                                                                                  [(inc sel-col) sel-row vert])))))
                                                                %)))
                                                (= (.-key evt) "Backspace")
                                                (set-state #(let [[col-idx row-idx vert] (:selected %)
                                                                  col-idx (if vert col-idx (dec col-idx))
                                                                  row-idx (if vert (dec row-idx) row-idx)
                                                                  p-idx (:current-player %)]
                                                              (-> (if (neg? p-idx) %
                                                                      (update-in % [:players p-idx :hand]
                                                                                 (fn [h] 
                                                                                   (if-let [s (re-matches #"[A-Za-z_]"
                                                                                                          (get-in % [:board row-idx col-idx]))]
                                                                                     (subs (str/replace-first (str h " ") #" " s)
                                                                                           0 7)
                                                                                     h))))
                                                                  (update-in [:board row-idx]
                                                                             (fn [r]
                                                                               (replace-nth-char r col-idx (get-in empty-board [col-idx row-idx]))))
                                                                  (update :selected (fn [s] (let [[sel-col sel-row vert] s]
                                                                                              (if vert
                                                                                                [sel-col (dec sel-row) vert]
                                                                                                [(dec sel-col) sel-row vert]))))))))
                                                (.preventDefault evt)))]
                       (when active (js/document.addEventListener "keydown" handle-key))
                       (fn [] (js/document.removeEventListener "keydown" handle-key))))
                   [set-state active])
    ($ :div.m10.inline
       {:style {:border (if active
                          "5px solid #88f"
                          "5px solid #bbb")}
        :tab-index tab
        :on-focus #(set-active true)
        :on-blur #(set-active false)}
       (map-indexed (fn [row-idx r]
                      ($ row (merge props {:key row-idx :row r :row-idx row-idx
                                           :state state :set-state set-state})))
                    (:board state)))))



(defui bag [{:keys [state set-state]}]
  (let [[active set-active] (uix/use-state false) 
        [open set-open] (uix/use-state true)
        current-player-idx (:current-player state)
        current-player (get-in state [:players ]) 
        {:keys [selected]} player
        remaining (->> (remaining-letters state)
                       frequencies
                       sort)
        selected (:bag-selected state)
        ] 
    ($ :div.p6.mb10 {:tab-index 0
                     :on-focus #(set-active true)
                     :on-blur #(set-active false)
                     :style {:border (if active
                                       "5px solid #88f"
                                       "5px solid #bbb")}}
       ($ :div.flexr.jcsb.p5
          ($ :button.pb3 {:on-click #(set-open not)} "Bag")
          ($ :input.w20.h20.pb3 {:type "checkbox"
                                 :on-click #(set-state (fn [s] (assoc s :current-player -1)))
                                 :on-change #()
                                 :checked (= current-player-idx -1)}))
       
       ($ :div.maxw500.tac
          (when open
            (map-indexed (fn [idx [l cnt]]
                           ($ :div.inline.w53.h53.b1.flexc.jcc.tac.pointer.bg-whi
                              {:key l
                               :on-click (fn []
                                           #_(set-state (fn [s] (assoc-in s [:players player-idx :selected] i))))
                               :style {:border-color (when (= selected idx)
                                                       "#00F")}}
                              ($ letter-tile {:l l :cnt cnt})))
                         remaining))))))

(defui app []
  (let [[state set-state] (uix/use-state {:board empty-board
                                          :current-player 0
                                          :players [{:name "Player 1"
                                                     :selected 2
                                                     :hand "ABCDEFG"}
                                                    {:name "Player 2"
                                                     :hand "ABCDEFG"}]
                                          :selected [7 7 nil]})
        [debug set-debug] (uix/use-state nil)] 
    
    ($ :div.flexc.aic
       ($ board {:state state :set-state set-state :tab 0}) 
       ($ :div.flexr.jcsb.w800
          (map-indexed (fn [player-idx p]
                         ($ player {:key player-idx
                                    :tab 0
                                    :player-idx player-idx
                                    :state state
                                    :set-state set-state}))
                       (:players state)))
       ($ bag {:state state :set-state set-state})
       ($ :button.brad3.bg-gre.c-whi.p10 
          {:on-click #(let [bp (scrab/best-play (:board state) 
                                                (subs (get-in state [:players (get state :current-player) :hand])
                                                      0 7))]
                        (set-debug bp))}
          "SHOW BEST PLAY")
       ($ :div.fs36.m20 (:score debug))
       ($ :pre.bg-whi.brad6 
          (with-out-str (pprint debug))))))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))