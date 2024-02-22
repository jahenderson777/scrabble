(ns scrabble.state
  (:require
   [uix.core :as uix :refer [defui $]]
   [scrabble.scrabble :as scrab]
   [uix.dom]
   [cljs.pprint :refer [pprint]]
   [clojure.string :as str]))

; pure functions that take some state data and return data

(def new-state 
  {:board scrab/board
   :current-player 0
   :players [{:name "Player 1"
              :selected 2
              :hand "ABCDEFG"}
             {:name "Player 2"
              :hand "ABCDEFG"}]
   :selected [7 7 nil]})

(defn replace-nth-char [s n new-char]
  (when s
    (let [before (subs s 0 n)
          after (subs s (inc n))]
      (str before new-char after))))

(defn remaining-letters [state]
  (let [board (get state :board)
        player-hands (apply str (map :hand (:players state)))]
    (->> (str/replace (apply str player-hands board) #"[^A-Za-z_]" "")
         (reduce (fn [bag l]
                   (str/replace-first bag (re-pattern l) ""))
                 (apply str scrab/full-bag)))))

(defn board-handle-key [k state] 
  (cond (= k "ArrowLeft")
        (update state :selected
                (fn [s] (let [[sel-col sel-row vert] s]
                          [(dec sel-col) sel-row vert])))
        (= k "ArrowRight")
        (update state :selected
                (fn [s] (let [[sel-col sel-row vert] s]
                          [(inc sel-col) sel-row vert])))
        (= k "ArrowUp")
        (update state :selected
                (fn [s] (let [[sel-col sel-row vert] s]
                          [sel-col (dec sel-row) vert])))
        (= k "ArrowDown")
        (update state :selected
                (fn [s] (let [[sel-col sel-row vert] s]
                          [sel-col (inc sel-row) vert])))
        (re-matches #"[A-Za-z ]" k)
        (let [k (str/upper-case k)
              [col-idx row-idx vert] (:selected state)
              p-idx (:current-player state)
              source (if (= p-idx -1)
                       (remaining-letters state)
                       (get-in state [:players p-idx :hand]))]
          (if (or (str/includes? source k) (neg? p-idx))
            (-> (if (neg? p-idx)
                  state
                  (update-in state [:players p-idx :hand]
                             (fn [h]
                               (str/replace-first h (re-pattern k) " "))))
                (update-in [:board row-idx]
                           (fn [r]
                             (if (= " " k)
                               (replace-nth-char r col-idx (get-in scrab/board [col-idx row-idx]))
                               (replace-nth-char r col-idx k))))
                (update :selected (fn [s] (let [[sel-col sel-row vert] s]
                                            (if vert
                                              [sel-col (inc sel-row) vert]
                                              [(inc sel-col) sel-row vert])))))
            state))
        (= k "Backspace")
        (let [[col-idx row-idx vert] (:selected state)
              col-idx (if vert col-idx (dec col-idx))
              row-idx (if vert (dec row-idx) row-idx)
              p-idx (:current-player state)]
          (-> (if (neg? p-idx) state
                  (update-in state [:players p-idx :hand]
                             (fn [h]
                               (if-let [s (re-matches #"[A-Za-z_]"
                                                      (get-in state [:board row-idx col-idx]))]
                                 (subs (str/replace-first (str h " ") #" " s)
                                       0 7)
                                 h))))
              (update-in [:board row-idx]
                         (fn [r]
                           (replace-nth-char r col-idx (get-in scrab/board [col-idx row-idx]))))
              (update :selected (fn [s] (let [[sel-col sel-row vert] s]
                                          (if vert
                                            [sel-col (dec sel-row) vert]
                                            [(dec sel-col) sel-row vert]))))))
        
        :else state))

(defn player-handle-key [k player-idx state]
  (cond (= k "ArrowLeft")
        (update-in state [:players player-idx :selected]
                   (fn [s] (dec s)))
        (= k "ArrowRight")
        (update-in state [:players player-idx :selected]
                   (fn [s] (inc s)))
        (re-matches #"[A-Za-z ]" k)
        (let [selected (get-in state [:players player-idx :selected])]
          (-> (update-in state [:players player-idx :hand]
                         (fn [r]
                           (if (= " " k)
                             (replace-nth-char r selected " ")
                             (replace-nth-char r selected (str/upper-case k)))))
              (update-in [:players player-idx :selected]
                         (fn [s] (inc s)))))
        (= k "Backspace")
        (let [selected (get-in state [:players player-idx :selected])]
          (-> (update-in state [:players player-idx :hand]
                         (fn [r]
                           (replace-nth-char r (dec selected) " ")))
              (update-in [:players player-idx :selected]
                         (fn [s] (dec s)))))
        :else state))

(defn bag-fill-hand [state]
  (let [p-idx (get state :current-player)
        remaining (shuffle (remaining-letters state))
        h (get-in state [:players p-idx :hand])
        h (subs (str h "       ") 0 7)
        new-h (first (reduce (fn [[s rem] l]
                               (if (= l " ")
                                 [(str s (nth rem 0 " ")) (rest rem)]
                                 [(str s l) rem]))
                             ["" remaining]
                             h))]
    (assoc-in state [:players p-idx :hand] new-h)))

(defn bag-click-letter [k state]
  (if (re-matches #"[A-Za-z ]" k)
    (let [k (str/upper-case k)
          [col-idx row-idx vert] (:selected state)
          p-idx (:current-player state)]
      (-> (if (neg? p-idx)
            (update-in state [:board row-idx]
                       (fn [r]
                         (if (= " " k)
                           (replace-nth-char r col-idx (get-in scrab/board [col-idx row-idx]))
                           (replace-nth-char r col-idx k))))
            (update-in state [:players p-idx :hand]
                       (fn [h]
                         (str/replace-first h (re-pattern k) " "))))
          
          (update :selected (fn [s] (let [[sel-col sel-row vert] s]
                                      (if vert
                                        [sel-col (inc sel-row) vert]
                                        [(inc sel-col) sel-row vert]))))))
    state))