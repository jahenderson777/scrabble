(ns scrabble.scrabble
  (:require    [clojure.set :as set]
               [clojure.string :as str]))

(def letter-scores {\A 1 \B 3 \C 3 \D 2 \E 1 \F 4 \G 2 \H 4
                    \I 1 \J 8 \K 5 \L 1 \M 3 \N 1 \O 1 \P 3
                    \Q 10 \R 1 \S 1 \T 1 \U 1 \V 4 \W 4 \X 8
                    \Y 4 \Z 10 \_ 0})

(def board ["≡..2...≡...2..≡" ;1
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

(defn score-letters-face-value [s]
  (reduce (fn [total l]
            (+ total (get letter-scores l)))
          0
          s))

(defn letter-sums [word]
  (reduce (fn [a b]
            (update a b #(+ 1 (or % 0))))
          {}
          (vec word)))

(defn letter-set [w]
  (into #{} (map (fn [[l c]]
                   (keyword (str l c)))
                 (frequencies w))))

(def letter-group
  {\J \2 \Q \3 \X \4 \Z \7
   \W \5 \V \6 \K \7 \F \8
   \Y \9 \B \2 \H \3 \G \4
   \M \5 \P \6 \U \7 \D \8
   \C \9 \L \9 \T \8 \O \7
   \N \6 \R \5 \A \4 \I \3
   \S \2 \E \1})

(def dictionary-text-file (atom nil))

(def dictionary 
  (-> (->> @dictionary-text-file 
           str/split-lines
           (reduce (fn [m b]
                     (let [[w def] (str/split b #"\t")
                           l1 (letter-set w)
                           l2 (->> l1
                                   (map (comp first name))
                                   (into #{}))
                           l3 (->> l2
                                   (map letter-group)
                                   (into #{}))]
                       (-> m
                           (assoc :words
                                  (assoc! (get m :words)
                                          w {:def def
                                             :letter-sums (letter-sums w)
                                             :score (score-letters-face-value w)}))
                           (assoc :index
                                  (assoc! (get m :index)
                                          l3
                                          (assoc-in (get-in m [:index l3] {})
                                                    [l2 l1]
                                                    (conj (get-in m [:index l3 l2 l1] [])
                                                          w)))))))
                   {:words (transient {})
                    :index (transient {})}))
             (update :words persistent!)
             (update :index persistent!)
             delay))

(defn can-make? [hand letter-sums]
  (let [num-blanks (get hand \_ 0)
        letter-sums (transient (vec letter-sums))
        hand (transient hand)]
    (loop [i 0
           hand hand
           ;blanks-remaining (or num-blanks 0)
           ]
      (let [[letter cnt] (nth letter-sums i)
            ;blanks-required (- (get hand letter 0) cnt)
            #_#_blanks-required (if (neg? blanks-required)
                                  (* -1 blanks-required)
                                  0)]
        (if  (> cnt (get hand letter 0))  ;(> blanks-required blanks-remaining)
          false
          (let [new-hand (assoc! hand letter (- (get hand letter 0) cnt))
                new-i (inc i)]
            (if (< new-i (count letter-sums))
              (recur new-i new-hand ;(- blanks-remaining blanks-required)
                     )
              (persistent! new-hand))))))))



(defn hand-letter-set [hand]
  (into #{} (persistent!
             (reduce (fn [v [l c]]
                       (loop [i c
                              v v]
                         (if (zero? i)
                           v
                           (recur (dec i) (conj! v (keyword (str l i)))))))
                     (transient [])
                     (frequencies hand)))))



#_(def letter-sets
  (->> (keys @dictionary)
       (group-by letter-set)
       (group-by (fn [[k v]]
                   (->> k
                        (map (comp first name))
                        (into #{}))))
       (group-by (fn [[k v]]
                   (->> k
                        (map letter-group)
                        (into #{})))) 
       delay))

(defn find-words [hand]
  (let [hand-ls (hand-letter-set hand)
        hand-l (->> hand-ls
                    (map (comp first name))
                    (into #{}))
        hand-lg (->> hand-l
                     (map letter-group)
                     (into #{}))]
    (->> (reduce (fn [l [k v]]
              ;(println k)
                   (if (set/subset? k hand-lg)
                     (reduce (fn [l [k v]]
                          ;(println "  " k)
                               (if (set/subset? k hand-l)
                                 (reduce (fn [l [k v]]
                                      ;(println "     " k v)
                                           (if (set/subset? k hand-ls)
                                             (reduce (fn [l w]
                                                       (conj! l w))
                                                     l
                                                     v)
                                             l))
                                         l
                                         v)
                                 l))
                             l
                             v)
                     l))
                 (transient [])
                 ;@letter-sets
                 (get @dictionary :index)
                 )
         persistent!)))

(defn blank? [l]
  (#{\3 \2 \≡ \= \. \*} l))

(defn can-word-fit-on-row-at-pos? [row hand' word pos]
  (when (or
         (= pos 0)
         (and (> pos 0)
              (blank? (nth row (- pos 1)))))
    (when (or (= 15 (+ pos (count word)))
              (blank? (nth row (+ pos (count word)))))
      (let [hand (transient hand')]
        (loop [i 0
               hand hand]
          (if (>= i (count word))
            (let [h (persistent! hand)]
              (when (not= hand' h)
                h))
            (let [l (nth word i)
                  row-l (nth row (+ i pos))]
              (if (= l row-l)
                (recur (inc i) hand)
                (when (blank? row-l)
                  (let [new-l-cnt (- (get hand l 0) 1)
                        new-hand (assoc! hand l new-l-cnt)]
                    (if (neg? new-l-cnt)
                      nil
                      (recur (inc i) new-hand))))))))))))

(defn calc-score [row word pos touching-letters]
  (loop [i 0
         total nil
         word-multipliers 1
         main-word-score 0 
         overlapping? false
         side-words []]
    (if (< i (count word))
      (let [[before after] (nth touching-letters (+ i pos))
            side-word (str before (nth word i) after)]
        (when-let [score
                   (or (and (not (or before after)) 0)
                       (:score (get-in @dictionary [:words side-word])))]
          (let [cell (nth row (+ pos i))
                this-letter-score (get letter-scores (nth word i))
                score (cond (= cell \≡)
                            (* 3 score)

                            (#{\= \*} cell)
                            (* 2 score)

                            (and (pos? score) (= cell \3))
                            (+ score (* 2 this-letter-score))

                            (and (pos? score) (= cell \2))
                            (+ score (* 1 this-letter-score))

                            :else score)]

            (recur (inc i)
                   (if total
                     (+ score total)
                     (when (pos? score) score))
                   (cond (= cell \≡) (* 3 word-multipliers)
                         (#{\= \*} cell) (* 2 word-multipliers)
                         :else word-multipliers)
                   (cond (= cell \3)
                         (+ main-word-score (* 3 this-letter-score))

                         (= cell \2)
                         (+ main-word-score (* 2 this-letter-score))

                         (= cell (nth word i)) (+ main-word-score this-letter-score)

                         :else (+ main-word-score this-letter-score))
                   (or overlapping?
                       (= cell (nth word i))
                       (= cell \*))
                   (if (pos? score) 
                     (conj side-words side-word)
                     side-words)))))
      (if (or overlapping? (pos? (or total 0)))
        [(+ (or total 0)
             (* main-word-score word-multipliers)) 
         side-words]

        nil))))

(defn word-fits [row hand word touching-letters]
  ;(println "row hand word" row hand word)
  (keep (fn [pos]
          ;(println "row hand word pos" row hand word pos touching-letters)
          (when-let [new-hand (can-word-fit-on-row-at-pos? row hand word pos)]
            (when-let [[score side-words] (calc-score row word pos touching-letters)]
              (let [score (if (and (every? zero? (vals new-hand))
                                   (= 7 (reduce + (vals hand))))
                            (do (println "BONUS! " word score (+ score 50))
                                (+ score 50))
                            score)]
                {:score score
                 :side-words side-words
                 :row row
                 :pos pos
                 :new-hand new-hand}))))
        (range (- 16 (count word)))))

(defn potential-words [tiles]
  (filter (fn [[w {ls :letter-sums}]]
            (when (can-make? tiles ls)
              w))
          (get @dictionary :words)))

(defn transpose [board]
  (doall
   (vec (for [x (range 15)]
          (apply str (for [y (range 15)]
                       (nth (nth board y) x)))))))

(defn touching-letters [board i]
  (let [this-row (nth board i)]
    (doall (map-indexed (fn [idx r]
                          (if (not (blank? (nth this-row idx)))
                            [nil nil]
                            (let [[a b] (split-at i r)]
                              [(when (not (blank? (last a)))
                                 (and (seq a) (last (str/split (apply str a) #"[32≡=\*\.]"))))
                               (when (not (blank? (second b)))
                                 (and (seq (drop 1 b)) (first (str/split (apply str (drop 1 b)) #"[32≡=\*\.]"))))])))
                        (transpose board)))))

(defn potential-row [board row-number]
  (if (= 7 row-number)
    true
    (let [r-1 (dec row-number)
          r-1 (when (>= r-1 0) (nth board r-1))
          r+1 (inc row-number)
          r+1 (when (<= r+1 14) (nth board r+1))]
      (re-find #"[A-Z]" (str r-1 (nth board row-number) r+1)))))

(defn check-row-word [board row-number word pos]
  (when (potential-row board row-number)
    (let [touching-letters (touching-letters board row-number)
          row (nth board row-number)]
      (calc-score row word pos touching-letters))))

(defn check-row [board row-number hand]
  (when (potential-row board row-number)
    (let [touching-letters (touching-letters board row-number)
          row (nth board row-number)
          row-letters (remove blank? row)
        ;tiles (letter-sums (apply str hand row-letters))
        ;words (potential-words tiles)
          ;_ (println "potential-words2" hand row-letters)
          words  (find-words (apply str hand row-letters))
          ;_ (println "word-fits" row )
          best (keep (fn [w]
                       (when-let [ws (seq (word-fits row (letter-sums hand) w touching-letters))]
                         (assoc (apply max-key :score ws)
                                :word w
                                :def (get-in @dictionary [:words w :def]))))
                     words)]
      best 
      #_(when (seq best)
        (apply max-key :score best)))))

(defn check-rows [b hand & [transpose?]]
  (let [b (if transpose? (transpose b)
              b)
        v (mapcat
           (fn [r]
             (when-let [v (check-row b r hand)]
               (map (fn [x]
                      (let [new-row (str (subs (:row x) 0 (:pos x))
                                         (:word x)
                                         (subs (:row x) (+ (:pos x) (count (:word x)))))
                            new-board (assoc b r new-row)
                            new-board (if transpose? (transpose new-board)
                                          new-board)]
                        (assoc x
                               :row-idx r
                               :new-row new-row
                               :new-board new-board)))
                    v)))
           (range 15))
        v (remove nil? v)]
    (seq v)
    #_(when (seq v)
      (apply max-key :score v))))

(defn best-play [b hand]
  (let [v [(check-rows b hand)
           (check-rows b hand true)]
        v (seq (remove nil? v))
        bp (when v
             (apply max-key :score v))] 
    (assoc bp :def (:def (get-in @dictionary [:words (:word bp)])))))

(defn all-plays [b hand]
  (sort-by :score >
           (concat (check-rows b hand)
                   (check-rows b hand true))))

(def full-bag [;1  2  3  4  5  6  7  8  9  
               \A \A \A \A \A \A \A \A \A
               \B \B \C \C \D \D \D \D
               \E \E \E \E \E \E \E \E \E \E \E \E
               \F \F \G \G \G \H \H
               \I \I \I \I \I \I \I \I \I
               \J \K \L \L \L \L \M \M
               \N \N \N \N \N \N
               \O \O \O \O \O \O \O \O \P \P \Q
               \R \R \R \R \R \R \S \S \S \S
               \T \T \T \T \T \T \U \U \U \U
               \V \V \W \W \X \Y \Y \Z ;\_ \_
               ])

(defn shuffled-bag []
  (shuffle full-bag))

(defn draw-tiles [{:keys [bag] :as state} player-key]
  (let [[new-tiles new-bag] (split-at (- 7 (count (get state player-key))) bag)]
    (assoc state
           :bag new-bag
           player-key (concat (get state player-key) new-tiles))))

(defn play [{:keys [board] :as state} player-key]
  (let [bp (best-play board (apply str (get state player-key)))]
    (->
     (assoc state
            :board (or (:new-board bp) board)
            player-key (apply str (mapcat (fn [[l q]]
                                            (repeat q l))
                                          (:new-hand bp))))
     (update-in [:scores player-key]
                (fn [x] (+ (or x 0) (or (:score bp) 0))))
     (update-in [:goes player-key]
                (fn [v] (conj v [(:word bp) ;(:def bp)
                                 (:score bp)])))
     (draw-tiles player-key))))

(def state (atom {:bag (shuffled-bag)
                  
                  :board board}))



(comment
  (do
    (reset! state {:bag (shuffled-bag)
                   :board board})
    (swap! state draw-tiles :player-1)
    (swap! state draw-tiles :player-2)
    (swap! state draw-tiles :player-3)
    (swap! state draw-tiles :player-4))
  
  
  (time
   (do
     (swap! state play :player-1)
     (swap! state play :player-2)
     (swap! state play :player-3)
     (swap! state play :player-4)))
  )


(def new-state
  {:board board
   :old-board board
   :current-player 0
   :players [{:name "Player 1"
              :selected 0
              :hand "       "}
             {:name "Player 2"
              :selected 0
              :hand "       "}]
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
                 (apply str full-bag)))))

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
              p-idx (:current-player state)]
          (-> (if (neg? p-idx)
                state
                (update-in state [:players p-idx :hand]
                           (fn [h]
                             (str/replace-first h (re-pattern k) " "))))
              (update-in [:board row-idx]
                         (fn [r]
                           (if (= " " k)
                             (replace-nth-char r col-idx (get-in board [col-idx row-idx]))
                             (replace-nth-char r col-idx k))))))
        (= k "Backspace")
        (let [[col-idx row-idx vert] (:selected state)]
          (update-in state [:board row-idx]
                     (fn [r]
                       (replace-nth-char r col-idx (get-in board [col-idx row-idx])))))

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
          [col-idx row-idx _] (:selected state)
          p-idx (:current-player state)]
      (-> (if (neg? p-idx)
            (update-in state [:board row-idx]
                       (fn [r]
                         (if (= " " k)
                           (replace-nth-char r col-idx (get-in board [col-idx row-idx]))
                           (replace-nth-char r col-idx k))))
            (update-in state [:players p-idx :hand]
                       (fn [h]
                         (println h)
                         (str/replace-first h #" " k))))

          #_(update :selected (fn [s] (let [[sel-col sel-row vert] s]
                                      (if vert
                                        [sel-col (inc sel-row) vert]
                                        [(inc sel-col) sel-row vert]))))))
    state))

(defn player-click-letter [k state]
  (if (re-matches #"[A-Za-z]" k)
    (let [k (str/upper-case k)
          [col-idx row-idx _] (:selected state)
          p-idx (:current-player state)]
      (-> (update-in state [:board row-idx]
                     (fn [r] (replace-nth-char r col-idx k)))
          (update-in [:players p-idx :hand]
                     (fn [h] (str/replace-first (or h "       ") (re-pattern k) " ")))

          #_(update :selected (fn [s] (let [[sel-col sel-row vert] s]
                                        (if vert
                                          [sel-col (inc sel-row) vert]
                                          [(inc sel-col) sel-row vert]))))))
    state))

(defn check-new-board' [old-board board]
  (let [different-rows (keep-indexed (fn [idx r]
                                       (when (not= r (nth old-board idx))
                                         idx))
                                     board)]
    (when (= 1 (count different-rows))
      (let [r (first different-rows)
            [x w] (reduce (fn [[idx w found] l]
                            (if (re-matches #"[A-Za-z]" l)
                              [(inc idx) (str w l) (or found (re-matches #"[^A-Za-z]" (nth (nth old-board r) idx)))]
                              (if found
                                (reduced [idx w])
                                [(inc idx) "" false])))
                          [0 "" false]
                          (nth board r))]
        (when-let [score (and (get-in @dictionary [:words w])
                              (check-row-word old-board r w (- x (count w))))]
          score)))))

(defn check-new-board [state]
  (let [{:keys [board old-board]} state]
    (or (check-new-board' old-board board)
        (check-new-board' (transpose old-board) (transpose board)))))
