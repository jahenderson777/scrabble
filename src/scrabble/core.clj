(ns scrabble.core
  (:require ;[clojure-mail.core :as mail]
            ;[clojure-mail.gmail :as gmail]
            ;[clojure-mail.message :refer (read-message)]
   [clojure.set :as set]
   [clojure.string :as str])
  (:gen-class))

(def letter-scores {\A 1 \B 3 \C 3 \D 2 \E 1 \F 4 \G 2 \H 4
                    \I 1 \J 8 \K 5 \L 1 \M 3 \N 1 \O 1 \P 3
                    \Q 10 \R 1 \S 1 \T 1 \U 1 \V 4 \W 4 \X 8
                    \Y 4 \Z 10 \_ 0})

(def b ["≡..2...≡...2..≡" ;1
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

(score-letters-face-value "ABC")

(defn letter-sums [word]
  (reduce (fn [a b]
            (update a b #(+ 1 (or % 0))))
          {}
          (vec word)))

(def words-freq (->> (slurp "words_freq.txt")
                     str/split-lines
                     ;(take 1000)
                     (map #(let [[w f] (str/split % #"\t")]
                             [(str/upper-case w) (parse-long f)]))
                     (into {})))

(def words-def (->> (slurp "words_def.txt")
                    str/split-lines
                    ;(take 1000)
                    (map #(let [[w def] (str/split % #"\t")]
                            [w {:def def
                                :letter-sums (letter-sums w)
                                :score (score-letters-face-value w)
                                :freq (get words-freq w)}]))

                    (into {})))


(reverse (sort-by #(:freq (second %)) (vec words-def)))


;(spit "out.txt" (with-out-str (clojure.pprint/pprint words-def)))

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
                              0)
            ]
        (if  (> cnt (get hand letter 0))  ;(> blanks-required blanks-remaining)
          false
          (let [new-hand (assoc! hand letter (- (get hand letter 0) cnt))
                new-i (inc i)]
            (if (< new-i (count letter-sums))
              (recur new-i new-hand ;(- blanks-remaining blanks-required)
                     )
              (persistent! new-hand))))))))

(can-make?
 {\A 1, \L 1, \I 2, \Q 1 \S 0 \_ 2}
 {\A 2, \L 1, \I 2, \S 1})

(can-make? 
 {\A 3, \B 1, \C 4, \E 2, \G 1, \I 1} {\C 1, \E 1, \G 1, \I 1, \N 1, \O 1, \P 1, \R 1, \T 2})

(can-make?
 (letter-sums (str "CAUE" "ATLOG"))
 (letter-sums "CATALOGUE"))

(defn word->code [w]
  (-> w
        (str/replace #"[A]" "0")
        (str/replace #"[BCD]" "1")
        (str/replace #"[E]" "2")
        (str/replace #"[FGH]" "3")
        (str/replace #"[I]" "4")
        (str/replace #"[JKL]" "5")
        (str/replace #"[MNO]" "6")
        (str/replace #"[PQR]" "7")
        (str/replace #"[ST]" "8")
        (str/replace #"[UVWXYZ]" "9")
        (->> (into #{}))))

(defn word->code3 [w]
  (into #{} w))


(defn letter-set [w]
  (into #{} (map (fn [[l c]]
                  (keyword (str l c))) 
                 (frequencies w))))

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

(def letter-group 
  {\J \2
   \Q \3
   \X \4
   \Z \7
   \W \5
   \V \6
   \K \7
   \F \8
   \Y \9
   \B \2
   \H \3
   \G \4
   \M \5
   \P \6
   \U \7
   \D \8
   \C \9 
   \L \9
   \T \8
   \O \7
   \N \6
   \R \5
   \A \4
   \I \3`
   \S \2
   \E \1})

;(frequencies (map letter-group (apply str  (keys words-def))))

(def letter-sets
  (->> (keys words-def)
       ;(take 1000)
       (group-by letter-set)
       (group-by (fn [[k v]]
                   (->> k
                        (map (comp first name))
                        (into #{}))))
       (group-by (fn [[k v]]
                   (->> k
                        (map letter-group)
                        (into #{}))))))


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
                 letter-sets)
         persistent!)))

(count (get letter-sets #{ \3 \5 \6 \7 \8 \9}))

(time (count (find-words (doto (apply str (take 17 (shuffled-bag)))
                           println))))

#_(doall (map (fn [[k v]]
              (println [ (count v) k])) letter-sets))

;(sort-by second (frequencies (apply str (keys words-def))))

 (->> (map (comp first name) #{:S4 :K1 :H1 :N1 :E1 :U1 :D1 :I1})
      (into #{})) 

#_(defn find-words [hand]
  (let [hand-ls (hand-letter-set hand)]
    (reduce (fn [l [k v]]
              (if (set/subset? k hand-ls)
                (concat l v)
                l))
            []
            letter-sets)))

(time (count (find-words "YHENTPWTEDII")))

(count letter-sets)






(word->code "FOO")
(into #{}"FOO")

(def grouped-words
  (group-by (fn [[_ w]] (word->code w))
            (map (fn [[w v]] [(:letter-sums v) w]) words-def)))

(count (keys grouped-words))

(doall (map (fn [[k v]] (doto [k (count v)]
                          println)) grouped-words))

(group-by first (get grouped-words #{\2 \3 \6 \7 \8 \9}))



(def grouped-words3
  (group-by (fn [[_ w]] (word->code3 w))
            (map (fn [[w v]] [(:letter-sums v) w]) words-def)))

(defn potential-words2 [hand]
  ;(println "potential-words2" hand) 
  (let [code (word->code hand)
        hand-letter-sums (letter-sums hand)
        diff (set/difference #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} code)]
    (->> grouped-words
         (reduce (fn [v [c ws]]
                   (if (not (some diff c))
                     (reduce (fn [a [ls w]]
                               (if (can-make? hand-letter-sums ls)
                                 (conj! a w)
                                 a))
                             v
                             ws)
                     v))
                 (transient []))
         persistent!)))

(defn potential-words3 [hand]
  (let [code (word->code3 hand)
        hand-letter-sums (letter-sums hand)
        diff (set/difference #{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z} code)]
    (->> grouped-words3
         (reduce (fn [v [c ws]]
                   (if (not (some diff c))
                     (reduce (fn [a [ls w]]
                               (if (can-make? hand-letter-sums ls)
                                 (conj! a w)
                                 a))
                             v
                             ws)
                     v))
                 (transient []))
         persistent!)))

(= {:a 1 :b 2}
   {:b 2 :a 1})

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
         overlapping? false]
    (if (< i (count word))
      (let [[before after] (nth touching-letters (+ i pos))]
        (when-let [score
                   (or (and (not (or before after)) 0)
                       (:score (get words-def (str before (nth word i) after))))]
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
                       (= cell \*))))))
      (if (or overlapping? (pos? (or total 0)))
        (+ (or total 0) 
           (* main-word-score word-multipliers))

        nil))))

(score-letters-face-value "BATTY")

(calc-score "..=.2...*...2..≡" "CAT" 7
                [[nil nil];0
                 [nil nil];1
                 [nil nil];2
                 [nil nil];3
                 [nil nil];4
                 [nil nil];5
                 [nil nil];6
                 [nil "AN"];7
                 [nil nil] 
                 ["BA" "TY"]
                 [nil nil]
                 [nil nil]
                 [nil nil]
                 [nil nil]
                 ["BA" "TY"]])


(calc-score "≡..2...*...2..≡" "CAT" 7
            [[nil nil];0
             [nil nil];1
             [nil nil];2
             [nil nil];3
             [nil nil];4
             [nil nil];5
             [nil nil];6
             [nil nil];7
             [nil nil]
             [nil nil]
             [nil nil]
             [nil nil]
             [nil nil]
             [nil nil]
             [nil nil]])

(defn overlapping? [word pos row]
  (not (every? blank? (take (count word) (drop pos row)))))

(overlapping? "CAT" 6 "....A......")

(defn word-fits [row hand word touching-letters]
  ;(println "row hand word" row hand word)
  (keep (fn [pos]
          ;(println "row hand word pos" row hand word pos touching-letters)
          (when-let [new-hand (can-word-fit-on-row-at-pos? row hand word pos)] 
            (when-let [score (calc-score row word pos touching-letters)] 
              (let [score (if (and (every? zero? (vals new-hand))
                                   (= 7 (reduce + (vals hand))))
                            (do (println "BONUS! " word score (+ score 50))
                                (+ score 50))
                            score)]
                {:score score
                 :row row
                 :pos pos
                 :new-hand new-hand})
              )))
        (range (- 16 (count word)))))

(comment
  (word-fits "...2AT........." {\C 1, \A 1, \U 1, \E 1 \T 1} "CAT"
             [["BA" "K"]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil "AN"]
              [nil nil]
              ["BA" "TY"]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              ["BA" "TY"]])
  

  (word-fits "≡..2.......2..≡" {\C 1, \A 1, \U 1, \E 1 \T 1} "CAT"
             [[nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]
              [nil nil]])
  

  (time
   (filter (fn [[w {ls :letter-sums}]]
             (can-make? (letter-sums (apply str [\E \O \B \S \E \O \E]))
                        ls))
           words-def))
  )

(defn potential-words [tiles]
  (filter (fn [[w {ls :letter-sums}]]
            (when (can-make? tiles ls)
              w)) 
          words-def))

(comment
  (potential-words {\A 3, \B 1, \C 4, \E 2, \G 1, \I 1}) 


  (time 
   (count (potential-words (letter-sums "RICRACSEMEUTA")))
   )
  )

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

(split-at 0 [1 2 3])

(defn potential-row [board row-number]
  (if (= 7 row-number)
    true
    (let [r-1 (dec row-number)
          r-1 (when (>= r-1 0) (nth board r-1))
          r+1 (inc row-number)
          r+1 (when (<= r+1 14) (nth board r+1))]
      (re-find #"[A-Z]" (str r-1 (nth board row-number) r+1)))))

(defn check-row [board row-number hand]
  (when (potential-row board row-number)
    (let [touching-letters (touching-letters board row-number)
          row (nth board row-number)
          row-letters (remove blank? row)
        ;tiles (letter-sums (apply str hand row-letters))
        ;words (potential-words tiles)
          ;_ (println "potential-words2" hand row-letters)
          words  (potential-words2 (apply str hand row-letters))
          ;_ (println "word-fits" row )
          best (keep (fn [w]
                          (when-let [ws (seq (word-fits row (letter-sums hand) w touching-letters))]
                            (assoc (apply max-key :score ws)
                                   :word w)))
                        words)]
    ;best
      (when (seq best)
        (apply max-key :score best)))))

(comment 
  (let [row-number 7
        board b]
    (let [r-1 (dec row-number)
          r-1 (when (>= r-1 0) (nth board r-1))
          r+1 (inc row-number)
          r+1 (when (>= r+1 0) (nth board r+1))]
      (re-find #"[A-Z*]" (str r-1 (nth board row-number) r+1)))
    
    )

  (touching-letters b 0)


  (time (check-row b 7 "Z"))
  )

(defn check-rows [b hand & [transpose?]]
  (let [b (if transpose? (transpose b)
              b)
        v (pmap
           (fn [r]
             (when-let [x (check-row b r hand)]
               (let [new-row (str (subs (:row x) 0 (:pos x))
                                  (:word x)
                                  (subs (:row x) (+ (:pos x) (count (:word x)))))
                     new-board (assoc b r new-row)
                     new-board (if transpose? (transpose new-board)
                                   new-board)]
                 (assoc x 
                        :row-idx r
                        :new-row new-row
                        :new-board new-board))))
           (range 15))
        v (remove nil? v)]
    (when (seq v)
      (apply max-key :score v))))

(defn best-play [b hand]
  (let [v [(check-rows b hand)
           (check-rows b hand true)]
        v (seq (remove nil? v))]
    (when v
      (apply max-key :score v))))

(potential-words (letter-sums "ENLRSIA"))

(comment (best-play b "EFNWNUE")
         (check-row b 7 "CATUELN")
         

         (transpose (transpose b))
         

         b
         

         (do (touching-letters b 2) nil)
         




         (touching-letters [;123456789012345
                            "hello....c....." ;1
                            ".........a....." ;2
                            "..............." ;3
                            ".........e....." ;4
                            ".........y....." ;5
                            "..............." ;6
                            "..............." ;7
                            "..............." ;8 ***
                            "..............." ;9
                            "..............." ;10
                            "..............." ;11
                            "..............." ;12
                            "..............." ;13
                            "..............." ;14
                            "..............." ;15
                            ]2)
         )

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

(defn shuffled-bag []
  (shuffle [;1  2  3  4  5  6  7  8  9  
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
            ]))


(def state (atom {:bag (shuffled-bag)
                  :board board}))

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
  


  (let [h (apply str (take 18 (shuffled-bag)))
      ;h "LBUDU"
        ]
    (println h)
    (time (println (count (potential-words2 h))))
    (time (println (count (potential-words3 h))))
    (time (println (count (find-words h))))
    (time (println (count (potential-words (letter-sums h))))))
  

  (def p1 (sort (keys (potential-words (letter-sums "LBUDU")))))
  
  (def p2 (sort (potential-words2 "LBUDU")))
  

  (can-make? {\L 1, \B 1, \U 2, \D 1} {\D 1, \U 1, \X 1})
  

  (get words-def "DUX")
  

  (some #{\0 \1 \2 \3 } #{\7})
  

  (let [ws  (keys words-def)]
    (->> ws
         (map (fn [w] 
                (-> w
                    (str/replace #"[A]" "0")
                    (str/replace #"[BCD]" "1")
                    (str/replace #"[E]" "2")
                    (str/replace #"[FGH]" "3")
                    (str/replace #"[I]" "4")
                    (str/replace #"[JKL]" "5")
                    (str/replace #"[MNO]" "6")
                    (str/replace #"[PQR]" "7")
                    (str/replace #"[ST]" "8")
                    (str/replace #"[UVWXYZ]" "9")
                    sort 
                    distinct
                    (->> (apply str)))))
         frequencies 
         sort
         (map println)
         doall
         )
    )
  



  (-> "IONRHIQPIUYT"
      (str/replace #"[A]" "0")
      (str/replace #"[BCD]" "1")
      (str/replace #"[E]" "2")
      (str/replace #"[FGH]" "3")
      (str/replace #"[I]" "4")
      (str/replace #"[JKL]" "5")
      (str/replace #"[MNO]" "6")
      (str/replace #"[PQR]" "7")
      (str/replace #"[ST]" "8")
      (str/replace #"[UVWXYZ]" "9")
      sort
      distinct
      (->> (apply str)))
  

  "023458"
  

; we don't have 1, 6, 7
  [167]
  

  (apply str (take 12 (shuffled-bag)))
  

  "346789"
  

  (time (some #{\0 \1 \2 \3} #{\2 \4 \5}))
  

  (def tokens (into #{} 
                    
                    (remove #(> (count %) 4)
                            (str/split-lines (slurp "gpt-4-tokens.txt")))))
  

  (def tokens-short (into #{} 
                          (str/split-lines (slurp "tokens.txt"))))
  

  (defn tokenize-word [w]
    (loop [w w
           rem ""
           tks (transient [])]
    ;(println w rem tks)
      (if (tokens w)
        (if (seq rem)
          (recur rem "" (conj! tks w))
          (persistent! (conj! tks w)))
        (recur (subs w 0 (dec (count w)))
               (str (last w) rem)
               tks))))
  

  (tokenize-word "FRESHING")
  

  (let [ws (keys words-def)
        tks (mapcat tokenize-word ws)]
    
    (spit "tokens.txt" (str/join "\n" tks))
    )
  

  (let [ws (take 100 (keys words-def))
        tks (map (fn [w] [w (tokenize-word w)]) ws)]

    tks)
  

  (let [ws (keys words-def)
        tks (reduce (fn [m w] 
                      (reduce (fn [a b]
                                (assoc! a b (conj (get a b []) w)))
                              m
                              (tokenize-word w))) 
                    (transient {})
                    ws)]
    (persistent! tks))
  

  (doseq [w (take 1880 (keys words-def))]
    (println w))
  

  (let [ws (take 1815 (keys words-def))
        tks (reduce (fn [m w]
                      (let [tks (tokenize-word w)]
                      ;(println m tks w)
                        (assoc-in m tks w)))
                    
                    {}
                    ws)]
    tks)
  

  (tokens "FRESHI")
  

; given a random hand of characters, create a set of all the tokens I can make.
; maybe I can make 300 tokens
  
  (defn pathize [w]
    )
  

  (def word-tree
    (reduce (fn [a b]
              (update-in a 
                         (map #(apply str (sort %)) (take 5 (partition 1 b))) 
                         (fn [x] 
                           (update x nil #(conj % b))
                           #_(if (map? x) 
                               (update x nil #(conj % b)) 
                               (let [z (conj x b)]
                                 {nil [b]})))))
            {}
            (take 5000 (keys words-def))))
  



  (count (keys word-tree
               ))
  

  (defn power [s]
    (loop [[f & r] (seq s) 
           p '(#{})]
      (if f
        (recur r (concat p (keep #(when (< (count %) 3)
                                    (conj % f)) p)))
        p)))
  

  (time (count (power "ABCDEOIANEMEWRJFWNME")))
  )