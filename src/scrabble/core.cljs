(ns scrabble.core
  (:require
   [uix.core :as uix :refer [defui $]]
   [scrabble.scrabble :as scrab]
   ;[scrabble.state :as state]
   [uix.dom]
   [cljs.pprint :refer [pprint]]
   [clojure.string :as str]))

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

(defui row [{:keys [row row-idx state !]}]
  (let [{:keys [selected]} state]
    ($ :div.flexr.ais
       (map-indexed (fn [col-idx c]
                      (let [oc (get-in scrab/board [row-idx col-idx])
                            [selected-col selected-row vert] selected]
                        ($ :div.w53.h53.b2.flexc.jcc.tac.pointer
                           {:key col-idx
                            :on-click (fn []
                                        (if (and (= row-idx
                                                    selected-row)
                                                 (= col-idx
                                                    selected-col))
                                          (! #(assoc % :selected [col-idx row-idx (not vert)]))
                                          (! #(assoc % :selected [col-idx row-idx vert]))))
                            :style {:border-color (cond (and (= row-idx
                                                                selected-row)
                                                             (= col-idx
                                                                selected-col))
                                                        "#40f"
                                                        
                                                        #_#_(or (and vert 
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

(defui player [{:keys [state ! player-idx]}]
  (let [[active set-active] (uix/use-state false)
        player (get-in state [:players player-idx])
        current-player (:current-player state)
        {:keys [selected]} player]
    (uix/use-effect (fn []
                      (let [handle-key (fn [evt]
                                         (when active
                                           (! (partial scrab/player-handle-key (.-key evt) player-idx) )
                                           (.preventDefault evt)))]
                        (when active (js/document.addEventListener "keydown" handle-key))
                        (fn [] (js/document.removeEventListener "keydown" handle-key))))
                    [! active player-idx])
    ($ :div.p6.mb10 {:tab-index 0
                     :on-focus #(set-active true)
                     :on-blur #(set-active false)
                     :style {:border (if active
                                       "5px solid #88f"
                                       "5px solid #bbb")}}
       ($ :div.flexr.jcsb.p5
          ($ :div.pb3 (:name player))
          ($ :input.w20.h20.pb3 {:type "checkbox"
                             :on-click #(! (fn [s] (update s :current-player 
                                                                   (fn [p-idx] 
                                                                     (if (= p-idx player-idx)
                                                                       -1
                                                                       player-idx)))))
                             :on-change #()
                             :checked (= current-player player-idx)}))
       ($ :div.flexr.ais
          (for [i (range 7)]
            ($ :div.w53.h53.b1.flexc.jcc.tac.pointer.bg-whi
               {:key i
                :on-click (fn []
                            (! (partial scrab/player-click-letter (nth (:hand player) i)) 
                             #_(fn [s] (assoc-in s [:players player-idx :selected] i))))
                :style {:border-color (when (= selected i)
                                        "#00F")}}
               ($ letter-tile {:l (nth (:hand player) i)})))))))

(defui board [{:keys [state ! tab] :as props}]
  (let [[active set-active] (uix/use-state false)]
    (uix/use-effect (fn []
                     (let [handle-key (fn [evt]
                                        (when active
                                          (! (partial scrab/board-handle-key (.-key evt)))
                                          (.preventDefault evt)))]
                       (when active (js/document.addEventListener "keydown" handle-key))
                       (fn [] (js/document.removeEventListener "keydown" handle-key))))
                   [! active])
    ($ :div.m10.inline
       {:style {:border (if active
                          "5px solid #88f"
                          "5px solid #bbb")}
        :tab-index tab
        :on-focus #(set-active true)
        :on-blur #(set-active false)}
       (map-indexed (fn [row-idx r]
                      ($ row (merge props {:key row-idx :row r :row-idx row-idx})))
                    (:board state)))))

(defui bag [{:keys [state !]}]
  (let [[open set-open] (uix/use-state true)
        remaining (->> (scrab/remaining-letters state)
                       frequencies
                       sort)
        selected (:bag-selected state)] 
    ($ :div.p6.mb10.b1.bc-g6
       ($ :div.flexr.jcsb.p5
          ($ :button.pb3 {:on-click #(set-open not)} "Bag")
          ($ :button.brad3.bg-gre.p3.c-whi {:on-click #(! scrab/bag-fill-hand)}
             "Fill hand")) 
       ($ :div.maxw500.tac
          (when open
            (map-indexed (fn [idx [l cnt]]
                           ($ :div.inline.w53.h53.b1.flexc.jcc.tac.pointer.bg-whi
                              {:key l
                               :on-click #(do (js/console.log "sadff "l) 
                                              (! (partial scrab/bag-click-letter l)))
                               :style {:border-color (when (= selected idx)
                                                       "#00F")}}
                              ($ letter-tile {:l l :cnt cnt})))
                         remaining))))))

(defui app []
  (let [[state !] (uix/use-state scrab/new-state)
        [debug set-debug] (uix/use-state nil)
        [ready set-ready] (uix/use-state nil)] 
    (uix/use-effect (fn []
                      (-> (.fetch js/window (str (.-URL js/document) "dictionary.txt"))
                          (.then #(.text %))
                          (.then #(do (reset! scrab/dictionary-text-file %) 
                                      (count @scrab/dictionary)
                                      (set-ready true))))))
    (if-not ready 
      ($ :h2.tac "Building dictionary, please wait...")
      ($ :div.flexc.aic 
         ($ board {:state state :! ! :tab 0}) 
         ($ :button.brad3.bg-gre.c-whi.p10.m5
            {:on-click #(! (fn [s] (assoc s :old-board (get s :board))))}
            "NEXT")
         ($ :div.p5
            (scrab/check-new-board state))
         ($ :div.flexr.jcsb.w800
            (map-indexed (fn [player-idx p]
                           ($ player {:key player-idx
                                      :tab 0
                                      :player-idx player-idx
                                      :state state
                                      :! !}))
                         (:players state)))
         ($ bag {:state state :! !})
         ($ :button.brad3.bg-gre.c-whi.p10 
            {:on-click #(let [ap (scrab/all-plays (:board state) 
                                                  (subs (get-in state [:players (get state :current-player) :hand])
                                                        0 7))
                              ]
                          (set-debug ap))}
            "SHOW BEST PLAY")
         ($ :div.fs36.m20 (:score (first debug)))
         ($ :div.fs16.m20 
            (for [i (range 15)] 
              ($ :pre.m0.fs12 {:key i} 
                 (nth (:new-board (first debug)) i))))
         ($ :pre.bg-whi.brad6 
            (with-out-str (pprint (take 10
                                        (dedupe (map (fn [{:keys [score word pos] :as m}]
                                                       (str score " " word " - " (:def m)))
                                                     debug))))))))))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))