(ns dev
  (:require [clojure.java.io]
            [clojure.string :as string]
            [clojure.pprint]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import
   (java.time ZonedDateTime)
   (java.time.format DateTimeFormatter)))

(def palette
  [["whi" "#FFF"]
   ["bla" "#000"]

   ["nav" "#1D3743"]

;["pur" "hsl(256, 49%, 33%)"]
   ;["bro" "hsl(42, 81%, 39%)"]

   ;["vio" "hsl(269, 49%, 59%)"]
   ;["blu" "hsl(230, 72%, 74%)"]
   ;["sky" "hsl(200, 86%, 64%)"]

   ["blu2" "#a8b1d8"]
   ["blu3" "rgb(232, 240, 254)"]
   ["sky2" "hsl(200, 86%, 74%)"]
   ["nav2" "hsl(228, 35%, 38%)"]

   ["red" "#D0210F"]
   ["red-l" "#FB8C81"]
   ["ora" "#FB5306"]
   ["ora-l" "#FFA77E"]
   ["tan" "#FA9B00"]
   ["tan-l" "#F5C982"]
   ["bro" "#C15F05"]
   ["bro-l" "#DFB48C"]
   ["oli" "#95A821"]
   ["oli-l" "#DFE9A4"]
   ["gre" "#64B236"]
   ["gre-l" "#B9DDA4"]
   ["gre2" "#00FFC7"]
   ["gre3" "#2FA78D"]
   ["gre4" "#A5FFEC"]
   ["blu" "#1541BE"]
   ["blu-l" "#ADC3FF"]
   ["blu-d" "#1541BE"]
   ["pur" "#A91B4E"]
   ["pur-l" "#D091A8"]
   ["pin" "hsl(308, 66%, 71%)"]
   ["whi-off" "#F2FBFE"]

   ["g1" "#111"]
   ["g2" "#222"]
   ["g3" "#333"]
   ["g4" "#555"]
   ["g5" "#777"]
   ["g6" "#999"]
   ["g7" "#BBB"]
   ["g8" "#CCC"]
   ["g9" "#DDD"]
   ["g10" "#EEE"]])

(def color-mapping
  ["c" "color"
   "bg" "background-color"
   "bc" "border-color"])

(defn colors []
  (->> color-mapping
       (partition 2)
       (mapcat (fn [[k attr]]
                 (map (fn [[col-name v]]
                        (str "." k "-" col-name " { " attr ": " v " }"))
                      palette)))
       sort
       (interpose "\n")
       (apply str)
       println))

(comment
  (colors))

(def mapping
  ["mt" ["margin-top"]
   "mb" ["margin-bottom"]
   "ml" ["margin-left"]
   "mr" ["margin-right"]
   "mv" ["margin-top" "margin-bottom"]
   "mh" ["margin-left" "margin-right"]
   "m" ["margin"]

   "pt" ["padding-top"]
   "pb" ["padding-bottom"]
   "pl" ["padding-left"]
   "pr" ["padding-right"]
   "pv" ["padding-top" "padding-bottom"]
   "ph" ["padding-left" "padding-right"]
   "p" ["padding"]

   "lh" ["line-height"]

   "w" ["width"]
   "h" ["height"]
   "t" ["top"]
   "l" ["left"]
   "r" ["right"]

   "minh" ["min-height"]
   "maxh" ["max-height"]
   "minw" ["min-width"]
   "maxw" ["max-width"]

   "z" ["z-index"]])

(defn all-cljs-files []
  (let [directory (clojure.java.io/file "src")
        extension-filter #(clojure.string/ends-with? (.getName %) ".cljs")
        dir? #(.isDirectory %)]
    (map #(.getPath %)
         (filter (fn [f]
                   (and (not (dir? f))
                        (extension-filter f)))
                 (tree-seq dir? #(.listFiles %) directory)))))

(defn classes-from-file [filename]
  (let [s (slurp filename)
        short-hands (take-nth 2 mapping)
        pat (re-pattern (str "\\.(" (apply str (interpose "|" short-hands)) ")(-?[\\d]+)"))]
    (re-seq pat s)))

(defn extract-classes
  {:shadow.build/stage :flush}
  [build-state]
  (let [used-classes
        (->> (all-cljs-files)
             (mapcat classes-from-file)
             (reduce (fn [m [_ s v]]
                       (update m s conj v))
                     {})
             (map (fn [[k v]]
                    [k (into #{} v)]))
             (into {}))]
    (println "extract classes")
    (->> (partition 2 mapping)
         (mapcat (fn [[k attrs]]
                   (map (fn [x]
                          (str "." k x " {"
                               (apply str (interpose "; "
                                                     (for [attr attrs]
                                                       (str attr ": " x
                                                            (cond (= k "z") ""
                                                                  :else "px")))))
                               "}\n"))
                        (get used-classes k))))
         (apply str)
         (spit "docs/atomic-dynamic.css")))
  build-state)


(defn iso-str
  "makes iso8601 timestamp manually
   (works pre java 8)
   from https://stackoverflow.com/questions/3914404/how-to-get-current-moment-in-iso-8601-format-with-date-hour-and-minute"
  [ts]
  (let [tz (java.util.TimeZone/getTimeZone "UTC")
        df (new java.text.SimpleDateFormat "yyyy-MM-dd'T'HH:mm:ss")]
    (.setTimeZone df tz)
    (.format df ts)))
