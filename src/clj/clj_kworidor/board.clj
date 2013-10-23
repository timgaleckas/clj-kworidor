(ns clj-kworidor.board
  "Notation:
                                                  +-+
                                                  | |
                                                  |C|
                                            C6    |6|    D6
                                                  |V|
   +-+-+-+-+-+-+-+-+-+                            | |
  9| | | | | | | | | |                 +----------+-+----------+
   +-+-+-+-+-+-+-+-+-+                 |         C6H           |
  8| | | | | | | | | |                 +----------+-+----------+
   +-+-+-+-+-+-+-+-+-+                            | |
  7| | | | | | | | | |                            | |
   +-+-+-+-+-+-+-+-+-+                      C5    | |    D5
  6| | | | | | | | | |                            | |
   +-+-+-+-+-+-+-+-+-+                            | |
  5| | | | | | | | | |                            +-+
   +-+-+-+-+-+-+-+-+-+
  4| | | | | | | | | |
   +-+-+-+-+-+-+-+-+-+
  3| | | | | | | | | |
   +-+-+-+-+-+-+-+-+-+
  2| | | | | | | | | |
   +-+-+-+-+-+-+-+-+-+
  1| | | | | | | | | |
   +-+-+-+-+-+-+-+-+-+
    A B C D E F G H I")


(defn column-label [index] (char (+ 97 index)))
(defn row-label    [index] (char (+ 49 index)))
(defn dir-label    [sym]   (case sym :v \v :h \h nil))
(defn square-label [s]
  (let [[x y d] s]
    (apply str [(column-label x) (row-label y) (dir-label d)])))

(defn vertical-oriented-player?
  [player-index]
  (< player-index 2))

(defn span-and-distance
  "Returns [span distance]."
  [player-index width height]
  (if (vertical-oriented-player? player-index)
    [width height]
    [height width]))

(defn get-start-pos
  [player-index width height]
  (let [[span distance] (span-and-distance player-index width height)]
    (cond-> [(int (dec (/ span 2))) (if (= (mod player-index 2) 0) 0 (dec distance))]

            (vertical-oriented-player? player-index) (reverse))))

(defn get-goal-pos
  [player-index width height]
  (let [[span distance] (span-and-distance player-index width height)]
    (map (fn [x] ((if (vertical-oriented-player? player-index) identity reverse ) [ x (if (= (mod player-index 2) 0) (dec distance) 0)]))
         (range span))))


(defn default-board
  [& [width height num-players]]
  (let [width (or width 9)
        height (or height 9)
        num-players (or num-players 2)]
    {:width width
     :height height
     :tiles-available 10
     :player-moves (repeat num-players [])
     :player-start-pos (map #(get-start-pos % width height) (range num-players))
     :player-goal-pos  (map #(get-goal-pos  % width height) (range num-players))}))

(def standard-two-player-board
  (default-board 7 11))

(defn board-possible-movement-moves [board]
  (for [x (range (:width board))
        y (range (:height board))]
    [x y]))

(defn board-possible-tile-moves [board]
  (concat
    (for [x (range (dec (:width board)))
          y (range (dec (:height board)))]
      [x (inc y) :h])
    (for []
      [x (inc y) :v])))

(defn current-player-pos
  "Takes a board and gives you back I suck because I'm a docstring."
  [{:keys [player-start-pos player-moves]}]
  (map (fn [start-square moves]
         (->> (cons start-square moves)
              (filter #(= 2 (count %)))
              (last)))
       player-start-pos
       player-moves))

standard-two-player-board

(current-player-pos standard-two-player-board)
