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

(defn get-start-pos
  [player-index width height]
  (let [span     (if (vertical-oriented-player? player-index) width height )
        distance (if (vertical-oriented-player? player-index) height width )]
  ((if (vertical-oriented-player? player-index) identity reverse ) [(int (dec (/ span 2))) (if (= (mod player-index 2) 0) 0 (dec distance))])))

(defn get-goal-pos
  [player-index width height]
  (let [span     (if (vertical-oriented-player? player-index) width height )
        distance (if (vertical-oriented-player? player-index) height width )]
  (mapv (fn [x] ((if (vertical-oriented-player? player-index) identity reverse ) [ x (if (= (mod player-index 2) 0) (dec distance) 0) ] ) )(range span))))


(defn default-board
  [& {:keys [width height num-players], :or {width 9, height 9, num-players 2}}]
  {:width width
   :height height
   :tiles-available 10
   :player-moves (vec (repeat num-players []))
   :player-start-pos (mapv #( get-start-pos % width height ) (range num-players))
   :player-goal-pos  (mapv #( get-goal-pos  % width height ) (range num-players))})

(def standard-two-player-board
  (default-board :width 7 :height 11))

(defn board-possible-movement-moves [board]
  (for [x (range (:width board)) y (range (:height board))]
    [x y]))

(defn board-possible-tile-moves [board]
  (concat
    (for [x (range (dec (:width board))) y (range (dec (:height board)))]
      [x (inc y) :h])
    (for [x (range (dec (:width board))) y (range (dec (:height board)))]
      [x (inc y) :v])))

(defn current-player-pos
  [board]
  (map (fn [start-pos moves] (last (filter #(= (count %) 2) (cons start-pos moves)))) (:player-start-pos board) (:player-moves board)))

standard-two-player-board
