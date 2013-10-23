(ns clj-kworidor.board)


"
Notation:
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
  A B C D E F G H I
"


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
  (map (fn [x] ((if (vertical-oriented-player? player-index) identity reverse ) [ x (if (= (mod player-index 2) 0) (dec distance) 0) ] ) )(range span))))


(defn default-board
  [& [width height num-players]]
  (let [width (or width 9)
        height (or height 9)
        num-players (or num-players 2)]
    { :width width
      :height height
      :tiles-available 10
      :player-moves (repeat num-players [])
      :player-start-pos (map #( get-start-pos % width height ) (range num-players))
      :player-goal-pos  (map #( get-goal-pos  % width height ) (range num-players))
      }))

(def standard-two-player-board
  (default-board 7 11))

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
  (let [[s1 s2] (:player-start-pos board) [m1 m2] (:player-moves board)]
        [
         (last (filter #(= (count %) 2) (cons s1 m1)))
         (last (filter #(= (count %) 2) (cons s2 m2)))
        ]))

standard-two-player-board

(current-player-pos standard-two-player-board)
