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

(use 'clojure.walk)
(require 'clojure.string)

;utilities
(defn movement? [move] (and (vector? move) (= 2 (count move)) (number? (nth move 0)) (number? (nth move 1))))
(defn tile?     [move] (and (vector? move) (= 3 (count move)) (number? (nth move 0)) (number? (nth move 1))))
; display stuff
(defn column-label [index] (char (+ (int \A) index)))
(defn row-label    [index] (char (+ (int \1) index)))
(defn dir-label    [sym]   (case sym :v \V :h \H nil))
(defn square-label [s]
  (let [[x y d] s]
    (apply str [(column-label x) (row-label y) (dir-label d)])))

(defn board-with-human-moves
  [board]
  (prewalk #( if (or (tile? %) (movement? %))
              (square-label %)
              %)
           board))

(defn translate-move-from-notation
  [move]
  (if (string? move)
    (let
      [[x y d] (clojure.string/upper-case move)]
      (vec (filter #(not (nil? %)) [(- (int x) (int \A)) (- (int y) (int \1)) (case d \v :v \V :v \h :h \H :h nil nil)])))
    move
  ))

;board construction
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
  "For a default 4 player board:
  Player 1: E1
  Player 2: E9
  Player 3: A5
  Player 4: I5"
  [player-index width height]
  (let [[span distance] (span-and-distance player-index width height)]
  ((if (vertical-oriented-player? player-index) identity reverse ) [(int (/ span 2)) (if (= (mod player-index 2) 0) 0 (dec distance))])))

(defn get-goal-pos
  [player-index width height]
  (let [[span distance] (span-and-distance player-index width height)]
  (mapv (fn [x] ((if (vertical-oriented-player? player-index) identity reverse ) [ x (if (= (mod player-index 2) 0) (dec distance) 0) ] ) )
        (range span))))


(defn default-board
  [& {:keys [width height num-players], :or {width 9, height 9, num-players 2}}]
  {:width width
   :height height
   :tiles-available 10
   :player-moves (vec (repeat num-players []))
   :player-start-pos (mapv #( get-start-pos % width height ) (range num-players))
   :player-goal-pos  (mapv #( get-goal-pos  % width height ) (range num-players))})

(def standard-two-player-board
  (default-board))

; misc board info
(defn board-possible-movement-moves [board]
  (for [x (range (:width board)) y (range (:height board))]
    [x y]))

(defn board-possible-tile-moves
  [{:keys [width height] :as board}]
  (let [tile-squares (filter
                       #(not (or
                               (= (dec width) (first %))
                               (= 0 (last %))))
                       (board-possible-movement-moves board))]
    (concat
      (map #(conj % :h) tile-squares)
      (map #(conj % :v) tile-squares))))

(defn current-player-pos
  [{:keys [player-start-pos player-moves]}]
  (map (fn [start-pos moves]
         (last
           (filter
             #(= 2 (count %))
             (cons start-pos moves))))
       player-start-pos player-moves))

(defn current-player-index [{:keys [player-moves]}] (mod (count (apply concat player-moves)) (count player-moves)) )

; movement taking and validation
(defn valid-movements-for-player
  [board player-index]
  (filter
    (fn [[x y]] (and (>= x 0) (>= y 0)))
    (mapv #(mapv + (nth (current-player-pos board) player-index) %) [[0 1] [1 0] [0 -1] [-1 0]]))
  )

(defn valid-movements-for-current-player
  [board]
  (valid-movements-for-player board (current-player-index board)))

(defn valid-movement-for-current-player?
  [board move]
  (some #{move} (valid-movements-for-current-player board)))

(defn num-tiles-per-player
  [{:keys [player-moves tiles-available]}]
  (/ tiles-available (count player-moves)))

(defn num-tiles-left-for-current-player
  [{:keys [player-moves] :as board}]
  (- (num-tiles-per-player board) (count (filter #(= 3 (count %)) (nth player-moves (current-player-index board))))))

(defn current-player-has-tiles-left?
  [board]
  (< 0 (num-tiles-left-for-current-player board)))

(defn played-tiles
  [{:keys [player-moves]}]
  (filter tile? (reduce concat player-moves)))

(defn abs "(abs n) is the absolute value of n" [n]
  (cond
   (not (number? n)) (throw (IllegalArgumentException.
                 "abs requires a number"))
   (neg? n) (- n)
   :else n))

(defn overlap?
  "Cases of overlap:
  Same tile
  Same origin square for a vertical and horizontal
  Tiles with adjacent origin squares along their orientation
  "
  [tile1 tile2]
  (or
    (= (take 2 tile1) (take 2 tile2))
    (and
      (= :v (last tile1) (last tile2))
      (= (first tile1) (first tile2))
      (= 1 (abs (- (nth tile1 1) (nth tile2 1)))))
    (and
      (= :h (last tile1) (last tile2))
      (= (nth tile1 1) (nth  tile2 1))
      (= 1 (abs (- (first tile1) (first tile2)))))
    false
  ))

(defn move-error
  [board move]
  (cond
    (and (:errors board) (not (empty? (:errors board))))
      (clojure.string/join ["Error adding " (square-label move) ". Board already has an error"])
    (and (movement? move) (not (valid-movement-for-current-player? board move)))
      (clojure.string/join (concat ["Move " (square-label move) " not in valid movement moves: "] (map square-label (valid-movements-for-current-player board))))
    (and (tile? move) (not (current-player-has-tiles-left? board)))
      "Player is out of tiles"
    (and (tile? move) (some #(overlap? move %) (played-tiles board)))
      (clojure.string/join (concat ["Tile " (square-label move) " overlaps an already played tile."]))
    :else nil))

(defn board-with-move
  [board move]
  (let [move  (translate-move-from-notation move)
        error (move-error board move)]
    (if error
      (assoc board :errors (concat (or (:errors board) []) [error]))
      (update-in board [:player-moves (current-player-index board)] #( vec (conj % move))))))


; testing
(board-with-human-moves (board-with-move (board-with-move (board-with-move standard-two-player-board "E2") "E8") "E1V"))

(def board-with-standoff
  (-> standard-two-player-board
    (board-with-move "E2") (board-with-move "E8")
    (board-with-move "E3") (board-with-move "E7")
    (board-with-move "E4") (board-with-move "E6")
    (board-with-move "E5")
    ))

(def another-board
  (-> board-with-standoff
    (board-with-move "A2V") (board-with-move "A8H")
    (board-with-move "A4V") (board-with-move "C8H")
    (board-with-move "C2V") (board-with-move "C6V")
    (board-with-move "D2V") (board-with-move "D6V")
    (board-with-move "E2V") (board-with-move "E6V")))


(current-player-has-tiles-left? another-board)

(played-tiles another-board)

another-board



;(valid-movement-for-current-player? standard-two-player-board (translate-move-from-notation "E2"))

;(some #{[4 3]} [[4 1]])

