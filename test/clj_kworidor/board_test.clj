(ns clj-kworidor.board-test
  (:use clojure.test
        clj-kworidor.board))

(deftest default-board-test
  (testing "default-board"
    (is (= (:width default-board) 9))
    (is (= (:height default-board) 9))))

(deftest board-possible-movement-moves-test
  (is (= 1 1)))
