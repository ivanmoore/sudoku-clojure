;; https://gist.github.com/swannodette/3217582 - with modifications
;; some modifications similar to https://gist.github.com/orb/5884956

(ns sudoku
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]))

(defn as-rows [a] (vec (map vec (partition 9 a))))

(defn as-cols [rows] (apply map vector rows))

(defn get-square [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in rows [x y])))

(defn as-squares [rows]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (get-square rows x y)))

(defn match-known-values [board puzzle]
  (matche [board puzzle]
    ([[] []]
      succeed)
    ([[_ . board_tail] [0 . puzzle_tail]]
      (match-known-values board_tail puzzle_tail))
    ([[known_value . board_tail] [known_value . puzzle_tail]]
      (match-known-values board_tail puzzle_tail))))

(defn sudokufd [puzzle]
  (let [board (repeatedly 81 lvar)
        rows (as-rows board)
        cols (as-cols rows)
        squares (as-squares rows)]
    (run* [q]
      (== q board)
      (everyg #(fd/in % (fd/interval 1 9)) board)
      (match-known-values board puzzle)
      (everyg fd/distinct rows)
      (everyg fd/distinct cols)
      (everyg fd/distinct squares))))

(def puzzle1 [0 0 3 0 2 0 6 0 0
              9 0 0 3 0 5 0 0 1
              0 0 0 0 0 6 4 0 0
              0 0 8 0 0 2 9 0 0
              7 0 0 0 0 0 0 0 8
              0 0 6 7 0 8 2 0 0
              0 0 2 6 0 9 5 0 0
              8 0 0 2 0 3 0 0 9
              0 0 5 0 1 0 3 0 0])

(defn result-as-string [result]
  (clojure.string/join "\n" (map vec (partition 9 result))))

(println (result-as-string puzzle1))

(println (clojure.string/join "\n\n" (map result-as-string (sudokufd puzzle1))))