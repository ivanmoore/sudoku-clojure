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
  (for [x [0 3 6]
        y [0 3 6]]
    (get-square rows x y)))

(defn match-known-values [solution puzzle]
  (matche [solution puzzle]
    ([[] []])
    ([[_ . solution_tail] [0 . puzzle_tail]]
      (match-known-values solution_tail puzzle_tail))
    ([[known_value . solution_tail] [known_value . puzzle_tail]]
      (match-known-values solution_tail puzzle_tail))))

(defn sudokufd [puzzle]
  (let [solution (repeatedly 81 lvar)
        rows (as-rows solution)
        cols (as-cols rows)
        squares (as-squares rows)]
    (run* [q]
      (== q solution)
      (everyg #(fd/in % (fd/interval 1 9)) solution)
      (match-known-values solution puzzle)
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

(println (as-rows puzzle1))
;[[0 0 3 0 2 0 6 0 0] [9 0 0 3 0 5 0 0 1] [0 0 0 0 0 6 4 0 0]
; [0 0 8 0 0 2 9 0 0] [7 0 0 0 0 0 0 0 8] [0 0 6 7 0 8 2 0 0]
; [0 0 2 6 0 9 5 0 0] [8 0 0 2 0 3 0 0 9] [0 0 5 0 1 0 3 0 0]]

(println (as-cols (as-rows puzzle1)))
;([0 9 0 0 7 0 0 8 0] [0 0 0 0 0 0 0 0 0] [3 0 0 8 0 6 2 0 5]
; [0 3 0 0 0 7 6 2 0] [2 0 0 0 0 0 0 0 1] [0 5 6 2 0 8 9 3 0]
; [6 0 4 9 0 2 5 0 3] [0 0 0 0 0 0 0 0 0] [0 1 0 0 8 0 0 9 0])

(println (as-squares (as-rows puzzle1)))
;((0 0 3 9 0 0 0 0 0) (0 2 0 3 0 5 0 0 6) (6 0 0 0 0 1 4 0 0)
; (0 0 8 7 0 0 0 0 6) (0 0 2 0 0 0 7 0 8) (9 0 0 0 0 8 2 0 0)
; (0 0 2 8 0 0 0 0 5) (6 0 9 2 0 3 0 1 0) (5 0 0 0 0 9 3 0 0))

(println (clojure.string/join "\n\n" (map result-as-string (sudokufd puzzle1))))
;[4 5 3 8 2 1 6 9 7]
;[9 6 7 3 4 5 8 2 1]
;[2 8 1 9 7 6 4 5 3]
;[5 4 8 1 3 2 9 7 6]
;[7 2 9 5 6 4 1 3 8]
;[1 3 6 7 9 8 2 4 5]
;[3 7 2 6 8 9 5 1 4]
;[8 1 4 2 5 3 7 6 9]
;[6 9 5 4 1 7 3 8 2]
; ...