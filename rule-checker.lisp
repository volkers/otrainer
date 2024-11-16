;;;; rule-checker.lisp

#|
    Copyright (C) 2023 Volker Sarodnick

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(in-package #:otrainer)

;; load order seems not perfect
(declaim (ftype (function () t) handle-rep-item-black))

(defun index2board (idx)
  "Return index relative to the board not the view"
  (if (equal :black *view*)
      (setf idx (- 63 idx))
      idx))

(defun k2i (key)
  "Translate key (e.g. :e4) to corresponding index (e.g. 36 when view is white)"
  (let ((idx
          (getf (list :a1 56 :b1 57 :c1 58 :d1 59 :e1 60 :f1 61 :g1 62 :h1 63
                      :a2 48 :b2 49 :c2 50 :d2 51 :e2 52 :f2 53 :g2 54 :h2 55
                      :a3 40 :b3 41 :c3 42 :d3 43 :e3 44 :f3 45 :g3 46 :h3 47
                      :a4 32 :b4 33 :c4 34 :d4 35 :e4 36 :f4 37 :g4 38 :h4 39
                      :a5 24 :b5 25 :c5 26 :d5 27 :e5 28 :f5 29 :g5 30 :h5 31
                      :a6 16 :b6 17 :c6 18 :d6 19 :e6 20 :f6 21 :g6 22 :h6 23
                      :a7 8  :b7 9  :c7 10 :d7 11 :e7 12 :f7 13 :g7 14 :h7 15
                      :a8 0  :b8 1  :c8 2  :d8 3  :e8 4  :f8 5  :g8 6  :h8 7)
                key)))
    (if (equal :black *view*)
        (- 63 idx)
        idx)))

;; todo: remove or improve manipulation of globals.
(defun rule-checker-move-correct-p (pos from to side-to-move expected-move)
  "Returns t if the move follows the rules and is equal to the expected move (if there is one)."
  ;; (princ (format t "from=~a to=~a" from to))
  (when expected-move
    (if (and (eql (index2board from) (k2i (first expected-move)))
             (eql (index2board to) (k2i (second expected-move))))
        (progn ;; it is the expected move
          (setf *expected-move* nil)
          (nodgui:after 1 #'handle-rep-item-black))
        (progn ;; not expected
          (nodgui:append-text *outconsole* "Wrong, expected: ")
          (nodgui:append-text *outconsole* *expected-move*)
          (newline-and-scroll)
          (return-from rule-checker-move-correct-p nil))))
  ;; check general check rules
  (let ((piece (char pos from)))
    (cond
      ((char= piece #\Space) nil) ; no piece on from square
      ((and (lower-case-p piece) (eql side-to-move :white)) nil)
      ((and (upper-case-p piece) (eql side-to-move :black)) nil)
       ;; more rules...
      (t t))))
