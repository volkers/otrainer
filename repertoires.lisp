;;;; repertoires.lisp

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

(defun choose-item-index ()
  (let ((prob-list (map 'list #'first *used-rep*)))
    (setf *item-index* (get-rand-index prob-list))
    (setf *following-moves* (third (svref *used-rep* *item-index*)))
    ;; (nodgui:append-text *outconsole* *item-index*)
    ;; (nodgui:append-newline *outconsole*)
    ))

(defun do-move (move)
  "Perform the actual move, where move is e.g. '(:e2 :e4)"
  (move-handler (k2i (first move)))
  (move-handler (k2i (second move))))

(defun do-automoves ()
  (let ((automove (second (svref *used-rep* *item-index*))))
    ;; (nodgui:append-text *outconsole* automove)
    (loop for move from *move-number* below automove do
      (let* ((move-parts (pop *following-moves*))
             (white-move (first move-parts))
             (black-move (second move-parts)))
        (do-move white-move)
        (do-move black-move))
      ;; (print move)
          finally (setf *move-number* move)) ;; maybe not needed?
    ))

(defun handle-rep-item-black ()
  "Choose repertoire item and handle the moves"
  (if *following-moves*
      ;; handle following moves
      (let* ((move-parts (pop *following-moves*))
             (white-move (first move-parts))
             (black-move (second move-parts)))
        (do-move white-move)
        (setf *expected-move* black-move))
      ;; choose a new index and restart
      (progn
        (setf *position* (get-starting-position))
        (draw-board *board* *position* *view*)
        (setf *move-number* 0)
        (setf *expected-move* nil)
        (choose-item-index)
        (do-automoves)
        (nodgui:after 1 #'handle-rep-item-black))))

;; started only with black generalize later
(defun make-rep-buttons (parent-menu rep colour)
  "Make the buttons for repertoires in the nodgui parent-menu"
  (dolist (rep-item rep)
    (nodgui:make-menubutton parent-menu
                            (first rep-item)
                            (lambda () (progn
                                         ;; (nodgui:append-text *outconsole* (first rep-item))
                                         ;; (nodgui:append-newline *outconsole*)
                                         (setf *used-rep* (second rep-item))
                                         (setf *view* colour)
                                         (setf *position* (get-starting-position))
                                         (draw-board *board* *position* *view*)
                                         (setf *move-number* 0)
                                         (setf *expected-move* nil)
                                         (choose-item-index)
                                         (do-automoves)
                                         (handle-rep-item-black)))
                            )))

;; add read, save and defaults
(load "myrepertoire.lisp")
;; syntax:
;; (setf *repertoires-black*
;;   '(("Sizilianisch, Grand Prix, Tal-Gambit"
;;      ;; #(prob automove (((w1-from w1-to) (b1-from b1-to)) ... ))
;;      #((3 1 (((:e2 :e4) (:c7 :c5))
;;              ((:f2 :f4) (:d7 :d5))
;;              ((:d2 :d3) (:b8 :c6))
;;              ((:g1 :f3) (:c8 :g4))))
;;        (3 1 (((:e2 :e4) (:c7 :c5))
;; ...
;;        ))))


(defvar *repertoires-white*
  '(("Reti"
     ;; #(prob automove (((w1-from w1-to) (b1-from b1-to)) ... ))
     )))

