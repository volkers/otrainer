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

(defvar *white-repo-filename* "./mywhiterepertoire.rep")
(defvar *black-repo-filename* "./myblackrepertoire.rep")

(defun choose-item-index ()
  (let ((prob-list (map 'list #'first *used-rep*)))
    (setf *item-index* (get-rand-index prob-list))
    (setf *following-moves* (third (svref *used-rep* *item-index*)))
    ;; (nodgui:append-text *outconsole* *item-index*)
    ;; (newline-and-scroll)
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

(defun new-item-and-restart ()
  "Choose a new index and restart."
  (nodgui:configure *next-button-handle* :state :disabled)
  (setf *position* (get-starting-position))
  (setf *side-to-move* :white)
  (draw-board *board* *position* *view*)
  (setf *move-number* 0)
  (setf *expected-move* nil)
  (choose-item-index)
  (do-automoves)
  (nodgui:after 1 #'handle-rep-item))

(defun handle-rep-item ()
  "Choose repertoire item and handle the moves"
  (if *following-moves*
      ;; handle following moves
      (let* ((move-parts (pop *following-moves*))
             (white-move (first move-parts))
             (black-move (second move-parts)))
        (if (equal :white *view*)
            (progn
              (when *black-move-to-do*
                (do-move *black-move-to-do*))
              (setf *expected-move* white-move)
              (setf *black-move-to-do* black-move)
              (setf *move-comment* (third move-parts)))
            (progn
              (do-move white-move)
              (setf *expected-move* black-move)
              (setf *move-comment* (third move-parts)))))
      ;; choose a new index and restart
      (nodgui:configure *next-button-handle* :state :normal)))

;; started only with black generalize later
(defun make-rep-buttons (parent-menu rep colour)
  "Make the buttons for repertoires in the nodgui parent-menu"
  (dolist (rep-item rep)
    (nodgui:make-menubutton parent-menu
                            (first rep-item)
                            (lambda () (progn
                                         ;; (nodgui:append-text *outconsole* (first rep-item))
                                         ;; (newline-and-scroll)
                                         (setf *first-field* nil) ;; release move handler
                                         (setf *used-rep* (second rep-item))
                                         (setf *view* colour)
                                         (new-item-and-restart)))
                            )))

;; add read, save and defaults

;; syntax:
;;   (("Sizilianisch, Grand Prix, Tal-Gambit"
;;     ;; #(prob automove (((w1-from w1-to) (b1-from b1-to) :optional "Comment") ... ))
;;     #((3 1 (((:e2 :e4) (:c7 :c5))
;;             ((:f2 :f4) (:d7 :d5))
;;             ((:d2 :d3) (:b8 :c6))
;;             ((:g1 :f3) (:c8 :g4))))
;;       (3 1 (((:e2 :e4) (:c7 :c5))
;; ...
;;       ))))

(defun load-reps ()
  "Load repertoirs."
  (with-open-file (in *white-repo-filename*)
    (with-standard-io-syntax
      (setf *repertoires-white* (read in))))
  (with-open-file (in *black-repo-filename*)
    (with-standard-io-syntax
      (setf *repertoires-black* (read in)))))

;; load edited rep
(defvar *repo-filename* "./myrepertoire.lisp")
(load *repo-filename*) ;; read manual written rep

;; load saved rep
;; (load-reps)

;; syntax:
;; (setf *repertoires-black*
;;   '(("Sizilianisch, Grand Prix, Tal-Gambit"
;;      ;; #(prob automove (((w1-from w1-to) (b1-from b1-to) :optional "Comment") ... ))
;;      #((3 1 (((:e2 :e4) (:c7 :c5))
;;              ((:f2 :f4) (:d7 :d5))
;;              ((:d2 :d3) (:b8 :c6))
;;              ((:g1 :f3) (:c8 :g4))))
;;        (3 1 (((:e2 :e4) (:c7 :c5))
;; ...
;;        ))))

(defun save-reps ()
  "Save repertoires to file."
  (with-open-file (out *white-repo-filename*
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *repertoires-white* out)))
  (with-open-file (out *black-repo-filename*
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *repertoires-black* out))))
