(ql:quickload 'nodgui)


(defpackage #:otrainer
  (:use :common-lisp)
  (:export #:main))

(in-package #:otrainer)

(setq nodgui:*wish-args* '("-name" "otrainer"))

(defvar *outconsole* nil)
(defvar *position* nil)
(defvar *board* (make-array 64))
(defvar *view* :white)
(defvar *side-to-move* :white)

(defvar *used-rep* nil "Active repertoire item")
(defvar *item-index* nil "Active repertoire item index")
(defvar *move-number* nil "Actual move number")
(defvar *ep-target-square* nil "En passant target square")
(defvar *repertoires-white* nil)
(defvar *repertoires-black* nil)
(defvar *following-moves* nil)
(defvar *expected-move* nil)
(defvar *black-move-to-do* nil)
(defvar *move-comment* nil)

(defvar *next-button-handle* nil)

(defvar *first-field* nil "Starting point of a move")

(defun newline-and-scroll ()
  "Append newline and show always last line."
  (nodgui:append-newline *outconsole*)
  (nodgui:see *outconsole* "end"))


(load "rule-checker")

(defun quit ()
  (nodgui:append-text *outconsole* (format nil "closing~%"))
  (sleep 0.5)
  (nodgui:exit-nodgui))

(defun toggle-side-to-move ()
  (if (eql *side-to-move* :white)
      (setf *side-to-move* :black)
      (setf *side-to-move* :white)))

(defun get-rand-index (prob-list)
  "Return random index of prob-list
where prob-list is a list of the probability of the index, where 0 means never."
  ;; e.g. '(3 0 2) -> '(2 2 0 0 0) and return a random element
  ;; i.e. 60% the 0 and 40% the 2
  (let* ((idx-list)
         (idx 0)
         (last-idx 0))
    (dolist (item prob-list)
      (dotimes (p item)
        (push idx idx-list) ; '(3 0 2) -> '(2 2 0 0 0)
        (incf last-idx))
      (incf idx)) ; next index of input list
    (nth (random last-idx) idx-list))) ; and return the index from the list

(defun whitep (row column)
  "Return true if field would be white"
  (or (and (evenp row) (evenp column))
      (and (oddp row) (oddp column))))

(defun draw-board (board position view)
  "Draw the board whith the pieces. If view is :white the board is shown from the white side otherwise from black side.
Index 0 is a8 i.e. upper left corner; this follows FEN notation"
  (let ((pos))
    (if (equal :white view)
        (setf pos position)
        (setf pos (reverse position)))
    (loop for i below (min (length board) (length pos)) do
      (let ((row (floor (/ i 8)))
            (column (rem i 8)))
        (cond
          ((char= (elt pos i) #\r) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_rdt60_white.png")
                                             (nodgui:make-image "png/Chess_rdt60.png"))))
          ((char= (elt pos i) #\n) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_ndt60_white.png")
                                             (nodgui:make-image "png/Chess_ndt60.png"))))
          ((char= (elt pos i) #\b) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_bdt60_white.png")
                                             (nodgui:make-image "png/Chess_bdt60.png"))))
          ((char= (elt pos i) #\q) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_qdt60_white.png")
                                             (nodgui:make-image "png/Chess_qdt60.png"))))
          ((char= (elt pos i) #\k) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_kdt60_white.png")
                                             (nodgui:make-image "png/Chess_kdt60.png"))))
          ((char= (elt pos i) #\p) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_pdt60_white.png")
                                             (nodgui:make-image "png/Chess_pdt60.png"))))
          ((char= (elt pos i) #\R) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_rlt60_white.png")
                                             (nodgui:make-image "png/Chess_rlt60.png"))))
          ((char= (elt pos i) #\N) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_nlt60_white.png")
                                             (nodgui:make-image "png/Chess_nlt60.png"))))
          ((char= (elt pos i) #\B) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_blt60_white.png")
                                             (nodgui:make-image "png/Chess_blt60.png"))))
          ((char= (elt pos i) #\Q) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_qlt60_white.png")
                                             (nodgui:make-image "png/Chess_qlt60.png"))))
          ((char= (elt pos i) #\K) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_klt60_white.png")
                                             (nodgui:make-image "png/Chess_klt60.png"))))
          ((char= (elt pos i) #\P) (setf (nodgui:image (svref board i))
                                         (if (whitep row column)
                                             (nodgui:make-image "png/Chess_plt60_white.png")
                                             (nodgui:make-image "png/Chess_plt60.png"))))
          (t (setf (nodgui:image (svref board i))
                   (if (whitep row column)
                       (nodgui:make-image "png/white60.png")
                       (nodgui:make-image "png/empty60.png")))))
        (nodgui:grid (svref board i) row column :sticky :nw)))))

(defun get-starting-position ()
  (copy-seq "rnbqkbnrpppppppp                                PPPPPPPPRNBQKBNR"))

(defun move-handler (idx)
  "Handle the mouse clicks on the board"
  ;; (format t "~a~%" idx)
  (when (equal :black *view*)
    (setf idx (- 63 idx)))
  (if *first-field*
      (let ((piece (char *position* *first-field*)))
        ;; (format t "Moved from ~a to ~a~%" *first-field* idx)
        (if (rule-checker-move-correct-p *position* *first-field* idx *side-to-move* *expected-move*)
            ;; correct move
            (progn
              (cond
                ((and (char= piece #\K) (= *first-field* (k2i :e1)) (= idx (k2i :g1)))
                 ;; short castling white
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) #\K)
                 (setf (char *position* (k2i :h1)) #\Space)
                 (setf (char *position* (k2i :f1)) #\R)
                 (setf *ep-target-square* nil))
                ((and (char= piece #\K) (= *first-field* (k2i :e1)) (= idx (k2i :c1)))
                 ;; long castling white
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) #\K)
                 (setf (char *position* (k2i :a1)) #\Space)
                 (setf (char *position* (k2i :d1)) #\R)
                 (setf *ep-target-square* nil))
                ((and (char= piece #\k) (= *first-field* (k2i :e8)) (= idx (k2i :g8)))
                 ;; short castling black
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) #\k)
                 (setf (char *position* (k2i :h8)) #\Space)
                 (setf (char *position* (k2i :f8)) #\r)
                 (setf *ep-target-square* nil))
                ((and (char= piece #\k) (= *first-field* (k2i :e8)) (= idx (k2i :c8)))
                 ;; long castling black
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) #\k)
                 (setf (char *position* (k2i :a8)) #\Space)
                 (setf (char *position* (k2i :d8)) #\r)
                 (setf *ep-target-square* nil))
                ;; dubbel step pawn moves, white
                ((and (char= piece #\P) (= *first-field* (k2i :a2)) (= idx (k2i :a4)))
                 (setf *ep-target-square* :a3)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\P) (= *first-field* (k2i :b2)) (= idx (k2i :b4)))
                 (setf *ep-target-square* :b3)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\P) (= *first-field* (k2i :c2)) (= idx (k2i :c4)))
                 (setf *ep-target-square* :c3)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\P) (= *first-field* (k2i :d2)) (= idx (k2i :d4)))
                 (setf *ep-target-square* :d3)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\P) (= *first-field* (k2i :e2)) (= idx (k2i :e4)))
                 (setf *ep-target-square* :e3)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\P) (= *first-field* (k2i :f2)) (= idx (k2i :f4)))
                 (setf *ep-target-square* :f3)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\P) (= *first-field* (k2i :g2)) (= idx (k2i :g4)))
                 (setf *ep-target-square* :g3)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                 ((and (char= piece #\P) (= *first-field* (k2i :h2)) (= idx (k2i :h4)))
                 (setf *ep-target-square* :h3)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ;; dubbel step pawn moves, black
                ((and (char= piece #\p) (= *first-field* (k2i :a7)) (= idx (k2i :a5)))
                 (setf *ep-target-square* :a6)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\p) (= *first-field* (k2i :b7)) (= idx (k2i :b5)))
                 (setf *ep-target-square* :b6)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\p) (= *first-field* (k2i :c7)) (= idx (k2i :c5)))
                 (setf *ep-target-square* :c6)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\p) (= *first-field* (k2i :d7)) (= idx (k2i :d5)))
                 (setf *ep-target-square* :d6)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\p) (= *first-field* (k2i :e7)) (= idx (k2i :e5)))
                 (setf *ep-target-square* :e6)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\p) (= *first-field* (k2i :f7)) (= idx (k2i :f5)))
                 (setf *ep-target-square* :f6)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\p) (= *first-field* (k2i :g7)) (= idx (k2i :g5)))
                 (setf *ep-target-square* :g6)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ((and (char= piece #\p) (= *first-field* (k2i :h7)) (= idx (k2i :h5)))
                 (setf *ep-target-square* :h6)
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece))
                ;; pawn ep?
                ((and (or (char= piece #\P) (char= piece #\p)) (eql idx (k2i *ep-target-square*)))
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece)
                 (cond
                   ((eql *ep-target-square* :a3) (setf (char *position* (k2i :a4)) #\Space))
                   ((eql *ep-target-square* :b3) (setf (char *position* (k2i :b4)) #\Space))
                   ((eql *ep-target-square* :c3) (setf (char *position* (k2i :c4)) #\Space))
                   ((eql *ep-target-square* :d3) (setf (char *position* (k2i :d4)) #\Space))
                   ((eql *ep-target-square* :e3) (setf (char *position* (k2i :e4)) #\Space))
                   ((eql *ep-target-square* :f3) (setf (char *position* (k2i :f4)) #\Space))
                   ((eql *ep-target-square* :g3) (setf (char *position* (k2i :g4)) #\Space))
                   ((eql *ep-target-square* :h3) (setf (char *position* (k2i :h4)) #\Space))
                   ((eql *ep-target-square* :a6) (setf (char *position* (k2i :a5)) #\Space))
                   ((eql *ep-target-square* :b6) (setf (char *position* (k2i :b5)) #\Space))
                   ((eql *ep-target-square* :c6) (setf (char *position* (k2i :c5)) #\Space))
                   ((eql *ep-target-square* :d6) (setf (char *position* (k2i :d5)) #\Space))
                   ((eql *ep-target-square* :e6) (setf (char *position* (k2i :e5)) #\Space))
                   ((eql *ep-target-square* :f6) (setf (char *position* (k2i :f5)) #\Space))
                   ((eql *ep-target-square* :g6) (setf (char *position* (k2i :g5)) #\Space))
                   ((eql *ep-target-square* :h6) (setf (char *position* (k2i :h5)) #\Space))
                   (t nil))
                 (setf *ep-target-square* nil))
                (t
                 ;; ordinary move
                 (setf (char *position* *first-field*) #\Space)
                 (setf (char *position* idx) piece)
                 (setf *ep-target-square* nil)))
              (draw-board *board* *position* *view*)
              (setf *first-field* nil)
              (toggle-side-to-move))
            ;; move didn't follow the rules, cancel it
            (setf *first-field* nil)))
      (setf *first-field* idx)))

(load "repertoires")

(defun print-debug ()
  (nodgui:append-text *outconsole* "Repertoire:")
  (nodgui:append-newline *outconsole*)
  (nodgui:append-text *outconsole* *used-rep*)
  (nodgui:append-newline *outconsole*)
  (nodgui:append-text *outconsole* "Item Index: ")
  (nodgui:append-text *outconsole* *item-index*)
  (nodgui:append-newline *outconsole*)
  (nodgui:append-text *outconsole* "Following moves:")
  (nodgui:append-newline *outconsole*)
  (nodgui:append-text *outconsole* *following-moves*)
  (newline-and-scroll))

(defun main ()
  "GUI window with menu bar and scrolled text output."
  (nodgui:with-nodgui ()
    (let* ((main-frame (make-instance 'nodgui:frame))
           (cmd-frame  (make-instance 'nodgui:frame :master main-frame))
           (outconsole (make-instance 'nodgui:scrolled-text :borderwidth 2 :relief :raised :master cmd-frame))
           (board-frame (make-instance 'nodgui:frame :master main-frame))
           (mb (nodgui:make-menubar))
           (mfile (nodgui:make-menu mb "File"))
           (mf-save (nodgui:make-menubutton mfile "Save repertoires" 'save-reps
                                     :underline 1
                                     :accelerator "Alt s"))
           (mf-exit (nodgui:make-menubutton mfile "Exit" 'quit
                                     :underline 1
                                     :accelerator "Alt Q"))
           (mrep (nodgui:make-menu mb "Repertoire"))
           (mr-white (nodgui:make-menu mrep "White"))
           (mr-black (nodgui:make-menu mrep "Black"))
           (next-button (nodgui:make-menubutton mb
                                                "Next"
                                                'new-item-and-restart
                                                ;; :accelerator "Alt N" doesn't work in menu???
                                                ;; :underline 0
                                                :state :disabled))
           (mhelp (nodgui:make-menu mb "Help"))
           (mh-help (nodgui:make-menubutton mhelp "Help"
                                         (lambda () (progn
                                                      (nodgui:append-text outconsole "Help called!")
                                                      (newline-and-scroll)))
                                         :underline 0
                                         :accelerator "Alt H"))
           (mh-debug (nodgui:make-menubutton mhelp
                                             "Debug"
                                             'print-debug
                                             :underline 0
                                             :accelerator "Alt D"))
           (mh-about (nodgui:make-menubutton mhelp "About"
                                          (lambda () (progn
                                                       (nodgui:append-text outconsole "About called!")
                                                       (newline-and-scroll)))
                                          :underline 0)))
      (declare (ignore mf-exit mf-save mh-help mh-debug mh-about))

      ;; fill repertoire menus
      (make-rep-buttons mr-white *repertoires-white* :white)
      (make-rep-buttons mr-black *repertoires-black* :black)

      ;; initialize the board
      (loop for i below (length *board*) do
        (let ((local-i i))
          (setf (svref *board* i) (make-instance 'nodgui:button :master board-frame :command (lambda ()
                                                                                             (move-handler local-i))))))
      ;; unfortunately a <ButtonPress-1> event grabs the mouse pointer and a release (<ButtonRelsease-1>)
      ;; over another widget is still an event of the "pressed" widget, see
      ;; https://web.archive.org/web/20201111211515id_/https://effbot.org/tkinterbook/tkinter-events-and-bindings.htm
      (nodgui:wm-title nodgui:*tk* "otrainer")
      (nodgui:bind nodgui:*tk* "<Alt-q>" (lambda (event) (declare (ignore event)) (quit)))
      (nodgui:bind nodgui:*tk* "<Alt-s>" (lambda (event) (declare (ignore event)) (save-reps)))
      (nodgui:pack main-frame)
      (nodgui:pack cmd-frame :side :left :fill :both :expand t)
      (nodgui:pack outconsole :side :top :fill :both :expand t)
      (nodgui:pack board-frame :side :left)
      (setf *position* (get-starting-position))
      (draw-board *board* *position* *view*)
      (setf *side-to-move* :white)
      (setq *outconsole* outconsole)
      (setq *next-button-handle* next-button))))
