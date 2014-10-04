;;;; cl-glass.lisp

(in-package #:cl-glass)
(defvar *default-glass-font*)
(defvar *default-glass*)
(defparameter *glass-font-dir* "~/src/lisp/cl-glass/fonts/")
;;; "cl-glass" goes here. Hacks and glory await!

(defclass glass-font ()
  ((font-name :initform "7x14-green1.png" :initarg :filename :accessor font-name )
   (src-pitch :initform 14 :initarg :file-pitch :accessor src-pitch ) ;spacing of glyphs in file
   (pitch :initform 7 :initarg :pitch :accessor pitch) ;normal spacing of glyphs
   (line-h :initform 14 :initarg :line-h :accessor line-h)
   ;;created and initialized after
   (src :initform nil :accessor src)              ;surface - loaded from bitmap             
   (srect :initform nil :accessor srect)          ;source rect - reused a lot
   (fp-srect :initform nil :accessor fp-srect)  ;internal pointer to srect, cached.
   (fp-src :initform nil :accessor fp-src)))
  
(defmethod initialize-instance :after ((gf glass-font) &key) 
;; todo: error protect
  (setf (src gf) (load-image (concatenate 'string *glass-font-dir* "7x14-green1.png"))
	(srect gf) (rectangle :x 0 :y 0 :w (pitch gf) :h (line-h gf))
	(fp-srect gf) (fp (srect gf))
	(fp-src gf) (fp (src gf)))
)

(defclass glass ()
  (
    (surface :initform *default-display* :initarg :surface :accessor surface)
    (font :initform (make-instance 'glass-font) :initarg :font :accessor font)
    (pitch :initform 7 :initarg :pitch :accessor pitch) ;may be different from font's!
    (line-h :initform 14 :initarg :line-h :accessor line-h)
   ;;created and initialized after
    (drect :initform nil :accessor drect)          ;dest rect - reused a lot
    (fp-drect :initform nil :accessor fp-drect)
    (fp-dest :initform nil :accessor fp-dest)
   ))


(defmethod initialize-instance :after ((gs glass) &key)
 (format t "~%BEFORE")
  =(setf (drect gs) (copy-rectangle (srect (font gs))) ;all we care is w and h
	(fp-drect gs) (fp (drect gs))
	(fp-dest gs) (fp (surface gs)))
 (format t "~%AFTER")
 )
;;------------------------------------------------------------------------------
;; 
(defun initialize (&key (surface *default-display*))
  "init font directory, default font and default glass, like lispbuilder-sdl"
  (format t "~%...~A" surface)
  (setf *glass-font-dir*
	(namestring (concatenate 'string (namestring (asdf:system-source-directory :cl-glass)) "fonts/")) )
  (setf *default-glass-font* (make-instance 'glass-font)
	*default-glass* (make-instance 'glass :surface surface :font *default-glass-font*)))
;;------------------------------------------------------------------------------
;; Digging low to lispbuilder-sdl's basic rect-rect blitter.  The 'cells'
;;  abstraction is a bit of a contrivance hoisted upon us, dontcha think?
;;
(defun blit-glyph (ch &key (glass *default-glass*))
  "blit a glyph using font."
  (with-accessors ((font font)) glass
    (setf (x (srect font)) (+ (pitch font) (* 2 (pitch font) (- ch 32))))
    (sdl-base::blit-surface 
     (fp-src font) (fp-dest glass) (fp-srect font) (fp-drect glass) )))
;;------------------------------------------------------------------------------ 
;; Basic output.  A wrapper may be written for special chars. 
;; Linewrap should be handled by word-break code; it is rarely useful to break
;; lines at margin...
;;
(defun out (str &key (from 0) (to (1- (length str))) (glass *default-glass*))
  "print a string.  No linewrap, control or escape sequence processing."
  (loop for index from from to to do
       (blit-glyph (char-code (aref str index)) :glass glass)
       (incf (x (drect glass)) (pitch glass)))
  #+-(loop for i across str do
	  (blit-glyph (char-code i) :glass glass)
	  (incf (x (drect glass)) (pitch glass))))
;;------------------------------------------------------------------------------
;;
(defun gotoxy (x y &key (glass *default-glass*))
  "position cursor at x y character coordinates, assuming static char grid"
  (setf (x (drect glass)) (* x (pitch glass))
	(y (drect glass)) (* y (line-h glass))))
;;------------------------------------------------------------------------------
;;
(defun clear (&key (glass *default-glass*))
  (fill-surface  *black* :surface (surface glass))
  (gotoxy 0 0 :glass glass))
;;------------------------------------------------------------------------------
;; Note: with custom pixel increment, gotoxy may not make sense.
(defun cr (&key (glass *default-glass*) (pixels (line-h glass)))
  "perform a cr, optionally specifying y increment in pixels.  Return T if fits"
  (let ((r (drect glass)))
    (setf (x r) 0)
    (incf (y r) pixels)))
;;------------------------------------------------------------------------------
;
(defun has-room (&key (glass *default-glass*))
  (< (y (drect glass)) (height (surface  glass))))t
;
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; T E S T
;; Invoke (test) for entire test.
;; For interactive development, feel free to test-init, mess around, 
;; call test-main, exit to REPL with escape, work some more...
;; at the end, call test-uninit... 

(defun test-init ()
  "initialize systems and open window"
  (init-sdl :flags 'nil)
  (sdl:window 800 600 :title-caption "SDL-GLASS Test" :icon-caption "SDL-GLASS Test")
  (setf (sdl:frame-rate) 30)
  (initialize)
)

(defun test-work ()
  "do something to fill the screen"
  (clear)
  (gotoxy 20 10)
  (out "Hello.  Welcome to the good ole glass console.") (cr) (cr)
  (out "Press <esc> to exit...")
  (update-display)
)
(defun test-main ()
  "main loop - process events"
  (sdl:with-events ()
    (:quit-event () t)
    (:video-expose-event () (sdl:update-display))
    (:key-down-event ()
		     (when (sdl:key-down-p :sdl-key-escape)
		       (print "haha")
		       (sdl:push-quit-event))
		     (print (sdl:get-keys-state))))  )
(defun test-uninit ()
  "uninit and close window"
  (sdl:push-quit-event)
  (close-audio)
  (quit-sdl :flags 'nil))

(defun test ()
  (test-init)
  (test-work)
  (test-main)
  (test-uninit))
#|
Notes:
=====
Glyphs are actually about a pixel wider then the character cells.  For now I am
clipping them to cells.  With color keying it's possible to do the right thing,
but there are so many issues with this monochromatic fonts anyway... 

The 'font' bitmap contains fonts spaced 2 cells apart to avoid glyph overlap.
This can be at least optimized by an extra step in font generation, but it's
another one of those 'why bother' things.

|#
