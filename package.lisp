;;;; package.lisp

(defpackage #:cl-glass
  (:nicknames :glass)
  (:use #:cl :lispbuilder-sdl)
  (:export
   :glass 
   :initialize
   :out
   :gotoxy
   :cr 
   :*default-glass*
   :*default-glass-font*)
)
