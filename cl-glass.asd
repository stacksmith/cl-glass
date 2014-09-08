;;;; cl-glass.asd

(asdf:defsystem #:cl-glass
  :serial t
  :description "Describe cl-glass here"
  :author "Your Name <your.name@example.com>"
  :license "Do Whatever The Fuck You Want (DWTFYF)"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-gfx)
  :components ((:file "package")
               (:file "cl-glass")))

