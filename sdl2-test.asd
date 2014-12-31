;;;; sdl2-test.asd

(asdf:defsystem #:sdl2-test
  :description "Describe sdl2-test here"
  :author "Nikhilesh Sigatapu <s.nikhilesh@gmail.com>"
  :license "..."
  :depends-on (#:cl-opengl
               #:sdl2)
  :serial t
  :components ((:file "package")
               (:file "sdl2-test")))

