;;;; silicon.asd

(asdf:defsystem #:silicon
  :serial t
  :components ((:file "package")
               (:file "silicon"))
  :depends-on ("lisp-unit"))

