;;;; package.lisp

(defpackage #:silicon
  (:use #:cl #:lisp-unit)
  #+skip(:import-from :my-utils
		:polyeval))

