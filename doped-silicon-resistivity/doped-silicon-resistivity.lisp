(in-package #:cl-user)

(defpackage #:doped-silicon-resistivity
  (:nicknames #:dsr)
  (:use #:cl #:lisp-unit))

(in-package #:doped-silicon-resistivity)

(defmacro assert-in-range (value min max)
  (alexandria:once-only (value min max)
    `(assert (and (>= ,value ,min)
		  (<= ,value ,max)) ()
		  "Value, ~a, must be between ~a and ~a" ,value ,min ,max)))

(defun horner (x &rest coeffs)
  (reduce #'(lambda (coef acc) (+ (* acc x) coef))
	  coeffs :from-end t :initial-value 0))

(defun n-b (rho)
  " Boron density [cm^-3] as function of resistivity [Ohm cm].

rho must be in the range of 1e12 - 1e21

ASTM F723 (1)"
  #+skip y(assert-in-range rho 1e-4 1e4)
  (/ (+ 1.330e16
	(/ 1.082e17
	   (+ 1.0 (expt (* 54.56 rho)
			1.105))))
     rho))


(defun n-p (rho)
  "Phosphorus density [cm^-3] as function of resistivity  [Ohm cm]

rho must be in the range 1e12 - 5e20

ASTM F723 (2,3)"
  (assert-in-range rho 1e-4 1e4)
    (let* ((A0 -3.1083)
	   (A1 -3.2626)
	   (A2 -1.2196)
	   (A3 -0.13923)
	   (B1 1.0265)
	   (B2 0.38755)
	   (B3 0.041833)
	   (x (log rho 10))
	   (z (/ (horner x a0 a1 a2 a3)
		 (horner x 1.0 b1 b2 b3))))
      (* (/ 6.242e18
	    rho)
	 (expt 10.0 z))))

(defun rho-b (n)
  "Boron-doped Silicon resistivity in Ohm-cm as function of Boron
density (cm^-3) at 296 K.

n must be in the range of 1e12 - 1e21

ASTM F723 (4)"
  (assert-in-range n 1e12 1e21)
  (+ (/ 1.305e16 N)
     (/ 1.133e17
	(* N (1+ (expt (* N 2.58e-19)
		       -0.737))))))

(defun rho-p (n)
  "Phosphorus-doped Silicon resistivity in Ohm-cm as function of
Phosphorus density (cm^-3) at 296 K.

n must be in the range 1e12 - 5e20

ASTM F723 (5, 6)"
  (assert-in-range n 1e12 1e21)
  (let ((A -3.0769)
	(B 2.2108)
	(C -0.62272)
	(D 0.057501)
	(E -0.68157)
	(F 0.19833)
	(G -0.018376)
	(X (- (log N 10) 16)))
    (let ((Z (/ (horner x A B C D)
		(horner x  1.0 E F G))))
      (* (/ 6.242e18 N)
	 (expt 10 Z)))))

(defun rho-as (n)
  "Arsenic-doped Silicon resistivity in Ohm-cm as function of Arsenic
density (cm^-3) at 296 K.

ASTM F723 (10)"
  (assert-in-range n 1e19 6e20)
  (let ((X (log N 10))
	(A 768.2531)
	(B -25.77373)
	(C 0.9658177)
	(D -0.05643443)
	(E -8.008543e-4)
	(F 9.055838e-5)
	(G -1.776701e-6)
	(H 1.953279e-7)
	(I -5.754599e-9)
	(J -1.31657e-11))
    (let ((log-10-rho
	   (horner X -6633.667 a b c d e f g h i j)))
      (expt log-10-rho 10))))