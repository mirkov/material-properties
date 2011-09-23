;;;; silicon.lisp

(in-package #:silicon)


(defun rho-b (n)
  "Boron-doped Silicon resistivity in Ohm-cm as function of Boron
density (cm^-3) at 296 K.

The fit is from the CINDAS database"
  (+ (/ 1.305e16 N)
     (/ 1.133e17
	(* N (1+ (expt (* N 2.58e-19)
		       -0.737))))))

(defun rho-p (n)
  "Phosphorus-doped Silicon resistivity in Ohm-cm as function of
Phosphorus density (cm^-3) at 296 K.

The fit is from the CINDAS database"
  (let ((A -3.0769)
	(B 2.2108)
	(C -0.62272)
	(D 0.057501)
	(E -0.68157)
	(F 0.19833)
	(G -0.018376)
	(X (- (log N 10.0) 16)))
    (let ((Z (/ (polyeval x (vector A B C D))
		(polyeval x (vector 1.0 E F G)))))
      (* (/ 6.242e18 N)
	 (expt 10 Z)))))

(defun n-p (rho &optional (print-steps nil))
  "Phosphorus density (cm^-3) of doped silicon as function of
resistivity (Ohm-cm)"
  (let ((rho-min 1.7d-4)
	(rho-max 4.3d3))
    (assert (and (>= rho rho-min)
		 (<= rho rho-max))
	    ()
	    "rho is outside the allowed range of ~a -- ~a" rho-min rho-max)
    (labels ((rho-root (N)
	       (- (rho-p N)
		  rho)))
      (let ((max-iter 50)
	    (solver
	     (make-one-dimensional-root-solver-f +brent-fsolver+
						 #'rho-root 1d12 9d20)))
	(when print-steps
	  (format t "iter ~6t   [lower ~24tupper] ~36troot ~50terr(est)~&"))
	(loop for iter from 0
	   for root = (solution solver)
	   for lower = (fsolver-lower solver)
	   for upper = (fsolver-upper solver)
	   do (iterate solver)
	   while  (and (< iter max-iter)
		       (not (root-test-interval lower upper 0.0d0 0.001d0)))
	   do
	   (when print-steps
	     (format t "~d~6t~10,6e~18t~10,6e~28t~12,9e ~44t~10,4e~&"
		     iter lower upper
		     root (- upper lower)))
	   finally (return root))))))

(defun n-B (rho &optional (print-steps nil))
  "Boron density (cm^-3) of doped silicon as function of
resistivity (Ohm-cm)"
  (let ((rho-min 1.4d-4)
	(rho-max 1.3d4))
    (assert (and (>= rho rho-min)
		 (<= rho rho-max))
	    ()
	    "rho is outside the allowed range of ~a -- ~a" rho-min rho-max)
    (labels ((rho-root (N)
	       (- (rho-b N)
		  rho)))
      (let ((max-iter 50)
	    (solver
	     (make-one-dimensional-root-solver-f +brent-fsolver+
						 #'rho-root 1d12 9d20)))
	(when print-steps
	  (format t "iter ~6t   [lower ~24tupper] ~36troot ~50terr(est)~&"))
	(loop for iter from 0
	   for root = (solution solver)
	   for lower = (fsolver-lower solver)
	   for upper = (fsolver-upper solver)
	   do (iterate solver)
	   while  (and (< iter max-iter)
		       (not (root-test-interval lower upper 0.0d0 0.001d0)))
	   do
	   (when print-steps
	     (format t "~d~6t~10,6e~18t~10,6e~28t~12,9e ~44t~10,4e~&"
		     iter lower upper
		     root (- upper lower)))
	   finally (return root))))))