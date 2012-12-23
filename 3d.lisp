(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload '(cl-glfw cl-opengl cl-glu)))

(declaim (optimize (debug 3) (speed 1) (safety 3)))

(defpackage :disp
  (:use :cl :gl))

(in-package :disp)


(defun point-on-sphere ()
  (tagbody
   again
     (let* ((x (+ -1s0 (random 2s0)))
	    (y (+ -1s0 (random 2s0)))
	    (z (+ -1s0 (random 2s0)))
	    (l2 (+ (* x x) (* y y) (* z z))))
       (if (< l2 1)
	   (let* ((l (sqrt l2))
		 (xx (/ x l))
		  (yy (/ y l))
		  (zz (/ z l)))
	     (return-from point-on-sphere (list xx yy zz)))
	   (go again)))))

#+nil
(point-on-sphere)


(defparameter *points-on-sphere*
  (loop for i below 300 collect
       (point-on-sphere)))


(defun draw-sphere ()
  (color 1 1 1)
  (gl:with-primitive :points
    (loop for (x y z) in *points-on-sphere* do
	 (vertex x y z))))

(defun draw-axes ()
  (let ((l 8))
   (gl:with-primitive :lines
     (color 1 .2 .2) (vertex 0 0 0) (vertex  l  0 0)
     (color .2 1 .2) (vertex 0 0 0) (vertex  0  l 0)
     (color .2 .2 1) (vertex 0 0 0) (vertex  0  0 3))))

(defun draw-scene (rot s)
  (progn
	  
	  (rotate rot 0 0 1)
	  (scale s s s)
	  
	  
	  ;(draw-axes)
	  (with-pushed-matrix
	    (let ((p (+ 1 (* .3 (1+ (sin (/ rot 10))))))) 
	      (scale p p p))
	    (draw-sphere))
	  
	  (translate -3 0 0)
	  (draw-sphere)
	  (translate 7.6 0 0)
	  (draw-sphere)))


(let ((rot 0))
 (defun draw ()
   ;;(format t "~a~%" (read *eye-input*))
   (sleep (/ 64))
   (point-size 2)
   (let ((s 3)
	 (target (list -40 0 0))
	 (cam1 (list 30 0 0))
	 (cam (list 30 -1 0)))
    (destructuring-bind (w h)
	(glfw:get-window-size)
      (progn
	(viewport 0 0 w (floor h 2))
	
	(load-identity)
	(glu:look-at  (first cam1) (second cam1) (third cam1);; cam
		     (first target) (second target) (third target);; target
		     0 0 1)
	(clear :color-buffer-bit)
	(clear-color 0 0 0 0)
	
	(incf rot 1.3)
	(draw-scene rot s))
      (progn
	(viewport 0 540 w (floor h 2))
	
	(load-identity)
	(glu:look-at (first cam) (second cam) (third cam);; cam
		      (first target) (second target) (third target);; target
		     0 0 1)
					;(clear :color-buffer-bit)
					;(clear-color 0 0 0 0)
	
	(draw-scene rot s))))))

#+nil
(glfw:do-window (:title "A simple example for hdmi 3d screen" :width 1920 :height 1080)
    ((glfw:swap-interval 1)
     (matrix-mode :projection)
     (load-identity)
     (unwind-protect (glu:perspective 45 16/9 0.1 80)
       (matrix-mode :modelview)))
  (when (eql (glfw:get-key glfw:+key-esc+) glfw:+press+)
    (return-from glfw::do-open-window))
  (draw))
