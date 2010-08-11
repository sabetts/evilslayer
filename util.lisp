(defpackage :util
  (:use :cl)
  (:nicknames :u)
  (:export :concat
           :string*
           :point
           :pt
           :pt-from
           :add
           :sub
           :x :y
           :rectangle
           :rect
           :rect-from
           :width :height
           :collide
           :right :bottom
           :resource
           :get-mouse-state
           :getkey
           :half
           :color
           :gray
           :dump-object
           :restore-object
           :load-object
           :load-font))

(in-package :util)

(defun concat (&rest strings)
  (format nil "~{~a~}" strings))

(defun string* (&rest chars)
  (format nil "~{~a~}" chars))

(defgeneric add (p1 p2))
(defgeneric sub (p1 p2))

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defun pt (x y)
  (make-instance 'point :x x :y y))

(defun pt-from (point)
  (pt (x point) (y point)))

(defmethod add ((p1 point) (p2 point))
  (pt (+ (x p1) (x p2))
      (+ (y p1) (y p2))))

(defmethod sub ((p1 point) (p2 point))
  (pt (- (x p1) (x p2))
      (- (y p1) (y p2))))

(defclass rectangle (point)
  ((width :initarg :width :initform 0 :accessor width)
   (height :initarg :height :initform 0 :accessor height)))

(defun rect (x y w h)
  (make-instance 'rectangle :x x :y y :width w :height h))

(defun rect-from (r)
  (rect (x r) (y r) (width r) (height r)))

(defmethod add ((p1 rectangle) (p2 point))
  (rect (+ (x p1) (x p2))
        (+ (y p1) (y p2))
        (width p1) (height p1)))

(defmethod sub ((p1 rectangle) (p2 point))
  (rect (- (x p1) (x p2))
        (- (y p1) (y p2))
        (width p1) (height p1)))

(defmethod add ((p1 point) (p2 rectangle))
  (add p2 p1))

(defmethod sub ((p1 point) (p2 rectangle))
  (sub p2 p1))

(defmethod right ((o rectangle))
  (+ (x o) (width o) -1))

(defmethod (setf right) (v (o rectangle))
  (setf (x o) (- v (width o) -1)))

(defmethod bottom ((o rectangle))
  (+ (y o) (height o) -1))

(defmethod (setf bottom) (v (o rectangle))
  (setf (y o) (- v (height o) -1)))

(defgeneric collide (obj1 obj2))

(defmethod collide ((obj1 rectangle) (obj2 point))
  (not
   (or (< (x obj2) (x obj1))
       (< (y obj2) (y obj1))
       (> (x obj2) (right obj1))
       (> (y obj2) (bottom obj1)))))

(defmethod collide ((obj1 point) (obj2 rectangle))
  (collide obj2 obj1))

(defmethod collide ((obj1 rectangle) (obj2 rectangle))
  (not
   (or (< (right obj1) (x obj2))
       (< (bottom obj1) (y obj2))
       (< (right obj2) (x obj1))
       (< (bottom obj2) (y obj1)))))

(defun resource (name type)
  (format nil "/Users/sabetts/src/evilslayer/gfx/~a.~a" name type))

(defun get-mouse-state ()
  "Return the absolute position and button state of the mouse."
  (cffi:with-foreign-objects ((x :int)
                              (y :int))
    (let ((buttons (sdl-cffi::sdl-get-mouse-state x y)))
      (values (cffi:mem-ref x :int) (cffi:mem-ref y :int)
              buttons))))

(defun getkey (value hash)
  (loop for v being each hash-value in hash using (hash-key k)
       when (eq v value) return k))

(defun half (n)
  (truncate n 2))

(defun color (r g b)
  (sdl:color :r r :g g :b b))

(defun gray (intensity)
  (color intensity intensity intensity))

(defgeneric dump-object (object &optional extra)
  (:method-combination append :most-specific-last))

(defmethod dump-object append ((object standard-object) &optional extra)
  (list (type-of object)))

(defgeneric restore-object (object &optional extra)
  (:method-combination progn))

(defmethod restore-object progn ((object standard-object) &optional extra)
  )

(defun load-object (list &optional extra)
  (let ((x (apply 'make-instance list)))
    (restore-object x extra)
    x))

(defmethod dump-object append ((object point) &optional extra)
  `(:x ,(x object) :y ,(y object)))

(defmethod dump-object append ((object rectangle) &optional extra)
  `(:width ,(width object) :height ,(height object)))

(defun load-font (file size)
  (sdl-ttf:open-font (make-instance 'sdl::font-definition
                                    :filename file
                                    :size size)))