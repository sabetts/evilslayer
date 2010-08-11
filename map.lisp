(defpackage :map
  (:use :cl)
  (:shadow :map :load)
  (:nicknames :m)
  (:export :tile
           :solid-tile
           :portal
           :map
           :exit
           :width :height
           :portals
           :get-tile
           :tile-at
           :draw
           :portal-exit-map
           :delete-portal
           :draw-portals
           :draw-portals-with
           :default-map
           :default-tiles
           :load
           :save
           :relink-portals))

(in-package :map)

(defvar *tile-width* 64)
(defvar *tile-height* 64)

(defclass tile (s:image)
  ())

(defclass solid-tile (tile)
  ())

(defclass portal (u:rectangle)
  ((exit :initarg :exit :accessor exit))
  (:default-initargs
   :width 64
    :height 128))

(defclass map ()
  ((tiles :accessor tiles)
   (background :initform nil :initarg :background :accessor background)
   (portals :initform nil :accessor portals :documentation "A portal links maps together.")
   (things :accessor things)))

(defmethod width ((map map))
  (array-dimension (tiles map) 1))

(defmethod height ((map map))
  (array-dimension (tiles map) 0))

(defmethod draw ((map map) ofs-x ofs-y width height)
  (labels ((bound (n dim)
             (max 0 (min n (array-dimension (tiles map) dim)))))
    (let ((start-x (floor ofs-x *tile-width*))
          (start-y (floor ofs-y *tile-height*))
          (ofsx (- (mod ofs-x *tile-width*)))
          (ofsy (- (mod ofs-y *tile-width*)))
          (w (truncate width *tile-width*))
          (h (truncate height *tile-height*)))
      (when (background map)
        (sdl:draw-surface (background map)))
      (loop for tile-y from start-y below (bound (+ start-y h 1) 0)
           for y from ofsy by *tile-height* do
           (loop for tile-x from start-x below (bound (+ start-x w 1) 1) 
           for x from ofsx by *tile-width* do
                (when (and (>= tile-x 0) (>= tile-y 0)
                           (aref (tiles map) tile-y tile-x))
                  (s:draw (aref (tiles map) tile-y tile-x) (u:pt x y))))))))

(defun portal-exit-map (portal maps)
  (loop for m across maps when (and m (find (exit portal) (portals m))) return m))

(defun delete-portal (portal maps)
  (loop for m across maps
     when m do
     (when (find portal (portals m))
       (setf (portals m) (delete portal (portals m))))
     (loop for i in (portals m)
        when (eq (exit i) portal)
        do (setf (exit i) nil))))

(defun draw-portals-with (portals animation ofs-x ofs-y width height)
  (loop for i in portals
     with view = (u:rect ofs-x ofs-y width height)
     when (u:collide i view) do
       (s:draw (s::current-image animation 0) (u:sub i (u:pt ofs-x ofs-y)))))

(defun draw-portals (portals current maps font ofs-x ofs-y width height)
  (loop for i in portals
       for map = (loop for m across maps when (and m (find (exit i) (portals m))) return m)
       for c from 0
       for color = (if (eq i current)
                        (u:color 255 0 0)
                        (u:gray 255))
     with view = (u:rect ofs-x ofs-y width height)
     when (u:collide i view) do
     (sdl-gfx:draw-rectangle-* (- (u:x i) ofs-x)
                               (- (u:y i) ofs-y)
                               (u:width i)
                               (u:height i)
                               :color color)
       (let* ((txt (format nil "~d: ~d|~d"
                           c (position map maps)
                           (when map (position (exit i) (portals map)))))
              (w (sdl:get-font-size txt :size :w :font font))
              (h (sdl:get-font-height :font font)))
     (sdl:draw-string-blended-* txt
                                (+ (- (u:x i) ofs-x)
                                   (u:half (- (u:width i) w)))
                                (+ (- (u:y i) ofs-y)
                                   (u:half (- (u:height i) h)))
                                :font font
                                :color color))))

(defmethod u:dump-object append ((object portal) &optional extra)
  (let ((m (portal-exit-map object extra)))
    (when m
      (list :exit (list (position m extra) (position (exit object) (portals m)))))))

(defun default-tiles ()
  (labels ((tile (file solid)
             (make-instance (if solid 'solid-tile 'tile) :surface (sdl-image:load-and-convert-image file))))
    (list
     (tile (u:resource "block" "png") t)
     (tile (u:resource "pillar" "png") nil)
     (tile (u:resource "water" "png") nil)
     (tile (u:resource "blue bricks" "png") nil)
     (tile (u:resource "bb-left" "png") nil)
     (tile (u:resource "bb-right" "png") nil)
     ;;(tile (u:resource "bb-bottom" "png") nil)
     (tile (u:resource "spikes" "png") nil)
     (tile (u:resource "stairs-left" "png") nil)
     (tile (u:resource "stairs-right" "png") nil))))

(defun default-map (width height background tiles)
  (let ((map (make-instance 'map
                            :background background)))
    (setf (tiles map) (make-array (list height width) :initial-element nil))
    (loop for h below height do
         (loop for w below width do
              (setf (aref (tiles map) h w)
                    (if (or (= h 0)
                            (= w 0)
                            (= w (1- width))
                            (= h (1- height)))
                        (first tiles)
                        nil))))
    (setf (portals map) (list (make-instance 'portal :exit nil
                                             :x 0 :y 0 :height (* height *tile-height*))
                              (make-instance 'portal :exit nil
                                             :x (- (* width *tile-width*) *tile-width*)
                                             :y 0
                                             :height (* height *tile-height*))
                              (make-instance 'portal :exit nil
                                             :x *tile-width* :y 0
                                             :height *tile-height*
                                             :width (- (* width *tile-width*)
                                                       (* 2 *tile-width*)))
                              (make-instance 'portal :exit nil
                                             :x *tile-width* :y (- (* height *tile-height*)
                                                                   *tile-height*)
                                             :height *tile-height*
                                             :width (- (* width *tile-width*)
                                                       (* 2 *tile-width*)))))
    map))

(defun get-tile (map x y)
  (when (and (<= 0 x (1- (array-dimension (tiles map) 1)))
             (<= 0 y (1- (array-dimension (tiles map) 0))))
    (aref (tiles map) y x)))

(defun tile-at (map x y)
  (let ((x (truncate x *tile-width*))
        (y (truncate y *tile-height*)))
    (when (and (<= 0 x (1- (array-dimension (tiles map) 1)))
               (<= 0 y (1- (array-dimension (tiles map) 0))))
      (aref (tiles map) y x))))

(defun (setf tile-at) (tile map x y)
  (let ((x (truncate x *tile-width*))
        (y (truncate y *tile-height*)))
    (when (and (<= 0 x (1- (array-dimension (tiles map) 1)))
               (<= 0 y (1- (array-dimension (tiles map) 0))))
      (setf (aref (tiles map) y x) tile))))

(defmethod save ((map map) maps stream tiles)
  (terpri stream)
  (write-line ";; map" stream)
  (write (loop for y below (height map)
            collect (loop for x below (width map)
                       collect (position (get-tile map x y) tiles)))
         :stream stream
         :lines nil :level nil :length nil)
  (terpri stream)
  (write-line ";; portals" stream)
  (write  (mapcar (lambda (p) (u:dump-object p maps)) (portals map))
         :stream stream
         :lines nil :level nil :length nil))

(defmethod load ((map map) stream tiles)
  (let* ((list (read stream))
         (portals (read stream))
         (height (length list))
         (width (length (first list)))
         (array (make-array (list height width) :initial-element nil)))
    (loop for y below height
       for i in list do
       (loop for x below width
          for j in i do
          (setf (aref array y x) (and j (elt tiles j)))))
    (setf (tiles map) array
          (portals map) (loop for i in portals
                           collect (u:load-object i)))))

(defun relink-portals (map maps)
  (dolist (i (portals map))
    (setf (exit i) (elt (portals (elt maps (first (exit i)))) (second (exit i))))))
