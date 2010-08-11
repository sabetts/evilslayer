(defpackage :sprite
  (:use :cl)
  (:shadow :step)
  (:nicknames :s)
  (:export :image
           :surface
           :offset
           :animation
           :images
           :timeline
           :sprite
           :images
           :ticks
           :step
           :draw
           :load-image
           :load-animation))

(in-package :sprite)

(defclass image ()
  ((surface :initarg :surface :accessor surface)
   (offset :initform (u:pt 0 0) :initarg :offset :accessor offset)))

(defclass animation ()
  ((images :initarg :images :accessor images)
   (timeline :initform nil :initarg :timeline :accessor timeline)))

(defclass sprite ()
  ((animation :initarg :animation :accessor animation)
   (ticks :initform 0 :initarg :ticks :accessor ticks)))

(defgeneric current-image (animation ticks))
(defgeneric step (sprite))
(defgeneric draw (sprite loc))

(defmethod current-image ((anim animation) ticks)
  (if (timeline anim)
      (loop for i = (timeline anim) then (if (cdr i) (cdr i) i)
         for image in (images anim)
         for tick = (car i)
         sum tick into sum
         when (> sum ticks)
         return image)
      (first (images anim))))

(defmethod step ((sprite sprite))
  (with-slots (ticks animation) sprite
    (incf ticks)
    (when (>= ticks
              (loop for i = (timeline animation) then (if (cdr i) (cdr i) i)
                 for ticks = (or (car i) 0)
                 for image in (images animation)
                 sum ticks))
      (setf ticks 0))))

(defmethod draw ((sprite sprite) loc)
  (draw (current-image (animation sprite) (ticks sprite)) loc))

(defmethod draw ((image image) loc)
  (sdl:draw-surface-at-* (surface image)
                         (+ (u:x loc) (u:x (offset image)))
                         (+ (u:y loc) (u:y (offset image)))))

(defun load-image (file &key (alpha t) (offset (u:pt 0 0)))
  (let ((surface (sdl-image:load-and-convert-image (u:resource file "png"))))
    (when (numberp alpha)
      (print `(alpha ,alpha))
      (setf (sdl:alpha-enabled-p surface) t
            (sdl:alpha surface) alpha))
  (make-instance 'image :surface surface :offset offset)))

(defun load-animation (files)
  (make-instance 'animation
                 :timeline (loop for f in files
                              collect (second f))
                 :images (loop for f in files
                            collect (make-instance 'image
                                                   :surface (sdl-image:load-and-convert-image (first f))))))
