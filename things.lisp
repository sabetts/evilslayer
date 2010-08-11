(defpackage :things
  (:use :cl)
  (:nicknames :t)
  (:shadow :speed)
  (:export :*animations*
           :default-animations
           :init-animations
           :thing
           :object
           :being
           :hero
           :inputs
           :platform
           :start
           :end
           :speed
           :state
           :velocity
           :jumped
           :facing
           :draw
           :default-things
           :aget
           :animation
           ))

(in-package :things)

(defvar *animations* (make-hash-table))

(defun aget (sym)
  (gethash sym *animations*))

(defun default-animations ()
  (let ((animations (make-hash-table)))
    (setf (gethash :zombie-walk animations)
          (make-instance 's:animation
                         :timeline (list 5 5)
                         :images (list
                                  (s:load-image "zombie 1")
                                  (s:load-image "zombie 2")))
          (gethash :hero-stand-right animations)
          (make-instance 's:animation
                         :images (list (s:load-image "hero 1" :offset (u:pt -20 0))))
          (gethash :hero-stand-left animations)
          (make-instance 's:animation
                         :images (list (s:load-image "hero-walk-left 1" :offset (u:pt -20 0))))
          (gethash :hero-jump-right animations)
          (make-instance 's:animation
                         :images (list (s:load-image "hero-jump-right" :offset (u:pt -20 0))))
          (gethash :hero-jump-left animations)
          (make-instance 's:animation
                         :images (list (s:load-image "hero-jump-left" :offset (u:pt -20 0))))
          (gethash :hero-walk-right animations)
          (make-instance 's:animation
                         :timeline (list 5 5)
                         :images (list (s:load-image "hero 1" :offset (u:pt -20 0))
                                       (s:load-image "hero 2" :offset (u:pt -20 0))))
          (gethash :hero-walk-left animations)
          (make-instance 's:animation
                         :timeline (list 5 5)
                         :images (list (s:load-image "hero-walk-left 1" :offset (u:pt -20 0))
                                       (s:load-image "hero-walk-left 2" :offset (u:pt -20 0))))
          (gethash :demon animations)
          (make-instance 's:animation
                         :timeline (list 6 4)
                         :images (list (s:load-image "demon-idle-1")
                                       (s:load-image "demon-idle-2")))
          (gethash :chest animations)
          (make-instance 's:animation
                         :images (list (s:load-image "chest")))
          (gethash :candle animations)
          (make-instance 's:animation
                         :images (list (s:load-image "candles")))
          (gethash :platform animations)
          (make-instance 's:animation
                         :images (list (s:load-image "platform")))
          (gethash :portal animations)
          (make-instance 's:animation
                         :images (list (s:load-image "portal"))))
    animations))

(defun init-animations ()
  (setf *animations* (default-animations)))

(defclass thing (u:rectangle)
  ((sprite :initarg :sprite :accessor sprite)))

(defclass platform (thing)
  ((start :initarg :start :accessor start)
   (end :initarg :end :accessor end)
   (speed :initarg :speed :initform 1 :accessor speed)
   (start-pause :initarg :start-pause :initform 0 :accessor start-pause)
   (end-pause :initarg :end-pause :initform 0 :accessor end-pause)
   (state :initform :to-end :accessor state))
  (:default-initargs
   :sprite (make-instance 's:sprite :animation (aget :platform))))

(defclass object (thing)
  ())

(defclass chest (object)
  ()
  (:default-initargs
   :sprite (make-instance 's:sprite :animation (aget :chest))))

(defclass candle (object)
  ()
  (:default-initargs
   :sprite (make-instance 's:sprite :animation (aget :candle))))

(defclass being (thing)
  ((health :accessor health)))

(defclass monster (being)
  ())

(defclass zombie (monster)
  ()
  (:default-initargs
   :sprite (make-instance 's:sprite :animation (aget :zombie-walk))))

(defclass demon (monster)
  ()
  (:default-initargs
   :sprite (make-instance 's:sprite :animation (aget :demon))))

(defclass hero (being)
  ((inputs :initform (make-hash-table) :accessor inputs)
   (platform :initform nil :accessor platform)
   (velocity :initform 0 :accessor velocity)
   (facing :initform :right :accessor facing)
   (jumped :initform nil :accessor jumped))
  (:default-initargs
   :sprite (make-instance 's:sprite :animation (aget :hero-stand-right))))

(defgeneric draw (thing offset))

(defmethod draw ((thing thing) offset)
  (s:step (sprite thing))
  (s:draw (sprite thing) (u:sub thing offset)))

(defun default-things ()
  )

(defmethod (setf animation) (key (thing thing))
  (unless (eq (s:animation (sprite thing)) (aget key))
    (setf (s:animation (sprite thing)) (aget key)
          (s:ticks (sprite thing)) 0)))
