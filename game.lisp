(defpackage :game
  (:use :cl)
  (:shadow :map)
  (:nicknames :g)
  (:export))

(in-package :game)

(defclass base-game ()
  ((maps :accessor maps)
   (map :initform nil :accessor map)
   (tiles :initarg :tiles :accessor tiles)
   (thing-hash :initarg :thing-hash :accessor thing-hash)
   (things :initform nil :initarg :things :accessor things)
   (offset :initarg :offset :accessor offset)
   (font :initarg :font :accessor font)
   (gravity :initarg :gravity :initform 1 :accessor gravity)
   (max-speed :initarg :max-speed :initform 10 :accessor max-speed)))

(defun platforms (game)
  (loop for i in (things game)
       when (typep i 't:platform)
       collect i))

(defclass game (base-game)
  ((hero :initarg :hero :accessor hero)
   (up :initform :sdl-key-up :accessor up)
   (down :initform :sdl-key-down :accessor down)
   (left :initform :sdl-key-left :accessor left)
   (right :initform :sdl-key-right :accessor right)
   (jump :initform :sdl-key-space :accessor jump)
   (shoot :initform :sdl-key-lmeta :accessor shoot))
  (:default-initargs
   :offset (u:pt 0 0)))

(defclass thing (u:rectangle)
  ((template :initarg :template :accessor template)))

(defclass platform (thing)
  ((end :initarg :end :accessor end)))

(defun template-to-thing (template)
  (intern (string template) :t))

(defun init-maps (n)
  (let ((maps (make-array n))
        (background (sdl-image:load-and-convert-image (u:resource "background" "png"))))
    (dotimes (i n)
      (setf (elt maps i) (make-instance 'm:map :background background)
            (m:portals (elt maps i)) nil))
    maps))

(defmethod load-level ((bg base-game) file)
  (with-open-file (s file)
    (let ((n (read s)))
      (setf (maps bg) (init-maps n))
      (loop for i below n
         do (m:load (elt (maps bg) i) s (tiles bg))))
    (loop for m across (maps bg)
       do (m:relink-portals m (maps bg)))
    (let ((things (mapcar (lambda (x) (u:load-object x bg)) (read s))))
      (setf (things bg)
            (loop for i in things
               for obj = (make-instance (template-to-thing (template i))
                                        :x (u:x i) :y (u:y i)
                                        :width (u:width i) :height (u:height i))
                 do (when (typep obj 't:platform)
                      (setf (t:start obj) (u:pt-from i)
                            (t:end obj) (u:load-object (end i))))
               collect obj)))
    t))

(defun handle-key (game key state)
  (labels ((setk (key)
             (setf (gethash key (t:inputs (hero game))) state)))
    (cond
      ((sdl:key= key (up game)) (setk :up))
      ((sdl:key= key (down game)) (setk :down))
      ((sdl:key= key (left game)) (setk :left))
      ((sdl:key= key (right game)) (setk :right))
      ((sdl:key= key (jump game)) (setk :jump))
      ((sdl:key= key (shoot game)) (setk :shoot))
      (t (print key)))))

(defun handle-key-up (game key)
  (handle-key game key nil))

(defun handle-key-down (game key)
  (handle-key game key t))

(defgeneric move (game thing))

(defmethod move ((game game) (thing t:object))
  t)

(defun duck (hero)
  hero)

(defun hero-jump (hero)
  (setf (t:velocity hero) -15
        (t:jumped hero) t
        (t:animation hero) (if (eq (t:facing hero) :left)
                            :hero-jump-left
                            :hero-jump-right))
  (decf (u:y hero)))

(defun teleport (game portal)
  (setf (u:x (hero game)) (u:x (m:exit portal))
        (u:y (hero game)) (u:y (m:exit portal))))

(defmethod move ((game game) (hero t:hero))
  (labels ((dir (dir)
             (gethash dir (t:inputs hero)))
           (left () (dir :left))
           (right () (dir :right))
           (up () (dir :up))
           (down () (dir :down))
           (shoot () (dir :shoot))
           (jmp () (dir :jump)))
  (let ((portal (find-if (lambda (p)
                           (u:collide p hero))
                         (m:portals (map game))))
        ;; (platform (find-if (lambda (p)
        ;;                      (u:collide p hero))
        ;;                    (platforms game)))
        ;; (thing (find-if (lambda (th)
        ;;                   (unless (typep th 't:platform)
        ;;                     (u:collide th hero)))
        ;;                 (things game)))
        ;; (top-left-tile (tile-at (map game) hero))
        ;; (top-right-tile (tile-at (map game) (u:pt (+ (u:x hero) (u:width hero)) (u:y hero))))
        (bottom-left-tile (typep (m:tile-at (map game) (u:x hero) (u:bottom hero)) 'm:solid-tile))
        (bottom-right-tile (typep (m:tile-at (map game) (u:right hero) (u:bottom hero)) 'm:solid-tile)))
    ;; falling
    (when (not (or bottom-left-tile
                   bottom-right-tile
                   (t:platform hero)))
      (incf (u:y hero) (t:velocity hero))
      (incf (t:velocity hero) (gravity game))
      (when (> (t:velocity hero) (max-speed game))
        (setf (t:velocity hero) (max-speed game)))
      (when (< (t:velocity hero) (- (max-speed game)))
        (setf (t:velocity hero) (- (max-speed game))))
      (setf bottom-left-tile (typep (m:tile-at (map game) (u:right hero) (u:bottom hero)) 'm:solid-tile)
            bottom-right-tile (typep (m:tile-at (map game) (u:x hero) (u:bottom hero)) 'm:solid-tile)))
    ;; standing on a tile
    (when (or bottom-left-tile
              bottom-right-tile)
      (setf (u:bottom hero) (* 64 (floor (u:bottom hero) 64))
            (t:velocity hero) 0)
      (when (and (up) portal)
        (teleport game portal)
        (return-from move nil))
      (when (down)
        (duck hero))
      (when (not (jmp))
        (setf (t:jumped hero) nil))
      (when (and portal
                 (not (down))
                 (not (jmp))
                 (up))
        (teleport game portal)))
    (when (left)
      (setf (t:facing hero) :left
            (t:animation hero) (if (or bottom-left-tile bottom-right-tile)
                                   :hero-walk-left
                                   :hero-jump-left))
      (decf (u:x hero) 5))
    (when (right)
      (setf (t:facing hero) :right
            (t:animation hero) (if (or bottom-left-tile bottom-right-tile)
                                   :hero-walk-right
                                   :hero-jump-right))
      (incf (u:x hero) 5))
    (unless (or (not (and bottom-right-tile bottom-left-tile))
                (left) (right) (jmp))
      (setf (t:animation hero) (if (eq (t:facing hero) :left)
                                   :hero-stand-left
                                   :hero-stand-right)))
    (when (and (not (down)) (jmp) (not (t:jumped hero)))
      (hero-jump hero)))))

(defmethod move ((game game) (thing t:thing))
  )

(defmethod move ((game game) (thing t:platform))
  (let ((dest (case (t:state thing)
                (:to-end (t:end thing))
                (:to-start (t:start thing)))))
    (labels ((move-it ()
               (cond
                 ((< (u:x thing) (u:x dest)) (incf (u:x thing) (t:speed thing)))
                 ((> (u:x thing) (u:x dest)) (decf (u:x thing) (t:speed thing))))
               (cond
                 ((< (u:y thing) (u:y dest)) (incf (u:y thing) (t:speed thing)))
                 ((> (u:y thing) (u:y dest)) (decf (u:y thing) (t:speed thing)))))
             (at-dest ()
               (and (eql (u:x thing) (u:x dest))
                    (eql (u:y thing) (u:y dest)))))
      (ecase (t:state thing)
        (:to-end
         (move-it)
         (when (at-dest)
           (setf (t:state thing) :to-start)))
        (:to-start
         (move-it)
         (when (at-dest)
           (setf (t:state thing) :to-end)))))))

(defun idle-loop (game)
  ;; move
  (dolist (i (things game))
    (move game i))
  (move game (hero game))
  ;; collide
  ;;(do-collisions game)
  ;; draw
  (setf (u:x (offset game)) (- (u:x (hero game)) 512)
        (u:y (offset game)) (- (u:y (hero game)) 384))
  (m:draw (map game) (u:x (offset game)) (u:y (offset game)) 1024 768)
  (m:draw-portals-with (m:portals (map game)) (t:aget :portal) (u:x (offset game)) (u:y (offset game)) 1024 768)
  (dolist (i (things game))
    (t:draw i (offset game)))
  (t:draw (hero game) (offset game))
  )

(defun title-screen ()
  (let ((big (u:load-font (u:resource "sans" "ttf") 36))
        (small (u:load-font (u:resource "sans" "ttf") 18)))
    (sdl:draw-string-blended-* "Evil Slayer" 400 200
                               :font big
                               :color (u:color 30 200 10))
    (sdl:draw-string-blended-* "Arrow keys move. Space jumps." 450 300
                               :font small
                               :color (u:color 10 70 200))
    (sdl:draw-string-blended-* "Press space to begin your quest" 500 500
                               :font small
                               :color (u:color 200 10 70))
    (sdl:update-display)
    (catch 'move-on
      (sdl:with-events ()
        (:quit-event () t)
        (:video-expose-event () (sdl:update-display))
        (:idle () (sdl:update-display))
        (:key-up-event (:key key)
                       (when (sdl:key= key :sdl-key-space)
                         (throw 'move-on :continue)))))))

(defun main ()
  (sdl:with-init ()
    (sdl-ttf:init-ttf)
    (sdl:window 1024 768 :title-caption "Evil Slayer")
    (let* ((tiles (m:default-tiles))
           (game (make-instance 'game
                                  :font (u:load-font (u:resource "sans" "ttf") 14)
                                  :tiles tiles
                                  :things nil)))
      (t:init-animations)
      (load-level game (u:resource "default" "map"))
      (setf (map game) (elt (maps game) 0)
            (hero game) (let ((portal (first (m:portals (map game)))))
                          (make-instance 't:hero :x (1+ (u:x portal)) :y (u:y portal) :width 30 :height 64))
            (sdl:frame-rate) 30)
      (when (eq (title-screen) :continue)
        (sdl:with-events ()
          (:quit-event () t)
          (:video-expose-event () (sdl:update-display))
          (:idle () (idle-loop game) (sdl:update-display))
          (:key-up-event (:key key)
                         (handle-key-up game key))
          (:key-down-event (:key key)
                           (handle-key-down game key)))))))
