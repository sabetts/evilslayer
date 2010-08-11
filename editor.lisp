(defpackage :editor1
  (:use :cl)
  (:shadow :editor :map)
  (:nicknames :e)
  (:export))

(in-package :editor1)

(defclass editor ()
  ((map :initarg :map :accessor map)
   (maps :accessor maps)
   (tiles :initarg :tiles :accessor tiles)
   (offset :initform (u:pt (* 50 64) (* 50 64)) :accessor offset)
   (thing-hash :initarg :thing-hash :accessor thing-hash)
   (things :initarg :things :accessor things)
   (scroll-x :initform 0 :accessor scroll-x)
   (scroll-y :initform 0 :accessor scroll-y)
   (current-thing :initform nil :accessor current-thing)
   (current-tile :initform nil :accessor current-tile)
   (current-portal :initform nil :accessor current-portal)
   (font :initarg :font :accessor font)))

(defun init-maps (editor tiles n)
  (let ((background (sdl-image:load-and-convert-image (u:resource "background" "png"))))
    (setf (maps editor) (make-array n))
    (dotimes (i n)
      (setf (elt (maps editor) i) (m:default-map 100 100 background tiles)))))

(defclass thing-template ()
  ((image :initarg :image :accessor image)
   (object :initarg :object :accessor object)))

(defclass platform-template (thing-template)
  ())

(defclass thing (u:rectangle)
  ((template :initarg :template :accessor template))
  (:default-initargs
   :width 64 :height 64))

(defmethod shared-initialize :after ((obj thing) slots &key)
  (when (typep (template obj) 'thing-template)
    (setf (u:width obj) (sdl:width (s:surface (image (template obj))))
          (u:height obj) (sdl:height (s:surface (image (template obj)))))))

(defclass platform (thing)
  ((end :initarg :end :accessor end)))

(defclass platform-end (thing)
  ())

(defgeneric draw (editor))

(defun draw-mini-map (editor)
  (let ((top 0)
        (left (- 1024 (m:width (map editor)))))
    (sdl:draw-box-* left top
                    (m:width (map editor)) (m:height (map editor))
                    :color (sdl:color :r 0 :g 0 :b 0))
    (sdl:draw-box-* (+ left (floor (u:x (offset editor)) 64))
                    (+ top (floor (u:y (offset editor)) 64))
                    (floor 1024 64)
                    (floor 768 64)
                    :color (sdl:color :r 100))
    (loop for y below (m:height (map editor)) do
         (loop for x below (m:width (map editor)) do
              (when (m:get-tile (map editor) x y)
                (sdl:draw-pixel-* (+ left x) (+ top y)
                                  :color (if (typep (m:get-tile (map editor) x y) 'm:solid-tile)
                                             (sdl:color :r 255 :g 255 :b 255)
                                             (sdl:color :r 100 :g 100 :b 100))))))))

(defmethod draw ((e editor))
  (m:draw (map e) (u:x (offset e)) (u:y (offset e)) 1024 768)
  (m:draw-portals (m:portals (map e)) (current-portal e) (maps e) (font e) (u:x (offset e)) (u:y (offset e)) 1024 768)
  (dolist (i (things e))
    (draw-thing e i))
  (when (current-thing e)
    (multiple-value-bind (x y) (u:get-mouse-state)
      (setf (u:x (current-thing e)) (+ (u:x (offset e)) x)
            (u:y (current-thing e)) (+ (u:y (offset e)) y))
      (draw-thing e (current-thing e))))
  (sdl:draw-string-blended-* (format nil "Map: ~d | Tile: ~d" (position (map e) (maps e)) (position (current-tile e) (tiles e)))
                             5 5 :font (font e) :color (u:gray 255))
  (draw-mini-map e))

(defmethod add-thing ((editor editor) (template thing-template) x y)
  (push (make-instance 'thing
                       :x x
                       :y y
                       :template template)
        (things editor)))

(defmethod add-thing ((editor editor) (template platform-template) x y)
  (let ((end (make-instance 'platform-end
                            :x x :y (+ y 128)
                            :template (gethash :platform-end (thing-hash editor)))))
    (push (make-instance 'platform
                         :x x
                         :y y
                         :template template
                         :end end)
          (things editor))
    (push end (things editor))))

(defmethod del-thing ((editor editor) (thing thing))
  (setf (things editor) (delete thing (things editor))))

(defmethod del-thing ((editor editor) (thing platform))
  (setf (things editor) (delete thing (things editor))
        (things editor) (delete (end thing) (things editor))))

(defmethod del-thing ((editor editor) (thing platform-end))
  ;; these can't be deleted
  )

(defmethod load-level ((editor editor) file)
  (with-open-file (s file)
    (let ((n (read s)))
      (init-maps editor (tiles editor) n)
      (loop for i below n
         do (m:load (elt (maps editor) i) s (tiles editor))))
    (loop for m across (maps editor)
         do (m:relink-portals m (maps editor)))
    (setf (map editor) (elt (maps editor) 0)
          (things editor) (mapcar (lambda (x) (u:load-object x editor)) (read s))
          (things editor) (nconc (things editor)
                                 (loop for i in (things editor)
                                    when (typep i 'platform)
                                    collect (end i))))))

(defmethod save-level ((editor editor) file)
  (with-open-file (s file :direction :output :if-exists :supersede)
    (write-line ";; num maps" s)
    (print (length (maps editor)) s)
    (print `save-map-go)
    (loop for m across (maps editor)
       do (m:save m (maps editor) s (tiles editor)))
    (terpri s)
    (write-line ";; things" s)
    (write (loop for i in (things editor)
                unless (typep i 'platform-end)
                collect (u:dump-object i editor))
           :stream s
           :lines nil :level nil :length nil)))

(defmethod u:dump-object append ((thing thing) &optional editor)
  (list :template (u:getkey (template thing) (thing-hash editor))))

(defmethod u:dump-object append ((thing platform) &optional editor)
  (list :end (u:dump-object (u:rect-from (end thing)))))

(defmethod u:restore-object progn ((thing thing) &optional editor)
  (setf (template thing) (or (gethash (template thing) (thing-hash editor))
                             (error "Can't find ~a template" (template thing)))))

(defmethod u:restore-object progn ((thing platform) &optional editor)
  (setf (end thing) (change-class (u:load-object (end thing)) 'platform-end
                                  :template (gethash :platform-end (thing-hash editor)))))

(defun handle-key (editor key)
  (labels ((set-current (n)
             (when (elt (tiles editor) n)
               (setf (current-tile editor) (elt (tiles editor) n))))
           (set-thing (key)
             (setf (current-thing editor) (make-instance 'thing :template (gethash key (thing-hash editor)))))
           (get-portal ()
             (multiple-value-bind (x y) (u:get-mouse-state)
               (let ((px (+ x (u:x (offset editor))))
                     (py (+ y (u:y (offset editor)))))
                 (find-if (lambda (p) (u:collide p (u:pt px py)))
                          (m:portals (map editor))))))
           (set-map (n)
             (let ((m (elt (maps editor) n)))
               (setf (map editor) m))))
  (let ((amt 32))
         ;;(print `(key ,key))
  (cond
    ((sdl:key= key :sdl-key-escape)
     (sdl:push-quit-event))
    ((sdl:key= key :sdl-key-left)
     (setf (scroll-x editor) (- amt)))
    ((sdl:key= key :sdl-key-right)
     (setf (scroll-x editor) amt))
    ((sdl:key= key :sdl-key-up)
     (setf (scroll-y editor) (- amt)))
    ((sdl:key= key :sdl-key-down)
     (setf (scroll-y editor) amt))
    ((sdl:key= key :sdl-key-b)
     (decf (u:x (offset editor)) (- 1024 128)))
    ((sdl:key= key :sdl-key-f)
     (incf (u:x (offset editor)) (- 1024 128)))
    ((sdl:key= key :sdl-key-u)
     (decf (u:y (offset editor)) (- 768 128)))
    ((sdl:key= key :sdl-key-d)
     (incf (u:y (offset editor)) (- 768 128)))
    ;; tiles
    ((sdl:key= key :sdl-key-0)
     (setf (current-tile editor) nil))
    ((sdl:key= key :sdl-key-1) (set-current 0))
    ((sdl:key= key :sdl-key-2) (set-current 1))
    ((sdl:key= key :sdl-key-3) (set-current 2))
    ((sdl:key= key :sdl-key-4) (set-current 3))
    ((sdl:key= key :sdl-key-5) (set-current 4))
    ((sdl:key= key :sdl-key-6) (set-current 5))
    ((sdl:key= key :sdl-key-7) (set-current 6))
    ((sdl:key= key :sdl-key-8) (set-current 7))
    ((sdl:key= key :sdl-key-9) (set-current 8))
    ;; load/save
    ((sdl:key= key :sdl-key-l)
     (load-level editor (u:resource "default" "map")))
    ((sdl:key= key :sdl-key-s)
     (save-level editor (u:resource "default" "map")))
    ;; portals
    ((sdl:key= key :sdl-key-e)
     (multiple-value-bind (x y) (u:get-mouse-state)
       (let* ((px (+ x (u:x (offset editor))))
              (py (+ y (u:y (offset editor))))
              (match (find-if (lambda (p) (u:collide p (u:pt px py)))
                              (m:portals (map editor)))))
         (if match
             (m:delete-portal match (maps editor))
             (setf (m:portals (map editor)) (append (m:portals (map editor))
                                                    (list (make-instance 'm:portal :exit nil :x (* 64 (floor px 64)) :y (* 64 (floor py 64))))))))))
    ((sdl:key= key :sdl-key-x)
     (let ((p (get-portal)))
       (when p
         (if (current-portal editor)
             (setf (m:exit p) (current-portal editor)
                   (m:exit (current-portal editor)) p
                   (current-portal editor) nil)
             (setf (current-portal editor) p)))))
    ((sdl:key= key :sdl-key-o)
     (let ((p (get-portal)))
       (when p
         (let ((x (m:portal-exit-map p (maps editor))))
           (when x
             (setf (map editor) x
                   (offset editor) (u:sub (u:pt-from (m:exit p)) (u:pt 10 10))))))))
    ((sdl:key= key :sdl-key-f1) (set-map 0))
    ((sdl:key= key :sdl-key-f2) (set-map 1))
    ((sdl:key= key :sdl-key-f3) (set-map 2))
    ((sdl:key= key :sdl-key-f4) (set-map 3))
    ((sdl:key= key :sdl-key-f5) (set-map 4))
    ((sdl:key= key :sdl-key-f6) (set-map 5))
    ((sdl:key= key :sdl-key-f7) (set-map 6))
    ((sdl:key= key :sdl-key-f8) (set-map 7))
    ((sdl:key= key :sdl-key-f9) (set-map 8))
    ((sdl:key= key :sdl-key-f10) (set-map 9))
    ;; things
    ((sdl:key= key :sdl-key-z)
     (set-thing :zombie))
    ((sdl:key= key :sdl-key-a)
     (set-thing :demon))
    ((sdl:key= key :sdl-key-i)
     (set-thing :chest))
    ((sdl:key= key :sdl-key-c)
     (set-thing :candle))
    ((sdl:key= key :sdl-key-p)
     (set-thing :platform))
    ((sdl:key= key :sdl-key-backspace)
     (multiple-value-bind (x y) (u:get-mouse-state)
       (let* ((px (+ x (u:x (offset editor))))
              (py (+ y (u:y (offset editor))))
              (match (find-if (lambda (thing) (u:collide (u:pt px py) thing))
                             (things editor))))
         (cond
           (match
            (del-thing editor match))
           (t
            (setf (current-thing editor) nil))))))
    ((sdl:key= key :sdl-key-space)
     (multiple-value-bind (x y) (u:get-mouse-state)
       (let* ((px (+ x (u:x (offset editor))))
              (py (+ y (u:y (offset editor))))
              (match (find-if (lambda (thing) (u:collide (u:pt px py) thing))
                              (things editor))))
     (cond
       (match
        (let ((o (u:pt-from match)))
          (catch 'done
            (sdl:with-events ()
              (:video-expose-event () (sdl:update-display))
              (:idle () (idle-loop editor))
              (:mouse-motion-event (:x mx :y my)
                                   (setf (u:x match) (+ (u:x o) (- mx x))
                                         (u:y match) (+ (u:y o) (- my y)))
                                   (draw editor)
                                   (sdl:update-display))
              (:key-up-event (:key key)
                             (when (sdl:key= key :sdl-key-space)
                               (throw 'done t)))))))
       ((current-thing editor)
        (add-thing editor
                   (template (current-thing editor))
                   (u:x (current-thing editor))
                   (u:y (current-thing editor))))))))))))

(defun handle-key-up (editor key)
  (cond
    ((or (sdl:key= key :sdl-key-left)
         (sdl:key= key :sdl-key-right))
     (setf (scroll-x editor) 0))
    ((or (sdl:key= key :sdl-key-up)
         (sdl:key= key :sdl-key-down))
     (setf (scroll-y editor) 0))))

(defun handle-button (editor x y button)
  (setf x (+ (u:x (offset editor)) x)
        y (+ (u:y (offset editor)) y)
        (m:tile-at (map editor) x y)
        (if (eql button 1)
            (current-tile editor)
            nil)))

(defun do-scrolling (editor)
  (incf (u:x (offset editor)) (scroll-x editor))
  (incf (u:y (offset editor)) (scroll-y editor)))

(defun visible-p (editor rectangle)
  (u:collide (u:rect (u:x (offset editor)) (u:y (offset editor))
                     (* (m:width (map editor)) 64)
                     (* (m:height (map editor)) 64))
             rectangle))

(defmethod draw-thing :after ((editor editor) (thing thing))
  (sdl-gfx:draw-rectangle-* (- (u:x thing) (u:x (offset editor)))
                            (- (u:y thing) (u:y (offset editor)))
                            (u:width thing) (u:height thing)
                            :color (u:gray 100)))

(defmethod draw-thing ((editor editor) (thing thing))
  (when (visible-p editor thing)
    (s:draw (image (template thing))
            (u:sub thing (offset editor)))))

(defmethod draw-thing ((editor editor) (platform platform))
  (labels ((draw-end-point (pt)
             (sdl-gfx:draw-line-* (- (+ (u:x platform) (u:half (u:width platform))) (u:x (offset editor)))
                                  (- (+ (u:y platform) (u:half (u:height platform))) (u:y (offset editor)))
                                  (- (+ (u:x pt) (u:half (u:width pt))) (u:x (offset editor)))
                                  (- (+ (u:y pt) (u:half (u:height pt))) (u:y (offset editor)))
                                  :color (sdl:color :r 255 :g 255 :b 255))))
    (when (visible-p editor platform)
      (s:draw (image (template platform))
              (u:sub platform (offset editor))))
    (draw-end-point (end platform))))

(defun default-editor-things ()
  (let ((things (make-hash-table)))
    (setf (gethash :zombie things)
          (make-instance 'thing-template
                         :object 't::zombie
                         :image (s:load-image "zombie 1"))
          (gethash :demon things)
          (make-instance 'thing-template
                         :object 't::demon
                         :image (s:load-image "demon-idle-1"))
          (gethash :chest things)
          (make-instance 'thing-template
                         :object 't::chest
                         :image (s:load-image "chest"))
          (gethash :candle things)
          (make-instance 'thing-template
                         :object 't::candle
                         :image (s:load-image "candles"))
          (gethash :platform things)
          (make-instance 'platform-template
                         :object 't::platform
                         :image (s:load-image "platform"))
          (gethash :platform-end things)
          (make-instance 'thing-template
                         :object 't::platform
                         :image (s:load-image "platform" :alpha 128)))
    things))

(defun idle-loop (editor)
  (do-scrolling editor)
  (draw editor)
  (sdl:update-display))

(defun main ()
  (sdl:with-init ()
    (sdl-ttf:init-ttf)
    (sdl:window 1024 768 :title-caption "Evil Slayer")
    (let* ((tiles (m:default-tiles))
           (editor (make-instance 'editor
                                  :font (sdl::open-font (make-instance 'sdl::font-definition
                                                                          :filename (u:resource "sans" "ttf")
                                                                          :size 14))
                                  :tiles tiles
                                  :things nil
                                  :thing-hash (default-editor-things))))
      (init-maps editor tiles 10)
      (setf (map editor) (elt (maps editor) 0)
            (sdl:frame-rate) 30)
      (sdl:with-events ()
        (:quit-event () t)
        (:video-expose-event () (sdl:update-display))
        (:idle () (idle-loop editor))
        (:key-up-event (:key key)
                         (handle-key-up editor key)
                         (draw editor)
                         (sdl:update-display))
        (:key-down-event (:key key)
                         (handle-key editor key)
                         (draw editor)
                         (sdl:update-display))
        (:mouse-motion-event (:x x :y y :state state)
                             (when (plusp state)
                               (handle-button editor x y state)
                               (draw editor)
                               (sdl:update-display)))
        (:mouse-button-down-event (:x x :y y :state state)
                                  (handle-button editor x y state)
                                  (draw editor)
                                  (sdl:update-display))
        ))))

