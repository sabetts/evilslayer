(defpackage :main
  (:use :cl)
  (:nicknames :m)
  (:export))

(in-package :main)

(defun start (main-fn)
  (sdl:with-init (sdl:sdl-init-audio)
    (sdl-mixer:open-audio)
    (sdl:window 1024 768 :title-caption "Evil Slayer")
    (unless (sdl-ttf:is-init)
      (sdl-ttf:init-ttf))
    (sdl:with-surface (disp sdl:*default-display*)
      (unwind-protect
           (catch 'quit
             (funcall main-fn))
        (sdl-mixer:halt-music)
        (sdl-mixer:close-audio t)
        (sdl-mixer::unregister-sample-finished-callback)))))
