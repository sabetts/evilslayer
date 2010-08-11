;;; -*- Mode: Lisp -*-

(defpackage :evilslayer-system
  (:use :cl :asdf))
(in-package :evilslayer-system)

(defsystem :evilslayer
  :name "Evil Slayer"
  :author "Shawn Betts <sabetts@gmail>"
  :serial t
  :depends-on (:lispbuilder-sdl :lispbuilder-sdl-image :lispbuilder-sdl-ttf :lispbuilder-sdl-gfx)
  :components ((:file "util")
               (:file "sprite")
               (:file "things")
               (:file "map")
               (:file "editor")
               (:file "game")))

