(load "main.lisp")

(sb-ext:save-lisp-and-die
 "ttf2asciiart"
 :toplevel #'main
 :executable t
 :compression t)
