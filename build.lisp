(load "main.lisp")

(sb-ext:save-lisp-and-die
 "ttf2ascliart"
 :toplevel #'main
 :executable t
 :compression t)
