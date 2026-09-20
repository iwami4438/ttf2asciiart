(load "main.lisp")

(sb-ext:save-lisp-and-die
 "main"
 :toplevel #'main
 :executable t)
