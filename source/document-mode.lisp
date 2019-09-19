(uiop:define-package :next/document-mode
    (:use :common-lisp :trivia :next)
  (:documentation "Mode for web pages"))
(in-package :next/document-mode)

(define-command foo ()
  "Dummy"
  nil)
