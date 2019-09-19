(uiop:define-package :next/document-mode
    (:use :common-lisp :trivia :next)
  (:documentation "Mode for web pages"))
(in-package :next/document-mode)

(define-mode document-mode ()
    "Base mode for interacting with documents."
    ((link-hints :accessor link-hints)))
