;;; mode.lisp --- Definition of the mode class and root-mode.
;;; All modes inherit from the root-mode.

(in-package :next)

(export 'define-mode)
(defmacro define-mode (name direct-superclasses docstring direct-slots)
  "Define mode NAME.
When DIRECT-SUPERCLASSES is T, then the mode has no parents.
Otherwise, the mode's parents are ROOT-MODE and DIRECT-SUPERCLASSES.

A mode toggler command is also defined as NAME.
Its arguments are passed to the class instantiation."
  `(progn
     (export ',name)
     (defclass ,name ,(unless (eq (first direct-superclasses) t)
                        (append direct-superclasses '(root-mode)))
       ,direct-slots
       (:documentation ,docstring))
     ,(unless (eq name 'root-mode)
        `(define-command ,name (&rest args)
           ,docstring
           (log:info "New mode: ~a" (apply #'make-instance ',name
                                           :name (format nil "~a" ',name)
                                           args))))))

(define-mode root-mode (t)
  "The root of all modes."
  ((name :accessor name :initarg :name)))
