;;; command.lisp --- command definition functions
;;; This file is licensed under license documents/external/LICENSE1.

(in-package :next)

(export 'define-command)
(defmacro define-command (name (&rest arglist) &body body)
  "Define new command NAME.
ARGLIST must be a list of optional arguments.
This macro also define two hooks, NAME-before-hook and NAME-after-hook.
Regardless of the hook, the command returns the last expression of BODY."
  (let ((documentation (if (stringp (first body))
                           (first body)
                           (warn (make-condition
                                  'command-documentation-style-warning
                                  :name name))))
        (body (if (stringp (first body))
                  (rest body)
                  body)))
    `(progn
       (export ',name)
       (defun ,name ,arglist
         ,documentation
         ,@body))))
