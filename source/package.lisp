;;; package.lisp --- Definition of packages used by Next.

(in-package :cl-user)

;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.
(uiop:define-package next
    (:use :common-lisp :trivia)
  ;; TODO: Ideally, we'd like to define exports at definition site.  Since we
  ;; have packages, ASDF fails to "compile-bundle-op" Next which is required by
  ;; Guix.
  ;; See https://gitlab.common-lisp.net/asdf/asdf/issues/11.

  ;; Uncommend the following export to fix the compile-bundle-op issue.
  ;; (:export :define-mode)
  )
