(defpackage :next/build-package
  (:use :cl)
  (:import-from :asdf #:perform #:system)
  (:import-from :cffi-toolchain #:static-program-op)
  (:import-from :cffi
                #:foreign-library-pathname
                #:foreign-library-type
                #:list-foreign-libraries
                #:close-foreign-library)
  (:import-from :ppcre #:split)
  (:import-from :uiop
                #:run-program
                #:getenv
                #:register-image-dump-hook)
  (:import-from :next #:+version+))

(in-package :next/build-package)

(defclass package-finder () ())

(defmethod find-library-packages ((f package-finder) ldconfig library)
  (with-input-from-string (s ldconfig)
    ;; skip the first line, it's a header
    (read-line s)

    (loop for line = (read-line s nil 'eof)
       until (eq line 'eof)
       when (let ((parts (split " " (subseq line 1))))
              (string= (first parts) (namestring (foreign-library-pathname library))))
       collect (let ((parts (split " " (subseq line 1))))
                 (find-package-providing f (first (last parts)))))))

(defmethod find-packages ((p package-finder) libraries)
  (let ((ldconfig (run-program "ldconfig -p" :output '(:string))))
    (reduce (lambda (packages library)
              (append
               packages
               (unless (eq (foreign-library-type library) :grovel-wrapper)
                 (find-library-packages ldconfig library))))
            libraries
            :initial-value nil)
    :test #'string=))

(defclass rpm-finder (package-finder) ())

(defmethod find-packages ((f rpm-finder) libraries)
  (append
   ;; sbcl images always depends on those
   #+sbcl '("zlib" "glibc")
   (call-next-method f libraries)))

(defmethod find-package-providing ((f rpm-finder) file)
  (run-program
   `("rpm" "-q" "--whatprovides" ,file "--qf" "%{NAME}")
   :output '(:string)))

(defclass deb-finder (package-finder) ())

(defmethod find-packages ((f deb-finder) libraries)
  (append
   ;; sbcl images always depends on those
   #+sbcl '("zlib1g" "libc6")
   (call-next-method f libraries)))

(defmethod find-package-providing ((f deb-finder) file)
  ;; output is: "<package>:amd64: <file>"
  (first (ppcre:split ":" (run-program
                           `("dpkg" "-S" ,file)
                           :output '(:string :stripped t)))))

(defclass pacman-finder (package-finder) ())

(defmethod find-packages ((f pacman-finder) libraries)
  (append
   ;; sbcl images always depends on those
   #+sbcl '("zlib" "glibc")
   (call-next-method f libraries)))

(defmethod find-package-providing ((f pacman-finder) file)
  ;; output is: "<file> is owned by <package> <version>"
  (fourth (ppcre:split " " (run-program
                            `("pacman" "-Qo" ,file)
                            :output '(:string :stripped t)))))

;;; Make sure we close statically linked libraries.
;;; Remove when this or similar is done in cffi: https://github.com/cffi/cffi/pull/163
(register-image-dump-hook
 (lambda ()
   (loop for library in (list-foreign-libraries)
      when (eq (foreign-library-type library) :grovel-wrapper)
      do (close-foreign-library library))))

(defclass build-package (static-program-op)
  ((package-type :initarg :package-type :reader package-type)))

(defmethod perform ((operation build-package) (system system))
  (call-next-method operation system)

  (let* ((package-finger
          (make-instance
           (cdr (assoc (package-type operation) '(("rpm" . rpm-finder)
                                                  ("deb" . deb-finder)
                                                  ("pacman" . pacman-finder))))))
         (deps (delete-duplicates
                (find-packages package-finder (list-foreign-libraries)))))
    (run-program `("fpm" "-s" "dir"
                         "-t" ,(package-type operation)
                         "-n" "next"
                         "-v" ,+version+
                         ,@(mapcar (lambda (dep) (format nil "--depends=~a" dep)) deps)
                         "next=/usr/bin/"
                         "assets/next.desktop=/usr/share/applications/")
                 :output :interactive
                 :error-output :interactive)))

(setf (find-class 'asdf::build-package) (find-class 'build-package))
