(asdf:defsystem :next
  :version "1.3.1"
  :author "Atlas Engineer LLC"
  :license "BSD 3-Clause"
  :serial t
  :defsystem-depends-on ("trivial-features")
  :depends-on (:log4cl
               :trivia)
  :components ((:module "source"
                :components
                (;; Core Functionality
                 (:file "package")
                 (:file "command")
                 (:file "mode")
                 (:file "document-mode"))))
  :build-operation "program-op"
  :build-pathname "next"
  :entry-point "next:entry-point")
