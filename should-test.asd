;;;; SHOULD-TEST system definition
;;;; (c) 2013-2015 Vsevolod Dyomkin

(asdf:defsystem #:should-test
  :version "1.0.0"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "Minimal yet feature-rich Common Lisp test framework."
  :depends-on (#:rutilsx #:local-time #:osicat #:cl-ppcre)
  :serial t
  :components ((:file "should-test")
               (:file "self-test")
               (:file "xunit")))
