;;;; SHOULD-TEST system definition
;;;; (c) 2013 Vsevolod Dyomkin

(asdf:defsystem #:should-test
  :version "0.4.0"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "Minimal yet feature-rich Common Lisp test framework."
  :depends-on (#:rutilsx)
  :serial t
  :components ((:file "should-test")
               (:file "self-test")))
