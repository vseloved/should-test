;;;;; SHOULD-TEST self-test suite
;;;;; (c) 2013 Vsevolod Dyomkin

(in-package #:should-test)
(named-readtables:in-readtable rutils-readtable)

(defmethod asdf:perform ((o asdf:test-op)
                         (s (eql (asdf:find-system :should-test))))
  (asdf:load-system :should-test)
  (let ((*verbose* nil))
    (test :package :should-test))
  t)


(deftest deftest ()
  (should be true
          (progn (deftest foo ())
                 (get 'foo 'test))))

(deftest undeftest ()
  (should be true
          (progn (deftest foo ())
                 (undeftest 'foo)))
  (should be null
          (undeftest 'foo)))

(deftest test ()
  (should signal should-test-error
          (test :test (gensym)))
  (should be true
          (test :test 'deftest))
  (should be true
          (test :package :cl))  ;; no tests defined for CL package
  (should be null
          (progn (deftest foo () (should be null t))
                 (prog1 (test :test 'foo)
                   (undeftest 'foo))))
  (should be null
          (progn (deftest foo ()
                   (let ((bar t))
                     (+ 1 2)
                     (should be true bar)))
                 (prog1 (test :test 'foo)
                   (undeftest 'foo)))))

(deftest should-be ()
  (let ((*test-output* (make-broadcast-stream)))
    (should be null
            (handler-case (should be eql nil t)
              (should-checked () nil)))))

(deftest should-signal ()
  (let ((*test-output* (make-broadcast-stream)))
    (should signal simple-error
            (error "Error"))))

(deftest should-print-to ()
  (let ((*verbose* t))
    (should print-to *test-output*
            #/(PRINC bar) FAIL
expect: "foo"
actual: "bar"
/#
    (handler-case
        (should print-to *standard-output* "foo" (princ "bar"))
      (should-checked () nil)))))
