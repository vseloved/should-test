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

(deftest should-be ()
  (let ((*test-output* (make-broadcast-stream)))
    (should be null
            (should be eql nil t))))

(deftest should-signal ()
  (let ((*test-output* (make-broadcast-stream)))
    (should signal simple-error
            (error "Error"))))

(deftest should-print-to ()
  (let ((*verbose* t))
    (should print-to *test-output*
            #/(PRINC testa) FAIL
expect: "test"
actual: "testa"
/#
            (should print-to *standard-output* "test" (princ "testa")))))
