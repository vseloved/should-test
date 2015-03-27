;;; (c) 2013 Vsevolod Dyomkin

(in-package #:should-test)
(named-readtables:in-readtable rutils-readtable)


(defmacro xml (tag &rest attrs-&-body)
  `(progn (format *xml-out* ,(fmt "<~A" tag))
          (loop :for (attr val) :on (butlast attrs-&-body) :by #'cddr :do
             (format *xml-out* ,(fmt " ~A=\"~~A\"" attr) ,val))
          ,(if (oddp attrs-&-body)
               `(progn ,(last1 attrs-&-body)
                       (format *xml-out* ,(fmt "</~A>" tag)))
               `(format *xml-out* " />"))))

(defun test-for-xunit (out &rest args &key (package *package*) test)
  "Like TEST but writes xunit-style XML test report to OUT."
  (let ((*xml-out* out)
        (start-ts (local-time:now)))
    (mv-bind (success? failures errors) (apply 'test args)
      (let ((tests (when test (list test)))
            (now (local-time:now)))
        (unless tests
          (do-symbols (sym package)
            (when-it (get sym 'test)
              (push it tests)))
          (reversef tests))
        (xml :testsuite
             :tests (length tests)
             :failures (length failures)
             :errors (length errors)
             :hostname (osicat-posix:gethostname)
             :name (fmt "~A~@[:~A~]" package test)
             :time (* (local-time:timestamp-difference now start-ts)
                      1000)
             :timestamp (local-time:to-rfc3339-timestring now)
             :system-out "" :system-err ""
             ;; (xml :property :name ... :value ...)
             (dolist (test tests)
               (let ((failure (get# test failures))
                     (error (get# test errors)))
               (xml :testcase :classname package :name test :time "0"
                    (when (or failure error)
                      (xml :failure :type (if failure "failure" "error")
                           ;; :message message
                           (or failure error)))))))))))

