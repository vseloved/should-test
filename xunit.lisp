;;; (c) 2013-2015 Vsevolod Dyomkin

(in-package #:should-test)
(named-readtables:in-readtable rutils-readtable)

(defparameter *xml-output* *standard-output*)

(defun xml-escape (value)
  (macrolet ((replace-all (text &rest from-to-pairs)
               (if (endp from-to-pairs)
                   text
                   `(re:regex-replace-all
                     ,(caar from-to-pairs)
                     (replace-all ,text ,@(cdr from-to-pairs))
                     ,(cdar from-to-pairs)))))
  (let ((value (princ-to-string value)))
    (replace-all value
                 ("<" . "&lt;")
                 (">" . "&gt;")
                 ("'" . "&#39;")
                 ("\"" . "&quot;")
                 ("&" . "&amp;")))))

(defmacro xml (tag &rest attrs-&-body)
  `(progn (write-string ,(fmt "<~(~A~)" tag) *xml-output*)
          ,@(loop :for (attr val) :on (butlast attrs-&-body) :by #'cddr :collect
               `(when ,val
                  (format *xml-output* ,(fmt " ~(~A~)=\"~~A\"" attr)
                          (xml-escape ,val))))
          ,(if (oddp (length attrs-&-body))
               `(progn (write-char #\> *xml-output*)
                       (when-it ,(last1 attrs-&-body)
                         (write-string (xml-escape it) *xml-output*))
                       (write-string ,(fmt "</~A>" tag) *xml-output*))
               `(write-string " />" *xml-output*))))

(defun test-for-xunit (out &rest args &key id (package *package*) test)
  "Like TEST but writes xunit-style XML test report to OUT."
  (let ((*xml-output* out)
        (start-ts (local-time:now))
        (*test-output* (make-string-output-stream))
        (*error-output* (make-string-output-stream)))
    (mv-bind (success? failures errors) (apply 'test args)
      (let ((tests (when test (list test)))
            (now (local-time:now)))
        (unless tests
          (do-symbols (sym package)
            (when-it (and (eql (symbol-package sym) (find-package package))
                          (get sym 'test))
              (push it tests)))
          (reversef tests))
        (unless id
          (write-line "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" *xml-output*))
        (xml :testsuite
             :tests (length tests)
             :failures (if failures (hash-table-count failures) 0)
             :errors (if errors (hash-table-count errors) 0)
             :hostname (osicat-posix:gethostname)
             :name (fmt "~A~@[:~A~]" package test)
             :time (coerce (* (local-time:timestamp-difference now start-ts)
                              1000)
                           'float)
             :timestamp (slice (local-time:to-rfc3339-timestring now)
                               0 19)  ; till seconds
             (progn
               (xml :properties nil)
               (dolist (test tests)
                 (let ((failure (when failures (get# test failures)))
                       (error (when errors (get# test errors))))
                   (xml :testcase :classname (package-name package)
                        :name test :time "0"
                        (cond
                          (failure (xml :failure :type "failure" failure))
                          (error (xml :error :type "error" error))))))
               (xml :system-out (get-output-stream-string *test-output*))
               (xml :system-err (get-output-stream-string *error-output*))))))))
