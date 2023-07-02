(defpackage cerberus/tests/main
  (:use :cl
        :cerberus
        :rove))
(in-package :cerberus/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cerberus)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))
