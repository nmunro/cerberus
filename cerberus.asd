(defsystem "cerberus"
  :version "0.0.1"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on (:ningle
               :cl-pass)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Generate a skeleton for modern project"
  :in-order-to ((test-op (test-op "cerberus/tests"))))

(defsystem "cerberus/tests"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on ("cerberus"
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cerberus"
  :perform (test-op (op c) (symbol-call :rove :run c)))
