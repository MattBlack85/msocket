(defsystem "msocket"
  :class :package-inferred-system
  :version "0.0.1"
  :description "Linux socket API into common lisp"
  :license "BSD"
  :depends-on ("cffi")
  :components ((:file "socket")))

(defsystem "msocket/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "msocket/tests/communication")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
