(asdf:defsystem :msocket
  :class :package-inferred-system
  :version "0.0.1"
  :description "Linux socket API into common lisp"
  :license "BSD"
  :depends-on (:cffi)
  :components ((:file "socket"))
  :serial t)
