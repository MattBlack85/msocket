(in-package #:cl-user)
(defpackage #:msocket/tests/communication
  (:use #:cl
        #:rove
        #:msocket))
(in-package #:msocket/tests/communication)

(deftest create-file-descriptor
  (testing "After instance is created a new fd is also created"
    (ok (= (slot-value msocket:stream-socket 'fd) 3))))
