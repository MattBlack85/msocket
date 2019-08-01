(require ':cffi)

(defpackage :cffi-sock
  (:use :common-lisp :cffi))

(in-package :cffi-sock)

(define-foreign-library msocket
  (:unix (:or "libc.so.6" "libc.so"))
  (t (:default "msocket")))

(use-foreign-library msocket)

(defcstruct socket-addr-in
  (sin-family :short)
  (sin-port :unsigned-short)
  (sin-addr :unsigned-long)
  (sin-zero :string))

(defun inet-addr (addr)
  (foreign-funcall "inet_addr" :string addr :long))


(defclass socket ()
  ((address
    :initarg :address
    :initform "127.0.0.1"
    :accessor address)
   (port
    :initarg :port
    :initform nil
    :accessor port)
   (fd
    :accessor fd)
   (domain
    :initform 2 ; AF_INET
    :accessor domain)
   (socket-type
    :initform 1 ; SOCK_STREAM
    :accessor socket-type)
   (backlog
    :initform 5
    :accessor backlog)
   (child-fd
    :accessor child-fd)))

(defmethod create-socket ((obj socket))
  (setf (slot-value obj 'fd)
	(foreign-funcall "socket" :int (domain obj) :int (socket-type obj) :int 0 :int)))

(defmethod close-socket ((obj socket))
  (foreign-funcall "close" :int (fd obj) :int)
  (foreign-funcall "close" :int (child-fd obj) :int))

(defmethod bind ((obj socket))
  (with-foreign-object (addr '(:struct socket-addr-in))
    (setf (foreign-slot-value addr '(:struct socket-addr-in) 'sin-family) (domain obj))
    (setf (foreign-slot-value addr '(:struct socket-addr-in) 'sin-addr) (inet-addr (address obj)))
    (setf (foreign-slot-value addr '(:struct socket-addr-in) 'sin-port) (foreign-funcall "htons" :int (port obj) :int))
    (with-foreign-slots ((sin-family sin-port sin-addr) addr (:struct socket-addr-in))
      (format t "Family: ~a Port: ~a Address: ~a" sin-family sin-port sin-addr))
    (foreign-funcall "bind"
		     :int (fd obj)
		     :pointer addr
		     :int (foreign-type-size '(:struct socket-addr-in))
		     :int)))

(defmethod socket-listen ((obj socket))
  (foreign-funcall "listen"
		   :int (fd obj)
		   :int (backlog obj)
		   :int))

(defmethod accept-connection ((obj socket))
  (with-foreign-objects ((client '(:struct socket-addr-in))
			 (len :int))
    (setf (slot-value obj 'child-fd) (foreign-funcall "accept"
						      :int (fd obj)
						      :pointer client
						      :pointer len
						      :int))))

(defmethod recv ((obj socket))
  (with-foreign-pointer-as-string (buf 4096)
    (foreign-funcall "read"
		     :int (child-fd obj)
		     :pointer buf
		     :int 4096
		     :int)buf)))
