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


(defclass base-socket ()
  ((address
    :accessor address)
   (port
    :accessor port)
   (fd
    :accessor fd)
   (domain
    :initarg :domain
    :accessor domain)
   (socket-type
    :initarg :socket-type
    :accessor socket-type)
   (backlog
    :initform 5
    :accessor backlog)
   (child-fd
    :accessor child-fd)))

(defvar stream-sock (make-instance 'base-socket
				     ; AF_INET
				     :domain 2
				     ; SOCK_STREAM
				     :socket-type 1))

(defmethod initialize-instance :after ((obj base-socket) &key)
  (create-socket obj))

(defmethod create-socket ((obj base-socket))
  (setf (slot-value obj 'fd)
	(foreign-funcall "socket" :int (domain obj) :int (socket-type obj) :int 0 :int)))

(defmethod close-socket ((obj base-socket))
  (if (fd obj)
      (progn
	(foreign-funcall "close" :int (fd obj) :int)
	(setf (slot-value obj 'fd) nil)))

  (handler-case
      (progn
	(if (child-fd obj)
	    (progn
	      (foreign-funcall "close" :int (child-fd obj) :int)
	      (setf (slot-value obj 'child-fd) nil))))
    (unbound-slot (err)
      (declare (ignore err))
      (format t "Child file descriptor was never set."))))

(defmethod bind ((obj base-socket) address port)
  (setf (slot-value obj 'address) address)
  (setf (slot-value obj 'port) port)
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

(defmethod socket-listen ((obj base-socket))
  (foreign-funcall "listen"
		   :int (fd obj)
		   :int (backlog obj)
		   :int))

(defmethod accept-connection ((obj base-socket))
  (with-foreign-objects ((client '(:struct socket-addr-in))
			 (len :int))
    (setf (slot-value obj 'child-fd) (foreign-funcall "accept"
						      :int (fd obj)
						      :pointer client
						      :pointer len
						      :int))))

(defmethod recv ((obj base-socket))
  (with-foreign-pointer-as-string (buf 4096)
    (foreign-funcall "read"
		     :int (child-fd obj)
		     :pointer buf
		     :int 4096
		     :int)buf)))
