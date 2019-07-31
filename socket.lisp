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

(defvar main-fd nil)
(defvar child-fd nil)

  
(defun create-socket (domain socket-type)
  (setq main-fd (foreign-funcall "socket" :int domain :int socket-type :int 0 :int)))

(defun close-socket (file-descriptor)
  (foreign-funcall "close" :int file-descriptor :int))

(defun inet-addr (addr)
  (foreign-funcall "inet_addr" :string addr :long))


(defun bind (fd address port)
  (with-foreign-object (addr '(:struct socket-addr-in))
    ;; Init the slots
    (setf (foreign-slot-value addr '(:struct socket-addr-in) 'sin-family) 2)
    (setf (foreign-slot-value addr '(:struct socket-addr-in) 'sin-addr) (inet-addr address))
    (setf (foreign-slot-value addr '(:struct socket-addr-in) 'sin-port) (foreign-funcall "htons" :int port :int))
    (with-foreign-slots ((sin-family sin-port sin-addr) addr (:struct socket-addr-in))
      (format t "Family: ~a Port: ~a Address: ~a" sin-family sin-port sin-addr))
    (foreign-funcall "bind"
		     :int fd
		     :pointer addr
		     :int (foreign-type-size '(:struct socket-addr-in))
		     :int)))


(defun socket-listen (fd backlog)
  (foreign-funcall "listen"
		   :int fd
		   :int backlog
		   :int))

(defun accept-connection (fd)
  (with-foreign-objects ((client '(:struct socket-addr-in))
			 (len :int))
    (setq child-fd (foreign-funcall "accept"
				    :int fd
				    :pointer client
				    :pointer len
				    :int))
    (with-foreign-pointer-as-string (buf 4096)
      (foreign-funcall "read"
		       :int child-fd
		       :pointer buf
		       :int 4096
		       :int)buf))))

(defun start-server (address port &optional (backlog 5))
  (create-socket 2 1)
  (bind main-fd address port)
  (socket-listen main-fd backlog)
  (accept-connection main-fd)
  (close-socket main-fd)
  (close-socket child-fd))
