(defpackage cerberus
  (:use :cl)
  (:export #:auth
           #:invalid-password
           #:invalid-user
           #:login
           #:logged-in-p
           #:logout
           #:msg
           #:roles
           #:role-p
           #:setup
           #:user-name))

(in-package cerberus)

(defparameter *user-p* nil)
(defparameter *user-pass* nil)
(defparameter *user-roles* nil)

(define-condition invalid-password (error)
  ((msg :initarg :msg :reader msg)))

(define-condition invalid-user (error)
  ((msg :initarg :msg :reader msg)))

(defun setup (&key user-p user-pass user-roles)
  (setf *user-p* user-p)
  (setf *user-pass* user-pass)
  (setf *user-roles* user-roles))

(defun login (&key user password)
  (cond
    ((not (funcall *user-p* user))
      (error 'invalid-user :msg (format nil "No such user ~A" user)))

    ((not (cl-pass:check-password password (funcall *user-pass* user)))
      (error 'invalid-password :msg (format nil "Invalid Password for ~A" user)))

    (t
      (setf (gethash :username ningle:*session*) user)
      (setf (gethash :roles ningle:*session*) (funcall *user-roles* user)))))

(defun logged-in-p ()
  (handler-case (gethash :username ningle:*session*)
    (type-error () nil)))

(defun user-name ()
  (logged-in-p))

(defun roles ()
  (gethash :roles ningle:*session*))

(defun role-p (role)
  (member role (roles) :test #'equal))

(defun logout ()
  (remhash :username ningle:*session*)
  (remhash :roles ningle:*session*))

(defun auth (&rest roles)
  (intersection roles (roles) :test #'equal))
