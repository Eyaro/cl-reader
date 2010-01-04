;;copied from ALEXANDRIA, which is in public domain

(in-package :cl-reader)

(defun required-argument (&optional name)
  "Signals an error for a missing argument of NAME. Intended for
use as an initialization form for structure and class-slots, and
a default value for required keyword arguments."
  (error "Required argument ~@[~S ~]missing." name))

(define-condition simple-style-warning (style-warning simple-warning)
  ())

(defun simple-style-warning (message &rest args)
  (warn 'simple-style-warning :format-control message :format-arguments args))

;; We don't specify a :report for simple-reader-error to let the
;; underlying implementation report the line and column position for
;; us. Unfortunately this way the message from simple-error is not
;; displayed, unless there's special support for that in the
;; implementation. But even then it's still inspectable from the
;; debugger...
(define-condition simple-reader-error
    #-sbcl(reader-error simple-error)
    #+sbcl(sb-int:simple-reader-error)
  ())

(defun simple-reader-error (stream message &rest args)
  (error 'simple-reader-error
         :stream stream
         :format-control message
         :format-arguments args))

(define-condition simple-parse-error (simple-error parse-error)
  ())

(defun simple-parse-error (message &rest args)
  (error 'simple-parse-error
         :format-control message
         :format-arguments args))

(define-condition simple-program-error (simple-error program-error)
  ())

(defun simple-program-error (message &rest args)
  (error 'simple-program-error
         :format-control message
         :format-arguments args))


(define-condition reader-eof-error (end-of-file)
  ((context :reader reader-eof-error-context :initarg :context))
  (:report
   (lambda (condition stream)
     (format stream
             "unexpected end of file on ~S ~A"
             (stream-error-stream condition)
             (reader-eof-error-context condition)))))

;;TODO: reader-eof-err?
(defun reader-eof-error (stream context)
  (error 'reader-eof-error :stream stream :context context))
  





