;;;; Created on 2010-01-02 12:27:19
(in-package :cl-reader)

;;;; the COLLECT macro
;;;;
;;;; comment from CMU CL: "the ultimate collection macro..."

;;; helper functions for COLLECT, which become the expanders of the
;;; MACROLET definitions created by COLLECT
;;;
;;; COLLECT-NORMAL-EXPANDER handles normal collection macros.
;;;
;;; COLLECT-LIST-EXPANDER handles the list collection case. N-TAIL
;;; is the pointer to the current tail of the list, or NIL if the list
;;; is empty.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun collect-normal-expander (n-value fun forms)
    `(progn
       ,@(mapcar (lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
       ,n-value))
  (defun collect-list-expander (n-value n-tail forms)
    (let ((n-res (gensym)))
      `(progn
         ,@(mapcar (lambda (form)
                     `(let ((,n-res (cons ,form nil)))
                        (cond 
                          (,n-tail
                           (setf (cdr ,n-tail) ,n-res)
                           (setq ,n-tail ,n-res))
                          (t
                           (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                   forms)
         ,n-value))))

;;; Collect some values somehow. Each of the collections specifies a
;;; bunch of things which collected during the evaluation of the body
;;; of the form. The name of the collection is used to define a local
;;; macro, a la MACROLET. Within the body, this macro will evaluate
;;; each of its arguments and collect the result, returning the
;;; current value after the collection is done. The body is evaluated
;;; as a PROGN; to get the final values when you are done, just call
;;; the collection macro with no arguments.
;;;
;;; INITIAL-VALUE is the value that the collection starts out with,
;;; which defaults to NIL. FUNCTION is the function which does the
;;; collection. It is a function which will accept two arguments: the
;;; value to be collected and the current collection. The result of
;;; the function is made the new value for the collection. As a
;;; totally magical special-case, FUNCTION may be COLLECT, which tells
;;; us to build a list in forward order; this is the default. If an
;;; INITIAL-VALUE is supplied for COLLECT, the stuff will be RPLACD'd
;;; onto the end. Note that FUNCTION may be anything that can appear
;;; in the functional position, including macros and lambdas.
(defmacro collect (collections &body body)
  (let ((macros ())
        (binds ()))
    (dolist (spec collections)
      (unless (proper-list-of-length-p spec 1 3)
        (error "malformed collection specifier: ~S" spec))
      (let* ((name (first spec))
             (default (second spec))
             (kind (or (third spec) 'collect))
             (n-value (gensym (concatenate 'string
                                           (symbol-name name)
                                           "-N-VALUE-"))))
        (push `(,n-value ,default) binds)
        (if (eq kind 'collect)
            (let ((n-tail (gensym (concatenate 'string
                                               (symbol-name name)
                                               "-N-TAIL-"))))
              (if default
                  (push `(,n-tail (last ,n-value)) binds)
                  (push n-tail binds))
              (push `(,name (&rest args)
                            (collect-list-expander ',n-value ',n-tail args))
                    macros))
            (push `(,name (&rest args)
                          (collect-normal-expander ',n-value ',kind args))
                  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))


(defun featurep (item) (member item *features* :test #'eq))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun proper-list-of-length-p (x min &optional (max min))
    ;; FIXME: This implementation will hang on circular list
    ;; structure. Since this is an error-checking utility, i.e. its
    ;; job is to deal with screwed-up input, it'd be good style to fix
    ;; it so that it can deal with circular list structure.
    (cond ((minusp max) nil)
      ((null x) (zerop min))
      ((consp x)
       (and (plusp max)
            (proper-list-of-length-p (cdr x)
                                     (if (plusp (1- min))
                                         (1- min)
                                         0)
                                     (1- max))))
      (t nil))))

;;;; miscellaneous iteration extensions

;;; like Scheme's named LET
;;;
;;; (CMU CL called this ITERATE, and commented it as "the ultimate
;;; iteration macro...". I (WHN) found the old name insufficiently
;;; specific to remind me what the macro means, so I renamed it.)
(defmacro named-let (name binds &body body)
  (dolist (x binds)
    (unless (proper-list-of-length-p x 2)
      (error "malformed NAMED-LET variable spec: ~S" x)))
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))

;;;; ONCE-ONLY
;;;;
;;;; "The macro ONCE-ONLY has been around for a long time on various
;;;; systems [..] if you can understand how to write and when to use
;;;; ONCE-ONLY, then you truly understand macro." -- Peter Norvig,
;;;; _Paradigms of Artificial Intelligence Programming: Case Studies
;;;; in Common Lisp_, p. 853

;;; ONCE-ONLY is a utility useful in writing source transforms and
;;; macros. It provides a concise way to wrap a LET around some code
;;; to ensure that some forms are only evaluated once.
;;;
;;; Create a LET* which evaluates each value expression, binding a
;;; temporary variable to the result, and wrapping the LET* around the
;;; result of the evaluation of BODY. Within the body, each VAR is
;;; bound to the corresponding temporary variable.
(defmacro once-only (specs &body body)
  (named-let frob ((specs specs)
                   (body body))
    (if (null specs)
        `(progn ,@body)
        (let ((spec (first specs)))
          ;; FIXME: should just be DESTRUCTURING-BIND of SPEC
          (unless (proper-list-of-length-p spec 2)
            (error "malformed ONCE-ONLY binding spec: ~S" spec))
          (let* ((name (first spec))
                 (exp-temp (gensym "ONCE-ONLY")))
            `(let ((,exp-temp ,(second spec))
                   (,name (gensym ,(symbol-name name))))
               `(let ((,,name ,,exp-temp))
                  ,,(frob (rest specs) body))))))))


(defun sequence-bounding-indices-bad-error (array start-value end-value)
  (error "Sequence Bounding Indices Are Bad~% array: ~A~% start: ~A~% end  : ~A" array start-value end-value))
(defun array-bounding-indices-bad-error (array start-value end-value)
  (error "Array Bounding Indices Are Bad~% array: ~A~% start: ~A~% end  : ~A" array start-value end-value))

(defmacro with-array-data (((data-var array &key offset-var)
                            (start-var &optional (svalue 0))
                            (end-var &optional (evalue nil))
                            &key  check-fill-pointer check-bounds)
                           &body forms)
  (once-only ((n-array array)
              (n-svalue  svalue) ;;(the index ,svalue)
              (n-evalue  evalue)) ;;TODO: (the (or index null) ,evalue)
    `(multiple-value-bind (,data-var
                           ,start-var
                           ,end-var
                           ,@(when offset-var `(,offset-var)))
       (let ((,n-array ,n-array))
         (declare (type (simple-array * (*)) ,n-array))
         ,(once-only ((n-len (if check-fill-pointer
                                 `(length ,n-array)
                                 `(array-total-size ,n-array)))
                      (n-end `(or ,n-evalue ,n-len)))
            (if check-bounds
                `(if (<= 0 ,n-svalue ,n-end ,n-len)
                     (values ,n-array ,n-svalue ,n-end 0)
                     ,(if check-fill-pointer
                          `(sequence-bounding-indices-bad-error ,n-array ,n-svalue ,n-evalue)
                          `(array-bounding-indices-bad-error ,n-array ,n-svalue ,n-evalue)))
                `(values ,n-array ,n-svalue ,n-end 0))))
       ,@forms)))

;;; Access *PACKAGE* in a way which lets us recover when someone has
;;; done something silly like (SETF *PACKAGE* :CL-USER). (Such an
;;; assignment is undefined behavior, so it's sort of reasonable for
;;; it to cause the system to go totally insane afterwards, but it's a
;;; fairly easy mistake to make, so let's try to recover gracefully
;;; instead.)
(defun sane-package ()
  (let ((maybe-package *package*))
    (cond ((and (packagep maybe-package)
                ;; For good measure, we also catch the problem of
                ;; *PACKAGE* being bound to a deleted package.
                ;; Technically, this is not undefined behavior in itself,
                ;; but it will immediately lead to undefined to behavior,
                ;; since almost any operation on a deleted package is
                ;; undefined.
                (package-name maybe-package))
           maybe-package)
      (t
       ;; We're in the undefined behavior zone. First, munge the
       ;; system back into a defined state.
       (let ((really-package (find-package :cl-user)))
         (setf *package* really-package)
         ;; Then complain.
         (error 'simple-type-error
                :datum maybe-package
                :expected-type '(and package (satisfies package-name))
                :format-control
                "~@<~S can't be a ~A: ~2I~_~S has been reset to ~S.~:>"
                :format-arguments (list '*package*
                                        (if (packagep maybe-package)
                                            "deleted package"
                                            (type-of maybe-package))
                                        '*package* really-package)))))))

(defmacro enforce-type (value type &aux (val (gensym)))
  `(let ((,val ,value))
     (unless (typep ,val ',type)
       (error "Failed enforce-type: ~A is not of type ~A" ,val ',type))))

;;; These macros handle the special cases of T and NIL for input and
;;; output streams.
;;;
;;; FIXME: Shouldn't these be functions instead of macros?
(defmacro in-synonym-of (stream &optional check-type)
  (let ((svar (gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) *standard-input*)
         ((eq ,svar t) *terminal-io*)
         (t ,@(when check-type `((enforce-type ,svar ,check-type))) ;
            ,svar)))))

;;; In the target Lisp, INTERN* is the primitive and INTERN is
;;; implemented in terms of it. This increases efficiency by letting
;;; us reuse a fixed-size buffer; the alternative would be
;;; particularly painful because we don't implement DYNAMIC-EXTENT. In
;;; the host Lisp, this is only used at cold load time, and we don't
;;; care as much about efficiency, so it's fine to treat the host
;;; Lisp's INTERN as primitive and implement INTERN* in terms of it.
(defun intern* (nameoid length package)
  (intern (replace (make-string length) nameoid :end2 length) package))

(defun find-symbol* (nameoid length package)
  (find-symbol (replace (make-string length) nameoid :end2 length) package))

;;; like (MEMBER ITEM LIST :TEST #'EQ)
(defun memq (item list)
  "Return tail of LIST beginning with first element EQ to ITEM."
  (member item list :test #'eq))

(defun bug (format-control &rest format-arguments)
  (error 'bug
         :format-control format-control
         :format-arguments format-arguments))




