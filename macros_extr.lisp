;;;
;;; Copyright (c) 2014 OOHASHI Daichi <dico.leque.comicron at gmail.com>,
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; 3. Neither the name of the authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this
;;;    software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(defmacro define (v expr)
  (let ((backing-var (intern (format nil "*define-backing-var-for ~A*"
                                     (symbol-name v))
                             (symbol-package v))))
    `(progn
       (defvar ,backing-var)
       (define-symbol-macro ,v ,backing-var)
       (setq ,backing-var ,expr))))

(defmacro @ (f &rest args)
  "funcall for curried functions"
  (reduce (lambda (acc x)
            `(funcall ,acc ,x))
          args
          :initial-value f))

(defmacro lambdas (args body)
  "curried lambda"
  (reduce (lambda (arg acc)
               `(lambda (,arg) ,acc))
             args
             :initial-value body
             :from-end t))

(defstruct promise
  thunk
  evaluated-p
  value)

(defmacro delay (expr)
  `(make-promise :thunk (lambda () ,expr)))

(defun force (promise)
  (if (promise-evaluated-p promise)
      (promise-value promise)
      (let ((v (funcall (promise-thunk promise))))
        (setf (promise-value promise) v)
        (setf (promise-evaluated-p promise) t)
        v)))

(defmacro letrec (binds &rest body)
  `(let (,@(mapcar #'car binds))
     ,@(mapcar (lambda (bind) `(setq ,@bind)) binds)
     ,@body))

(defmacro match (expr &rest clauses)
  (let ((v (gensym)))
    `(let ((,v ,expr))
       (cond ,@(loop for ((c . args) . body) in clauses
                     collect `((eq (car ,v) ,c)
                               (destructuring-bind ,args (cdr ,v)
                                 ,@body)))))))
