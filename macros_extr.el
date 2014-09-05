;;; -*- lexical-binding: t -*-
;;;
;;; Copyright (c) 2014 OOHASHI Daichi <dico.leque.comicron at gmail.com>,
;;; All rights reserved.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; For the complete terms of the GNU General Public License,
;;; please see the URL:
;;; http://www.gnu.org/licenses/gpl-2.0.htm
;;;

(require 'cl-lib)

(defmacro define (v expr)
  `(progn
     (defvar ,v)
     (internal-make-var-non-special ',v)
     (setq ,v ,expr)))

(defmacro @ (f &rest args)
  "funcall for curried functions"
  (cl-reduce (lambda (acc x)
               `(funcall ,acc ,x))
             args
             :initial-value f))

(defmacro lambdas (args body)
  "curried lambda"
  (declare (indent 1))
  (cl-reduce (lambda (arg acc)
               `(lambda (,arg) ,acc))
             args
             :initial-value body
             :from-end t))

(cl-defstruct promise
  thunk evaluated-p value)

(defmacro delay (expr)
  `(make-promise :thunk (lambda () ,expr)))

(defun force (promise)
  (if (promise-evaluated-p promise)
      (promise-value promise)
    (let ((v (funcall (promise-thunk promise))))
      (setf (promise-value promise) v)
      (setf (promise-evaluated-p promise) t)
      v)))

(defmacro match (expr &rest clauses)
  (declare (indent 1))
  `(pcase ,expr
     ,@(cl-loop for (tag&args body) in clauses
                collect (let ((tag (car tag&args))
                              (args (cdr tag&args)))
                          `((,backquote-backquote-symbol
                             (,tag
                              ,@(mapcar (lambda (arg)
                                          `(,backquote-unquote-symbol ,arg))
                                        args)))
                            ,body)))))
