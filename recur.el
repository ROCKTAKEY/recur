;;; recur.el --- Tail call optimization              -*- lexical-binding: t; -*-

;; Copyright (C) 2021  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: lisp

;; Version: 0.1.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/ROCKTAKEY/recur
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Tail call optimization for Emacs.
;;   `recur' means recursive call (like clojure).  All `recur' must be called on tail.
;;
;;     (recur-loop                             ;Same as `recur-let'
;;         ((x 5)
;;          (y 1))
;;       (if (eq x 0)
;;           y
;;         (recur (1- x) (* x y))))
;;     ;; => 120
;;
;;     (recur-progv '(x y) '(5 1)
;;       (if (eq x 0)
;;           y
;;         (recur (1- x) (* x y))))
;;     ;; => 120
;;
;;     (recur-defun factorial (x &optional y)
;;       (setq y (or y 1))
;;       (if (eq x 0)
;;           y
;;         (recur (1- x) (* x y))))
;;     ;; => factorial
;;
;;     (factorial 5 1)
;;     ;; => 120
;;
;;     (factorial 5)
;;     ;; => 120
;;
;;; How to Use?
;;;; `recur-loop' / `recur-let'
;;    Recursively callable `let'.  Similar to `loop' in clojure.
;;    This is same as `recur-progv', except using `BINDINGS' like `let' instead of
;;    `SYMBOLS' and `INITVALUES'.  `BODY' is evaluated with variables set by `BINDINGS'.
;;
;;    Note that `recur' MUST be tail recursion and this macro optimize tail call.
;;
;;;; `recur-progv'
;;    Recursively callable `cl-progv'.
;;    `SYMBOLS', `INITVALUES' (as `VALUES'), and `BODY' are same as arguments of `cl-progv'.
;;    In addition, you can use function `recur' in `BODY' form.  This function take
;;    arguments same number as length of `SYMBOLS', and evaluate `BODY' form with `SYMBOLS'
;;    bounded by each value of arguments.
;;
;;    Note that `recur' MUST be tail recursion and this macro optimize tail call.
;;
;;;; `recur-defun'
;;    Define function with tail call optimization.
;;    `NAME', `ARGLIST', `DOCSTRING' and `BODY' is same as arguments of `defun'.
;;    `recur' in `BODY' calls the function named NAME itself with arguments.
;;    `recur' cannot recieve variable length arguments, so you must pass one list
;;    even as =&rest= argument.
;;
;;    Note that `recur' MUST be tail recursion and this macro optimize tail call.
;;
;;; License
;;   This package is licensed by GPLv3. See LICENSE.

;;; Code:

(require 'cl-lib)

;;;###autoload
(defmacro recur-progv (symbols initvalues &rest body)
  "Recursively callable `cl-progv'.
SYMBOLS, INITVALUES (as VALUES), and BODY are same as arguments of `cl-progv'.
In addition, you can use function `recur' in BODY form.  This function take
arguments same number as length of SYMBOLS, and evaluate BODY form with SYMBOLS
bounded by each value of arguments.

Note that `recur' MUST be tail recursion and this macro optimize tail call."
  (declare (indent 2) (debug (form form body)))
  (let ((continue (cl-gensym "continue"))
        (values (cl-gensym "values"))
        (result (cl-gensym "result")))
    `(let ((,continue t)
           (,values ,initvalues)
           ,result)
       (while ,continue
         (setq ,continue nil)
         (setq
          ,result
          (cl-labels ((recur
                       (&rest args)
                       (setq ,continue t)
                       (setq ,values args)))
            (cl-progv ,symbols ,values
              ,@body))))
       ,result)))

;;;###autoload
(defmacro recur-loop (bindings &rest body)
  "Recursively callable `let'.  Similar to loop in clojure.
This is same as `recur-progv', except using BINDINGS like `let' instead of
SYMBOLS and INITVALUES.  BODY is evaluated with variables set by BINDINGS.

Note that `recur' MUST be tail recursion and this macro optimize tail call."
  (declare (indent 1) (debug t))
  `(recur-progv
       ',(mapcar #'car bindings)
       (list ,@(mapcar #'cadr bindings))
     ,@body))

;;;###autoload
(defalias 'recur-let #'recur-loop)

;;;###autoload
(defmacro recur-defun (name arglist &optional docstring &rest body)
  "Define function with tail call optimization.
NAME, ARGLIST, DOCSTRING and BODY is same as arguments of `defun'.
`recur' in BODY calls the function named NAME itself with arguments.
`recur' cannot recieve variable length arguments, so you must pass one list
even as `&rest' argument.

Note that `recur' MUST be tail recursion and this macro optimize tail call."
  (declare (doc-string 3) (indent 2))
  (unless (stringp docstring)
    (setq body (cons docstring body))
    (setq docstring nil))
  (let* ((symbols (cl-remove-if
                   (lambda (arg)
                     (string-match-p "^&" (symbol-name arg)))
                   arglist))
         (gensyms (mapcar #'cl-gensym (mapcar #'symbol-name symbols))))
    `(defun ,name ,arglist ,docstring
            (recur-progv  ',gensyms (list ,@symbols)
              (cl-symbol-macrolet
                  ,(cl-mapcar
                    (lambda (symbol gensym)
                      (list symbol gensym))
                    symbols
                    gensyms)
                ,@body)))))

(provide 'recur)
;;; recur.el ends here
