;;; recur-test.el --- test for recur

;; Copyright (C) 2021  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords:

;; Version: 0.0.0
;; Package-Requires:
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

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'recur)

(ert-deftest recur-progv ()
  (should
   (eq
    (recur-progv '(x y) '(5 1)
      (if (eq x 0)
          y
        (recur (1- x) (* x y))))
    120)))

(ert-deftest recur-loop ()
  (should
   (eq
    (recur-loop
        ((x 5)
         (y 1))
      (if (eq x 0)
          y
        (recur (1- x) (* x y))))
    120)))

(ert-deftest recur-defun-normal ()
  (should
   (eq
    (let ((factorial (cl-gensym "factorial")))
      (eval
       `(recur-defun ,factorial (x y)
          (if (eq x 0)
              y
            (recur (1- x) (* x y)))))
      (funcall factorial 5 1))
    120)))

(ert-deftest recur-defun-optional ()
  (should
   (eq
    (let ((factorial (cl-gensym "factorial")))
      (eval
       `(recur-defun ,factorial (x &optional y)
          (setq y (or y 1))
          (if (eq x 0)
              y
            (recur (1- x) (* x y)))))
      (funcall factorial 5))
    120)))

(ert-deftest recur-defun-deep ()
  (should-error
   (let ((max-lisp-eval-depth 5)
         (bad-factorial (cl-gensym "bad-factorial")))
     (eval
      `(defun ,bad-factorial (x &optional y)
         (setq y (or y 1))
         (if (eq x 0)
             y
           (,bad-factorial (1- x) (* x y)))))
     (funcall bad-factorial 4000)))

  ;; Should not error
  (let ((max-lisp-eval-depth 5)
        (factorial (cl-gensym "factorial")))
    (eval
     `(recur-defun ,factorial (x &optional y)
        (setq y (or y 1))
        (if (eq x 0)
            y
          (recur (1- x) (* x y)))))
    (funcall factorial 4000)))

(provide 'recur-test)
;;; recur-test.el ends here
