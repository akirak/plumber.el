;;; plumber-elixir.el --- Plumber configuration for Elixir -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (avy "0.4") (dash "2.12"))
;; Keywords: convenience
;; URL: https://github.com/akirak/plumber.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library adds support for Elixir to plumber.

;;; Code:

(require 'plumber)
(require 'elixir-mode)

(defconst plumber-elixir-block-begin-regexp
  (rx (or (seq symbol-start "do")
          (seq "," (+ space) "do:"))
      symbol-end))

(defconst plumber-elixir-list-item-regexp
  (let ((open-pair `(or ,@(-map (lambda (p) (char-to-string (car p)))
                                elixir-sigil-delimiter-pair))))
    (rx-to-string
     `(or ,open-pair
          (and symbol-start (char alnum "&:_^@"))
          (and (char "\"'^%!") (not space))))))

(defconst plumber-elixir-word-regexp
  (rx symbol-start (or (syntax word) (syntax symbol))))

(defun plumber-elixir-block-end-function ()
  "Find an end of the block."
  (let ((open (thing-at-point 'symbol)))
    (if (string-equal "do:" open)
        (end-of-line 1)
      (let ((level (save-excursion
                     (back-to-indentation)
                     (car (posn-col-row (posn-at-point))))))
        (when (re-search-forward (concat "^" (make-string level ?\s)
                                         "end\\>"))
          (backward-char 1)
          (goto-char (car (bounds-of-thing-at-point 'symbol))))))))

;;;###autoload
(defun plumber-elixir-load-settings ()
  (plumber-define-language elixir-mode
    :beginning-of-defun elixir-beginning-of-defun
    :function-body-begin-regexp plumber-elixir-block-begin-regexp
    :function-body-end-function plumber-elixir-block-end-function
    :down-list-begin-regexp plumber-elixir-block-begin-regexp
    :list-end-function plumber-elixir-block-end-function
    :list-item-regexp plumber-elixir-list-item-regexp
    :word-regexp plumber-elixir-word-regexp))

(provide 'plumber-elixir)
;;; plumber-elixir.el ends here
