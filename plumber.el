;;; plumber.el --- Utilities for editing source code -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (avy "0.4") (dash "2.12")
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

;; This is a collection of utilities for editing source code
;; efficiently. It is based on avy and some other packages but allows
;; language-specific settings.

;;; Code:

(require 'avy)
(require 'dash)
(require 'subr-x)

(eval-and-compile
  (with-no-warnings
    (if (version< emacs-version "26")
        (progn
          (defalias 'plumber--if-let* 'if-let)
          (defalias 'plumber--when-let* 'when-let))
      (defalias 'plumber--if-let* 'if-let*)
      (defalias 'plumber--when-let* 'when-let*))
    (function-put #'plumber--if-let* 'lisp-indent-function 2)
    (function-put #'plumber--when-let* 'lisp-indent-function 1)))

;;;; Custom variables

(defgroup plumber nil
  "Language-specific fast source code navigation and editing."
  :group 'convenience)

;; TODO: Define a widget

;;;###autoload
(defcustom plumber-language-settings
  nil  
  "List of language-specific settings for plumber."
  :group 'plumber
  :type '(repeat (cons (symbol :tag "Major mode")
                       (plist))))

(defcustom plumber-fallback-settings nil
  "Set of fallback settings for plumber.

This is loaded when `plumber-language-settings' does not contain
  an entry for the major mode of the current buffer."
  :group 'plumber)

(defcustom plumber-avy-word-style avy-style
  "`avy-style' used for selecting a word in plumber."
  :group 'plumber)

(defcustom plumber-fallback-word-regexp
  (rx symbol-start
      (or (syntax word) (syntax symbol)))
  "Fallback regular expressions for words.

This is used when :word-regexp property is not defined in the
settings of the current language."
  :group 'plumber)

(defcustom plumber-enable-keybindings nil
  "Enable additional keybindings defined in `plumber-remap-mode-map'.

If this variable is non-nil, `plumber-remap-mode' is turned on
when `plumber-mode' is turned on, and hence additional
keybindings defined in the keymap activates."
  :group 'plumber)

;;;; Variables

(defvar-local plumber-buffer-language nil
  "Major mode used by plumber in the current buffer.")

(defvar plumber-remap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap down-list] 'plumber-down-block)
    map))

(defvar plumber-enabled-hook nil
  "Hook run after `plumber-mode' is enabled.")
(defvar plumber-disabled-hook nil
  "Hook run after `plumber-mode' is disabled.")

;;;; Autoload functions

;;;###autoload
(define-minor-mode plumber-mode
  "Minor mode for editing source code."
  :init-value nil
  :lighter ("  plumber" (:eval (when plumber-buffer-language
                                 (format "[%s]" plumber-buffer-language))))
  :group 'plumber
  (cond
   (plumber-mode
    (progn
      (add-hook 'after-change-major-mode-hook 'plumber-set-buffer-language)
      (plumber-set-buffer-language)
      (run-hooks 'plumber-enabled-hook)))
   (t
    (progn
      (remove-hook 'after-change-major-mode-hook 'plumber-set-buffer-language)
      (run-hooks 'plumber-disabled-hook))))
  (plumber-remap-mode (and plumber-mode
                           plumber-enable-keybindings)))

;;;###autoload
(define-globalized-minor-mode plumber-global-mode
  plumber-mode
  plumber-mode)

;;;###autoload
(define-minor-mode plumber-remap-mode
  "Turn on/off additional keybindings for `plumber-mode'.

\\{plumber-remap-mode-map}"
  :init-value nil
  :group 'plumber
  :keymap plumber-remap-mode-map)

;;;###autoload
(defun plumber-set-buffer-language ()
  "Set a supported language for plumber."
  (interactive)
  (let* ((language (apply #'derived-mode-p (-map #'car plumber-language-settings)))
         (settings (and language (alist-get language plumber-language-settings))))
    (setq plumber-buffer-language language)
    ;; If the settings is not a list but a symbol, consider it as a
    ;; function to load the actual settings. This allows the user to
    ;; lazily load the settings.
    (when (and settings (symbolp settings))
      (if (fboundp settings)
          (funcall settings)
        (error "%s is not a valid function" settings)))))

;;;;; Defining languages

;;;###autoload
(cl-defmacro plumber-define-language (mode &rest args)
  "Define settings for a major mode.

MODE is a symbol of the mode, and ARGS is a plist which should be
added to `plumber-language-settings'."
  (declare (indent 1))
  `(plumber--if-let* ((pair (assoc (quote, mode) plumber-language-settings)))
       (setcdr pair (quote ,args))
     (push (cons (quote ,mode) (quote ,args)) plumber-language-settings)))

;;;;; Jumping

;;;###autoload
(defun plumber-jump-to-word-in-function-0 ()
  "Jump to a word inside the body of the current function."
  (interactive)
  (let (begin end)
    (save-excursion
      (plumber--beginning-of-current-function-body)
      (setq begin (+ (point) (length (thing-at-point 'symbol))))
      (plumber--matching-function-body-end)
      (setq end (1- (point))))
    (avy-with plumber-jump-to-word-in-function-0
      (avy--generic-jump (rx symbol-start
                             (or (syntax word) (syntax symbol)))
                         t plumber-avy-word-style
                         begin end))))
;;;###autoload
(defun plumber-down-block ()
  "Enter the block under the current level."
  (interactive)
  (let (begin end)
    (save-excursion
      (plumber--beginning-of-down-block)
      (setq begin (+ (point) (length (thing-at-point 'symbol))))
      (plumber--matching-block-end)
      (setq end (1- (point))))
    (pcase current-prefix-arg
      ((or '() (pred integerp))
       (progn
         (goto-char begin)
         (when (re-search-forward (plumber--list-item-regexp)
                                  end t
                                  (or current-prefix-arg 1))
           (backward-char 1))))
      ('(4)
       (avy-with plumber-down-block
         (avy--generic-jump (plumber--list-item-regexp)
                            t plumber-avy-word-style
                            begin end))))))

;;;; Utility functions

;;;;; Property access

(defun plumber--lookup (key &optional allow-fallback)
  "Look up a property in the current language settings.

KEY is a key in the plist.

When ALLOW-FALLBACK is non-nil, a corresponding value is looked
up in `plumber-fallback-settings' if and only if no non-nil value
is defined in the current language."
  (unless plumber-mode
    (user-error "Please turn on plumber-mode"))
  (cond
   (plumber-buffer-language
    (plist-get (alist-get plumber-buffer-language plumber-language-settings)
               key))
   (allow-fallback
    (plist-get plumber-fallback-settings key))))

(defun plumber--lookup-value (key &optional allow-fallback)
  "Like `plumber--lookup', but expand the symbol of its returned value."
  (let ((ret (plumber--lookup key allow-fallback)))
    (cl-typecase ret
      (nil nil)
      (symbol (symbol-value ret))
      (t ret))))

(defun plumber--word-regexp ()
  (or (plumber--lookup-value :word-regexp)
      plumber-fallback-word-regexp))

(defun plumber--list-item-regexp ()
  (or (plumber--lookup-value :list-item-regexp)
      plumber-falback-word-regexp))

;;;;; Jumping

(defun plumber--beginning-of-defun ()
  "`beginning-of-defun'-equivalent for the current language."
  (funcall (or (plumber--lookup :beginning-of-defun)
               'beginning-of-defun)))

(defun plumber--beginning-of-current-function-body (&optional inner)
  "Go to the beginning of the current function body."
  (plumber--if-let* ((regexp (plumber--lookup-value :function-body-begin-regexp)))
      (unless (re-search-backward regexp
                                  (save-excursion
                                    (plumber--beginning-of-defun)
                                    (point))
                                  t)
        (error "Cannot find the beginning"))
    (message "Go to the beginning of the function instead.")
    (plumber--beginning-of-defun))
  (when inner
    (forward-char (length (thing-at-point 'symbol)))))

(defun plumber--end-of-defun ()
  "`end-of-defun'-equivalent for the current language."
  (funcall (or (plumber--lookup :end-of-defun)
               'end-of-defun)))

(defun plumber--matching-function-body-end ()
  "Go to the end of the function body matching the beginning."
  (plumber--if-let* ((func (plumber--lookup :function-body-end-function)))
      (funcall func)
    (plumber--end-of-defun)))

(defun plumber--beginning-of-down-block ()
  "Go to the beginning of the list under the current position."
  (plumber--if-let* ((regexp (plumber--lookup-value :down-block-begin-regexp)))
      (when (re-search-forward regexp
                               (save-excursion
                                 (plumber--end-of-defun)
                                 (point))
                               t)
        (backward-char (length (thing-at-point 'symbol))))
    ;; FIXME: Implement down-list
    (error ":down-block-begin-regexp is undefined, and no fallback is defined")))

(defun plumber--matching-block-end ()
  "Go to the end of the list matching the beginning."
  (plumber--if-let* ((func (plumber--lookup :block-end-function)))
      (funcall func)
    ;; FIXME: Implement a fallback
    (error "fallback")))

(provide 'plumber)
;;; plumber.el ends here
