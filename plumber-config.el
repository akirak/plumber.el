;;; plumber-config.el --- Activate all bundled configurations for plumber -*- lexical-binding: t -*-

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

;; This library adds all built-in language supports to
;; `plumber-language-settings'.

;;; Code:

(defun plumber-config-add-language (mode function file)
  (autoload function file)
  (push `(,mode . ,function) plumber-language-settings))

(plumber-config-add-language 'elixir-mode
                             'plumber-elixir-load-settings
                             "plumber-elixir")

(provide 'plumber-config)
;;; plumber-config.el ends here
