;;; pcre.el --- Emacs Lisp binding of libpcre -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-pcre
;; Version: 0.01
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'pcre-core)
(require 'cl-lib)

(defun pcre--flags (flags)
  (cl-assert (listp flags))
  (let ((vflags (apply #'vector flags)))
    (pcre--core-flags vflags)))

(defun pcre-string-match (regexp str &optional flags)
  (pcre--core-string-match regexp str (pcre--flags flags)))

(defun pcre-string-match-p (regexp str &optional flags)
  (pcre--core-string-match-p regexp str (pcre--flags flags)))

(defun pcre-looking-at (regexp &optional flags)
  (let* ((str (buffer-substring-no-properties (point-min) (point-max)))
         (matched (pcre--core-string-match regexp str (pcre--flags flags) 1)))
    (not (null matched))))

(defun pcre-looking-at-p (regexp &optional flags)
  (let* ((str (buffer-substring-no-properties (point-min) (point-max)))
         (matched (pcre--core-string-match-p regexp str (pcre--flags flags) 1)))
    (not (null matched))))

(defun pcre-looking-back (regexp &optional bound flags)
  (let* ((str (buffer-substring-no-properties (point-min) (or bound (point))))
         (regexp (if (string-match-p "\\z\\'" regexp)
                     regexp
                   (concat regexp "\\z")))
         (matched (pcre--core-string-match
                   regexp str (pcre--flags (cons 'multiline flags)) -1)))
    (not (null matched))))

(defun pcre-re-search-forward (regexp &optional bound noerror count flags)
  (let* ((str (buffer-substring-no-properties (point-min) (or bound (point-max))))
         (matched (pcre--core-string-match
                   regexp str (pcre--flags (cons 'multiline flags)) 1 (or count -1))))
    (if (not matched)
        (unless noerror
          (error "Search failed %s" regexp))
      (goto-char (match-end 0))
      (point))))

(provide 'pcre)

;;; pcre.el ends here
