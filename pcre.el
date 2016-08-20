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

(defconst pcre--flags
  '((ignorecase . 1)  ;; PCRE_CASELESS
    (extended . 2)    ;; PCRE_EXTENDED
    (multiline . 4))) ;; PCRE_MULTILINE

(defun pcre--flags (flags)
  (cl-loop with ret = 0
           for f in flags
           when (assoc-default f pcre--flags)
           sum it into ret
           finally return ret))

(defun pcre-match-string (regexp str)
  (let ((flags (if case-fold-search (pcre--flags '(ignorecase)) 0)))
    (pcre--core-match-string regexp str flags)))

(defun pcre-match-string-p (regexp str)
  (let ((flags (if case-fold-search (pcre--flags '(ignorecase)) 0)))
    (pcre--core-match-string-p regexp str flags)))

(defun pcre-looking-at (regexp)
  (let ((flags (if case-fold-search (pcre--flags '(ignorecase)) 0)))
    (pcre--core-match-string
     regexp (buffer-substring-no-properties (point) (point-max)) flags t)))

(defun pcre-looking-at-p (regexp)
  (let ((flags (if case-fold-search (pcre--flags '(ignorecase)) 0)))
    (pcre--core-match-string-p
     regexp (buffer-substring-no-properties (point) (point-max)) flags t)))

(defun pcre-re-search-forward (regexp &optional bound non-error count)
  (let* ((str (buffer-substring-no-properties (point) (or bound (point-max))))
         (flags (if case-fold-search (pcre--flags '(ignorecase)) 0))
         (matched (pcre--core-match-string regexp str 0 t (or count -1))))
    (if (not matched)
        (unless not-error
          (error "Search failed %s" regexp))
      (goto-char (match-end 0))
      (point))))

(provide 'pcre)

;;; pcre.el ends here
