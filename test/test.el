;;; test.el --- libpcre binding test

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

(require 'ert)
(require 'pcre)

(ert-deftest string-match ()
  "PCRE string-match"
  (let* ((str "aa-bb")
         (ret (pcre-match-string "\\A(\\w+)-(b{2,})\\z" str)))
    (should ret)
    (should (string= (match-string 0 str) str))
    (should (string= (match-string 1 str) "aa"))
    (should (string= (match-string 2 str) "bb"))))

(ert-deftest string-match-p ()
  "PCRE string-match-p"
  (set-match-data nil)
  (let* ((str "aa-bb")
         (ret (pcre-match-string-p "\\A(\\w+)-(b{2,})\\z" str)))
    (should ret)
    (should-not (string= (match-string 0 str) str))))

(ert-deftest looking-at ()
  "PCRE looking-at"
  (with-temp-buffer
    (insert "abbc")
    (goto-char (point-min))
    (should (pcre-looking-at "a(b{1,2})c$"))
    (should (string= (match-string 1) "bb"))))

(ert-deftest looking-at-p ()
  "PCRE looking-at-p"
  (set-match-data nil)
  (with-temp-buffer
    (insert "abbc")
    (goto-char (point-min))
    (should (pcre-looking-at-p "a(b{1,})c$"))))

;;; test.el ends here
