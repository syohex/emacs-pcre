# emacs-pcre

[libpcre](http://www.pcre.org/) binding of Emacs Lisp.

## Sample

``` emacs-lisp
(let ((str "012-345-567"))
  (when (pcre-match-string "\\A(\\d+)-(\\d+)-(\\d+)\\z" str)
    (match-string 1 str))) ;; => 012

(with-temp-buffer
  (insert "apple orange melon\n")
  (insert "red blue green\n")
  (insert "vim atom sublime\n")
  (goto-char (point-min))
  (let (matches)
    (while (pcre-re-search-forward "^\\S+ ([^[:space:]]+)" nil t)
      (push (match-string 1) matches))
    (reverse matches))) ;; '(orange blue atom)
```

## Interfaces

##### `(pcre-string-match regexp string &optional flags)`

Works like `string-match`.

##### `(pcre-string-match-p regexp string &optional flags)`

Works like `string-match-p`.

##### `(pcre-looking-at regxp &optional flags)`

Works like `looking-at`.

##### `(pcre-looking-at-p regxp &optional flags)`

Works like `looking-at-p`.

##### `(pcre-looking-back regxp &optional bound flags)`

Works like `looking-back`.

##### `(pcre-re-search-forward regxp &optional bound non-error count flags)`

Works like `re-search-forward`.

### flags argument

Following flags are supporeted now

- `ignorecase` : ignore case matching. However pcre.el is set this flag by current `case-fold-search`
- `multiline` : match for multiple line strings
- `dotall` : `.` matches all charactres including newline
- `extended`: white space data in regexp is ignored
