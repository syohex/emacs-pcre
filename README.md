# emacs-pcre

[libpcre](http://www.pcre.org/) binding of Emacs Lisp.

## Sample

``` emacs-lisp
(let ((str "012-345-567"))
  (when (pcre-match-string "\\A(\\d+)-(\\d+)-(\\d+)\\z" str)
    (match-string 1 str))) ;; => 012
```

## Interfaces

##### `(pcre-string-match regexp string)`

Works like `string-match`.

##### `(pcre-string-match-p regexp string)`

Works like `string-match-p`.

##### `(pcre-looking-at regxp)`

Works like `looking-at`.

##### `(pcre-looking-at-p regxp)`

Works like `looking-at-p`.

##### `(pcre-re-search-forward regxp)`

Works like `re-search-forward`.
