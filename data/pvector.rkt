#lang debug racket/base

(require data/pvector/base
         data/pvector/for)

(provide (all-from-out data/pvector/base
                       data/pvector/for))

(module reader racket/base
  (require racket/match
           racket/syntax
           (only-in syntax/module-reader
                    lang-reader-module-paths
                    make-meta-reader)
           syntax/readerr
           syntax/strip-context
           syntax/stx)

  (provide (rename-out [-read read]
                       [-read-syntax read-syntax]
                       [-get-info get-info]))

  (define (make-pvector-readtable #:readtable [readtable (current-readtable)])
    (make-readtable
     readtable
     #\p 'dispatch-macro
     (read-pvector-syntax readtable)))

  (define ((make-read base-read) . args)
    (syntax->datum (apply (make-read-syntax base-read) args)))

  (define ((make-read-syntax base-read-syntax) . args)
    (parameterize ([current-readtable (make-pvector-readtable)])
      (apply base-read-syntax args)))

  (define-values [-read -read-syntax -get-info]
    (make-meta-reader
     'data/pvector "module path" lang-reader-module-paths
     make-read make-read-syntax values))

  (define ((read-pvector-syntax base-readtable) c in src line-num col-num position)
    (cond
      [(char=? #\v (peek-char in))
       (read-char in)
       (let ([elems (read-syntax/recursive src in)])
         (strip-context
          #`(#%namespaced (submod data/pvector transformer) (quote-pvector . #,elems))))]
      [else
       (match-define-values [_ _ _] (readtable-mapping base-readtable c))
       (error)]))

  (define (read-whitespace in)
    (let loop ()
      (when (char-whitespace? (peek-char in))
        (read-char in)
        (loop)))))

(module transformer racket/base
  (require (for-syntax racket/base
                       syntax/parse)
           data/pvector/base)

  (provide quote-pvector)

  (define-syntax quote-pvector
    (syntax-parser
      [(_ . elems)
       #'(apply pvector 'elems)])))
