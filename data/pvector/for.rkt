#lang racket/base

(require data/collection
         data/pvector/base
         (for-syntax racket/base))

(provide for/pvector for*/pvector)

(define-syntax-rule (for/pvector . rest)
  (extend (pvector) (for/sequence/derived for/pvector . rest)))

(define-syntax-rule (for*/pvector . rest)
  (extend (pvector) (for*/sequence/derived for*/pvector . rest)))

(define-syntax-rule (for/immutable-vector . rest)
  (extend #() (for/sequence/derived for/immutable-vector . rest)))
