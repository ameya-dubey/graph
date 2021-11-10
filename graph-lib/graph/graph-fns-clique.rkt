#lang racket/base

(require racket/set
         racket/generator)

(require "gen-graph.rkt"
         "graph-weighted.rkt"
         "graph-unweighted.rkt")

(provide find-cliques in-cliques get-cliques)

(define-syntax-rule (push! x ls) (set! ls (cons x ls)))
(define-syntax-rule (pop! ls)
  (let ([head (car ls)])
    (set! ls (cdr ls))
    head))
(define-syntax-rule (set-pop! s)
  (let ([q (set-first s)])
    (set-remove! s q)
    q))

(define (find-cliques G)
  (define (set-argmax s f)
    (define init (set-first s))
    (for/fold ([max-val (f init)] [max-arg init] #:result max-arg)
              ([v (in-set (set-rest s))]
               #:when (> (f v) max-val))
      (values (f v) v)))
  
  (define (get-u subg cand)
    (set-argmax subg
      (λ (v)
        (set-count (set-intersect (list->set (get-neighbors G v)) cand)))))

  (define (get-ext-u u cand)
    (define adj-u (list->set (get-neighbors G u)))
    (for/mutable-set ([v (in-mutable-set cand)]
                      #:unless (set-member? adj-u v))
      v))

  (define (set-intersect/mutable A B)
    (for/mutable-set ([v (in-mutable-set A)]
                      #:when (set-member? B v))
      v))

  (generator ()
    (with-handlers ([exn:fail:contract? (λ (exn) (void))])
      (let* ([Q     '((void))]
             [V      (get-vertices G)]
             [subg   (list->set V)]
             [cand   (list->mutable-set V)]
             [u      (get-u subg cand)]
             [ext-u  (get-ext-u u cand)]
             [stack '()])

        (let loop ()
          (cond [(not (set-empty? ext-u))
                 (let* ([q      (set-pop! ext-u)]
                        [adj-q  (list->set (get-neighbors G q))]
                        [subg-q (set-intersect subg adj-q)])
                   (set-remove! cand q)
                   (set! Q (cons q (cdr Q)))
                   (cond [(set-empty? subg-q) (yield (list* Q))]
                         [(let ([cand-q (set-intersect/mutable cand adj-q)])
                            (and (not (set-empty? cand-q)) cand-q))
                          =>
                          (λ (cand-q)
                            (push! (list subg cand ext-u) stack)
                            (push! (void) Q)
                            (set! subg subg-q)
                            (set! cand cand-q)
                            (set! u (get-u subg cand))
                            (set! ext-u (get-ext-u u cand)))]))]
                [else (pop! Q)
                      (set!-values (subg cand ext-u) (apply values (pop! stack)))])
          (loop))))))

(define-syntax-rule (in-cliques G)
  (in-producer (find-cliques G) (void)))

(define (get-cliques G)
  (for/list ([clique (in-cliques G)])
    clique))
