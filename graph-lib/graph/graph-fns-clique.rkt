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

(define-syntax-rule (loop-forever body ...)
  (let loop ()
    body ...
    (loop)))

;; ----------------------------------------------------------------
;; find-cliques
;;
;; Takes an undirected graph G and returns a generator
;; which yields each maximal clique in the graph once.
;; A clique is represented as a list of vertices.
;; The algorithm terminates when the generator yields #<void>.
;;
;; NOTE: This algorithm is inspired by the Python NetworkX
;; implementation of the algorithm in this paper.
;;
;;    Etsuji Tomita, Akira Tanaka, Haruhisa Takahashi,
;;    "The worst-case time complexity for generating all maximal
;;    cliques and computational experiments",
;;    *Theoretical Computer Science*, Volume 363, Issue 1,
;;    Computing and Combinatorics,
;;    10th Annual International Conference on
;;    Computing and Combinatorics (COCOON 2004),
;;    25 October 2006, Pages 28--42
;;    <https://doi.org/10.1016/j.tcs.2006.06.015>
;;
;; TODO: Make this implementation more idiomatic, if possible
;;
(define (find-cliques G)
  (define (set-argmax s f)
    (define init (set-first s))
    (for/fold ([max-val (f init)] [max-arg init] #:result max-arg)
              ([v (in-set (set-rest s))]
               #:when (> (f v) max-val))
      (values (f v) v)))
  
  ;; get-u takes two sets - subg and cand, and returns
  ;; u = { v ∈ subg : | adj(v) ∩ cand | is maximized }
  (define (get-u subg cand)
    (set-argmax subg
                (λ (v) (set-count (set-intersect (list->set (get-neighbors G v)) cand)))))

  ;; get-ext-u takes a vertex u and a mutable-set cand,
  ;; and returns a new mutable-set cand - adj(u)
  (define (get-ext-u u cand)
    (define adj-u (list->set (get-neighbors G u)))
    (for/mutable-set ([v (in-mutable-set cand)]
                      #:unless (set-member? adj-u v))
      v))

  ;; returns a new mutable-set A ∩ B
  (define (set-intersect/mutable A B)
    (define-values (intersectand intersector)
      (if (<= (set-count A) (set-count B))
          (values A B) (values B A)))
    (for/mutable-set ([v (in-set intersectand)]
                      #:when (set-member? intersector v))
      v))

  (generator ()
    (let* ([Q     '((void))]
           [V      (get-vertices G)]
           [subg   (list->set V)]
           [cand   (list->mutable-set V)]
           [u      (get-u subg cand)]
           [ext-u  (get-ext-u u cand)]
           [stack '()])

      (with-handlers ([exn:fail:contract? (λ (_) (void))])
        (loop-forever
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
                     (set!-values (subg cand ext-u) (apply values (pop! stack)))]))))))


;; ----------------------------------------------------------------
;; in-cliques & get-cliques
;;
;; in-cliques  : expands to a function call that returns a
;;               sequence of maximal cliques
;; get-cliques : returns a list of maximal cliques
;;
(define-syntax-rule (in-cliques G)
  (in-producer (find-cliques G) (void)))

(define (get-cliques G)
  (for/list ([clique (in-cliques G)])
    clique))
