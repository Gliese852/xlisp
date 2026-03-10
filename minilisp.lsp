; 1 subst clashing symbols
; 2 define macros and other
; 3 define all needed globals

; ---- straight defines
(define listp list?)
(define strlen string-length)
(define strcat string-append)
(define fix truncate)
(define rem remainder)
(define equal equal?)
(define getenv get-environment-variable)

(define __xl_apply apply)
(define-macro
  (apply fnc . args)
  (if (and (list? fnc) (eq? (car fnc) 'quote))
    `(__xl_apply ,(cadr fnc) ,@args)
    `(__xl_apply (eval ,fnc) ,@args)))

(define-macro
  (mapcar fnc . args)
  (if (and (list? fnc) (eq? (car fnc) 'quote))
    `(map ,(cadr fnc) ,@args)
    `(map (eval ,fnc) ,@args)))


; ---- utils ----

(define (__make_pairs lst)
  (if (not (null? (cdr lst)))
    (cons (list (car lst)(cadr lst))
          (__make_pairs (cddr lst)))
    '()))

(define (__cut n lst)
  (if (or (null? lst) (< n 1))
    ()
    (cons (car lst) (__cut (- n 1) (cdr lst)))))

(define (__tree_map pred fnc lst)
  (if (null? lst)
    lst
    (let ((item (car lst)))
      (cons
        (if (pred item)
          (fnc item)
          (if (pair? item)
            (__tree_map pred fnc item)
            item))
        (__tree_map pred fnc (cdr lst))))))

(define (__tree_subst from to lst)
  (__tree_map (lambda (x) (eq? from x))
              (lambda (x) to)
              lst))

(define (__make_cars n)
  (letrec ((ad-func
             (lambda (n)
               (if (> n 0)
                 (append
                   '(())
                   (__xl_apply append
                          (map (lambda (seq)
                                 (list
                                   (cons #\a seq)
                                   (cons #\d seq)))
                               (ad-func (- n 1)))))
                 '(())))))
    (map (lambda (seq)
           (list->string
             (cons #\c
                   (append seq
                           '(#\r)))))
         (cdr (ad-func n)))))

(define (__splitlist testFnc lst)
  (if (not (null? lst))
    ((lambda (x y)
       (if (testFnc x)
         (list (cons x (car y)) (cadr y)) ; добавляем к 1 списку
         (list (car y) (cons x (cadr y))))) ; добавляем ко 2 списку
     (car lst)
     (__splitlist testFnc (cdr lst)))
    (list ()())))

; ---- subst funcs & macro ----

(define
  (__cons x y)
  (if y
    (cons x y)
    (list x)))

(define-macro
  (__lambda args&loc . code)
  (let ((/args (member '/ (reverse args&loc))))
    (if /args
      (let ((args (reverse (cdr /args)))
            (loc (map
                   (lambda (x)
                     (list x #f))
                   (cdr (member '/ args&loc)))))
        `(lambda ,args (let ,loc ,@code)))
      `(lambda ,args&loc ,@code))))

; "polymorfic" comparison
(define (__< x1 x2) (if (string? x1) (string-ci<? x1 x2) (< x1 x2)))
(define (__> x1 x2) (if (string? x1) (string-ci>? x1 x2) (> x1 x2)))

; ---- non-scheme funcs & macro (no-subst) ----

(define (1- x) (- x 1))
(define (1+ x) (+ x 1))

(define (set . bindings)
  (eval
    (cons 'setq bindings)))

(define (substr str . start-len)
  (let ((start (car start-len)))
    (let ((len (let ((maybelen (cadr start-len)))
                 (if (null? maybelen)
                   (+ 1
                      (- (string-length str) start))
                   maybelen))))
      (substring str (- start 1) (+ start len -1)))))

(define (vl-load-com) T)
(define vl-symbol-name symbol->string)

(define (vlax-get-acad-object)
  'fake-acad-object)

(define (vla-get-activedocument acad-object)
  'fake-activedocument)

(define (vla-get-modelspace active-document)
  'fake-modelspace)

(define (vl-string-search pattern str &optional start-pos)
  (if start-pos
    (string-search pattern str :start2 start-pos)
    (string-search pattern str)))

(define (vl-directory-files &optional path pattern flag)
  (directory-files path pattern flag))

(define (vl-string->list str) (map char->integer (string->list str)))
(define (nth item lst) (list-ref lst item))
(define (remove-if-not fnc lst)
  (if (pair? lst)
    (if (fnc (car lst))
      (cons (car lst) (remove-if-not fnc (cdr lst)))
      (remove-if-not fnc (cdr lst)))
    ()))
(define (remove-if fnc lst)
  (remove-if-not (lambda (x) (not (fnc x))) lst))

(define (vl-some fnc . all-args)
  (let ((args (map car all-args)))
    (if (null? (car args))
      #f
      (let
        ((result (apply fnc args)))
        (if result
          result
          (__xl_apply vl-some (cons fnc (map cdr all-args))))))))
(define (chr int) (list->string (list (integer->char int))))
(define (ascii ch) (char->integer ch))

(define (vl-sort lst qfnc)
  (if (pair? lst)
    (let* ((fnc (eval qfnc))
           (splitted
             (__splitlist
               (lambda (x)
                 (fnc x (car lst)))
               (cdr lst))))
      (append (vl-sort (car splitted) qfnc) (cons (car lst) (vl-sort (cadr splitted) qfnc))))
    ()))

(define (vl-sort-i lst fnc)
  (let
    ((i -1))
    (map car
         (vl-sort
           (map (lambda (x) (cons (set! i (+ 1 i)) x)) lst)
           `(lambda (x1 x2) (,fnc (cdr x1) (cdr x2)))))))

(define (princ . x)
  (if (= 0 (length x))
    (display "")
    (__xl_apply display x)))

(define-macro
  (progn . body) `(begin ,@body))

(define-macro
  (repeat n . code)
  `(let
     ((ans #f))
     (do ((repeat_iterator 0))
       ((> (set! repeat_iterator (+ 1 repeat_iterator)) ,n))
       (set! ans (begin ,@code)))
     ans))

(define-macro
  (setq . bindings)
  (let ((sets
          (map (lambda (x) (cons 'set! x))
               (__make_pairs bindings))))
    `(begin ,@sets)))

(define-macro
  (defun name args&loc . code)
  `(setq ,name (__lambda ,args&loc ,@code)))

(define-macro
  (foreach var . lst&prc)
  `(for-each (lambda (,var) ,@(cdr lst&prc)) ,(car lst&prc)))

(define-macro
  (function x)
  `(quote ,x))

(define-macro
  (while test . code)
  `(do ((thedummy 0)) ((not ,test)) ,@code))

(define (type item)
  (cond
    ((symbol? item)
     'sym)
    ((integer? item)
     'int)
    ((real? item)
     'real)))

; ---- transform autolisp code to scm ----

; XXX probably don't work
(define (__transform_to_scheme al_code)
  (let ((subs (append '((cons . __cons)
                        (lambda . __lambda)
                        (apply . __apply)
                        (< . __<)
                        (> . __>))
                      (map
                        (lambda (x)
                          (cons (string->symbol x)
                                (string->symbol (string-append "__" x))))
                        (__make_cars 4)))))
    (__tree_map (lambda (x) (assoc x subs))
                (lambda (x) (cdr (assoc x subs)))
                al_code)))

; works in xlisp, though we moved to redefining apply
(define (__adapt_al_code al_code)
  (let ((subs '((apply . __apply))))
    (__tree_map (lambda (x) (assoc x subs))
                (lambda (x) (cdr (assoc x subs)))
                al_code)))

(define-macro
  (= x y)
  `(if (and (string? ,x) (string? ,y))
     (string=? ,x ,y)
     (eq? ,x ,y)))

; TODO implement all
(define (rtos n &optional mode precision)
  (number->string n))

; lib

; TODO implement all
(defun getvar (x / s)
  (setq s (if (= 'sym (type x))
            (vl-symbol-name x)
            x))
  (cond
    ((= s "CDATE")
     (get-date)
     )))
