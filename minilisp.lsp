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
(define vl-mkdir make-directory)
(define vl-file-systime file-modification-time)
(define close close-port)

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

; TODO more general transformation? (many similaf functions)
(define-macro
  (vl-some fnc . args)
  (if (and (list? fnc) (eq? (car fnc) 'quote))
    `(some ,(cadr fnc) ,@args)
    `(some (eval ,fnc) ,@args)))

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

(define-macro
  (vl-remove-if fnc lst)
  (if (and (list? fnc) (eq? (car fnc) 'quote))
    `(remove-if ,(cadr fnc) ,lst)
    `(remove-if (eval ,fnc) ,lst)))

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

(define-macro
  (/= x y)
  `(if (and (string? ,x) (string? ,y))
     (string/=? ,x ,y)
     (not (eq? ,x ,y))))

; TODO implement all
(define (rtos n &optional mode precision)
  (number->string n))

; TODO any general type-checking?
(define (itoa n)
  (if (integer? n)
    (number->string n)
    (error "argument must be integer")))

(define (atoi str)
  (setq num (string->number str))
  (if (not num) (setq num 0))
  (setq num (truncate num)))

; TODO implement all
(defun getvar (x / s)
  (setq s (if (= 'sym (type x))
            (vl-symbol-name x)
            x))
  (cond
    ((= s "CDATE")
     (get-date)
     )))

(defun boole (op x y / m1 m2 m3 m4)
  ; x y op's bit
  ; 0 0 8
  ; 0 1 4
  ; 1 0 2
  ; 1 1 1
  (setq m1 0 m2 0 m3 0 m4 0)
  (if (/= 0 (logand op 1))
    (setq m1 (logand x y)))
  (if (/= 0 (logand op 2))
    (setq m2 (logand x (lognot y))))
  (if (/= 0 (logand op 4))
    (setq m3 (logand (lognot x) y)))
  (if (/= 0 (logand op 8))
    (setq m4 (logand (lognot x) (lognot y))))
  (logior m1 m2 m3 m4))

(defun vl-filename-extension (str / index)
  (setq index (string-search "." str :from-end? T))
  (if index (substring str index)))

(defun vl-filename-base (str / index)
  (multiple-value-bind
    (path file) (split-path-from-filename str)
    (setq index (string-search "." file :from-end? T))
    (if index
      (substring file 0 index)
      file)))

(defun vl-filename-directory (str / index)
  (multiple-value-bind
    (path file) (split-path-from-filename str)
    (or path "")))

(defun vl-file-copy (src dst &optional append? / src_port dst_port b)
  (setq src_port (open-input-file src 'binary))
  (if (not src_port)
    nil
    (progn
      (setq dst_port (if append?
                       (open-append-file dst 'binary)
                       (open-output-file dst 'binary)))
      (if (not dst_port)
        (progn (close-port src_port) nil)
        (progn
          (while (not (eof-object? (setq b (read-byte src_port))))
                 (write-byte b dst_port))
          (close-port src_port)
          (close-port dst_port)
          T)))))

(defun open (filename mode)
  (cond
    ((= mode "r")
     (open-input-file filename))
    ((= mode "w")
     (open-output-file filename))
    ((= mode "a")
     (open-append-file filename))))

(defun write-line (str &optional port)
  (display str port)
  (newline port)
  str)

(defun startapp (cmd &optional (file "") / result)
  (setq result (system (strcat cmd " " file)))
  (if (= 0 result)
    33
    result))
