(defvar *tokens*)
(defvar *curr*)

(defun scan ()
    "Reads text token from 'tokens'"
    (setq *curr* (car *tokens*))
    (setq *tokens* (cdr *tokens*)))
    
(defun is-num (x)
    "Tests if x is num token"
    (and (listp x) (eq (car x) 'num)))

(defun factor ()
    "Factor non-terminal"
    (let (e)
    (cond
        ((eq *curr* 'lpt)
            (scan)
            (setq e (expr))
            (unless (eq *curr* 'rpt) (format t "rpt is expected"))
            (scan)
            e)
        ((is-num *curr*)
            (setq e *curr*)
            (scan)
            e)
        (t
            (format t "~a isn't expected~%" *curr*)))))

(defun term ()
    "Term and term-list non-terminals"
    (let ((f (factor)))
    (cond
        ((eq *curr* 'mul)
            (scan)
            `(expr mul ,f ,(term)))
        ((eq *curr* 'div)
            (scan)
            `(expr div ,f ,(term)))
        (t
            f))))
            
(defun expr ()
    "Expr and expr-list non-terminals"
    (let ((f (term)))
    (cond
        ((eq *curr* 'plus)
            (scan)
            `(expr add ,f ,(expr)))
        ((eq *curr* 'minus)
            (scan)
            `(expr sub ,f ,(expr)))
        (t
            f))))

(defun parser (file-name)
    "Parses a token string recived from a lexer"
    (setq *tokens* (lexer file-name))
    (scan)
    (expr))
