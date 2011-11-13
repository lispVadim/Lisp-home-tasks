(defvar *tokens*)
(defvar *curr*)

(defun scan ()
    (setq *curr* (car *tokens*))
    (setq *tokens* (cdr *tokens*)))

(defun factor ()
    (cond
        ((eq *curr* 'lpt)
            (scan)
            (setq e (expr))
            (unless (eq *curr* 'rpt) (format t "rpt is expected"))
            (scan)
            e)
        ((numberp *curr*)
            (setq e `(num ,*curr*))
            (scan)
            e)
        (t
            (format t "~a isn't expected~%" *curr*))))

(defun term-list ()
    (scan)
    (setq f (factor))
    (cond
        ((eq *curr* 'mul)
            `(expr mul ,f ,(term-list)))
        ((eq *curr* 'div)
            `(expr div ,f ,(term-list)))
        ((null *curr*)
            f)))

