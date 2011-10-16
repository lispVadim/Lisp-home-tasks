(defun min-in-list (lst &optional m)
    "Function that searches a minimal value in list 'lst'. 'm' is a likely minimal value."
    (let ((head (car lst)) (tail (cdr lst)))
        (cond
            ((null lst) m)
            ((null m) (min-in-list tail head))
            ((< head m) (min-in-list tail head))
            (T (min-in-list tail m)))))
            
(defun list-without-elem (lst elem)
    "Function that returns a copy of list 'lst' without first element 'elem'."
    (let ((head (car lst)) (tail (cdr lst)))
        (cond
            ((null lst) nil)
            ((= head elem) tail)
            (T (cons head (list-without-elem tail elem))))))
            
(defun select-sort (lst)
    (let ((min-elem (min-in-list lst)))
        (if (null lst)
            nil
            (cons min-elem (select-sort (list-without-elem lst min-elem))))))
