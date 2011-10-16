(defun list-get-val (node)
    "Returns value of node. <node> -> (<node-val> [(<child-node1> [, <child-nodeN])])"
    (car node))
	
(defun list-get-children (node)
    "Return list of node's children."
    (cdr node))

(defun list-make-node (val children)
    "Makes a new node with specified value and children."
    (apply #'list val children))
	
(defun traverse (func tree get-val get-children make-node)
    "Makes a new tree by applying 'func' to each node of 'tree'."
    (if tree
        (funcall    make-node
                    (funcall func (funcall get-val tree))
                    (mapcar 
                        (lambda (child) (traverse func child get-val get-children make-node))
                        (funcall get-children tree)))
        nil))

;Tests
(unless (equal nil (traverse #'1+ nil #'list-get-val #'list-get-children #'list-make-node))
    (print "ERROR1"))

(unless (equal '(2) (traverse #'1+ '(1) #'list-get-val #'list-get-children #'list-make-node))
    (print "ERROR2"))
    
(unless (equal '(2 nil nil) (traverse #'1+ '(1 nil nil) #'list-get-val #'list-get-children #'list-make-node))
    (print "ERROR3"))

(unless (equal '(2 (3) (4)) (traverse #'1+ '(1 (2) (3)) #'list-get-val #'list-get-children #'list-make-node))
    (print "ERROR4"))

(unless (equal '(2 (3) (4 (5) (6))) (traverse #'1+ '(1 (2) (3 (4) (5))) #'list-get-val #'list-get-children #'list-make-node))
    (print "ERROR5"))