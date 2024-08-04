(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English")

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(defvar *grammar* *simple-grammar*)

(defun rule-lhs (rule)
  (first rule))
(defun rule-rhs (rule)
  (rest (rest rule)))
(defun rewrites (category)
  (rule-rhs (assoc category *grammar*)))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (let ((choices nil))
    (cond ((listp phrase)
         (mappend #'generate phrase))
        ((setf choices (rewrites phrase)) (generate (random-elt choices)))
        (t (list phrase)))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase"
  (let ((choices nil))
    (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
          ((setf choices (rewrites phrase)) (cons phrase
                                                  (generate-tree (random-elt choices))))
        (t (list phrase)))))

(defun generate-all (phrase)
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
               ylist))
         
                  
         

