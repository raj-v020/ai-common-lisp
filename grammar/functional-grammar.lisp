(defun sentence () (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article () (one-of '(the a)))
(defun Noun () (one-of '(man ball woman table)))
(defun Verb () (one-of '(hit took saw liked)))
(defun Adj () (one-of '(big little green blue adiabatic)))
(defun Prep () (one-of '(on with to in by)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))
(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      '()))
           
(defun one-of (set)
  "Pick one element of the set, and make a list of it"
  (list (random-elt set)))

(defun random-elt (choices)
  (elt choices (random (length choices))))
