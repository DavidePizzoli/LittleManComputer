;;;; -*- Mode: Lisp -*-
;;;; lmc.lisp


;; inizio 28/11
;; CREA BENE L'INTESTAZIONE DEL FILE

;; controlla che la memoria sia 100 celle
(defun mem-is-in-range (x)
  (if (<= x 100) T
    nil))

;; controlla che x sia compreso tra 0 e 99
(defun mod-100 (x)
  (if (< x 100) T
    nil))

;; controlla che x sia compreso tra 0 e 999
(defun mod-1000 (x)
  (if (and (> x -1) (< x 1000)) T
    nil))

;; incrementa pc di 1. Se è 99 allora il prox numero è 0
(defun sum-pc (x)
  (if (null (mod-100 (+ 1 x))) 
    0
    (+ 1 x)))

;; controlla che gli elementi siano tutti compresi tra 0 e 999
(defun is-lista-1000 (l)
  (cond ((null l) T)
        ((null (mod-1000 (first l))) nil)
        (T (is-lista-1000 (rest l)))))

;; definizione di flag e noflag
(defparameter flag (quote flag))
(defparameter noflag (quote noflag))

;; definizione di state e halted-state
(defstruct (state (:type list) :named) A ACC P PC M MEM I IN O OUT F FLAG)
(defstruct (halted-state (:type list) :named)  A ACC P PC M MEM I IN O OUT F FLAG)

(defun new-state (acc pc mem in out f)
  (if (and (mod-100 pc)
           (or (equal flag f) (equal noflag f))
           (listp mem)
           (listp in)
           (listp out)
           (is-lista-1000 mem)
           (is-lista-1000 in)
           (is-lista-1000 out))
  (make-state :A :ACC :ACC acc :P :PC :PC pc :M :MEM :MEM mem :I :IN :IN in :O :OUT :OUT out :F :FLAG :FLAG f)))

(defun new-halted-state (acc pc mem in out f)
  (if (and (mod-100 pc)
           (or (equal flag f) (equal noflag f))
           (listp mem)
           (listp in)
           (listp out)
           (is-lista-1000 mem)
           (is-lista-1000 in)
           (is-lista-1000 out))
  (make-halted-state :A :ACC :ACC acc :P :PC :PC pc :M :MEM :MEM mem :I :IN :IN in :O :OUT :OUT out :F :FLAG :FLAG f)))

;; controllo se non ci sono errori
(defun one-instruction (s) 
  (cond ((null (state-p s)) nil)
        (T (let ((acc (state-ACC s))
                 (pc (state-PC s))
                 (mem (state-MEM s))
                 (in (state-IN s))
                 (out (state-OUT s))
                 (f (state-FLAG s)))
           (scorri-mem acc pc mem in out f)))))

;; scorro la memoria
(defun scorri-mem (acc pc mem in out f) 
  (let ((ist (nth pc mem)))
  ;;(print ist)
  (if (null ist) nil
    (cond ((and (>= ist 100) (< ist 200)) (add-op acc ist pc mem in out))
          ((and (>= ist 200) (< ist 300)) (sub-op acc ist pc mem in out))
          ((and (>= ist 300) (< ist 400)) (store-op acc ist pc mem in out f))
          ((and (>= ist 500) (< ist 600)) (load-op acc ist pc mem in out f))
          ((and (>= ist 600) (< ist 700)) (branch-op acc ist pc mem in out f))
          ((and (>= ist 700) (< ist 800)) (branch-zero-op acc ist pc mem in out f))
          ((and (>= ist 800) (< ist 900)) (branch-positive-op acc ist pc mem in out f))
          ((= ist 901) (input-op acc pc mem in out f))
          ((= ist 902) (output-op acc pc mem in out f))
          ((and (>= ist 0) (< ist 100)) (halt-op acc pc mem in out f))))))  

;; somma (1xx)
(defun add-op (acc ist pc mem in out) 
  (let ((elemento (nth (mod ist 100) mem)))
    (cond ((null elemento) nil)
          ((null (mod-1000 (+ elemento acc))) (new-state (+ elemento acc) (sum-pc pc) mem in out flag))
          (T (new-state (+ elemento acc) (sum-pc pc) mem in out noflag)))))

;; sottazione (2xx)
(defun sub-op (acc ist pc mem in out) 
  (let ((elemento (nth (mod ist 100) mem)))
    (cond ((null elemento) nil)
          ((null (mod-1000 (- acc elemento))) (new-state (- acc elemento) (sum-pc pc) mem in out flag))
          (T (new-state (- acc elemento) (sum-pc pc) mem in out noflag)))))

;; store (3xx)
(defun store-op (acc ist pc mem in out f)
  (setf cell (mod ist 100))
  (setf a (nth cell mem))
  (cond ((null a) nil)
        (T (and (setf (nth cell mem) acc) (new-state acc (sum-pc pc) mem in out f)))))

;; load (5xx)
(defun load-op (acc ist pc mem in out f)
  (let ((elemento (nth (mod ist 100) mem)))
    (cond ((null elemento) nil)
          (T (new-state elemento (sum-pc pc) mem in out f)))))

;; branch (6xx) 
(defun branch-op (acc ist pc mem in out f)
  (let ((cell (mod ist 100)))
    (new-state acc cell mem in out f)))

;; branch if zero (7xx) 
(defun branch-zero-op (acc ist pc mem in out f)
  (let ((cell (mod ist 100)))
    (if (and (equal f noflag) (= acc 0)) (new-state acc cell mem in out f)
      nil)))

;; branch if positive (8xx) 
(defun branch-positive-op (acc ist pc mem in out f)
  (let ((cell (mod ist 100)))
    (if (equal f noflag) (new-state acc cell mem in out f)
      nil)))
    
;; input (901)
(defun input-op (acc pc mem in out f)
  (let (( primo (first in)))
    (cond ((null primo) nil)
          (T (new-state primo (sum-pc pc) mem (rest in) out f)))))

;; output (902)
(defun output-op (acc pc mem in out f)
  (cond ((null out) (push acc out))
        (T (push acc (cdr (last out)))))
  (new-state acc (sum-pc pc) mem in out f))

;; halt (0xx)
(defun halt-op (acc pc mem in out f)
  (new-halted-state acc pc mem in out f))


(defun execution-loop (s)
  (cond ((null (state-p s)) nil)
        (T (exe-out s (state-MEM s)))))

;; controlla se è halted-state o è arrivata alla fine della memoria
(defun exe-out (s l)
  (let ((newstate (one-instruction s)))
    (cond ((halted-state-p newstate) (halted-state-OUT newstate))
          ((null (cdr l)) nil)
          (T (exe-out newstate (cdr l))))))
  
;; aggiunge alla memoria 0 fino ad arrivare a 100 elementi
(defun aggiungi-zeri (l)
  (if (< (length l) 100)
      (let ((app (make-list (- 100 (length l)) :initial-element 0)))
         (append l app))
    nil))

;; lmc-load legge il file e produce la memoria iniziale 
(defun lmc-load (file)
  (split-lista (rimuovi-commenti (with-open-file (in file
                      :direction :input
                      :if-does-not-exist :error)
    (read-list-from in)))))
  
(defun read-list-from (input-stream)
  (let ((e (read-line input-stream nil 'eof)))
    (unless (eq e 'eof) ;;(print e)
      (cons e (read-list-from input-stream)))))

(defun split-string (l cont)
  (cond ((/= cont (length l)) 
        (cons (remove-all (nth cont l)) (split-string l (+ cont 1))))))

(defun remove-all (stringa)
  (remove #\Space (remove #\Tab (remove #\Newline stringa))))

(defun is-commento (s)
  (let ((first-slash (position #\/ s)))
    (cond ((null first-slash) nil)
          ((not (null (position #\/ s :start (+ first-slash 1)))) first-slash))))

(defun rimuovi-commenti (l)
  (cond ((not (null (first l)))
         (cons (rimuovi-commento-stringa (first l)) (rimuovi-commenti (rest l))))))

(defun rimuovi-commento-stringa (s)
  (let ((posizione (is-commento s)))
    (cond ((null posizione) s)
        (T (subseq s 0 posizione)))))

(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

  
(defun split-lista (l)
  (cond ((not (null (first l)))
         (cons (split-str (first l) " ") (split-lista (rest l))))))

;; lmc-run produce la coda di output (execution-loop)



;;;; end of file -- lmc.lisp 
