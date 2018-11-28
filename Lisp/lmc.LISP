;; 28/11 inizio scrittura funzioni più semplici

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

(defun set-flag-0 () 0)
(defun set-flag-1 () 1)
(defun set-acc-0 () 0)

(defun is-flag-1 (x)
  (if (= 1 x) T
    nil))

(defun is-flag-0 (x)
  (if (= 0 x) T
    nil))

;; definizione di state e halted-state
(defstruct state ACC PC MEM IN OUT FLAG)
(defstruct halted-state ACC PC MEM IN OUT FLAG)

(defun new-state (Acc PC Mem In Out Flag)
  (make-state :ACC Acc :PC PC :MEM Mem :IN In :OUT Out :FLAG Flag))

(defun new-halted-state (Acc PC Mem In Out Flag)
  (make-halted-state :ACC Acc :PC PC :MEM Mem :IN In :OUT Out :FLAG Flag))

;; controllo se non ci sono errori
(defun one-istruction (s) 
  (setq acc (state-ACC s))
  (setq pc (state-PC s))
  (setq mem (state-MEM s))
  (setq in (state-IN s))
  (setq out (state-OUT s))
  (setq flag (state-FLAG s))
  (and (mod-1000 pc)
       (listp mem)
       (is-lista-1000 mem))
  (scorri-mem acc pc mem in out flag))

;; scorro la memoria
(defun scorri-mem (acc pc mem in out flag) 
  (setq ist (nth pc mem))
  (print ist)
  (if (null ist) nil 
      (cond ((and (>= ist 100) (< ist 200)) (add-op acc ist pc mem in out flag))
            ((and (>= ist 200) (< ist 300)) (sub-op acc ist pc mem in out flag))
            ((and (>= ist 300) (< ist 400)) (store-op acc ist pc mem in out flag))
            ((and (>= ist 400) (< ist 500)) (error "Non valido 400-499"))
            ((and (>= ist 500) (< ist 600)) (load-op acc ist pc mem in out flag))
            ((and (>= ist 600) (< ist 700)) (branch-op acc ist pc mem in out flag))
            ((and (>= ist 700) (< ist 800)) (branch-zero-op acc ist pc mem in out flag))
            ((and (>= ist 800) (< ist 900)) (branch-positive-op acc ist pc mem in out flag))
            ((= ist 901) (input-op acc ist pc mem in out flag))
            ((= ist 902) (output-op acc ist pc mem in out flag))
            ((and (>= ist 0) (< ist 100)) (halt-op acc ist pc mem in out flag))
    )
  )
  )  

;; somma (1xx)
(defun add-op (acc ist pc mem in out flag) 
  (setq cell (mod ist 100))
  (setq a (nth cell mem))
  (cond ((null a) nil)
        ((null (mod-1000 (+ a acc))) (new-state (+ a acc) (sum-pc pc) mem in out 1))
        (T (new-state (+ a acc) (sum-pc pc) mem in out 0))))

;; sottazione (2xx)
(defun sub-op (acc ist pc mem in out flag) 
  (setq cell (mod ist 100))
  (setq a (nth cell mem))
  (cond ((null a) nil)
        ((null (mod-1000 (- acc a))) (new-state (- acc a) (sum-pc pc) mem in out 1))
        (T (new-state (- acc a) (sum-pc pc) mem in out 0))))

;; store (3xx)
(defun store-op (acc ist pc mem in out flag)
  (setq cell (mod ist 100))
  (setq a (nth cell mem))
  (cond ((null a) nil)
        (T (and (setf (nth cell mem) acc) (new-state acc (sum-pc pc) mem in out flag)))))

;; load (5xx)
(defun load-op (acc ist pc mem in out flag)
  (setq cell (mod ist 100))
  (setq a (nth cell mem))
  (cond ((null a) nil)
        (T (new-state a (sum-pc pc) mem in out flag))))

;; branch (6xx) (controllo che esiste quella parte di memoria o sti cazzi?)
(defun branch-op (acc ist pc mem in out flag)
  (setq cell (mod ist 100))
  (new-state acc cell  mem in out flag))

;; branch if zero (7xx) 
(defun branch-zero-op (acc ist pc mem in out flag)
  (setq cell (mod ist 100))
  (if (and (= flag 0) (= acc 0)) (new-state acc cell mem in out flag)
    nil))

;; branch if positive (8xx) 
(defun branch-positive-op (acc ist pc mem in out flag)
  (setq cell (mod ist 100))
  (if (= flag 0) (new-state acc cell mem in out flag)
    nil))
    
;; input (901)
(defun input-op (acc ist pc mem in out flag)
  (setq a (first in))
  (cond ((null a) nil)
        (T (new-state a (sum-pc pc) mem (rest in) out flag))))

;; output (902)
(defun output-op (acc ist pc mem in out flag)
  (setq l (nreverse out))
  (new-state acc (sum-pc pc) mem in (nreverse (push acc l)) flag))

;; halt (0xx)
(defun halt-op (acc ist pc mem in out flag)
  (new-halted-state acc pc mem in out flag))






 