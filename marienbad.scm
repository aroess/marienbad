;; Miseré-Spiel (Marienbad Variante) Regeln: Ein Spieler legt sechzehn
;; Streichhölzer in vier Reihen gemäß dem neben stehenden Schema auf:
;;
;;    |
;;   |||
;;  |||||
;; |||||||
;;
;; Die beiden Spieler nehmen abwechselnd Streichhölzer aus einer der
;; Reihen weg. Bei einem Zug dürfen nur Streichhölzer aus einer
;; einzigen Reihe entfernt werden; es ist jedoch der Entscheidung des
;; Spielers überlassen wie viele: mindestens eines, höchstens alle.
;; Der Spieler, der das letzte Streichholz wegnehmen muss verliert.
;; Im Folgenden wird das Miserè-Spiel rein funktional in Scheme nach
;; R5RS implementiert.

;; Die Datenstruktur des Spielfeldes ist eine einfache Liste. Jeder
;; Listeneintrag korrespondiert mit der Anzahl der Streichhölzer in
;; der jeweiligen Reihe:

(define initial '(1 3 5 7))

;; Die Anzahl der Reihen entspricht somit der Höhe des Spielfeldes.

(define (calc-field-height field)
  (length field))

;; Die Breite der einzelnen Reihen errechnet sich aus der Formel
;; 1+2*(n-1). Die Breite des gesamten Spielfeldes entspricht der
;; Anzahl der Streichhölzer in der letzten Reihe (in der
;; Ausgangssituation).  Als n wird dementsprechend die Höhe des
;; Spielfeldes gewählt

(define (calc-field-width field)
  (1+ (* (1- (length field)) 2)))

;; Gibt eine Liste mit *count* Elementen zurück. Jedes Element hat den
;; Wert *symbol*.

(define (generate count symbol)
  (let rec ((count count)
             (symbol symbol)
             (lst '()))
  (if (= count 0)
      lst
      (rec (1- count) symbol (cons symbol lst)))))

(define (generate-row num)
  (generate num "|"))

(define (generate-whitespace num)
  (generate num " "))

;; Funktion zur Darstellung des Spielfeldes.

(define (draw-field field)
  (let ((width (calc-field-width field)))
    (map
     (lambda (x)
       (map
        display
        (generate-whitespace
         (floor (/ (- width x) 2))))
       (map display (generate-row x))
       (newline))
     field)))

;; Funktion zur Überprüfung eines Zuges. Man soll nicht mehr
;; Streichhölzer entnehmen können als in der jeweiligen Reihe
;; vorhanden sind. Außerdem muss der Wert für die zu bearbeitende
;; Reihe ein Integer im entsprechenden Intervall [0, Spielfeldhöhe]
;; sein.

(define (sanity-check sexp field)
    (let ((row (car sexp))
          (how-many (cadr sexp)))
      (if
       (or
        (eq? (integer? row) #f)
        (eq? (integer? how-many) #f)
        (>= row (calc-field-height field))
        (< row 0)
        (> how-many (get-nth field row))
        (< how-many 1))
       #f
       #t)))

;; Fragt solange nach einer Spielereingabe in der Form (Reihenindex
;; Anzahl), bis alle Testbedingungen für den Ausdruck erfüllt sind. Die
;; erste Reihe hat den Reihenindex 0.

(define (read-user-input sexp field)
  (if
   (not
    (and
     (pair? sexp)
     (= (length sexp) 2)
     (eq? (sanity-check sexp field) #t)))
   (begin
     (display "your move: ")
     (read-user-input (read) field))
   sexp))

;; (get-nth '(1 3 5 7) 2)
;; -> 5

(define (get-nth lst n)
  (if (> n 0)
      (get-nth (cdr lst) (1- n))
      (car lst)))

;; Nimmt eine List entgegen und gibt eine neue veränderte Liste
;; zurück.
;;
;;(set-nth '(1 3 5 7) 2 99)
;; -> (1 3 99 7)

(define (set-nth lst n val)
  (if (> n 0)
      (cons (car lst)
            (set-nth (cdr lst) (1- n) val))
      (cons val (cdr lst))))

;; Nimmt ein Spielfeld entgegen, entfernt Streichhölzer entsprechend
;; des übergebenen Ausdrucks und gibt ein neues, verändertes Spielfeld
;; zurück.

(define (take-objects field sexp)
  (let ((row (car sexp))
        (how-many (cadr sexp)))
    (set-nth field row (- (get-nth field row) how-many))))

(define (convert-binary-helper num bit list-bin)
  (if
   (> bit 0)
   (convert-binary-helper
    num
    (1- bit)
    (cons (logbit? (1- bit) num) list-bin))
   list-bin))

;; Konvertiert eine Zahl (Basis 10) in eine Liste der Länge *len* mit
;; den entsprechenden Binärwerten.
;;
;; (convert-binary 7 3)
;; -> (1 1 1)
;; (convert-binary 7 5)
;; -> (0 0 1 1 1)

(define (convert-binary num len)
  (reverse
   (map
    (lambda (x)
      (if (eq? x #t) 1 0))
    (convert-binary-helper num len '()))))

;; (accumulate + 0 '(1 3 5 7))
;; -> 16

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; (sum '(1 3 5 7)
;; -> 16

(define (sum lst)
  (accumulate + 0 lst))

;; (accumulate-n + 0 '((1 2 3) (3 2 1)))
;; -> (4 4 4)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons
       (accumulate op init (map car seqs))
       (accumulate-n op init (map cdr seqs)))))

;; Diese Funktion berechnet wieviel Bits zur Darstellung des
;; übergebenen Integers benötigt werden.
;;
;; (calc-bit-len 7)
;; -> 3
;; (calc-bit-len 255)
;; -> 8

(define (calc-bit-len num)
  (inexact->exact (ceiling (/ (log (1+ num)) (log 2)))))

;; Diese Funktion testet ob die Spaltensummen des übergeben Spielfelds
;; gerade sind. Beispiel:
;;
;; 1 =   0 0 1
;; 3 =   0 1 1
;; 5 =   1 0 1
;; 7 =   1 1 1
;; Summe 2 2 4
;;
;; (map convert-binary '(1 3 5 7) '(3 3 3 3))
;; -> ((0 0 1) (0 1 1) (1 0 1) (1 1 1))
;; (accumulute-n + 0 ANS)
;; -> (2 2 4)
;; (filter even? ANS)
;; -> (2 2 4)
;; (= (length ANS) 3)
;; -> #t

(define (sums-even? field)
  (let* ((bit-len (calc-bit-len (calc-field-width field)))
         (height (calc-field-height field))
         (bin-rows (map
                    (lambda (x)
                      (convert-binary x bit-len))
                    field)))
    (if (=
         (length
          (filter
           even?
           (accumulate-n + 0 bin-rows)))
         bit-len)
        #t
        #f)))
  
;; (range 3 6)
;; -> (3 4 5 6)

(define (range low high)
  (if (> low high)
      '()
      (cons low (range (+ low 1) high))))

;; (permutate-n-to-many 1 '(1 2 3))
;; -> ((1 1) (1 2) (1 3))

(define (permutate-n-to-many elem lst)
  (accumulate-n
   cons
   '()
   (list (generate (length lst) elem) lst)))

;; (permutate-many-to-many '(1 2) '(1 2 3))
;; -> (((1 1) (1 2) (1 3)) ((2 1) (2 2) (2 3))) 

(define (permutate-many-to-many lst1 lst2)
  (if (null? lst1)
      '()
      (cons 
       (permutate-n-to-many (car lst1) lst2)
       (permutate-many-to-many (cdr lst1) lst2))))

;; (flatten (permutate-many-to-many '(1 2) '(1 2 3)))
;; -> ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3))

(define (flatten lst)
  (cond ((null? lst)
         '())
        ((pair? lst)
         (append (car lst) (flatten (cdr lst))))
        (else
         (list lst))))

;; Generiert alle möglichen Spielzüge zu gegebenem Spielfeld und prüft
;; sie auf Validität.

(define (possible-moves field)
  (let ((width (calc-field-width field))
        (height (calc-field-height field)))
    (filter
     (lambda (x)
       (sanity-check x field))
     (flatten (permutate-many-to-many
               (range 0 (1- height))
               (range 1 width))))))

;; Testet ob das Spielfeld so manipuliert werden kann, dass in jeder
;; Reihe nur noch ein Streichholz verbleibt.

(define (endgame? field)
  (if (= (length
          (filter
           (lambda (x) (> x 1))
           field))
         1)
      #t
      #f))

;; Testet ob die Anzahl der Reihen, in denen noch Streichhölzer
;; liegen, gerade ist.

(define (rows-even? field)
  (if (even?
       (length
        (filter
         (lambda (x) (> x 0))
         field)))
      #t
      #f))

;; Durchsucht eine Liste nach einem bestimmten Element und gibt den
;; entsrechenden Index zurück.
;;
;; (find-at '(1 3 5 7) 7)
;; -> 3

(define (find-at lst elem)
  (let rec ((lst lst)
            (elem elem)
            (count 0))
    (if (= (car lst) elem)
        count
        (rec (cdr lst) elem (1+ count)))))

;; Spielt einen zufälligen, validen Zug. Wird nur aufgerufen, falls
;; sich keine optimale Strategie Spielen lässt, oder dem Spieler eine
;; Chance auf den Sieg ermöglicht werden soll.

(define (computer-random-move field moves)
  ;;(display "playing random strategy")(newline)
  (set! *random-state* (random-state-from-platform))
  (let ((rand
         (random (accumulate + 0 field))))
    (take-objects
     field
     (get-nth moves rand))))

;; Es wird per Brute-Force-Methode jeder mögliche Zug getestet bis
;; alle die Spaltensummen des Spielfeldes gerade sind. Diese Funktion
;; führt nur zu einem Ergebnis, falls die Spaltensummen im übergebenen
;; Feld ungerade sind.

(define (computer-strategy-1 field moves)
  ;;(display "playing strategy 1")(newline)
  (let ((new-field (take-objects field (car moves))))
    (if (sums-even? new-field)
        new-field
        (computer-move field (cdr moves)))))

;; Der Computer nimmt aus derjenigen Reihe in der mehr als ein
;; Streichholz liegt, entweder alle Streichhölzer, oder alle bis auf
;; eines. Er trifft die Entscheidung so, dass nach seinem Zug eine
;; ungerade Anzahl von Reihen mit je einem Streichholz verbleibt.

(define (computer-strategy-2 field moves)
  ;;(display "playing strategy 2")(newline)
  (let* ((long-row (find-at field (apply max field)))
         (possible-moves
          (reverse
           (filter
            (lambda (x) (= (car x) long-row))
            moves)))
         (take-all (take-objects field (car possible-moves)))
         (take-all-but-one (take-objects field (cadr possible-moves))))
    (if (rows-even? take-all)
        take-all-but-one
        take-all)))

;; Sind alle Spaltensummen gerade wählt der Computer einen zufälligen
;; Zug, ansonsten spielt er die optimale Strategie 1. Kann das
;; Spielfeld so manipuliert werden, dass in jeder Reihe nur noch ein
;; Streichholz verbleibt, spielt der Computer die optimale Strategie
;; 2.

(define (computer-move field moves)
  (cond
   ((endgame? field)
    (computer-strategy-2 field moves))
   ((sums-even? field)
    (computer-random-move field moves))
   (else
    (computer-strategy-1 field moves))))

;; Die Bedingung für Abschluss des Spiels: liegt nur noch ein
;; Streichholz auf dem Brett hat derjenige verloren, der gerade am Zug
;; ist.

(define (one-left? field)
  (if (= (sum field) 1) #t #f))

(define (game-loop field)
  (draw-field field)
  (display field)
  (newline)
  (if (one-left? field)
      (display "you lose")
      (begin
        (display "your move: ")
        (let ((new-field
               (take-objects
                field
                (read-user-input (read) field))))
          (if (one-left? new-field)
              (display "you win")
              (game-loop
               (computer-move
                new-field
                (possible-moves new-field))))))))

;; Der Computer muss den ersten Zug machen, sonst kann der Spieler
;; nicht gewinnen! Der erste Zug kann nie optimal sein, da alle
;; Spaltensummen im Ausgangszustand bereits gerade sind.

(game-loop (computer-move initial (possible-moves initial)))
(newline)
