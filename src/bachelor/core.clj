(ns bachelor.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str])
  (:use [clojure.contrib.generic.math-functions])
  ;(:import )  
  )

(import '(java.io BufferedReader FileReader)
	'(java.lang String))

(defstruct Company :id :name :session)

(defstruct Session :open :high :low :close :vol)

(defstruct Fact :short :description)

(defstruct Rule :name :args)

(defn processFileLine
  "Przetwarza linie z pliku do postaci uzywanej w aplikacji"
  [ind line]
  ;(println line)
  (let [[name open high low close vol] (str/split line #";")]
    (def s (struct Session (read-string open) (read-string high) (read-string low) (read-string close) (read-string vol)))
    (def c (struct Company ind name s)))
  c
  )

(defn readFile
  "Wczytuje plik tekstowy"
  [filename]
  (with-open [rdr (BufferedReader. (FileReader. filename))]
    (doall (line-seq rdr)))
  )

(defn createCompany
  "Tworzy dane sp�ki"
  []
  (def Notowania (reverse (for [line (with-open [rdr (BufferedReader. (FileReader. "C:/bachelor/src/bachelor.data/FUNMEDIA.csv"))]
			      (doall (line-seq rdr)))
		       ]
		   (processFileLine 0 line))))
  )

(defn printNotowania
  []
  (doseq [i (range (count Notowania))]
    (println (:vol (:session (nth Notowania i "nie ma"))))))

(defn getClose
  "Pobiera cen� zamkni�cia sesji pierwszej pozycji z listy"
  [lst]
  (cond
   (empty? lst) 0
   :else
   (:close (:session (first lst))))
  )

(defn getAllClose
  "Pobiera ceny zamkni�cia z listy notowa�"
  [lst]
  (cond
   (empty? lst) ()
   :else
   (cons (getClose lst) (getAllClose (rest lst))))
  )

(defn getCloseNth
  "Pobiera cen� zamkni�cia z wybranej sesji w li�cie"
  [k lst]
  (cond
   (empty? lst) 0
   :else
   (:close (:session (nth lst k 0))))
  )

(defn getKClose
  "Pobiera ceny zamkni�cia z ostatnich k sesji"
  [k lst]
  (cond
   (or (empty? lst) (zero? k)) ()
   :else
   (cons (getClose lst) (getKClose (dec k) (rest lst))))
  )

(defn getVol
  "Pobiera wolument w pierwszego elementu listy"
  [lst]
  (:vol (:session (first lst)))
  )

(defn getKElements
  "Pobiera k element�w z listy"
  [k lst]
  (cond
   (or (empty? lst) (zero? k)) ()
   :else
   (cons (nth lst k) (getKElements (dec k) (rest lst))))
  )

(defn createMapClose
  "Tworzy map� cen zamkni�cia"
  [lst]
  (map :close (map :session lst))
  )

(defn posSubs
  "Zwraca sum� dodatnich r�nic pomi�dzy cenami zamkni�cia"
  [lst]
  (cond
   (< (count lst) 2) 0
   :else
   (if (pos? (- (first lst) (first (rest lst))))
     (+ (- (first lst) (first (rest lst))) (posSubs (rest lst)))
     (+ 0 (posSubs (rest lst)))))
  )

(defn negSubs
  "Zwraca sum� ujemnych r�nic pomi�dzy cenami zamkni�cia"
  [lst]
  (cond
   (< (count lst) 2) 0
   :else
   (if (neg? (- (first lst) (first (rest lst))))
     (+ (- (first lst) (first (rest lst))) (negSubs (rest lst)))
     (+ 0 (negSubs (rest lst)))))
  )

(defn multiplyWeightsWaz
  "Mno�y ceny przez wagi wazone i zwraca sum�"
  [k lst]
  (cond
   (empty? lst) 0
   (< k 1) 0
   :else
   (+ (* k (getClose lst)) (multiplyWeightsWaz (dec k) (rest lst))))
  )

(defn sumWeightsWaz
  "Sumuje wagi wazone"
  [k]
  (cond
   (< k 1) 0
   :else
   (+ k (sumWeightsWaz (dec k))))
  )

(defn alfa
  "Wylicza wsp�czynnik alfa dla wyk�adniczej �redniej krocz�cej"
  [k]
  (/ 2 (inc k))
  )

(defn sumWeightsWyk
  "Sumuje wagi wyk�adnicze"
  [k n]
  (cond
   (< n 1) 1
   :else
   (+ (pow (- 1 (alfa k)) n) (sumWeightsWyk k (dec n))))
  )

(defn multiplyWeightsWyk
  "Mno�y ceny przez wagi wyk�adnicze"
  [k n lst]
  (cond
   (empty? lst) 0
   (< k n) 0
   :else
   (+ (* (getClose lst) (pow (- 1 (alfa k)) n)) (multiplyWeightsWyk k (inc n) (rest lst))))
  )

(defn wykSK
  "Wylicza wyk�adnicz� �redni� krocz�c� dla k sesji"
  [k lst]
  (cond
   (empty? lst) ()
   (>= k (count lst)) (cons 0 (wykSK k (rest lst)))
   :else
   (cons (/ (multiplyWeightsWyk k 0 lst) (sumWeightsWyk k k)) (wykSK k (rest lst))))
  )

(defn wazSK
  "Wylicza wa�on� �redni� krocz�c� dla k sesji"
  [k lst]
  (cond
   (empty? lst) ()
   (>= k (count lst)) (cons 0 (wazSK k (rest lst)))
   :else
   (cons (/ (multiplyWeightsWaz k lst) (sumWeightsWaz k)) (wazSK k (rest lst))))
  )

(defn liczROC
  "Wylicza wska�nik ROC dla ka�dej sesji"
  [k lst]
  (cond
   (empty? lst) ()
   (>= k (count lst)) (cons 0 (liczROC k (rest lst)))
   :else
   (cons (* (/ (getClose lst) (getCloseNth k lst)) 100) (liczROC k (rest lst))))
  )

(defn liczRS
  "Wylicza sk�adow� RS dla wska�nika RSI"
  [k lst]
  (cond
   (empty? lst) ()
   (>= k (count lst)) (cons 0 (liczRS k (rest lst)))
   (zero? (negSubs (getKClose k lst))) (cons 0 (liczRS k (rest lst)))
   :else
   (cons (/
	  (posSubs (getKClose k lst))
	  (* -1 (negSubs (getKClose k lst)))
	  )
	 (liczRS k (rest lst))))
  )

(defn liczRSI
  "Wylicza wska�nik RSI"
  [lst rs]
  (cond
   (empty? lst) ()
   :else
   (cons (- 100 (/ 100 (inc (first rs)))) (liczRSI (rest lst) (rest rs))))
  )

(defn printList
  [lst]
  (cond
   (empty? lst) ()
   :else
   (do
     (println (first lst))
     (printList (rest lst))))
  )

(defn liczMomentum
  "Wylicza wska�nik ROC dla ka�dej sesji"
  [k lst]
  (cond
   (empty? lst) ()
   (>= k (count lst)) (cons 0 (liczMomentum k (rest lst)))
   :else
   (cons (- (getClose lst) (getCloseNth k lst)) (liczMomentum k (rest lst))))
  )		    

(defn linSK
  "Wylicza liniow� �redni� krocz�c� dla ka�dej sesji"
  [k lst]
  (cond
   (empty? lst) ()
   (>= k (count lst)) (cons 0 (linSK k (rest lst)))
   :empty
   (cons (/ (reduce + (createMapClose (getKElements k lst))) k) (linSK k (rest lst))))
  )

(defn liczWskazniki
  "Oblicza wszystkie zaprogramowane wska�niki"
  []
  (def listROC (liczROC 10 Notowania))
;  (printList listROC)

  (def listMomentum (liczMomentum 3 Notowania))
;  (printList listMomentum)

  (def listLinSK4 (linSK 4 Notowania))
;  (printList listLinSK4)

  (def listLinSK9 (linSK 9 Notowania))

  (def listLinSK18 (linSK 18 Notowania))

  (def listRSI (liczRSI Notowania (liczRS 5 Notowania)))
;  (printList listRSI)

  (def listWazSK4 (wazSK 4 Notowania))
;  (printList listWazSK4)

  (def listWykSK4 (wykSK 4 Notowania))
;  (printList listWykSK4)

  
  )

(defn checkROC
  "Sprawdza ROC w ostatnich k sesjach"
  [k lst]
  (cond
   (or (empty? lst) (zero? k)) 0
   :else
   (if (> (first lst) 100)
     (inc (checkROC (dec k) (rest lst)))
     (dec (checkROC (dec k) (rest lst)))))
  )

(defn checkMomentum
  "Sprawdza Momentum w ostatnich k sesjach"
  [k lst]
  (cond
   (or (empty? lst) (zero? k)) 0
   :else
   (if (pos? (first lst))
     (inc (checkMomentum (dec k) (rest lst)))
     (dec (checkMomentum (dec k) (rest lst)))))
  )

(defn checkLinSK2
  "Sprawdza 2 liniowe �rednie krocz�ce w ostatnich k sesjach"
  [k lin1 lin2]
  (cond
   (or (empty? lin1) (zero? k)) ()
   :else
   (if (< (first lin1) (first lin2))
     (cons -1 (checkLinSK2 (dec k) (rest lin1) (rest lin2)))
     (cons 1 (checkLinSK2 (dec k) (rest lin1) (rest lin2)))))
  )

(defn checkVol
  "Sprawdza zachowanie wolumeny w ostatnich k sesjach"
  [k lst]
  (cond
   (or (empty? lst) (zero? k)) ()
   :else
   (if (< (getVol lst) (getVol (rest lst)))
     (cons 1 (checkVol (dec k) (rest lst)))
     (cons -1 (checkVol (dec k) (rest lst)))))
  )


(createCompany)
;(printNotowania)

(liczWskazniki)

(def x 10)

;(println "sprawdzian ROC:" (checkROC x listROC))
;(println "sprawdzian Momentum:" (checkMomentum x listMomentum))
;(println "sprawdzian linSK2:" (checkLinSK2 x listLinSK4 listLinSK9))
;(println "sprawdzian wolumenu:" (checkVol x Notowania))

(defn verifyROC
  "Weryfikuje wiedz� o ROC"
  []
  (if (> (checkROC x listROC) (/ x 2))
        (str "ROC:+")
	(str "ROC:-"))
  )

(defn verifyMomentum
  "Weryfikuje wiedz� o Momentum"
  []
  (if (> (checkMomentum x listMomentum) (/ x 2))
	(str "Momentum:+")
	(str "Momentum:-"))
  )

;(println (conj Facts (verifyROC)))
;(println (conj Facts (verifyMomentum)))


(defn acceptOpL
  "Sprawdza czy symbol jest operatorem logicznym"
  [sym]
  (cond
   (= sym "AND") sym
   (= sym "OR") sym
   :else
   ())
  )

(defn acceptOpA
  "Sprawdza czy symbol jest operatorem arytmetycznym"
  [sym]
  (cond
   (= sym ">") sym
   (= sym "<") sym
   (= sym "==") sym
   :else
   ())
  )

(defn acceptWSK
  "Sprawdza czy symbol jest prawid�owym wska�nikiem"
  [sym]
  (cond
   (= sym "ROC") sym
   (= sym "MOMENTUM") sym
   (= sym "RSI") sym
   (= sym "LINSK4") sym
   (= sym "LINSK9") sym
   (= sym "LINSK18") sym
   (= sym "WAZSK4") sym
   (= sym "WAZSK9") sym
   (= sym "WAZSK18") sym
   (= sym "WYKSK4") sym
   (= sym "WYKSK9") sym
   (= sym "WYKSK18") sym
   :else
   ())
  )

(defn checkString
  "Sprawdza czy string sk�ada si� wy��cznie z ma�ych liter"
  [sym]
  (cond
   (empty? sym) ()
   (= (str (first sym)) (str/upper-case (first sym))) sym
   :else
   (checkString (rest sym)))
  )

(defn acceptFACT
  "Sprawdza czy symbol jest faktem"
  [sym]
  (cond
   (empty? (checkString sym)) sym
   :else
   ())
  )

(defn acceptNUM
  "Sprawdza czy symbol jest liczb�"
  [sym]
  (cond
   (float? (read-string sym)) sym
   (integer? (read-string sym)) sym
   :else
   ())
  )

(defn acceptLP
  "Sprawdza czy symbol to nawias otwieraj�cy"
  [sym]
  (cond
   (= sym "(") sym
   :else
   ())
  )

(defn acceptPP
  "Sprawdza czy symbol to nawias zamykaj�cy"
  [sym]
  (cond
   (= sym ")") sym
   :else
   ())
  )

(defn acceptImplication
  "Sprawdza poprawno�� symbolu implikacji"
  [sym]
  (cond
   (= sym ">>") sym
   :else
   ())
  )

(defn expect
  "Sprawdza czy pobrany symbol jest prawid�owy"
  [expected sym]
  (cond
   (= expected "OpL") (acceptOpL sym)
   (= expected "OpA") (acceptOpA sym)
   (= expected "WSK") (acceptWSK sym)
   (= expected "FACT") (acceptFACT sym)
   (= expected "NUM") (acceptNUM sym)
   (= expected "LP") (acceptLP sym)
   :else
   (acceptPP symbol))
  )

(defn splitExpr
  "Wydziela tokeny z linii z regu��"
  [line]
  (str/split line #" ")
  )

(defn wyr
  "nowe wyrazenie"
  [przetworzone doPrzetworzenia]
  (cond
   (empty? doPrzetworzenia) przetworzone
   (empty? przetworzone) (if (empty? (acceptLP (first doPrzetworzenia)))
			   "error-poczatek";todo - error
			   (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
   (not (empty? (acceptLP (first przetworzone)))) (if (empty? (acceptFACT (first doPrzetworzenia)))
						    (if (empty? (acceptWSK (first doPrzetworzenia)))
						      (if (empty? (acceptLP (first doPrzetworzenia)))
							"error-LP";todo - error
							(wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
						      (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
						    (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
   (not (empty? (acceptFACT (first przetworzone)))) (if (empty? (acceptPP (first doPrzetworzenia)))
						      "error-FACT";todo - error
						      (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
   (not (empty? (acceptWSK (first przetworzone)))) (if (empty? (acceptOpA (first doPrzetworzenia)))
						     (if (empty? (acceptPP (first doPrzetworzenia)))
						       "error-WSK";todo - error
						       (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
						     (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
   (not (empty? (acceptOpA (first przetworzone)))) (if (empty? (acceptNUM (first doPrzetworzenia)))
						     (if (empty? (acceptWSK (first doPrzetworzenia)))
						       "error-OpA";todo - error
						       (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
						     (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
   (not (empty? (acceptPP (first przetworzone)))) (if (empty? (acceptOpL (first doPrzetworzenia)))
						    (if (empty? (acceptImplication (first doPrzetworzenia)))
						      "error-PP";todo - error
						      (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
						    (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
   (not (empty? (acceptOpL (first przetworzone)))) (if (empty? (acceptLP (first doPrzetworzenia)))
						     "error-OpL";todo - error
						     (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
   (not (empty? (acceptImplication (first przetworzone)))) (if (empty? (acceptFACT (first doPrzetworzenia)))
							     "error-Implication";todo - error
							     (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
   (not (empty? (acceptNUM (first przetworzone)))) (if (empty? (acceptPP (first doPrzetworzenia)))
						     "error-NUM";todo - error
						     (wyr (cons (first doPrzetworzenia) przetworzone) (rest doPrzetworzenia)))
   :else
   "error")
  )

(def tokens (for [line (with-open [rdr (BufferedReader. (FileReader. "C:/bachelor/src/bachelor.data/rules.txt"))]
					 (doall (line-seq rdr)))
				  ]
			      (reverse (wyr [] (splitExpr line)))))

;(println  tokens)

;(defn createRules
;  "przetwarza sekwencje token�w na list� regu�"
;  [tokensList rules]
;  (cond
;   (empty? tokensList) rules
;   :else
;   (createRules (rest tokensList) (cons (createRule (first tokensList)) rules)))
;  )
;
;(defn createRule
;  "przetwarza pojedyncz� list� z tokenami na funckj� - regu��"
;  [tokens]
;  (cond
;   (empty? tokens) ()
;   :else
;   ())
;  )
;
;(defn createStringFromTokens
;  "tworzy z token�w string dla funkcji (eval (str ...))"
;  [tokens expression]
;  (cond
;   (empty? tokens) expression
;   (not (empty? (acceptLP (first tokens)))) (if (empty? (acceptLP (first (rest tokens))))
;					      (createStringFromTokens (rest tokens)
;								      (str expression (first tokens)))
;					      ());przypadek (EXPR, czyli ((
;   (not (empty? (acceptWSK (first tokens)))) (if (empty? (acceptOpA (first (rest tokens))))
;					       (createStringFromTokens (rest tokens)
;								       (str expression "(" (first tokens) ") "))
;					       (createStringFromTokens (rest (rest tokens))
;								       (str expression (first (rest tokens)) " (" (first tokens) ") ")))
;   (not (empty? (acceptNUM (first tokens)))) (createStringFromTokens (rest tokens)
;								     (str expression (first tokens)))
;   (not (empty? (acceptPP (first tokens)))) (if (empty? (acceptPP (first (rest tokens))))
;					      (createStringFromTokens (rest tokens)
;								      (str expression (first tokens)))
;					      ());przypadek EXPR), czyli ))
;   :else
;   ())
;  )
  
(def as-and-bs
  (insta/parser
    "S = AB*
     AB = A B
     A = 'a' +
     B = 'b' +")
  )
    
    
    
    
    
    
    
    
    
    
    
    
    
    

