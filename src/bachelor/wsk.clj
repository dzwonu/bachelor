(ns bachelor.wsk
  (:require [clojure.string :as str])
  (:use [clojure.contrib.generic.math-functions])
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
  (reverse (for [line (with-open [rdr (BufferedReader. (FileReader. "resources/FUNMEDIA.csv"))]
			      (doall (line-seq rdr)))
		       ]
		   (processFileLine 0 line)))
  )

(def Notowania (createCompany))

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
  "Wylicza wska�nik Momentum dla ka�dej sesji"
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
  (def listROC10 (liczROC 10 Notowania))

  (def listROC5 (liczROC 5 Notowania))
  
  (def listMomentum (liczMomentum 3 Notowania))

  (def listLinSK4 (linSK 4 Notowania))

  (def listLinSK9 (linSK 9 Notowania))

  (def listLinSK18 (linSK 18 Notowania))

  (def listRSI (liczRSI Notowania (liczRS 5 Notowania)))

  (def listWazSK4 (wazSK 4 Notowania))

  (def listWykSK4 (wykSK 4 Notowania))
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
  "Sprawdza zachowanie wolumenu w ostatnich k sesjach"
  [k lst]
  (cond
   (or (empty? lst) (zero? k)) ()
   :else
   (if (< (getVol lst) (getVol (rest lst)))
     (cons 1 (checkVol (dec k) (rest lst)))
     (cons -1 (checkVol (dec k) (rest lst)))))
  )

(defn verifyROC
  "Weryfikuje wiedz� o ROC"
  []
  (if (> (checkROC 10 listROC10) (/ 10 2))
        (str "ROC:+")
	(str "ROC:-"))
  )

(defn verifyMomentum
  "Weryfikuje wiedz� o Momentum"
  []
  (if (> (checkMomentum 10 listMomentum) (/ 10 2))
	(str "Momentum:+")
	(str "Momentum:-"))
  )

(defn ROC10
  ""
  []
  (first listROC10)
  )

(defn ROC5
  ""
  []
  (first listROC5)
  )

(defn MOMENTUM3
  ""
  []
  (first listMomentum)
  )