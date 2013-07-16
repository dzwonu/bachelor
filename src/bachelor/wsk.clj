(ns bachelor.wsk
  (:require [clojure.string :as str])
  (:use [clojure.contrib.generic.math-functions]
        [clj-time.core]
        [clj-time.format])
  )

(import '(java.io BufferedReader FileReader)
	'(java.lang String))

(defstruct Company :id :name :session)

(defstruct Session :date :open :high :low :close :vol)

(defstruct Fact :short :description)

(def custom-formatter (formatter "yyyyMMdd"))

(defn parseDate
  "Parses string with session date to date var"
  [date]
  (parse custom-formatter date)
  )

(defn processFileLine
  "Przetwarza linie z pliku do postaci uzywanej w aplikacji"
  [ind line]
  ;(println line)
  (let [[name date open high low close vol] (str/split line #",")]
    (def s (struct Session (parseDate date) (read-string open) (read-string high) (read-string low) (read-string close) (read-string vol)))
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
  [company]
  (reverse (for [line (with-open [rdr (BufferedReader. (FileReader. (str "resources/notowania/" company ".mst")))]
			      (doall (rest (line-seq rdr))))
		       ]
		   (processFileLine 0 line)))
  )

(defn printNotowania
  [notowania]
  (doseq [i (range (count notowania))]
    (println (:vol (:session (nth notowania i "nie ma"))))))

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
  [notowania]
  (def listROC10 (liczROC 10 notowania))

  (def listROC5 (liczROC 5 notowania))
  
  (def listMomentum (liczMomentum 3 notowania))

  (def listLinSK4 (linSK 4 notowania))

  (def listLinSK9 (linSK 9 notowania))

  (def listLinSK18 (linSK 18 notowania))

  (def listRSI (liczRSI notowania (liczRS 5 notowania)))

  (def listWazSK4 (wazSK 4 notowania))

  (def listWykSK4 (wykSK 4 notowania))
  
  (def listVol (for [line notowania]
                 (:vol (:session line))))
  
  (def listClose (for [line notowania]
                   (:close (:session line)))
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
  "ROC10 for last session"
  []
  (first listROC10)
  )

(defn ROC5
  "ROC5 for last session"
  []
  (first listROC5)
  )

(defn MOMENTUM3
  "MOMENTUM3 for last session"
  []
  (first listMomentum)
  )

(defn LASTVOL
  "Gets last vol value"
  []
  (getVol listVol)
  )

(defn VOL10
  "Gets vol value for 10th session back"
  []
  (nth listVol 10 0)
  )

(defn VOL20
  "Gets vol value for 20th session back"
  []
  (nth listVol 20 0)
  )

(defn VOL30
  "Gets vol value for 30th session back"
  []
  (nth listVol 30 0)
  )

(defn RSI
  "Gets lat RSI value"
  []
  (first listRSI)
  )

(defn LASTCLOSE
  "Gets close price from last session"
  []
  (first listClose)
  )
















