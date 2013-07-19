(ns bachelor.wsk
  (:require [clojure.string :as str])
  (:use [clojure.contrib.generic.math-functions]
        [clj-time.core]
        [clj-time.format])
  )

(import '(java.io BufferedReader FileReader)
	'(java.lang String))

(defstruct Company :name :session)

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
  [line]
  ;(println line)
  (let [[name date open high low close vol] (str/split line #",")]
    (def s (struct Session (parseDate date) (read-string open) (read-string high) (read-string low) (read-string close) (read-string vol)))
    (def c (struct Company name s)))
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
		   (processFileLine line)))
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
   (cons (nth lst (dec k) 0) (getKElements (dec k) lst)))
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

(defn typicalPrice
  "Calculates typical price for session"
  [session]
  (/ (+ (:high session) (:low session) (:close session)) 3)
  )

(defn moneyFlow
  "Calculates money flow for session"
  [session]
  (* (typicalPrice session) (:vol session))
  )

(defn posMoneyFlow
  "Calculates positive money flow for k sessions"
  [k lst]
  (cond
    (empty? lst) 0
    (zero? k) 0
    (= (count lst) 1) 0
    (> (typicalPrice (:session (first lst))) (typicalPrice (:session (first (rest lst))))) (+ (moneyFlow (:session (first lst))) (posMoneyFlow (dec k) (rest lst)))
    :else
    (+ 0 (posMoneyFlow (dec k) (rest lst))))
  )

(defn negMoneyFlow
  "Calculates negative money flow for k sessions"
  [k lst]
  (cond
    (empty? lst) 0
    (zero? k) 0
    (= (count lst) 1) 0
    (< (typicalPrice (:session (first lst))) (typicalPrice (:session (first (rest lst))))) (+ (moneyFlow (:session (first lst))) (negMoneyFlow (dec k) (rest lst)))
    :else
    (+ 0 (negMoneyFlow (dec k) (rest lst))))
  )

(defn mfi
  "Calculates money flow index for last k sessions"
  [k lst]
  (* 100 (/ (posMoneyFlow k lst) (+ (posMoneyFlow k lst) (negMoneyFlow k lst))))
  )

(defn TR
  "Calculates True Range"
  [previous actual]
  (max (abs (- (:high actual) (:low actual)))
       (abs (- (:close previous) (:high actual)))
       (abs (- (:close previous) (:low actual))))
  )

(defn sumTR
  "Sums True Range for last k sessions"
  [k lst]
  (cond
    (empty? lst) 0
    (zero? k) 0
    (= (count lst) 1) 0
    :else
    (+ (TR (:session (first (rest lst))) (:session (first lst)))
       (sumTR (dec k) (rest lst))))
  )

(defn atr
  "Calculates Average True Range for k sessions"
  [k lst]
  (cond
    (empty? lst) 0
    (zero? k) 0
    :else
    (/ (sumTR k lst) k))
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

(defn multiplyWeightsWykMACD
  "Mno�y ceny przez wagi wyk�adnicze"
  [k n lst]
  (cond
   (empty? lst) 0
   (< k n) 0
   :else
   (+ (* (first lst) (pow (- 1 (alfa k)) n)) (multiplyWeightsWykMACD k (inc n) (rest lst))))
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

(defn CLV
  "Calculates CLV - close location value"
  [session]
  (if (zero? (- (:high session) (:low session)))
    (/ (- (- (:close session) (:low session)) (- (:high session) (:close session))) 0.01)
    (/ (- (- (:close session) (:low session)) (- (:high session) (:close session))) (- (:high session) (:low session))))
  )

(defn VOLxCLV
  "Multiply vol and CLV"
  [lst]
  (cond
    (empty? lst) ()
    :else
    (cons (* (:vol (:session (first lst))) (CLV (:session (first lst)))) (VOLxCLV (rest lst))))
  )

(defn Accum
  "Calculates Accumulation/Distribution index"
  [lst]
  (cond
    (empty? lst) ()
    (= (count lst) 1) (cons (first lst)
                            (Accum (rest lst)) 
                            )
    :else
    (cons (+ 
            (first (rest lst))
            (first lst))
          (Accum (rest lst)) 
          ))
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

(defn checkSK
  "Checks how many times first SK cuts from bottom second SK in last k days"
  [sk1 sk2 k]
  (cond
    (or (empty? sk1) (empty? sk2)) 0
    (or (empty? (rest sk1)) (empty? (rest sk2))) 0
    (zero? k) 0
    :else
    (if (and
          (< (first (rest sk1)) (first (rest sk2)))
          (> (first sk1) (first sk2)))
      (inc (checkSK (rest sk1) (rest sk2) (dec k)))
      (checkSK (rest sk1) (rest sk2) (dec k)))
    )
  )

(defn lineMACD
  "Calculates MACD line"
  [ema12 ema26]
  (cond
    (or (empty? ema12) (empty? ema26)) ()
    :else
    (cons (- (first ema26) (first ema12)) (lineMACD (rest ema12) (rest ema26))))
  )

(defn signalLine
  "Wylicza linię sygnału"
  [k lst]
  (cond
   (empty? lst) ()
   (>= k (count lst)) (cons 0 (signalLine k (rest lst)))
   :else
   (cons (/ (multiplyWeightsWykMACD k 0 lst) (sumWeightsWyk k k)) (signalLine k (rest lst))))
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
  
  (def listWykSK12 (wykSK 12 notowania))
  
  (def listWykSK26 (wykSK 26 notowania))
  
  (def listAccum (Accum (VOLxCLV notowania)))
  
  (def mfi10 (mfi 10 notowania))
  
  (def mfi20 (mfi 20 notowania))
  
  (def mfi30 (mfi 30 notowania))
  
  (def atr10 (atr 10 notowania))
  
  (def atr20 (atr 20 notowania))
  
  (def atr30 (atr 30 notowania))
  
  (def macd (lineMACD listWykSK12 listWykSK26))
  
  (def sigLine (signalLine 9 macd))
  
  (def listVol (for [line notowania]
                 (:vol (:session line))))
  
  (def listClose (for [line notowania]
                   (:close (:session line))))
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
  
(defn CLOSE10
  "Gets close price for 10th session back"
  []
  (nth listClose 10 0)
  )
  
(defn CLOSE20
  "Gets close price for 20th session back"
  []
  (nth listClose 20 0)
  )
  
(defn CLOSE30
  "Gets close price for 30th session back"
  []
  (nth listClose 30 0)
  )

(defn MAX10ROC10
  "Gets max ROC10 value for last 10 sessions"
  []
  (apply max (getKElements 10 listROC10))
  )

(defn MAX20ROC10
  "Gets max ROC10 value for last 20 sessions"
  []
  (apply max (getKElements 20 listROC10))
  )

(defn MAX30ROC10
  "Gets max ROC10 value for last 30 sessions"
  []
  (apply max (getKElements 30 listROC10))
  )

(defn MAX5ROC5
  "Gets max ROC5 value for last 5 sessions"
  []
  (apply max (getKElements 5 listROC5))
  )

(defn MAX10ROC5
  "Gets max ROC5 value for last 10 sessions"
  []
  (apply max (getKElements 10 listROC5))
  )

(defn MAX15ROC5
  "Gets max ROC5 value for last 15 sessions"
  []
  (apply max (getKElements 15 listROC5))
  )

(defn MAX5CLOSE
  "Gets max close price for last 5 sessions"
  []
  (apply max (getKElements 5 listClose))
  )

(defn MAX10CLOSE
  "Gets max close price for last 10 sessions"
  []
  (apply max (getKElements 10 listClose))
  )

(defn MAX15CLOSE
  "Gets max close price for last 15 sessions"
  []
  (apply max (getKElements 15 listClose))
  )

(defn MAX20CLOSE
  "Gets max close price for last 20 sessions"
  []
  (apply max (getKElements 20 listClose))
  )

(defn MAX30CLOSE
  "Gets max close price for last 30 sessions"
  []
  (apply max (getKElements 30 listClose))
  )

(defn MIN10ROC10
  "Gets min ROC10 value for last 10 sessions"
  []
  (apply min (getKElements 10 listROC10))
  )

(defn MIN20ROC10
  "Gets min ROC10 value for last 20 sessions"
  []
  (apply min (getKElements 20 listROC10))
  )

(defn MIN30ROC10
  "Gets min ROC10 value for last 30 sessions"
  []
  (apply min (getKElements 30 listROC10))
  )

(defn MIN5ROC5
  "Gets min ROC5 value for last 5 sessions"
  []
  (apply min (getKElements 5 listROC5))
  )

(defn MIN10ROC5
  "Gets min ROC5 value for last 10 sessions"
  []
  (apply min (getKElements 10 listROC5))
  )

(defn MIN15ROC5
  "Gets min ROC5 value for last 15 sessions"
  []
  (apply min (getKElements 15 listROC5))
  )

(defn MIN5CLOSE
  "Gets min close price for last 5 sessions"
  []
  (apply min (getKElements 5 listClose))
  )

(defn MIN10CLOSE
  "Gets min close price for last 10 sessions"
  []
  (apply min (getKElements 10 listClose))
  )

(defn MIN15CLOSE
  "Gets min close price for last 15 sessions"
  []
  (apply min (getKElements 15 listClose))
  )

(defn MIN20CLOSE
  "Gets min close price for last 20 sessions"
  []
  (apply min (getKElements 20 listClose))
  )

(defn MIN30CLOSE
  "Gets min close price for last 30 sessions"
  []
  (apply min (getKElements 30 listClose))
  )

(defn MAX10ACCUM
  "Gets max value for Accumulation/Distribution index for last 10 sessions"
  []
  (apply max (getKElements 10 listAccum))
  )

(defn MAX20ACCUM
  "Gets max value for Accumulation/Distribution index for last 20 sessions"
  []
  (apply max (getKElements 20 listAccum))
  )

(defn MAX30ACCUM
  "Gets max value for Accumulation/Distribution index for last 30 sessions"
  []
  (apply max (getKElements 30 listAccum))
  )

(defn MIN10ACCUM
  "Gets min value for Accumulation/Distribution index for last 10 sessions"
  []
  (apply min (getKElements 10 listAccum))
  )

(defn MIN20ACCUM
  "Gets min value for Accumulation/Distribution index for last 20 sessions"
  []
  (apply min (getKElements 20 listAccum))
  )

(defn MIN30ACCUM
  "Gets min value for Accumulation/Distribution index for last 30 sessions"
  []
  (apply min (getKElements 30 listAccum))
  )

(defn LASTACCUM
  "Gets last value for Accumulation/Distribution index"
  []
  (first listAccum)
  )

(defn MFI10
  "Gets MFI value for k = 10"
  []
  mfi10
  )

(defn MFI20
  "Gets MFI value for k = 20"
  []
  mfi20
  )

(defn MFI30
  "Gets MFI value for k = 30"
  []
  mfi30
  )

(defn ATR10PERCENT
  "Gets what percent of last close price is ATR10"
  []
  (* 100 (/ atr10 (LASTCLOSE)))
  )

(defn ATR20PERCENT
  "Gets what percent of last close price is ATR20"
  []
  (* 100 (/ atr20 (LASTCLOSE)))
  )

(defn ATR30PERCENT
  "Gets what percent of last close price is ATR30"
  []
  (* 100 (/ atr30 (LASTCLOSE)))
  )

(defn DAYS10EMA12CUTEMA26
  "Gets how many times EMA12 cuts EMA26 from bottom in last 10 days"
  []
  (checkSK listWykSK12 listWykSK26 10)
  )

(defn DAYS10EMA26CUTEMA12
  "Gets how many times EMA26 cuts EMA12 from bottom in last 10 days"
  []
  (checkSK listWykSK26 listWykSK12 10)
  )

(defn DAYS10MACDCUTSIGNAL
  "Gets how many times MACD line cuts signal line from bottom in last 10 days"
  []
  (checkSK macd sigLine 10)
  )

(defn DAYS10SIGNALCUTMACD
  "Gets how many times signal line cuts MACD line from bottom in last 10 days"
  []
  (checkSK sigLine macd 10)
  )



