(ns bachelor.core
  (:require [instaparse.core :as insta]
            [incanter.charts :as charts]
            [incanter.core :as incanter]
            [clojure.string :as str]
            [bachelor.wsk :as wsk])
  (:use [clojure.contrib.generic.math-functions]
        [seesaw.core]
        [seesaw.font]
        [seesaw.graphics]
        [clj-time.coerce])
  (:import java.io.File)
  (:import java.io.FileNotFoundException)
  )

(import '(java.io BufferedReader FileReader)
	'(java.lang String))

(import 'org.jfree.chart.ChartPanel)

(def grammar
  (insta/parser
    "RULE = EXPR' >> 'FACT
     EXPR = '('EXPR' 'OpL' 'EXPR')' | '('WSK' 'OpA' 'NUM')' | '('WSK' 'OpA' 'WSK')' | '('FACT')'
     OpL = 'and' | 'or'
     OpA = '>' | '<' | '=='
     WSK = #'[A-Z]+[A-Z0-9]*'
     FACT = #'[a-z]+[a-z0-9]*'
     NUM = #'[-+]?[0-9]+[.]?[0-9]*|[0-9]+'")
  )
    
(def Facts [])

(defn parse-expr 
  "parses expr struct into list of symbols"
  [expr arg]
  (cond
    (empty? expr) ()
    :else
    (let [[_ _ [part1-type part1-val] _ [part2-type part2-val] _ [part3-type part3-val] _] expr]
      (cond 
        (and (= :WSK part1-type) (= :OpA part2-type) (= :NUM part3-type)) (let 
                                                                            [wsk (symbol (str "bachelor.wsk/" part1-val))
                                                                             opa (symbol part2-val)
                                                                             num (Integer/valueOf part3-val)]
                                                                            (list opa (list wsk) num))
        (and (= :WSK part1-type) (= :OpA part2-type) (= :WSK part3-type)) (let
                                                                            [wsk1 (symbol (str "bachelor.wsk/" part1-val))
                                                                             opa (symbol part2-val)
                                                                             wsk2 (symbol (str "bachelor.wsk/" part3-val))]
                                                                            (list opa (list wsk1) (list wsk2)))
        (= :OpL part2-type) (let
                              [expr1 (parse-expr (get expr 2) arg)
                               opl (symbol part2-val)
                               expr2 (parse-expr (get expr 6) arg)]
                              (list opl expr1 expr2))
        (= :FACT part1-type) (let
                              [fact (str part1-val)
                               bool (symbol "boolean")
                               some (symbol "some")]
                              (list bool (list some #{fact} arg)))
        :else
        ())
      )
    )
  )

(defmacro generate-funcs 
  "creates function from parse-tree"
  [parse-tree]
  (let [[_ expr _ [_ fact-value]] parse-tree
        arg (gensym "facts")
        expression (parse-expr expr arg)
        new-fact (str fact-value)]
    `(fn 
       [~arg] 
       (if ~expression
         (str ~new-fact) 
         ())
       )
    )
  )

(defn processRulesFromFile
  "processes file with rules"
  []
  (for [line (with-open 
               [rdr (BufferedReader. (FileReader. "resources/rules.txt"))]
               			      (doall (line-seq rdr)))
        		       ]
    (macroexpand-1 `(generate-funcs ~(grammar line))))
  )

(defn containsFact?
  "check if fact is on list of facts"
  [fact facts]
  (boolean (some #{(str fact)} facts))
  )

(defn buy?
  "check if list of facts contain buy fact"
  [facts]
  (boolean (some #{"buy"} facts))
  )

(defn sell?
  "check if list of facts contain sell fact"
  [facts]
  (boolean (some #{"sell"} facts))
  )

(defn done?
  "checks if list of facts contain buy or sell fact"
  [facts]
  (boolean (or (buy? facts) (sell? facts)))
  )

(defn evaluateRules
  "evaluates rules"
  [rules facts]
  (cond
    (empty? rules) facts
    (empty? (eval (list (first rules) facts))) (evaluateRules (rest rules) facts)
    (containsFact? (eval (list (first rules) facts)) facts) (evaluateRules (rest rules) facts)
    :else
    (cons (eval (list (first rules) facts)) (evaluateRules (rest rules) facts)))
  )

(defn inference
  "evaluates all rules as long as don't get buy or sell conclusion"
  [rules facts]
  (cond
    (done? facts) facts
    (empty? rules) facts
    (= (count facts) (count (vec (evaluateRules rules facts)))) facts
    :else
    (inference rules (vec (evaluateRules rules facts))))
  )

(defn checkFacts
  "checks if list of facts contains conclusion"
  [facts]
  (cond
    (done? facts) (if (buy? facts)
                    "buy"
                    "sell")
    :else
    "no conclusion")
  )
           

(defn as-file
  "Return whatever we have as a java.io.File object"
  [s]
  (cond (instance? File s) s ; already a file, return unchanged
        (string? s) (File. s) ; return java.io.File for path s
        :else (throw (FileNotFoundException. (str s)))))
 
(defn walk
  [^File dir]
  (let [children (.listFiles dir)
        files (filter #(.isFile %) children)]
    files))

(defn createCompaniesList
  "Creates list of companies"
  [files]
  (cond
    (empty? files) ()
    :else
    (cons (first (str/split (.getName (first files)) #"\.")) (createCompaniesList (rest files))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;
; GUI

(native!)

(def rulesList (listbox :model (for [line (with-open 
                                            [rdr (BufferedReader. (FileReader. "resources/rules.txt"))]
                                            			      (doall (line-seq rdr)))
                                     		       ]
                                 line)))

(def conclusion (text :text "Naciśnij przycisk Analizuj"
                      :editable? false))

(def explanation (listbox :model []))

(def companiesList (combobox :model (createCompaniesList (walk (as-file "resources/notowania")))))

(def sessions (combobox :model ["All" 15 30 45 60 90 150 200 250]))

(def vol (checkbox :selected? true))

(defn x
  "Creates seq with sessions dates for last k sessions"
  [k lst]
  (cond
    (= (str k) "All") (for [line lst]
                        (to-long (:date (:session line))))
    (empty? lst) ()
    (zero? k) ()
    :else
    (cons (to-long (:date (:session (first lst)))) (x (dec k) (rest lst))))
  )

(defn y
  "Creates seq with sessions close prices from last k sessions"
  [k lst]
  (cond
    (= (str k) "All") (for [line lst]
                        (:close (:session line)))
    (empty? lst) ()
    (zero? k) ()
    :else
    (cons (:close (:session (first lst))) (y (dec k) (rest lst))))
  )

(defn y-vol
  "Creates seq with sessions vol value from last k sessions"
  [k lst]
  (cond
    (= (str k) "All") (for [line lst]
                        (/ (:vol (:session line)) 1000))
    (empty? lst) ()
    (zero? k) ()
    :else
    (cons (/ (:vol (:session (first lst))) 1000) (y-vol (dec k) (rest lst))))
  )

(def graph
  (ChartPanel. (charts/time-series-plot (x (read-string (config sessions :text)) (wsk/createCompany (config companiesList :text))) 
                                        (y (read-string (config sessions :text)) (wsk/createCompany (config companiesList :text)))
                                        :title (config companiesList :text)
                                        :x-label "Czas"
                                        :y-label "Wartość [zł]"))
  )

(def graph-vol
  (ChartPanel. (charts/time-series-plot (x (read-string (config sessions :text)) (wsk/createCompany (config companiesList :text))) 
                                        (y-vol (read-string (config sessions :text)) (wsk/createCompany (config companiesList :text)))
                                        :title "Wolumen"
                                        :x-label "Czas"
                                        :y-label "Ilość [tyś.]"
                                        ))
  )

(def center-split (top-bottom-split graph graph-vol :divider-location 3/4))

(def center (grid-panel :border "Notowania"
                        :columns 1
                        :items [center-split]))

(defn drawGraphs
  "Draws graphs"
  []
  (if (selection vol)
    (config! center 
             :items [(top-bottom-split
                       (ChartPanel. (charts/time-series-plot (x (read-string (config sessions :text)) (wsk/createCompany (config companiesList :text))) 
                                                             (y (read-string (config sessions :text)) (wsk/createCompany (config companiesList :text)))
                                                             :title (config companiesList :text)
                                                             :x-label "Czas"
                                                             :y-label "Wartość [zł]"))
                       (ChartPanel. (charts/time-series-plot (x (read-string (config sessions :text)) (wsk/createCompany (config companiesList :text))) 
                                                             (y-vol (read-string (config sessions :text)) (wsk/createCompany (config companiesList :text)))
                                                             :title "Wolumen"
                                                             :x-label "Czas"
                                                             :y-label "Ilość [tyś.]"))
                       :divider-location 3/4)
                     ])
    (config! center 
             :items [(ChartPanel. (charts/time-series-plot (x (read-string (config sessions :text)) (wsk/createCompany (config companiesList :text))) 
                                                           (y (read-string (config sessions :text)) (wsk/createCompany (config companiesList :text)))
                                                           :title (config companiesList :text)
                                                           :x-label "Czas"
                                                           :y-label "Wartość [zł]"))
                     ])
    )
  )

(listen companiesList :selection (fn [e]
                                   (wsk/liczWskazniki (wsk/createCompany (config companiesList :text)))
                                   (config! conclusion :text "Naciśnij przycisk Analizuj")
                                   (config! explanation :model [])
                                   (drawGraphs)))

(listen sessions :selection (fn [e]
                              (drawGraphs)))

(listen vol :selection (fn [e]
                         (drawGraphs)))

(def inferenceBtn (button :text "Analizuj"
                          :listen [:action (fn [e] 
                                             (config! conclusion :text (checkFacts (inference (processRulesFromFile) []))))]))

(def explainBtn (button :text "Wyjaśnij"
                        :listen [:action (fn [e]
                                           (config! explanation :model (reverse (inference (processRulesFromFile) []))))]))

(def tb (toolbar :orientation :horizontal
                 :floatable? false
                 :items ["Pasek stanu"]))

(def west (grid-panel :border "Reguły"
                      :columns 1
                      :items [(scrollable rulesList)]))

(def east (grid-panel :border "Wyjaśnienie"
                      :columns 1
                      :items [(scrollable explanation)]))

(def Aktywa (grid-panel :border "Aktywa"
                        :columns 1
                        :hgap 5
                        :items [companiesList]))

(def Wykres (grid-panel :border "Wykres"
                        :columns 4
                        :hgap 5
                        :items [(label "Liczba sesji") sessions
                                (label "Wolumen?") vol]))

(def north-left (grid-panel :columns 2
                            :items [Aktywa
                                    Wykres]))

(def Wnioskowanie (grid-panel :border "Wnioskowanie"
                              :columns 2
                              :hgap 5
                              :items [inferenceBtn
                                      explainBtn]))

(def Wniosek (grid-panel :border "Wniosek"
                         :columns 1
                         :items [conclusion]))

(def north-right (grid-panel :columns 2
                             :items [Wnioskowanie
                                     Wniosek]))

(def north (grid-panel :columns 2
                       :hgap 100
                       :items [north-left
                               north-right]))

(def bp (border-panel
          ;:south tb
          :west center
          :east east
          :north north
          :center west
          :vgap 5 :hgap 5 :border 5))

(def f (frame :title "Bachelor",
               :minimum-size [1024 :by 768],
               :content bp,
               :on-close :exit
               ))

(defn -main
  "main function - loading gui"
  [& args]
  (wsk/liczWskazniki (wsk/createCompany (config companiesList :text)))
  (invoke-later
    (-> f
        pack!
        show!))
  )





















