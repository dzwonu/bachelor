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
     WSK = #'[A-Z]+[0-9]*'
     FACT = #'[a-z]+[0-9]*'
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
    (done? facts) (if (buy? facts)
                    "buy"
                    "sell")
    (empty? rules) "no conclusion"
    (= (count facts) (count (vec (evaluateRules rules facts)))) "no conclusion"
    :else
    (inference rules (vec (evaluateRules rules facts))))
  )

(defn as-file [s]
  "Return whatever we have as a java.io.File object"
  (cond (instance? File s) s ; already a file, return unchanged
        (string? s) (File. s) ; return java.io.File for path s
        :else (throw (FileNotFoundException. (str s)))))
 
(defn walk [^File dir]
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
                      :columns 17
                      :editable? false))

(def companiesList (combobox :model (createCompaniesList (walk (as-file "resources/notowania")))))

(defn x
  "Creates seq with sessions dates"
  []
  (for [line (wsk/createCompany (config companiesList :text))]
    (to-long (:date (:session line))))
  )

(defn y
  "Creates seq with sessions close prices"
  []
  (for [line (wsk/createCompany (config companiesList :text))]
    (:close (:session line)))
  )

(defn y-vol
  "Creates seq with sessions vol value:"
  []
  (for [line (wsk/createCompany (config companiesList :text))]
    (:vol (:session line)))
  )

(def graph
  (ChartPanel. (charts/time-series-plot (x) (y)
                                        :title (config companiesList :text)
                                        :x-label "Czas"
                                        :y-label "Wartość"))
  )

(def graph-vol
  (ChartPanel. (charts/time-series-plot (x) (y-vol)
                                        :title "Wolumen"
                                        ))
  )

(def inferenceBtn (button :text "Analizuj"
                          :listen [:action (fn [e] 
                                               (config! conclusion :text (inference (processRulesFromFile) [])))]))

(def explainBtn (button :text "Wyjaśnij"))

(def explanation (text :multi-line? true
                       :font "MONOSPACED-PLAIN-14"
                       :text "Wyjaśnienie \nprocesu \nwnioskowania"))

(def tb (toolbar :orientation :horizontal
                 :floatable? false
                 :items ["Pasek stanu"]))

(def west (grid-panel :border "Reguły"
                      :columns 1
                      :items [(scrollable rulesList)]))

(def east (grid-panel :border "Wyjaśnienie"
                      :columns 1
                      :items [(scrollable explanation)]))

(def center (grid-panel :border "Notowania"
                        :columns 1
                        :items [graph
                                graph-vol]))

(def loadBtn (button :text "Załaduj notowania"
                     :listen [:action (fn [e] 
                                        (wsk/liczWskazniki (wsk/createCompany (config companiesList :text)))
                                        (config! center 
                                                 :items [(ChartPanel. (charts/time-series-plot (x) (y)
                                                                                               :title (config companiesList :text)
                                                                                               :x-label "Czas"
                                                                                               :y-label "Wartość"))
                                                         (ChartPanel. (charts/time-series-plot (x) (y-vol)
                                                                                               :title "Wolumen"))
                                                                                               ]))]))

(def north-left (grid-panel :border "Aktywa"
                       :columns 2
                       :hgap 5
                       :items [companiesList
                               loadBtn]))

(def north-center (grid-panel :border "Wnioskowanie"
                       :columns 2
                       :hgap 5
                       :items [inferenceBtn
                               explainBtn]))

(def north-right (grid-panel :border "Wniosek"
                             :columns 1
                             :items [conclusion]))

(def north (flow-panel :align :left
                       :hgap 30
                       :items [north-left
                               north-center
                               north-right]))

(def bp (border-panel
          :south tb
          :west west
          :east east
          :north north
          :center center
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





















