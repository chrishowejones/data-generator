(ns data-generator.core
  (:require [clojure.test.check.generators :as gen]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:gen-class))

(def number-of-codes 1000)

(defn alphas [len]
  (.toUpperCase (apply str (gen/sample gen/char-alpha len))))

(defn numerics-as-str [len]
  (apply str (gen/sample gen/pos-int len)))

(defn alphabetic-as-str [len]
  (apply str (gen/sample gen/char-alphanumeric len)))

(defn decimal-as-str [scale precision]
  (let [int-digits (- scale precision)]
    (do
      (str (apply str (gen/sample gen/pos-int int-digits)) "." (apply str (gen/sample gen/pos-int precision))))))

(defn isin []
  (apply str (alphas 2) (numerics-as-str 10)))

(defn cusip []
  (alphabetic-as-str 9))

(defn sedol []
  (numerics-as-str 9))

(def isins
  (take number-of-codes
        (repeatedly isin)))

(def cusips
  (take number-of-codes
        (repeatedly cusip)))

(def sedols
  (take number-of-codes
        (repeatedly sedol)))

(def code-types [:cusip :sedol :isin] )

(defn rand-code-indexes []
  (set (repeatedly 3 #(get code-types (rand-int 3)))))

(defn security-code [key]
  (condp = key
    :isin (rand-nth isins)
    :cusip (rand-nth cusips)
    :sedol (rand-nth sedols)
    nil))


(defn code-index []
 (let [codes (rand-code-indexes)]
   (vec (concat (repeat (- 3 (count codes)) nil) codes))))

(defn code-values []
  {:isin (rand-nth isins)
   :cusip (rand-nth cusips)
   :sedol "sedol"})

(defn isin-code [set-of-code-types]
  (if (set-of-code-types :isin) (rand-nth isins) nil))

(defn cusip-code [set-of-code-types]
  (if (set-of-code-types :cusip) (rand-nth cusips) nil))

(defn sedol-code [set-of-code-types]
  (if (set-of-code-types :sedol) (rand-nth sedols) nil))

(defn trade-ids [start-num]
  (let [trade-id (atom start-num)]
    #(swap! trade-id inc)))

(def next-trade-id (trade-ids 1199))

(def header
  ["tradeid" "isin" "cusip" "sedol" "text" "number"])

(defn line [] (let [code-keys (rand-code-indexes)]
                (vector
                 (next-trade-id)
                 (isin-code code-keys)
                 (cusip-code code-keys)
                 (sedol-code code-keys)
                 (alphas (rand-int 101))
                 (numerics-as-str 10))))

(def lines (repeatedly line))

(defn output-csv
  ([filename] (output-csv filename 10))
  ([filename num-lines]
   (with-open [out-file (io/writer filename)]
     (csv/write-csv out-file
                    (cons header
                          (take num-lines lines))
                    :separator \,
                    :quote \"
                    :newline :lf))))


(defn -main
  "Generate a file specified by file and number of lines."
  [& args]
  (if (empty? args) (println "Usage: -f filename -l number of lines" )))
