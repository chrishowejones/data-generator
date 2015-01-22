(ns data-generator.core
  (:require [clojure.test.check.generators :as gen]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clj-time.core :as t]
            [clj-time.format :as format]
            [clojure.tools.cli :refer [parse-opts]])
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


(defn new-date []
  (let [days-to-subtract (rand-int 1000)]
    (format/unparse-local-date
     (format/formatter "yyyy-MM-dd")
     (t/minus (t/today) (t/days days-to-subtract)))))

(defn later-date [date-str]
  (let [date (format/parse (format/formatter "yyyy-MM-dd") date-str)
        interval-in-days (t/in-days (t/interval date (t/today-at-midnight)))
        days-to-add (rand-int interval-in-days)]
    (format/unparse (format/formatter "yyyy-MM-dd")
     (t/plus date (t/days days-to-add)))))

(defn buy-sell []
  (rand-nth
   ["BSBK"
    "SBBK"
    "SBSB"
    nil]))

(defn currency []
  (rand-nth
   ["GBP"
    "EUR"
    "USD"
    "HKD"
    "SAR"
    "JPY"
    "ILS"
    "CNY"
    "AUD"
    nil]))

(defn security-type []
  (rand-nth
   [
"SYNTHETIC LOC"
"SYNTHETIC REVOLVER"
"TAX-SPARED"
"TERM"
"TERM GUARANTEE FAC"
"TERM INCREMENT"
"TERM LIFO"
"TERM MULTI-DRAW"
"TERM OVERDRAFT"
"TERM REV"
"TERM TAX-SPARED"
"TERM VAT-TRNCH"
"UK GILT STOCK"
"US DOMESTIC"
"US NON-DOLLAR"
"VAT-TRNCH"
"WARRANT"
"YANKEE"
"BANKERS ACCEPTANCE"
"BASIS SWAP"
"BUTTERFLY SWAP"
"CAPS & FLOORS"
"CD"
"COMMERCIAL PAPER"
"CONTRACT FRA"
"CREDIT DEFAULT SWAP"
"CROSS"
"Currency future"
"Currency option"
"Currency spot"
nil    ]))

(defn market []
  (rand-nth
   ["XNYS"
"APXL"
"AQUA"
"AQXE"
"XNYS"
"XNYS"
"XNYS"
"ASEX"
"XASX"
"XASX"
"XASX"
"XASX"
"XASX"
"ATDF"
"DBOX"
"AWBX"
"AWEX"
"BACE"
"XBAB"
"BALT"
"BAML"
"BAPX"
"BARX"
"BARX"
"BARX"
"BCXE"
"BCXE"
"BCXE"
"BATS"
"BATS"
"BATS"
"BBSF"
"BARX"
"BCFS"
"BCMM"
"BCSE"
"BCXE"
"BEEX"
"XBER"
"XBER"
"XBER"
"BETA"
"BFEX"
"BGCI"
"BGCF"
"BGCI"
"BIDS"
"XBLB"
"CHEV"
"BLOX"
"BLPX"
"BLTD"
"BALT"
nil]))

(defn trade-type []
  (rand-nth
   ["RT"
    "ST"
    "SW"
    "UT"
    "X"
    "Y"
    "AT"
    "PA"
    "PC"
    "T"
    "WN"
    "TS"
    "WT"
    "CT"
    "AI"
    "PN"
    "VW"
    "RC"
    nil]))


(defn reversal []
  (rand-nth
   ["Y"
    "N"
    nil]))

(def header
  ["TRADE_ID" "ISIN" "CUSIP" "TRADE_DATE" "SETTLEMENT_DATE" "BUY_SELL" "QUANTITY" "GROSS_PRICE" "NET_PRICE" "CURRENCY" "ACCOUNT_ID" "PORTFOLIO_ID" "NET_AMOUNT" "MARKET" "SECURITY_TYPE" "REVERSAL" "TRADE_TYPE"])


(defn line [] (let [code-keys (rand-code-indexes)
                    trade-date (new-date)]
                (vector
                 (next-trade-id)
                 (isin-code code-keys)
                 (cusip-code code-keys)
                 trade-date
                 (later-date trade-date)
                 (buy-sell)
                 (numerics-as-str 10)
                 (decimal-as-str 10 2)
                 (decimal-as-str 10 2)
                 (currency)
                 (numerics-as-str 9)
                 (numerics-as-str 8)
                 (decimal-as-str 12 3)
                 (market)
                 (security-type)
                 (reversal)
                 (trade-type))))

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

(def cli-options
  ;; file name
  [["-f" "--filename FILENAME" "File name for output"
    :default "trade.csv"
    :validate [#(and (not (nil? %)) (not (empty? %))) "Must not be empty or nil"]]
   ["-l" "--lines LINES" "Number of lines to generate"
    :default 10
    :parse-fn #(Integer/parseInt (clojure.string/trim %))
    ]
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["This program generates a CSV of trade data."
        ""
        "Usage: java -jar data-generator.jar [options]"
        ""
        "Options:"
        options-summary]
       (clojure.string/join \newline)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (clojure.string/join \newline errors)))

(defn -main
  "Generate a file specified by file and number of lines."
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (println (usage summary))
      errors (exit 1 (error-msg errors)))
    (output-csv (options :filename) (options :lines))))
