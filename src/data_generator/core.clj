(ns data-generator.core
  (:require [clojure.test.check.generators :as gen]
            [clojure.java.io :as io]
            [clj-time.core :as t]
            [clj-time.format :as format]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def number-of-codes 1000)

(def error-rate-percentage 0.001)

(defn my-proxy-with [this]
  (let [metadata (atom {})]
    (proxy [Object clojure.lang.IObj] []
      (withMeta [new-meta] (do
                             (swap! metadata merge-with new-meta)
                             this))
      (meta [] @metadata)
      (toString [] (.toString this)))))

(def proxied-date
    (proxy [java.util.Date] []
        (toString [] "My date")))


(defn alphas
  {:return-type :alpha}
  [len]
  (.toUpperCase (apply str (gen/sample gen/char-alpha len))))

(defn- -rand-number [len]
  (apply str
   (take len
         (repeatedly #(rand-nth [1 2 3 4 5 6 7 8 9 0])))))

(defn numerics-as-str
  {:return-type :numeric}
  [len]
  (-rand-number len))

(defn alphabetic-as-str [len]
  (apply str (gen/sample gen/char-alphanumeric len)))

(defn decimal-as-str [scale precision]
  (let [int-digits (- scale precision)]
    (do
      (str (apply str (gen/sample gen/pos-int int-digits)) "." (apply str (gen/sample gen/pos-int precision))))))

(defn error-alphas [alpha-value]
  (str/replace alpha-value
                          (str (get alpha-value (rand-int (count alpha-value))))
                          (-rand-number 1)))

(defn error-numerics [numeric-value]
  (str/replace numeric-value
                          (str (get numeric-value (rand-int (count numeric-value))))
                          (alphas 1)))

(defn new-date
  {:return-type :date}
  []
  (let [days-to-subtract (rand-int 1000)]
    (format/unparse-local-date
     (format/formatter "yyyy-MM-dd")
     (t/minus (t/today) (t/days days-to-subtract)))))

(defn later-date
  {:return-type :date}
  [date-str]
  (let [date (format/parse (format/formatter "yyyy-MM-dd") date-str)
        interval-in-days (t/in-days (t/interval date (t/today-at-midnight)))
        days-to-add (rand-int interval-in-days)]
    (format/unparse (format/formatter "yyyy-MM-dd")
     (t/plus date (t/days days-to-add)))))

(defmacro error-value
  "Generate error values for function if function has :return-type of :alpha :numeric or :date"
  [func]
  (let [return-type (get
          (meta
           (resolve (first func))) :return-type :not-found)]
    (condp = return-type
      :alpha `(error-alphas ~func)
      :numeric `(error-numerics ~func)
      :date `(error-numerics ~func)
      :not-found func)))


(defn isin []
  (apply str (alphas 2) (numerics-as-str 10)))

(defn cusip []
  (alphabetic-as-str 9))

(defn sedol []
  (numerics-as-str 9))

(defonce isins
  (take number-of-codes
        (repeatedly isin)))

(defonce cusips
  (take number-of-codes
        (repeatedly cusip)))

(defonce sedols
  (take number-of-codes
        (repeatedly sedol)))

(def code-types [:cusip :sedol :isin] )

(defn rand-code-indexes []
  (set (repeatedly 3 #(get code-types (rand-int 3)))))

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


(defn buy-sell []
  (rand-nth
   ["BSBK"
    "SBBK"
    "SBSB"]))

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
    "AUD"]))

(defn security-type []
  (rand-nth [
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
"Currency spot"]))

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
"BALT"]))

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
    "RC"]))


(defn reversal []
  (rand-nth
   ["Y"
    "N"]))


(def header
  [["TRADE_ID" "ISIN" "CUSIP" "TRADE_DATE" "SETTLEMENT_DATE" "BUY_SELL" "QUANTITY" "GROSS_PRICE" "NET_PRICE" "CURRENCY" "ACCOUNT_ID" "PORTFOLIO_ID" "NET_AMOUNT" "MARKET" "SECURITY_TYPE" "REVERSAL" "TRADE_TYPE"]])

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

(defn lines [] (repeatedly line))

(defn csv-line [l]
  (str (str/join "," l) "\n"))

(defn output-csv
  ([filename] (output-csv filename 10))
  ([filename num-lines]
   (with-open [out-file (io/writer filename)]
     (.write out-file (csv-line (first header)))
     (doseq [l (take num-lines (lines))]
       (.write out-file
               (csv-line l))))))

(def cli-options
  ;; file name
  [["-f" "--filename FILENAME" "File name for output"
    :default "trade.csv"
    :validate [#(and (not (nil? %)) (not (empty? %))) "Must not be empty or nil"]]
   ["-l" "--lines LINES" "Number of lines to generate"
    :default 10
    :parse-fn #(Integer/parseInt (str/trim %))
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
       (str/join \newline)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn -main
  "Generate a file specified by file and number of lines."
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (println (usage summary))
      errors (exit 1 (error-msg errors)))
    (time (output-csv (options :filename) (options :lines)))))
