(ns data-generator.core-test
  (:require [midje.sweet :refer :all]
            [data-generator.core :refer :all]))

(defn element-contained-in [expected-elements]
        (fn [actual]
          (some (hash-set actual) expected-elements)))

(fact "isin returns a random isin from isins when :isin is in code types collection"
      (let [dummy-code-types #{:isin nil}]
        (isin-code dummy-code-types) => (element-contained-in isins)))

(fact "isin returns nil when :isin is NOT in code types collection"
      (let [dummy-code-types #{:cusip nil}]
        (isin-code dummy-code-types) => nil?))

(fact "-main with -h switch outputs usage information."
      (with-out-str
        (-main "-h")) => #"Usage: java -jar data-generator\.jar \[options\]"
      (with-out-str
        (-main "-h")) => #"-f, --filename FILENAME"
      (with-out-str
        (-main "-h")) => #"-l, --lines LINES")
