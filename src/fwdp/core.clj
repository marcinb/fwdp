(ns fwdp.core
  (:require [clojure.string :as string]))

(def filename "database.csv")

;; Parsing

(defn str->int [str]
  (Integer. str))

(defn headers->symbols [headers]
  (map (fn [header]
         (get {"Name" :name "Glitter Index" :glitter-index} header))
       headers))

(def conversions
  {:name identity :glitter-index str->int})

(defn convert-row-value [header value]
  ((get conversions header) value))

(defn parse [contents]
  (map (fn [str]
         (string/split str #","))
       (string/split contents #"\n")))

(defn zip [keys values]
  (map (fn [k, v]
         {k v})
       keys values))

(defn mapify [data]
  (let [headers (headers->symbols (first data))
        rows (rest data)]
    (map (fn [row]
           (into {} (zip headers (map convert-row-value headers row))))
         rows)))

;; Filtering

(defn glitter-filter [max-glitter suspects]
  (filter (fn [suspect]
            (> (get suspect :glitter-index) max-glitter))
          suspects))

(defn suspect-names [suspects]
  (map (fn [suspect] (get suspect :name)) suspects))

;; Validations

(def validations
  {:name present? :glitter-index non-negative-number?})

(def present? (complement empty?))

(defn non-negative-number? [n]
  (or (> n 0) (= n 0)))

(defn invalid-field? [field-name value]
  (not ((get validations field-name) value)))

(defn invalid-record? [record]
  (some (fn [field]
         (let [field-name (first field)]
           (invalid-field? field-name (get record field-name))))
        record))

(def valid-record? (complement invalid-record?))

;; Convenience

(defn read-suspects [filename]
  (filter valid-record?
          (mapify
            (parse
              (slurp filename)))))
(suspect-names
  (glitter-filter
    3 (read-suspects filename)))

