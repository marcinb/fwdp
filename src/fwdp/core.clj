(ns fwdp.core
  (:require [clojure.string :as string]))

(def filename "database.csv")

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

(defn glitter-filter [max-glitter suspects]
  (filter (fn [suspect]
            (> (get suspect :glitter-index) max-glitter))
          suspects))

(glitter-filter 3
  (mapify (parse (slurp filename))))
