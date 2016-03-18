(ns markov-chains.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn remove-punctuation
  [text]
  (str/replace text #"[^A-Za-z0-9\s]" ""))

(defn generate-trigrams
  [tokens]
  (letfn [(splitter [list result]
                     (if (= (count list) 3)
                       (conj result list)
                       (let [elements (take 3 list)
                             new-result (conj result elements)]
                         (recur (rest list) new-result))))]
    (splitter tokens [])))

(defn generate-aggregation
  [trigrams]
  (reduce (fn [agg t]
            (merge-with set/union
                        agg
                        {(take 2 t) (drop 2 t)})) {} trigrams))

(defn generate-nonsense
  [text]
  (let [clean-text (-> text remove-punctuation str/lower-case)
        tokens (str/split clean-text #"\s")
        length (count tokens)
        trigrams (generate-trigrams tokens)
        aggregation (generate-aggregation trigrams)
        words []
        key (-> aggregation keys rand-nth)]
    (loop [w words
           k key]
      (if (< (count w) length)
        (let [new-words (-> w
                            (concat k)
                            (conj (-> aggregation (get k) rand-nth)))]
          (recur new-words (take-last 2 new-words)))
        w))))

(defn -main
  [& args]
  (let [text (slurp (io/file (io/resource (first args))))]
    (-> (generate-nonsense text)
        (str/join " ")
        println)))


