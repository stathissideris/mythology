(ns myth.nlp
  (:require [clojure.java.io :as io])
  (:import [org.deeplearning4j.models.embeddings.loader WordVectorSerializer]))

;; https://deeplearning4j.org/api/latest/

(def model
  (WordVectorSerializer/readWord2VecModel (io/file "/Users/sideris/Downloads/glove.42B.300d.txt")))
;;"Elapsed time: 78954.211261 msecs"

(def model2
  (WordVectorSerializer/readWord2VecModel (io/file "/Users/sideris/Downloads/glove.6B/glove.6B.300d.txt")))

(.similarity model "day" "night")
(.wordsNearest model "glorious" 5)
(.wordsNearestSum model ["king" "woman"] ["man"] 2)
(.wordsNearestSum model ["paris" "greece"] ["france"] 2)
(.wordsNearestSum model ["actor" "woman"] ["man"] 2)
(.wordsNearestSum model ["minnie" "donald"] ["mickey"] 3)
