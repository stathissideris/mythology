(ns myth.util
  (:require [clojure.string :as str]))

(defn format-like [word other]
  (if (Character/isUpperCase (first other))
    (str/capitalize word)
    word))
