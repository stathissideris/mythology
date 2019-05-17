(ns myth.core
  (:require [clojure.string :as str]))

;;http://wiki.c2.com/?TheOrderOfThings

(def colour
  #{"azure"
    "beige"
    "blue"
    "brown"
    "coral"
    "crimson"
    "cyan"
    "gray"
    "gold"
    "green"
    "grey"
    "indigo"
    "ivory"
    "khaki"
    "lime"
    "maroon"
    "navy"
    "olive"
    "orange"
    "orchid"
    "pink"
    "purple"
    "red"
    "silver"
    "violet"})

(def adjective
  #{"ancient"
    "benevolent"
    "vengeful"
    "all-seeing"
    "all-loving"
    "lame"
    "fear-inspiring"
    "sleeping"
    "forgotten"
    "obscure"
    "secret"
    "mysterious"
    "glorious"
    "brave"
    "non-corporeal"})

;; inspiration from https://en.wikipedia.org/wiki/List_of_Mesopotamian_deities

(def syllable
  #{"a" "ab" "zu" "an" "nam" "nan" "nin" "ki" "tia" "resh" "du" "mu" "shti" "na" "nna" "gil" "ga" "gu" "ish" "shu"
    "ru" "tum" "bu" "ne" "pa"})

(def ending-syllable
  #{"shar" "mat" "shur" "gan" "gal" "mesh" "kur" "bur"})

(def quality
  #{"stubbing your toe on the corner of the bed and shouting \"ow!\""
    "misfiled taxes"
    "well-paved roads"
    "sturdy walls"
    "warm nights"
    "fragnance-free flowers"
    "dim stars"
    "toys with wheels"
    [:part "bugs with 8 legs or" #{"less" "more"}]
    "skirmishes" ;;TODO
    [:part
     #{nil "violent" "mildly violent"}
     #{"battles" "fights" "skirmishes"}
     "between"
     #{"kittens" "puppies" "children"}]
    "imperfect snowflakes"
    "typos"
    "corners"
    "zippers"
    "hammers"
    [:part "haberdashery" [:opt #{"(excluding wigs)"
                                  "(excluding needles)"
                                  "(excluding thread)"
                                  "(excluding buttons)"
                                  "(excluding zippers)"}]]
    [:part #{"slight" "steep"} "hills"]
    [:part "birds with" colour "feathers"]
    "non-specific maladies"
    "ointment"
    "skin boils"
    [:part #{"foot" "nose" "ear" "witch"} "doctors"]
    "silent farts"
    [:part #{"yellow" "green" "white"} "snot"]
    "bottle caps"
    "short hens"
    "night strolls"
    "erotic poetry of questionable quality"
    "animals that from a long way off look like flies"})

(def god-name
  [:part syllable syllable [:opt syllable] [:opt [:opt "'"] ending-syllable]])

(defmulti render (fn [x]
                   (cond (set? x) :pick
                         (fn? x) :function
                         (sequential? x) (first x)
                         (nil? x) nil
                         :else :default)))

(defn make-god []
  (str/capitalize (str/replace (render god-name) " " "")))

(defmethod render :default [x] x)

(defmethod render nil [_] nil)

(defmethod render :function [fun] (fun))

(defn s-trim [s] (when s (str/trim s)))

(defmethod render :part [x] (-> (str/join " " (map (comp s-trim render) (remove nil? (rest x))))
                                (s-trim)
                                (str/replace " ," ",")
                                (str/replace #" +" " ")))

(defmethod render :pick [x] (render (rand-nth (seq x))))

(defmethod render :opt [x] (render #{nil (concat [:part] (rest x))}))

(def god
  [:part make-god ", the" [:opt adjective] #{"god" "godess"} "of" quality])
