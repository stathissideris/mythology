(ns myth.core
  (:require [clojure.string :as str]
            [myth.wordnet :as wn]
            [myth.stanford-nlp :as st])
  (:import [edu.stanford.nlp.simple Document Sentence]))

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

(defn enrich [pos s]
  (set (mapcat #(let [w (try (wn/get-word wn/dict pos %) (catch Exception _ nil))]
                  (concat [%] (when w (wn/get-synonym-lemmas w)))) s)))

(def adjective
  (->> #{"ancient"
         "benevolent"
         "vengeful"
         "vindictive"
         "forgiving"
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
         "non-corporeal"}
       (enrich :adjective)
       (enrich :adjective)))

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
    [:part "individually packaged" #{"tea bags"
                                     "biscuits"
                                     "wet wipes"
                                     "donuts"
                                     "pickles"}]
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
    [:part #{"silent" "runny"} "farts"]
    [:part #{nil "yellow" "green" "white" "black"} #{"snot" "bile" "phlegm" "blood"}]
    "bottle caps"
    "short hens"
    "night strolls"
    "erotic poetry of questionable quality"
    "animals that from a long way off look like flies"
    [:part "unspent" #{"dimes" "change"}]})

(def god-name
  [:part syllable syllable [:opt syllable] [:opt [:opt "'"] ending-syllable]])

(def god
  [:part [:capitalize [:collapse god-name]] ", the" adjective #{"god" "goddess" "diety"} "of" quality])

;; engine

(defmulti render (fn [x]
                   (cond (set? x) :pick
                         (fn? x) :function
                         (sequential? x) (first x)
                         (nil? x) nil
                         :else :default)))

(defmethod render :default [x] x)

(defmethod render nil [_] nil)

(defmethod render :function [fun] (fun))

(defmethod render :capitalize [x]
  (str/capitalize (render (concat [:part] (rest x)))))

(defmethod render :collapse [x]
  (str/replace (render (concat [:part] (rest x))) " " ""))

(defn s-trim [s] (when s (str/trim s)))

(defmethod render :part [x] (-> (str/join " " (map (comp s-trim render) (remove nil? (rest x))))
                                (s-trim)
                                (str/replace " ," ",")
                                (str/replace #" +" " ")))

(defmethod render :pick [x] (render (rand-nth (seq x))))

(defmethod render :opt [x] (render #{nil (concat [:part] (rest x))}))


;;; usage:

;; (render god)

(def dd
  (st/document
   "Total orders mainly in CL and mainly from Natural Search rose
   by 739% to 1.4K, and surpassed the expected interval.

   The increase was evident for users mostly for audio,
   tv-home-cinema and cameras product categories, from PC, Mobile and
   Tablet devices. The trend was spread across pages.

   This was recorded in parallel with an expected variation in page
   views, entries, review clicks, revenue and exit rate in the
   overall site."))

(-> dd st/sentences first st/tag-pos)

(defn synonymize-word [s]
  (try
    (rand-nth (drop 1 (wn/get-synonym-lemmas (wn/get-word wn/dict :verb s))))
    (catch Exception _ s)))

(defn synonymize-text [s]
  (->> s
       split-words
       (map synonymize-word)
       (str/join " ")))
