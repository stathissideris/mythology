(ns myth.simple-nlg
  (:import [simplenlg.lexicon Lexicon]
           [simplenlg.framework NLGFactory NLGElement WordElement InflectedWordElement LexicalCategory]
           [simplenlg.realiser.english Realiser]
           [simplenlg.features Feature Tense Person NumberAgreement Form InterrogativeType]))

;;https://cdn.rawgit.com/simplenlg/simplenlg/master/docs/javadoc/index.html

(def lexicon (Lexicon/getDefaultLexicon))
(def factory (NLGFactory. lexicon))
(def realiser (Realiser. lexicon))

(defn realise [x]
  (-> realiser
      (.realise x)
      (.getRealisation)))

;;TODO (past "climb up") => "climb uped"
(defn past [s]
  (realise
   (doto (InflectedWordElement. (.getWord lexicon s LexicalCategory/VERB))
     (.setFeature Feature/TENSE Tense/PAST))))

;; (defn past [s]
;;   (realise
;;    (doto (InflectedWordElement. (.getWord lexicon s LexicalCategory/VERB))
;;      (.setFeature Feature/TENSE Tense/PAST)
;;      (.setFeature Feature/PERFECT Tense/PAST))))

(defn plural [s]
  (realise
   (doto (InflectedWordElement. (.getWord lexicon s LexicalCategory/NOUN))
     (.setFeature Feature/NUMBER NumberAgreement/PLURAL))))

(defn gerund [s]
  (realise
   (doto (.createClause factory)
     (.setVerbPhrase (.createVerbPhrase factory s))
     (.setFeature Feature/FORM Form/GERUND))))
