(ns myth.wordnet
  (:import [net.sf.extjwnl.dictionary Dictionary]
           [net.sf.extjwnl.data POS PointerUtils]))

(def map-pos {:noun      POS/NOUN
              :verb      POS/VERB
              :adjective POS/ADJECTIVE
              :adverb    POS/ADVERB})

;; http://extjwnl.sourceforge.net/javadocs/index.html

(def dict (Dictionary/getDefaultResourceInstance))

;; (def accomplish (.getIndexWord d POS/VERB "accomplish"))
;;
;; (-> accomplish .getSenses first (PointerUtils/getDirectHypernyms) .print)
;;
;; (def dog (.getIndexWord dict POS/NOUN "dog"))
;; (-> dog .getSenses first PointerUtils/getDirectHypernyms .print)
;; (-> dog .getSenses first (PointerUtils/getHyponymTree) .print)
;;
;; (.lookupIndexWord dict POS/VERB "running-away")
;; => #object[net.sf.extjwnl.data.IndexWord 0x39cc1973 "[IndexWord: [Lemma: run away] [POS: verb]]"]

(defn get-word [dict pos s]
  (or (.getIndexWord dict (map-pos pos) s)
      (throw (ex-info "Word not found" {:pos  pos
                                        :word s}))))

(defn get-noun [dict s] (get-word dict :noun s))
(defn get-adjective [dict s] (get-word dict :adjective s))
(defn senses [word] (seq (.getSenses word)))
(defn lemma [word] (.getLemma word))
(defn definitions [word]
  (->> word senses (map #(.getGloss %))))

;; holonym: A term that denotes a whole whose part is denoted by
;; another term, such as 'face' in relation to 'eye'.
;;
;; (-> (noun d "eye") senses first PointerUtils/getHolonyms seq) ;; visual system, face
;;
;; (-> (adjective d "ancient") senses first PointerUtils/getSynonyms seq)
;; (-> (adjective d "mysterious") senses first PointerUtils/getSynonyms seq)

(defn get-synonym-lemmas
  ([word]
   (get-synonym-lemmas word Integer/MAX_VALUE))
  ([word factor]
   (let [ss (take factor (senses word))]
     (distinct
      (apply concat
             (for [sense ss]
               (->> sense .getWords (map lemma))))))))
