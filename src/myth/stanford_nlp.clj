(ns myth.stanford-nlp
  (:import [edu.stanford.nlp.simple Document Sentence]))

(def friendly-tags
  "Map of core-nlp classes to sane names.
  http://www.clips.ua.ac.be/pages/mbsp-tags

  and

  https://gist.github.com/nlothian/9240750"
  {"ADJP"   #{:adjective-phrase}
   "ADVP"   #{:adverb-phrase}
   "SBAR"   #{:subordinating-conjunction}
   "WHADVP" #{:wh-adverb-phrase}
   "CC"     #{:coordinating-conjunction}
   "CD"     #{:cardinal-number}
   "DT"     #{:determiner}
   "EX"     #{:existential-there}
   "FW"     #{:foreign-word}
   "IN"     #{:preposition-or-subordinating-conjunction}
   "JJ"     #{:adjective}
   "JJR"    #{:adjective :comparative}
   "JJS"    #{:adjective :superlative}
   "LS"     #{:list-item-marker}
   "MD"     #{:modal}
   "NN"     #{:noun :singular-or-mass}
   "NNS"    #{:noun :plural}
   "NP"     #{:noun :proper :singular}
   "NNP"    #{:noun :proper :singular} ; Renamed in Penn POS
   "NPS"    #{:noun :proper :plural}
   "NNPS"   #{:noun :proper :plural} ; Renamed
   "PDT"    #{:predeterminer}
   "POS"    #{:possessive-ending}
   "PP"     #{:pronoun :personal}
   "PRP"    #{:pronoun :personal} ; renamed
   "PP$"    #{:pronoun :possessive}
   "PRP$"   #{:pronoun :possessive} ; renamed
   "RB"     #{:adverb}
   "RBR"    #{:adverb :adverb-comparative}
   "RBS"    #{:adverb :adverb-superlative}
   "ROOT"   #{:root}
   "RP"     #{:particle}
   "SYM"    #{:symbol}
   "S"      #{:s} ; ???
   "SINV"   #{:inverted-declarative-sentence}
   "INTJ"   #{:interjection}
   "TO"     #{:to}
   "UH"     #{:interjection}
   "VB"     #{:verb :base-form}
   "VBD"    #{:verb :past-tense}
   "VBG"    #{:verb :gerund :gerund-or-present-participle}
   "VBN"    #{:verb :past-participle}
   "VBP"    #{:verb :non-3rd-person-singular-present}
   "VP"     #{:verb :phrase}
   "VBZ"    #{:verb :3rd-person-singular-present}
   "WDT"    #{:wh-determiner}
   "WP"     #{:wh-pronoun}
   "WP$"    #{:possessive-wh-pronoun}
   "WRB"    #{:wh-adverb}
   ","      #{:comma}
   "."      #{:full-stop}})

(defn plural? [{{:keys [plural proper noun]} :pos}]
  (and plural noun (not proper)))

(defn tag-pos [sentence]
  (map vector
       (.words sentence)
       (->> sentence .posTags (map friendly-tags))))

(defn document [s] (Document. s))

(defn sentences [^Document document]
  (.sentences document))

(defprotocol StanfordNLPEntity
  (tag [this opts]))

(defn- tags [sentence kw fun]
  (map (fn [t] {kw t}) (fun sentence)))

(extend-protocol StanfordNLPEntity
  String
  (tag [this opts]
    (tag (document this) opts))

  Document
  (tag [this opts]
    {:type      :document
     :sentences (map #(tag % opts) (.sentences this))})

  Sentence
  (tag [this {:keys [pos lemma ner]}]
    (let [words (map (fn [w] {:type :word :text w}) (.words this))]
      {:type :sentence
       :words (cond-> words
                pos   (#(->> (map merge % (tags this :pos (memfn posTags)))
                             (map (fn [x] (update x :pos friendly-tags)))
                             (map (fn [{:keys [text] :as x}]
                                    (if (= ":" text) (assoc x :pos #{:semi-colon}) x)))))
                lemma (#(map merge % (tags this :lemma (memfn lemmas))))
                ner   (#(->> (map merge % (tags this :ner (memfn nerTags)))
                             (map (fn [x] (if (= "O" (:ner x)) (dissoc x :ner) x))))))})))
