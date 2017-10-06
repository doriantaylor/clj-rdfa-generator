(ns rdfa-generator.core
  (:require
   ;;[org.httpkit.client :as http]
   [clojure.set :as set]
   [clojure.data.json :as json])
  (:import
   [org.apache.jena.riot RDFDataMgr]
   [org.apache.jena.rdf.model
    RDFNode Model ModelFactory Statement Resource Property AnonId]
   [org.apache.jena.ontology OntModel OntModelSpec OntResource OntProperty]
   )
)

;; XXX do something better than this
(def RDF_TYPE "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(def XSD "http://www.w3.org/2001/XMLSchema#")


;; we should probably have a persistent evaluation context of some kind

;; you feed this thing a subject URI and it punts out an abstract
;; structure which can then be mechanistically transformed
;; into (e.g.) (X)HTML markup.

;; things we care about (besides just punting out the immediate topology):

;; * inbound links (obvs)
;; * remapping inverse relations from inbound to outbound
;; * collating asserted predicates under the most numerous equivalentProperty
;; * blank nodes and their contents
;; * appropriate representations for lists, collections, &c
;; * Fresnel-ish processing expectations(?)

;; inbound links from bnodes should hop left until a (URI) resource node is
;; discovered.

;; should produce a JSON-LD-esque structure

;; get all the URIs of predicates for a given subject:

;; (set (map #(.getURI (.getPredicate %))
;;           (iterator-seq (.listProperties s))))

;; can also swap in .getNameSpace to get at those

(defprotocol ModelOps
  "Operations over models"
  (namespaces [model] "Retrieve map of namespaces")
  (resource [model uri] "Retrieve or generate a resource")
  (literal  [model content] [model content type] [model content lang type]
    "Retrieve or generate a literal")
  (blank    [model] [model identifier] "Retrieve or generate a blank node")
  (property [model uri] "Retrieve or generate a property")
  (subjects [model] "List all subjects")
  (size     [model] "Retrieve model size, in number of statements")
  (parse    [model src] [model src syntax] [model src base syntax]
    "Read RDF content into the model"))

(defprotocol StatementOps
  "Operations over RDF statements."
  (subject   [^Statement stmt] "Retrieve statement subject.")
  (predicate [^Statement stmt] "Retrieve statement predicate.")
  (object    [^Statement stmt] "Retrieve statement object.")
)

(defprotocol ResourceOps
  "Operations over resources"
  (model [rsrc] "Retrieve the model the resource is attached to.")
;;  (types [me] [me inferred] "Get the asserted RDF types of the resource.")
  (types [rsrc] "Get the asserted RDF types of the resource.")
  (properties
    [rsrc] [rsrc prop] "Get the properties associated with the resource.")
  (inbound-properties
    [rsrc] [rsrc prop] "Get the properties pointing to the resource.")
  (abbreviate [rsrc] "Get the foo:bar (or _:) notation for the resource.")
  (outbound-map [rsrc] [rsrc as-label]
    "Get the predicate-object map of the resource.")
  (inbound-map [rsrc] "Get the predicate-object map of the resource.")
)

(defprotocol OntModelOps
;;  OntModel
  (ontology-node  [model uri] "Retrieve a resource as an ontology")
  (ontology-class [model uri] "Retrieve a resource as a class")
)

(defprotocol OntPropertyOps
;;  OntProperty
  (inverse          [prop] "Get owl:inverseOf this property")
  (symmetric?       [prop] "Answer if property is symmetric")
  (super-properties [prop] "Get super-properties of this property")
  (sub-properties   [prop] "Get sub-properties of this property")
  (equivalents      [prop] "Get equivalents of this property")
)

(extend-type Statement
  ResourceOps
  (model     [^Statement stmt] (.getModel stmt))
  StatementOps
  (subject   [^Statement stmt] (.getSubject stmt))
  (predicate [^Statement stmt] (.getPredicate stmt))
  (object    [^Statement stmt] (.getObject stmt))
)

(extend-type Model
  ModelOps
  (namespaces [model] (into {} (.getNsPrefixMap model)))
  (resource [model uri]
    ;; TODO add polymorphism on uri
    (or (.getResource model uri) (.createResource model uri)))
  (literal
    ;; TODO add polymorphism on typed literal
    ([model content] (.createLiteral model content))
    ([model content type]
     (.createTypedLiteral model content type))
    ([model content _ lang]
     (.createLiteral model content lang)))
  (blank
    ([model] (.createResource model))
    ([model id] (let [i (if (instance? AnonId id) id (AnonId. (str id)))]
                  (.createResource model i))))
  (property [model uri]
    (if (and (instance? Resource uri) (= model (.getModel uri)))
      (if (instance? Property uri) uri (.as uri Property))
      (let [u (if (instance? Property uri) (.getURI uri) (str uri))]
        (or (.getProperty model u) (.createProperty model u)))))

  (subjects [model] (iterator-seq (.listSubjects model)))
  (size [model] (.size model))
  ;; XXX do something smarter with type checking here
  (parse
    ([model src] (RDFDataMgr/read model src))
    ([model src syntax] (RDFDataMgr/read model src syntax))
    ([model src base syntax] (RDFDataMgr/read model src base syntax)))
)

(extend-type OntModel
  ModelOps
  (resource [^OntModel model uri]
    (or (.getOntResource model uri) (.createOntResource model uri)))
  (property [^OntModel model uri]
    (if (and (instance? OntResource uri) (= model (.getModel uri)))
      (if (instance? OntProperty uri) uri (.as uri OntProperty))
      (let [u (if (instance? OntProperty uri) (.getURI uri) (str uri))]
        (or (.getOntProperty model u) (.createOntProperty model u)))))
;;  (property [model uri]
;;    (or (.getOntProperty model uri) (.createOntProperty model uri)))
  OntModelOps
  (ontology-node [^OntModel model uri]
    (or (.getOntology model uri) (.createOntology model uri)))
  (ontology-class [^OntModel model uri]
    (or (.getOntClass model uri) (.createClass model uri)))
)

(extend-type Resource
  ResourceOps
  (model [r] (.getModel r))
  (types
    ([r] (set (map object (properties r RDF_TYPE)))))
  
  (properties
    ([r] (iterator-seq (.listProperties r)))
    ([r p]
     ;; XXX redo this as a macro
     (let [m (.getModel r)
           prop (cond (instance? Property p) p
                      (instance? Resource p) (.as p Property)
                      (and m p) (property m (str p)) :else nil)]
       (when (and m p)
         (iterator-seq (.listProperties r prop))))))
  (inbound-properties
    ([r] (inbound-properties r nil))
    ([r p] 
     ;; ditto
     (let [m (.getModel r)
           prop (cond (instance? Property p) p
                      (instance? Resource p) (.as p Property)
                      (and m p) (property m (str p)) :else nil)]
       (when m (iterator-seq (.listStatements m nil prop r))))))
  (abbreviate [r]
    (if (.isAnon r)
      (str "_:" (.getLabelString (.getId r)))
      (let [m (.getModel r) u (.getURI r) q (.qnameFor m u)] (or q u))))
  (outbound-map
    ([r] (outbound-map r nil))
    ([r as-label]
     (let [props (if (nil? as-label) (properties r)
                     (filter #(or (.isLiteral (object %))
                                  (= (.getURI (predicate %)) RDF_TYPE))
                             (properties r)))]
       (apply merge-with set/union
              (map (fn [x] { (predicate x) (set [(object x)]) }) props)))))
  ;; (outbound-map [r]
  ;;   (apply merge-with set/union
  ;;          (map (fn [x] { (predicate x) (set [(object x)]) }) (properties r))))
  (inbound-map [r]
    (apply merge-with set/union
           (map (fn [x] { (predicate x) (set [(subject x)]) })
                (inbound-properties r))))
)

(extend-type OntProperty
  OntPropertyOps
  (inverse          [prop] (or (.getInverse prop) (.getInverseOf prop)))
  (symmetric?       [prop] (.isSymmetricProperty prop))
  (super-properties [prop] (iterator-seq (.listSuperProperties prop)))
  (sub-properties   [prop] (iterator-seq (.listSubProperties prop)))
  (equivalents      [prop] (iterator-seq (.listEquivalentProperties prop)))
)

(defprotocol AbstractStructure
  "A protocol for turning RDF into abstract hierarchical structures"
  (populate-ontology [me] "Scan the model and load the ontology")
  (generate          [me uri]
    "Generate the hierarchical structure for the subject URI")
  (inverse-of        [me uri] "Get the inverse of the given predicate")
  (normalized-maps   [me uri] "Produce a normalized pair of (out in) maps")
)

(defn- -create-resource [^Model model uri]
  (.createResource model uri))

(defn- -create-property [^Model model uri]
  (.createProperty model uri))

(defn- -create-ontology-property [^OntModel ontology uri]
  (let [u (if (instance? Resource uri) (.getURI uri) (str uri))]
    (or (.getOntProperty ontology uri) 
        (.createOntProperty ontology uri))))

(defn- -abbrev-maybe [^Resource r]
  (let [^Model m (.getModel r)
        u (.getURI r)]
    (if (and m u)
      (.qnameFor m u)
      (or u (str "_:" (.getId r))))))

(defn list-properties
  "List the properties of a given resource."
  ([^Resource r]
   (let [^Model m (.getModel r)]
     (if (nil? m) (seq nil)
         (iterator-seq (.listProperties r)))))
  ([^Resource r property]
   (let [^Model m (.getModel r)]
     (if (nil? m) (seq nil)
         (let [p (.createProperty m (if (instance? Resource property)
                                      (.getURI property) (str property)))]
           (iterator-seq (.listProperties r p)))))))

(defn- -types [^Resource r]
  (let [^Model m (.getModel r)
        ^Property t (when m (-create-property m RDF_TYPE))]
    (when m
      (vec (sort (map #(-abbrev-maybe (.getObject %))
                      (list-properties r t)))))))

(defn- -inverse-of [context uri]
  (let [^Model    m (:model context)
        ^OntModel o (:ontology context)
        ^OntProperty p (-create-ontology-property o uri)
        ^OntProperty i (when p (or (.getInverse p) (.getInverseOf p)))]
    (when i (.getURI i)))
)


;;(defn- -generate-one [context uri ^Resource r]
;;)

(defn- -faux-jsonld-node [^RDFNode node]
  (if (.isLiteral node)
    (let [val  (.getLexicalForm node)
          lang (.getLanguage node)
          dt   (.getDatatypeURI node)]
      (cond (and (not (nil? lang)) (> (count lang) 0))
            { "@language" lang "@value" val }
            (and (not (nil? dt))
                 (not= dt "http://www.w3.org/2001/XMLSchema#string"))
            { "@type" (abbreviate (.createResource (.getModel node) dt))
             "@value" val }
            :else val))
    (let [t (-types node) ct (count t)
          tl (cond (> ct 1) t (= ct 1) (first t) :else nil)]
      (merge { "@id" (abbreviate node) } (when tl { "@type" tl })
           (dissoc (-as-faux-jsonld (outbound-map node true)) "rdf:type")))))

(defn- -as-faux-jsonld [obj]
  (into {} (map (fn [[k v]]
                  [(abbreviate k) (let [x (map -faux-jsonld-node v)]
                                    (if (> (count x) 1) (vec x) (first x)))])
                (seq obj))))

(defn- -generate [context uri]
  (let [m (:model context)
        s (if (instance? Resource uri) uri (.getResource m uri))
        t (sort (-types s))
        [fwd rev] (normalized-maps context uri)
        -glob (merge-with set/union fwd rev)
        preds (into {} (map #(vec % (outbound-map % true)) (keys -glob)))
        nbrs  (into {} (map #(vec % (outbound-map % true))
                            (filter #(not (.isLiteral %))
                                    (apply set/union (vals -glob)))))]
    (merge {
            ;; generate context (right now only namespaces)
            "@context" (into {} (.getNsPrefixMap m))

            ;; then generate subject node (plus embedded resources
            ;; like lists)
            "@id" (abbreviate s)

            ;; then generate ordered list of (asserted) properties and
            ;; labels

            ;; then generate ordered list of (asserted) classes and labels

            ;; then generate ordered list of nearest resource nodes that are
            ;; at the head of a list/seq/bag/alt or other bnode
            ;; structure

            ;; will need some kind of register to prevent cycles

            }
           ;; then generate the @type
           (when (> (count t) 0)
             { "@type" (if (= (count t) 1) (first t) t) } )
           ;; then generate all forward nodes minus rdf:type
           (dissoc (-as-faux-jsonld fwd) "rdf:type")
           ;; then generate immediate reverse relations
           ;; (we only do reverse relations for the initial context
           ;; node)
           (when (> (count rev) 0) { "@reverse" (-as-faux-jsonld rev) }))))
           

(defn- -inx [ontology]
  (fn [k]
    (let [x (property ontology k)
          i (inverse x)
          v (if (symmetric? x) k (when i (.as i Property)))]
      ;; if it isn't clear, we're returning a vector which will be
      ;; consumed by `into`.
      (when v [k v]))))

(defn- -normalized-maps [ctx uri]
  (let [u (if (instance? Resource uri) uri (resource (:model ctx) uri))
        o (:ontology ctx) ; some coercions/shorthands
        in (inbound-map u) out (outbound-map u) ; initial materials
        inx (into {} (map (-inx o) (keys in)))]
    ;; returns a pair of maps
   (list
    ;; out
    (merge-with
     set/union out (into {} (map (fn [[x y]] [y (get in x)]) (seq inx))))
    ;; in
    (select-keys in (filter #(not (contains? inx %)) (keys in))))))

(defrecord JenaContext [^Model model ^OntModel ontology]
  AbstractStructure
  (populate-ontology [me]
    (let [ns (iterator-seq (.listNameSpaces model))]
      (doseq [uri ns]
        (try (RDFDataMgr/read ontology uri)
        ;;(try (.read ontology uri)
             (catch Exception e (println uri))))))

  (generate   [me uri] (-generate   me uri))
  (inverse-of [me uri] (-inverse-of me uri))
;;  (rdf-types  [me uri] (-types 
  (normalized-maps [me uri] (-normalized-maps me uri))
)


(defn new-context
 "Create a new serialization context"
 ([] (new-context (. ModelFactory createDefaultModel)))
 ([model] (new-context model (. ModelFactory createOntologyModel
                                (. OntModelSpec OWL_MEM_MINI_RULE_INF))))
 ([model ontology] (->JenaContext model ontology))
)
