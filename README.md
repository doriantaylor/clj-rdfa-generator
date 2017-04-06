# rdfa-generator

DOES WHAT IT SAYS ON THE TIN!

## Usage

```
;; load the damn thing
(require '[rdfa-generator.core :as r])

;; fire up a context
(def ctx (r/new-context))

;; load some rdf data
(r/parse (:model ctx) "some:/uri/to/some/rdf.n3")

;; now load all the vocabs found in the data
(r/populate-ontology ctx)

;; now you can generate some crap that looks like json-ld
(r/generate ctx "some://uri/to/some/resource")

;; and now i need to write some more code
```

## License

Copyright © 2016 Dorian Taylor

Distributed under
the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0).
