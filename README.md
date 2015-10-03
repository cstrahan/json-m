# json-m

A Clojure library providing monadic parser combinators for JSON.

## Motivation

As someone who implements web services, I need an effective tool for
validating and parsing user input. After initially surveying the open
source landscape, it looked like
[prismatic/schema](https://github.com/Prismatic/schema) would be my best
bet. However, while `schema` looks like an excellent tool, there are
some parts of its design that I find unfortunate:

* Many concepts. It breaks things into Schemas, Transformations and
  Coercions. I'd much prefer a simpler, more principled approach
  consisting of only a few, highly composable primitives.
* Size. As of writing this, schema is over 2,600 LOC (excluding tests
  and the project file), while this library consists of less than 250 LOC.
* Awkward looking code.

By modeling the process of parsing as a monad, I can write code that looks
very close to typical Clojure while getting informative error messages
for free.

Check out the examples below to see just how easy it is to parse JSON
while getting excellent error messages when things go wrong (including
the [JSONPath](http://goessner.net/articles/JsonPath/) of the value that
failed to parse).

## Examples

```clojure
(use 'json-m.core)
(use 'clojure.algo.monads)
(use '[clojure.pprint :only (pprint)])

;; let's define some records
(defrecord HomeAddress [street])
(defrecord WorkAddress [street])
(defrecord Person [first-name last-name age address])

;; define some parsers
(with-monad json-m
  (defn parse-address [obj]
    (field-case obj :type parse-string
      "work" (domonad
               [street (field obj :street parse-string)]
               (WorkAddress. street))
      "home" (domonad
               [street (field obj :street parse-string)]
               (HomeAddress. street))))

  (defn parse-person [obj]
    (domonad
      [fname   (field obj :first parse-string)
       lname   (field obj :last parse-string)
       age     (field obj :age parse-number)
       address (field-if-present obj :address parse-address)]
      (Person. fname lname age address))))

;; let's try it out!
;; -----------------

(run-parser (parse-person { :first "Charles", :last "Strahan", :age 26 }))
;; #user.Person{:first-name "Charles", :last-name "Strahan", :age 26, :address nil}

(run-parser (parse-person { :first "Charles", :last "Strahan", :age "26" }))
;; Error in $.age: failed to parse field age: value is not a number

(run-parser (parse-person { :first "Charles", :last "Strahan" }))
;; Error in $: key age not present

(run-parser
  (parse-person
    {:first "Charles"
     :last  "Strahan"
     :age    26
     :address {:type   "work"
               :street "Main St"}}))
;; #user.Person{:first-name "Charles", :last-name "Strahan", :age 26, :address #user.WorkAddress{:street "Main St"}}

;; properly formatted person:
(def good-person
    {:first "Charles"
     :last  "Strahan"
     :age    26
     :address {:type   "work"
               :street "Main St"}})

(pprint
  (run-parser
    ((parse-vec-of parse-person)
     [good-person
      good-person])))
;; ({:first-name "Charles",
;;   :last-name "Strahan",
;;   :age 26,
;;   :address {:street "Main St"}}
;;  {:first-name "Charles",
;;   :last-name "Strahan",
;;   :age 26,
;;   :address {:street "Main St"}}

;; _improperly_ formatted person:
(def bad-person
    {:first "Charles"
     :last  "Strahan"
     :age    26
     :address {:type   "boom!" ;; <--- yikes!
               :street "Main St"}})

(run-parser
  ((parse-vec-of parse-person)
   [good-person
    bad-person])) ;; <--- uh-oh!

;; Error in $[1].address.type: failed to parse element 1: failed to parse field address: failed to parse field type: value is not one of ["work", "home"]

```

## License

Copyright © 2015 Charles Strahan

Distributed under the MIT License.
