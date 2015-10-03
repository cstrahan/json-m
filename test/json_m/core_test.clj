(ns json-m.core-test
  (:require [clojure.test :refer :all]
            [clojure.algo.monads :refer :all]
            [json-m.core :refer :all]))

;-------------------------------------------------------------------------------
; util fns

(with-monad json-m
  (def return m-result)
  (def bind   m-bind)
  (def mplus  m-plus))

(defn- str->re [str]
  (re-pattern (java.util.regex.Pattern/quote str)))

;-------------------------------------------------------------------------------
; dummy data types

(defrecord Address [street])
(defrecord Person [first-name last-name age address])

(defn parse-address [obj]
  (domonad json-m
    [street (field obj :street parse-string)]
    (Address. street)))

(defn parse-person [obj]
  (domonad json-m
    [fname   (field obj :first parse-string)
     lname   (field obj :last parse-string)
     age     (field obj :age parse-number)
     address (if-contains field obj :address parse-address)]
    (Person. fname lname age address)))

;-------------------------------------------------------------------------------
; tests

(deftest parser-monad
  (testing "it works"
    (is (= "baz"
           (run-parser ((parse-one-of ["foo" "bar" "baz"])
                        "baz"))))
    (is (= :bar
           (run-parser ((parse-case
                          "foo" (return :foo)
                          "bar" (return :bar)
                          "baz" (return :baz))
                        "bar"))))
    (is (= :quux
           (run-parser ((parse-case
                          "foo" (return :foo)
                          "bar" (return :bar)
                          "baz" (return :baz)
                          (return :quux))
                        "boom!"))))
    (is (= "test"
           (run-parser (elem ["test"] 0 parse-string))))
    (is (= "test"
           (run-parser (field {:a "test"} :a parse-string))))
    (is (= 1
           (run-parser (field {:a 1.0} :a parse-integral))))
    (is (= 1.0
           (run-parser (field {:a 1.0} :a parse-number))))
    (is (= {:b 123}
           (run-parser (field {:a {:b 123}} :a parse-map))))
    (is (= [3 2 1]
           (run-parser (if-success (return [1 2 3])
                                   #(return (reverse %))
                                   (return :xyz)))))
    (is (= :xyz
           (run-parser (if-success (fail "oops!")
                                   #(return (reverse %))
                                   (return :xyz)))))
    (is (= :abc
           (run-parser (mplus (fail   "failure 1")
                              (fail   "failure 2")
                              (return :abc)
                              (fail   "failure 3")))))
    (is (= (Person. "Charles" "Strahan" 26 nil)
           (run-parser
             (parse-person {:first "Charles"
                            :last "Strahan"
                            :age   26}))))
    (is (= (Person. "Charles" "Strahan" 26 (Address. "Main St"))
           (run-parser
             (parse-person {:first "Charles"
                            :last "Strahan"
                            :age   26
                            :address {:street "Main St"}}))))
    (is (= ["1" "2" "3"]
           (run-parser ((parse-vec-of #(return (str %)))
                        [1 2 3]))))
    (is (= [0 1 2]
           (run-parser ((parse-vec-of-indexed (fn [idx itm] (return idx)))
                        [:x :y :z]))))
    (is (thrown-with-msg? Exception (str->re "Error in $: value is not one of [\"foo\", \"bar\", \"baz\"]")
           (run-parser ((parse-case
                          "foo" (return :foo)
                          "bar" (return :bar)
                          "baz" (return :baz))
                        "boom!"))))
    (is (thrown-with-msg? Exception (str->re "Error in $: boom!")
           (run-parser (if-success (return :yay)
                                   (constantly (fail "boom!"))
                                   (return :nay)))))
    (is (thrown-with-msg? Exception (str->re "Error in $: value is not one of [\"foo\", \"bar\", \"baz\"]")
           (run-parser ((parse-one-of ["foo" "bar" "baz"])
                        "boom!"))))
    (is (thrown-with-msg? Exception (str->re "Error in $: value is not an object")
          (run-parser
            (field 123 :blah parse-string))))
    (is (thrown-with-msg? Exception (str->re "Error in $.a[0]: failed to parse field a: failed to parse element 0: boom!")
          (run-parser
            (field {:a [1 2 3]}
                    :a
                    (parse-vec-of (constantly (fail "boom!")))))))
    (is (thrown-with-msg? Exception (str->re "Error in $.age: failed to parse field age: value is not a number")
          (run-parser
            (parse-person {:first "Charles"
                           :last "Strahan"
                           :age  "26"}))))
    (is (thrown-with-msg? Exception (str->re "Error in $.address: failed to parse field address: value is not an object")
          (run-parser
            (parse-person {:first   "Charles"
                           :last    "Strahan"
                           :age     26
                           :address "Main St"}))))
    (is (thrown-with-msg? Exception (str->re "Error in $.a.b.c: failed to parse field a: failed to parse field b: failed to parse field c: value is not a string")
          (run-parser
            (field {:a {:b {:c 123}}} :a
              (fn [a]
                (field a :b
                  (fn [b]
                    (field b :c parse-string))))))))))
