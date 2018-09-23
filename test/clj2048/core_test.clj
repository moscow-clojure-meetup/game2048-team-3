(ns clj2048.core-test
  (:require [clojure.test :refer :all]
            [clj2048.core :refer :all]))

(deftest transpose-test
  (testing "singleton matrix"
    (is (= (transpose [[1]]) [[1]])))
  (testing "diagonal matrix"
    (let [m [[1 0]
             [0 2]]]
      (is (= (transpose m) m))))
  (testing "typical case"
    (is (= (transpose
            [[1 2 3]
             [4 5 6]
             [7 8 9]])
           [[1 4 7]
            [2 5 8]
            [3 6 9]])))
  (testing "form of output"
    (let [m (transpose [[1 2]
                        [3 4]])]
      (is (vector? m))
      (is (every? vector? m)))))

(deftest mirror-test
  (testing "singleton matrix"
    (is (= (mirror [[1]]) [[1]])))
  (testing "symmetrical matrix"
    (let [m [[1 1]
             [2 2]]]
      (is (= (mirror m) m))))
  (testing "typical case"
    (is (= (mirror
            [[1 2 3]
             [4 5 6]
             [7 8 9]])
           [[3 2 1]
            [6 5 4]
            [9 8 7]])))
  (testing "form of output"
    (let [m (mirror [[1 2]
                     [3 4]])]
      (is (vector? m))
      (is (every? vector? m)))))
