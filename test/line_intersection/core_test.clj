(ns line-intersection.core-test
  (:require [clojure.test :refer :all]
            [line-intersection.core :refer :all]))

(deftest test-line-segment-intersection
  (let [arr0 ["(9,-2)","(-2,9)","(3,4)","(10,11)"]
        arr1 ["(3,0)","(1,4)","(0,-3)","(2,3)"]
        arr2 ["(1,2)","(3,4)","(5,6)","(7,8)"]
        arr3 ["(-1,4)","(2,-5)","(5,6)","(7,8)"]
        arr4 ["(0,0)","(2,0)","(0,-2)","(2,2)"]
        arr5 ["(2,0)","(0,5)","(0,3)","(3,3)"]
        arr6 ["(0,0)","(0,5)","(0,3)","(6,3)"]
        arr7 ["(0,0)","(2,2)","(1,1)","(0,2)"]
        arr8 ["(0,-1)","(0,1)","(1,0)","(-1,0)"]
        arr9 ["(-1,-1)","(1,1)","(1,-1)","(-1,1)"]
        arr10 ["(0,0)","(2,0)","(1,0)","(2,1)"]
        arr11 ["(0,-2)","(0,2)","(-1,1)","(2,1)"]
        arr12 ["(0,2)","(2,5)","(-1,0)", "(2,3)"]
        arr13 ["(0,1)","(3,0)","(9,-5)","(0,-5)"]
        arr14 ["(4,4)","(10,4)","(4,0)","(10,6)"]
        arr15 ["(0,-2)","(4,10)","(2,-2)","(0,4)"]
        arr16 ["(10000,500)","(10000,5000)","(3000,2000)","(17532,2000)"]
        arr17 ["(0,200)","(600,200)","(0,-100)","(400,200)"]
        arr18 ["(-200,400)","(600,-800)","(200,-600)","(800,-300)"]
        arr19 ["(60,-224)","(62,-228)","(58,-226)","(64,-226)"]
        arr20 ["(2,0)","(6,-8)","(3,-5)","(5,-4)"]
        arr21 ["(-3,-1)","(-4,-5)","(-4,-2)","(-2,-4)"]
        arr22 ["(-20,-20)","(-120,-80)","(-110,-60)","(-50,-80)"]
        arr23 ["(0,15)","(3,-12)","(2,1)","(13,7)"]
        arr24 ["(6,12)","(7,14)","(-100,-3)","(-3,-5)"]]

    (testing "line-segment-intersection"
      (is (= (line-segment-intersection arr0) "(3,4)"))
      (is (= (line-segment-intersection arr1) "(9/5,12/5)"))
      (is (= (line-segment-intersection arr2) "no intersection"))
      (is (= (line-segment-intersection arr3) "no intersection"))
      (is (= (line-segment-intersection arr4) "(1,0)"))
      (is (= (line-segment-intersection arr5) "(4/5,3)"))
      (is (= (line-segment-intersection arr6) "(0,3)"))
      (is (= (line-segment-intersection arr7) "(1,1)"))
      (is (= (line-segment-intersection arr8) "(0,0)"))
      (is (= (line-segment-intersection arr9) "(0,0)"))
      (is (= (line-segment-intersection arr10) "(1,0)"))
      (is (= (line-segment-intersection arr11) "(0,1)"))
      (is (= (line-segment-intersection arr12) "no intersection"))
      (is (= (line-segment-intersection arr13) "no intersection"))
      (is (= (line-segment-intersection arr14) "(8,4)"))
      (is (= (line-segment-intersection arr15) "(1,1)"))
      (is (= (line-segment-intersection arr16) "(10000,2000)"))
      (is (= (line-segment-intersection arr17) "(400,200)"))
      (is (= (line-segment-intersection arr18) "(400,-500)"))
      (is (= (line-segment-intersection arr19) "(61,-226)"))
      (is (= (line-segment-intersection arr20) "(21/5,-22/5)"))
      (is (= (line-segment-intersection arr21) "(-17/5,-13/5)"))
      (is (= (line-segment-intersection arr22) "(-95,-65)"))
      (is (= (line-segment-intersection arr23) "no intersection"))
      (is (= (line-segment-intersection arr24) "no intersection")))))