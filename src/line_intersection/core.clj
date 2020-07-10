(ns line-intersection.core
  (:gen-class))

(defn intersection-found
  "Returns a string when intersection is found successfully."
  [{:keys [x y]}]
  (str "(" x "," y ")"))

(def intersection-not-found "no intersection")

(defn intersection-within-point-axis?
  "Calculates whether a single axis of the intersection is within an axis 
   of a line-segment."
  [{:keys [line-segment axis-keyword intersection]}]
  (let [axis1             (->> line-segment :point1 axis-keyword)
        axis2             (->> line-segment :point2 axis-keyword)
        intersection-axis (axis-keyword intersection)]
    (cond
      (< axis1 axis2) (<= axis1 intersection-axis axis2)
      :else           (<= axis2 intersection-axis axis1))))

(defn intersection-within-boundaries?
  "Calculates whether the intersection of the lines happens 
   happens in the line segments."
  [{:keys [intersection line-segment1 line-segment2]}]
  (let [line-segment1-x {:line-segment line-segment1 :axis-keyword :x :intersection intersection}
        line-segment1-y {:line-segment line-segment1 :axis-keyword :y :intersection intersection}
        line-segment2-x {:line-segment line-segment2 :axis-keyword :x :intersection intersection}
        line-segment2-y {:line-segment line-segment2 :axis-keyword :y :intersection intersection}
        axises-to-check [line-segment1-x line-segment1-y line-segment2-x line-segment2-y]]
    (every? intersection-within-point-axis? axises-to-check)))

(defn calculate-y
  "Calculates the y of the intersection. If a horizontal line segment exists then the 
   intersection must exist on that axis."
  [{:keys [constant line-segment1 line-segment2 slope1 slope2 x]}]
  (cond
    (= slope1 0) (->> line-segment1 :point1 :y)
    (= slope2 0) (->> line-segment2 :point1 :y)
    :else        (->> x (* slope1) (+ constant))))

(defn slopes->combined-slope
  "Combines the slopes by substracting one from the other. If a slope doesn't
   exist then the existing slope is returned."
  [{:keys [slope1 slope2]}]
  (if (or (nil? slope1) (nil? slope2))
    (or slope1 slope2)
    (- slope1 slope2)))

(defn calculate-non-vertical-x
  "Calculates x, should be called when a vertical line segment doesn't exist."
  [{:keys [constant1 constant2 slope1 slope2]}]
  (let [combined-constant (- constant2 constant1)
        combined-slope    (slopes->combined-slope {:slope1 slope1
                                                   :slope2 slope2})]
    (/ combined-constant combined-slope)))

(defn calculate-x
  "Calculates the x of the intersection. If a vertical line segment exists then the 
   intersection must exist on that axis."
  [{:keys [slope1 slope2 line-segment1 line-segment2] :as args}]
  (cond
    (nil? slope1) (->> line-segment1 :point1 :x)
    (nil? slope2) (->> line-segment2 :point1 :x)
    :else         (calculate-non-vertical-x args)))

(defn calculate-constant
  "Calculates the constant of the line's form. If the slope doesn't exist then 0 is returned."
  [{:keys [line-segment slope]}]
  (let [x (->> line-segment :point1 :x)
        y (->> line-segment :point1 :y)]
    (if (nil? slope)
      0
      (->> (- x) (* slope) (+ y)))))

(defn line-segments-and-slopes->intersection
  "Returns the point where the lines given by the coordinates intersect. Should be called 
   when it's been proven that lines intersect at some point. Note that the function checks where 
   lines intersect, which doesn't mean line segments necessarily intersect."
  [{:keys [line-segment1 line-segment2 slopes]}]
  (let [slope1    (:slope1 slopes)
        slope2    (:slope2 slopes)
        constant1 (calculate-constant {:line-segment line-segment1 :slope slope1})
        constant2 (calculate-constant {:line-segment line-segment2 :slope slope2})
        x         (calculate-x {:constant1     constant1
                                :constant2     constant2
                                :line-segment1 line-segment1
                                :line-segment2 line-segment2
                                :slope1        slope1
                                :slope2        slope2})
        y         (calculate-y {:constant      constant1
                                :line-segment1 line-segment1
                                :line-segment2 line-segment2
                                :slope1        slope1
                                :slope2        slope2
                                :x             x})]
    {:x x :y y}))

(defn line-segments-parallel?
  "Calculates whether the slopes are equal, which would mean 
   the line segments are parallel."
  [{:keys [slope1 slope2]}]
  (= slope1 slope2))

(defn line-segment->slope
  "Calculates the slope of a single line segment. If the line segment is vertical
   the slope doesn't exist, and it will be marked with null."
  [{:keys [point1 point2]}]
  (let [x1 (:x point1)
        x2 (:x point2)
        y1 (:y point1)
        y2 (:y point2)]
    (if (= x1 x2)
      nil
      (/ (- y2 y1) (- x2 x1)))))

(defn coordinates->slopes
  "Calculates the slopes of the line-segments"
  [{:keys [line-segment1 line-segment2]}]
  (let [slope1 (line-segment->slope line-segment1)
        slope2 (line-segment->slope line-segment2)]
    {:slope1 slope1 :slope2 slope2}))

(defn point-str->point
  "Takes a coordinate string from the initial input, eventually returns both x and y 
   in integer for using recursion."
  [{:keys [point-str first-index last-index x]}]
  (let [axis-character (subs point-str first-index last-index)
        next-character (subs point-str last-index (inc last-index))]
    (cond
      (= next-character ",") (point-str->point {:first-index (inc last-index)
                                                :last-index  (+ 2 last-index)
                                                :point-str   point-str
                                                :x           axis-character})
      (= next-character ")") {:x (Integer. x) :y (Integer. axis-character)}
      :else                  (point-str->point {:first-index first-index
                                                :last-index  (inc last-index)
                                                :point-str   point-str
                                                :x           x}))))

(defn point-str->point-str-with-initial-values
  "Initializes point string with initial values used by the algorithms."
  [point-str]
  {:first-index 1
   :last-index  2
   :point-str   point-str
   :x           nil})

(defn str-arr->points
  "Takes the string array and returns a sequence of vectors."
  [str-arr]
  (mapv point-str->point (map point-str->point-str-with-initial-values str-arr)))

(defn str-arr->coordinates
  "Takes the string array and turns it into a usable map."
  [str-arr]
  (let [points (str-arr->points str-arr)]
    {:line-segment1 {:point1 (get points 0)
                     :point2 (get points 1)}
     :line-segment2 {:point1 (get points 2)
                     :point2 (get points 3)}}))

(defn line-segment-intersection
  "Checks whether the line segments given by the coordinates intersect.
   This program assumes that the given lines cannot be on top of each other,
   for example: ['(0,0)','(0,4)','(0,1)','(0,5)']"
  [strArr]
  (let [coordinates   (str-arr->coordinates strArr)
        slopes        (coordinates->slopes coordinates)]
    (if (line-segments-parallel? slopes)
      intersection-not-found
      (let [line-segment1 (:line-segment1 coordinates)
            line-segment2 (:line-segment2 coordinates)
            intersection  (line-segments-and-slopes->intersection {:line-segment1 line-segment1
                                                                   :line-segment2 line-segment2
                                                                   :slopes        slopes})]
        (if (intersection-within-boundaries? {:intersection  intersection
                                              :line-segment1 line-segment1
                                              :line-segment2 line-segment2})
          (intersection-found intersection)
          intersection-not-found)))))
