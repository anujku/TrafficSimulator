#lang racket

;; Start the program by giving input as  :
;; (run Number (initial-world Number))
;; (run frame-rate (initial-world rate))

;;frame-rate can be : 0.1, 0.2, 0.25, 0.50 .... 1  etc
;;rate can be 1, 2 , 3 ,4 .. and so on.

;; Example : (run 0.25 (initial-world 1))

;; Note : A Vehicle is always one of the these 3 :
;; -- Car
;; -- Truck
;; -- Tractor-Trailer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REQUIRED TECHPACKS

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)   ; only if you are doing an animation
(require 2htdp/image)      ; only if you are manipulating images

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS
;; Number Constants
(define ZERO 0)
(define TWO 2)

;; Canvas Dimensions
(define CANVAS-WIDTH 1000)
(define CANVAS-HEIGHT 400)
(define CANVAS-CENTER-X (/ CANVAS-WIDTH 2))
(define CANVAS-CENTER-Y (/ CANVAS-HEIGHT 2))
(define CANVAS-COLOR "black")
(define SCREEN-START 0)

;; Street Dimensions
(define STREET-WIDTH CANVAS-WIDTH)
(define STREET-HEIGHT 50)
(define MID-STREET-X (/ STREET-WIDTH 2))
(define STREET-HEIGHT-HALF (/ STREET-HEIGHT TWO))
(define STREET-COLOR "gray")
(define STREET-MODE "solid")
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT CANVAS-COLOR))

;; Vehicle Constants
(define VEHICLE-MODE "solid")
(define CAR-WIDTH 20)
(define TRUCK-WIDTH 30)
(define TRACTOR-TRAILER-WIDTH 60)
(define VEHICLE-HEIGHT 10)
(define CAR-WIDTH-HALF (/ CAR-WIDTH TWO))
(define TRUCK-WIDTH-HALF (/ TRUCK-WIDTH TWO))
(define TRACTOR-TRAILER-WIDTH-HALF (/ TRACTOR-TRAILER-WIDTH TWO))
(define INIT-CAR-POS (- ZERO CAR-WIDTH-HALF))
(define INIT-TRUCK-POS (- ZERO TRUCK-WIDTH-HALF))
(define INIT-TRACTOR-TRAILER-POS (- ZERO TRACTOR-TRAILER-WIDTH))
(define SAFE-DIST 5)
(define CAR "Car")
(define TRUCK "Truck")
(define TRACTOR-TRAILER "Tractor-Trailer")

;; Traffic-Light constant
(define TRAFFIC-LIGHT-SIZE 15)
(define TRAFFIC-LIGHT-SIZE-HALF (ceiling (/ TRAFFIC-LIGHT-SIZE TWO)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

;; A ListOf<StreetCar> is one of
;; -- empty                                ; There is no StreetCar
;; -- (cons StreetCar ListOf<StreetCar>)   ; A StreetCar can be followed by
;;                                         ; a ListOf<StreetCar>

;; Template :
;; los-fn : ListOf<StreetCar> -> ??
;;(define (los-fn l)
;;  (cond
;;    [(empty? l) ...]
;;    [else (...(first l)
;;              (los-fn (rest l)))]))

;; lox-fn : ListOf<X> -> Z
;; strategy: Structural decomposition [lst] + accumulator [acc]
;;(define (lox-fn lst0)
;;  (local
;;    (;; lox-fn-in-context : ListOf<X> Y -> Z
;;     ;; INVARIANT: explain the relationship between lst0, lst, and acc.
;;     ;; short purpose statement
;;     (define (lox-in-context lst acc)
;;      (cond
;;        [(empty? lst) ...]
;;        [else
;;         (... (first lst)
;;              (lox-in-context
;;                (rest lst)
;;                (... (first lst) acc) ))]))
;;   (lox-in-context lst0 ...)))

;; TEMPLATE :Structural Decomposition using Abstraction
;; list-fn : ListOf<X> -> ??
;; Given a List of attributes produce ....

;; (define (list-fn lox)
;;   (local (; contract for combiner
;;           (define (combiner ...) ...))
;;     (abstraction combiner ... lox)))

;; Data Definition :
;; A MaybeStreetCar is one of:
;; -- false
;; interp: if there is no car in front of the given car.
;; -- StreetCar%
;; interp : if there is a car at immediate front of given car.

;; TEMPLATE for MaybeStreetCar :
;; mbs-fn : MaybeStreetCar -> ??
;;(define (mbs-fn a-mbs)
;;  (cond
;;    [(false? a-mbs) ...]
;;    [else  ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE DEFINITIONS

;; Stateful Objects:  they don't return a new WorldObj-- they just
;; change their state

(define StatefulWorldObj<%>
  (interface ()
    on-tick       ; ->  Void
    ;; Passes on-tick to the Stateful Object, collects their
    ;; on-tick results and updates the Stateful World Object respectively.
    add-to-scene  ; Scene -> Scene
    ;; Given a scene, returns a scene adding more images to current scene.
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Interfaces.

(define Car<%>
  (interface ()
    front       ; -> Number
    ; It gives the x-position of the front bumper of the car
    rear        ; -> Number
    ; It gives the x-position of the rear bumper of the car
    hvel        ; -> Number
    ; horizontal velocity of the car, in pixels/tick
    ))

(define Traffic<%>
  (interface ()
    cars        ; ListOf< Car<%> >:
    ; The list of cars on the street from left to right.
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS DEFINITIONS
;; A World is (new World% [vehicles ListOf<vehicle>]
;;                        [traffic-lights ListOf<Trafficlight>]
;;                        [tick-count Number]
;;                        [rate Number])

;; A World represents the stateful world where a list of Vehicles created so
;; far move on the street every tick from left to right.
;; It has a list of Traffic lights which are draggable.
;; It implements StatefulWorldObj<%> and Traffic<%>.

(define World%
  (class* object% (StatefulWorldObj<%> Traffic<%>)
    (init-field vehicles)        ; represents the ListOf<Vehicles> which have
    ; been created so far.It consists of Vehicles which are on the street at
    ; that time

    (init-field traffic-lights)  ; represensts a ListOf<Trafficlight> which
    ; are available on the canvas at that tick.

    (init-field tick-count)      ; represents the ticks happened till now.

    (init-field rate)            ; represents the tick-rate given by the user
    ;for new-car generation.

    (field [HUNDRED-PERCENT 100])          ; Total Percentage

    (field [PERCENT-TRUCKS 30])            ; Required Percenatage of trucks on
    ; the street

    (field [PERCENT-TRACTOR-TRAILERS 20])  ; Required Percenatage of
    ; tractor-trailer on the street

    (field [PERCENT-CARS (- 100 (+ PERCENT-TRUCKS PERCENT-TRACTOR-TRAILERS))])
    ; Required Percenatage of cars on the street

    (super-new)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; cars : -> ListOf<Vehicles>
    ;; It returns the ListOf<vehicles> in the street at that tick after
    ;; filtering vehicles which have crossed the canvas.
    ;; Examples are with tests
    ;; Each vehicle in the list moves from left to right on the street.
    ;; Strategy : Structural Decomposition using abstraction on World%
    (define/public (cars)
      (filter
       (lambda (vehicle)
         (< (send vehicle rear) CANVAS-WIDTH))
       (reverse vehicles)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; for-each-vehicle : -> void
    ;; It passes on-tick to each of the vehicle.
    ;; Examples are with tests
    ;; Strategy : Structural Decomposition on World% using Abstraction
    (define/public (for-each-vehicle)
      (begin
        (for-each
         (lambda (obj) (send obj on-tick))
         vehicles)
        vehicles))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-car-count : -> Number
    ;; Returns the number of cars on the street at that tick.
    ;; Examples are with tests.
    ;; Strategy : Structural Decomposition on World% using Abstraction
    (define/public (get-car-count)
      (length
       (filter
        (lambda (vehicle) (equal? (send vehicle get-vehicle-type) CAR))
        (cars))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-truck-count : -> Number
    ;; Returns the number of trucks on the street at that tick.
    ;; Examples are with tests.
    ;; Strategy : Structural Decomposition on World% using Abstraction
    (define/public (get-truck-count)
      (length
       (filter
        (lambda (vehicle) (equal? (send vehicle get-vehicle-type) TRUCK))
        (cars))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-tractor-trailer-count : -> Number
    ;; Returns the number of tractor-trailer on the street at that tick.
    ;; Examples are with tests.
    ;; Strategy : Structural Decomposition on World% using Abstraction
    (define/public (get-tractor-trailer-count)
      (length
       (filter
        (lambda (vehicle) (equal? (send vehicle get-vehicle-type) TRACTOR-TRAILER))
        (cars))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-car-percent : -> Number
    ;; Returns the percentage of car on the street at that tick.
    ;; Examples are with tests.
    ;; Strategy : Function Composition
    (define/public (get-car-percent)
      (* (/ (get-car-count) (length (cars))) HUNDRED-PERCENT))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-truck-percent : -> Number
    ;; Returns the percentage of truck on the street at that tick.
    ;; Examples are with tests.
    ;; Strategy : Function Composition
    (define/public (get-truck-percent)
      (* (/ (get-truck-count) (length (cars))) HUNDRED-PERCENT))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-tractor-trailer-percent : -> Number
    ;; Returns the percentage of tractor-trailer on the street at that tick.
    ;; Examples are with tests.
    ;; Strategy : Function Composition
    (define/public (get-tractor-trailer-percent)
      (* (/ (get-tractor-trailer-count) (length (cars))) HUNDRED-PERCENT))


    (define/public (on-tick)
      (if (= (length vehicles) ZERO)
          (first-car-creator)
          (if (= (modulo tick-count rate) ZERO)
              (continue-with-new-vehicle)
              (continue-with-other-vehicles)))
      (set! tick-count (add1 tick-count)))

    ;; first-car-creator : -> StreetCar%
    ;; Makes the first car on the street by making its x value such that
    ;; the front bumper touches 0. The vehicle-infront field as false as
    ;; there is no car in-front of the first car.
    ;; Examples: see tests below
    ;; Strategy: Function Composition
    (define/public (first-car-creator)
      (set! vehicles  (list
                       (new StreetCar%
                            [x INIT-CAR-POS]
                            [vehicle-type CAR]
                            [vehicle-infront false]))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; continue-with-new-vehicle : -> Void
    ;; Checks as to which vehicle type has to be created to satisfy the
    ;; required percentage set and calls that vehicle creation function
    ;; accordingly.
    ;; Examples are with tests
    ;; Strategy : Structural Decomposition on World%
    (define/public (continue-with-new-vehicle)
      (cond
        ((< (get-car-percent) PERCENT-CARS) (generate-new-car))
        ((< (get-truck-percent) PERCENT-TRUCKS) (generate-new-truck))
        (else (generate-new-tractor-trailer))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; generate-new-car : -> Void
    ;; Checks for sufficient space for car to be added on street and then only
    ;; adds a car else just continues with the existing ListOf<Vehicles>
    ;; Examples are with test cases.
    ;; Strategy : Function Composition
    (define/public (generate-new-car)
      (if (>= (send (get-last-vehicle) rear) (+ CAR-WIDTH-HALF SAFE-DIST))
          (add-car)
          (continue-with-other-vehicles)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-car : -> Void
    ;; EFFECT : Sends each vehicle to for-each-vehicle function and adds a new
    ;; StreetCar% (car) to the existing list of vehicles.
    ;; Examples are with test cases.
    ;; Strategy : Function Composition
    (define/public (add-car)
      (set! vehicles (append
                      (send this for-each-vehicle)
                      (list (add-object StreetCar% CAR (get-last-vehicle))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; generate-new-tractor-trailer : -> Void
    ;; Checks for sufficient space for tractor-trailer to be added on street and
    ;; then only adds a tractor-trailer else just continues with the existing
    ;; ListOf<Vehicles>
    ;; Examples are with test cases.
    ;; Strategy : Function Composition
    (define/public (generate-new-tractor-trailer)
      (if (>= (send (get-last-vehicle) rear) (+ TRACTOR-TRAILER-WIDTH-HALF SAFE-DIST))
          (add-tractor-trialer)
          (continue-with-other-vehicles)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-tractor-trialer : -> Void
    ;; EFFECT : Sends each vehicle to for-each-vehicle function and adds a new
    ;; Tractor-Trailer% (Tractor-Trailer) to the existing list of vehicles.
    ;; Examples are with test cases.
    ;; Strategy : Function Composition
    (define/public (add-tractor-trialer)
      (set! vehicles (append
                      (send this for-each-vehicle)
                      (list (add-object Tractor-Trailer% TRACTOR-TRAILER (get-last-vehicle))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; generate-new-truck : -> Void
    ;; Checks for sufficient space for truck to be added on street and
    ;; then only adds a truck else just continues with the existing
    ;; ListOf<Vehicles>
    ;; Examples are with test cases.
    ;; Strategy : Function Composition
    (define/public (generate-new-truck)
      (if (>= (send (get-last-vehicle) front) (+ TRUCK-WIDTH-HALF SAFE-DIST))
          (add-new-truck)
          (continue-with-other-vehicles)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-new-truck : -> Void
    ;; EFFECT : Sends each vehicle to for-each-vehicle function and adds a new
    ;; Truck% (Truck) to the existing list of vehicles.
    ;; Examples are with test cases.
    ;; Strategy : Function Composition
    (define/public (add-new-truck)
      (set! vehicles (append
                      (send this for-each-vehicle)
                      (list (add-object Truck% TRUCK (get-last-vehicle))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-last-vehicle : -> Vehicle%
    ;; Returns the vehicle which has been created at last..i.e It returns the
    ;; latest vehicle.
    ;; Examples ar with test cases.
    ;; Domain Knowledge
    (define/public (get-last-vehicle)
      (first (reverse vehicles)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; continue-with-other-vehicles : -> Void
    ;; EFFECT : Sends the existing list of vehicles to the for-each-vehicle
    ;; function.
    ;; Examples ar with test cases.
    ;; Domain Knowledge
    (define/public (continue-with-other-vehicles)
      (set! vehicles (send this for-each-vehicle)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; on-key : KeyEvent -> Void
    ;; Creates a new traffic light on key 't' and removes a traffic light on
    ;; key 'x' and ignores the other keyevents.
    ;; Examples are with tests.
    ;; Structural Decomposition on KeyEvent
    (define/public (on-key keyevt)
      (cond
        [(key=? keyevt "t") (generate-new-traffic-light)]
        [(key=? keyevt "x") (remove-new-traffic-light)]
        [else this]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; remove-new-traffic-light : -> Void
    ;; Removes the traffic light which ever is in selected state from the
    ;; ListOf<TrafficLight>
    ;; Examples are with tests.
    ;; Structural Decomposition on World% using abstraction.
    (define/public (remove-new-traffic-light)
      (set! traffic-lights
            (filter
             (lambda (traffic-light) (not (send traffic-light get-selected?)))
             traffic-lights)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; generate-new-traffic-light : -> Void
    ;; Adds a new trafficlight to the existing ListOf<Trafficlight>
    ;; Examples are with tests.
    ;; Structural Decomposition on World% using abstraction.
    (define/public (generate-new-traffic-light)
      (set! traffic-lights (append (list
                                    (new TrafficLight%
                                         [x (generate-random-x)]
                                         [y (generate-random-y)]
                                         [selected? false]
                                         [on-street? false])) traffic-lights)))

    (define/public (generate-random-y)
      (check-random-y (random (- CANVAS-HEIGHT TRAFFIC-LIGHT-SIZE-HALF)))) ; 392.5

    (define/public (generate-random-x)
      (check-random-x (random (- CANVAS-WIDTH TRAFFIC-LIGHT-SIZE-HALF)))) ; 992.5

    (define/public (check-random-x x)
      (if (> x TRAFFIC-LIGHT-SIZE-HALF)
          x
          (generate-random-x)))

    (define (check-random-y y)
      (if
       (or
        (> y (+ CANVAS-CENTER-Y STREET-HEIGHT-HALF TRAFFIC-LIGHT-SIZE-HALF)) ; 232.5
        (< y (- CANVAS-CENTER-Y STREET-HEIGHT-HALF TRAFFIC-LIGHT-SIZE-HALF)) ; 217.5
        (> y TRAFFIC-LIGHT-SIZE-HALF)) ;7.5
       y
       (check-random-y (generate-random-y))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-to-scene : Scene -> Scene
    ;; Returns a scene adding images to existing scene.
    ;; Examples are with test cases.
    ;; Adds the various existing vehicles and traffic lights on the scene.
    ;; Structural decomposition on World% using abstraction
    (define/public (add-to-scene s)
      (local (
              ;; car-display : -> Void
              ;; Adds the existing vehicles to the scene at thier respective
              ;; positions given after creating the street on the canvas center
              (define car-display
                (foldr
                 (lambda (obj s1) (send obj add-to-scene s1))
                 (place-image (rectangle STREET-WIDTH STREET-HEIGHT
                                         STREET-MODE STREET-COLOR)
                              CANVAS-CENTER-X
                              CANVAS-CENTER-Y
                              s)
                 (reverse vehicles))))
        (if (= (length traffic-lights) ZERO)
            car-display
            (foldr
             (lambda (obj s3) (send obj add-to-scene s3))
             car-display
             traffic-lights))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; on-mouse : Number Number KeyEvent -> Void
    ;; Sends each mouse event to their respective traffic light to handle them
    ;; indiidually.
    ;; Examples are with test cases.
    ;; Structural Decomposition on MouseEvent.
    (define/public (on-mouse x y evt)
      (begin
        (for-each
         (lambda (obj) (send obj on-mouse x y evt))
         traffic-lights)
        traffic-lights))

    ))

(define (add-object class vehicle last-vehicle)
  (new class
       [x INIT-TRUCK-POS]
       [vehicle-type vehicle]
       [vehicle-infront last-vehicle]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Vehicle is (new Vehicle% [x Number]
;;                        [vehicle-type String]
;;                        [vehicle-in-front Maybevehicle])
;; A Vehicle is StatefulWorld object implementing StatefulWorldObj and Car
;; interface.It represents either a car, truck or a tractor-trailer.
(define Vehicle%
  (class* object% (StatefulWorldObj<%> Car<%>)

    (init-field x)                        ;; Center position of the vehicle
    (init-field vehicle-type)             ;; Represents the type of vehicle
    (init-field vehicle-infront)          ;; Represents the vehicle-infront
    ;; of this vehicle.

    (field [MAX-SPEED 20])                ;; maximum allowed speed
    (field [CAR-FIRST-HALF-SPEED 20])     ;; First half speed of car
    (field [speed CAR-FIRST-HALF-SPEED])  ;; Initial value of speeed.
    (field [CAR-SECOND-HALF-SPEED 4])     ;; Second half speed of car
    (field [truck-speed 2])               ;; Speed of truck
    (field [MAX-TRACTOR-SPEED 15])        ;; Maximum speed of tractor-trailer
    (field [truck-acc 2])                 ;; acceleration rate of truck

    (super-new)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-vehicle-type : -> String
    ;; Returns the type of vehicle for the given vehicle.
    ;; Examples are with tests.
    ;; Domain Knowledge
    (define/public (get-vehicle-type)
      vehicle-type)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; on-tick : -> void
    ;; Purpose :
    ;; Examples are with test cases
    ;; Stategy : Structural Decomposition on MaybeVehicle
    (define/public (on-tick)
      (cond
        [(false? vehicle-infront) (car-movement)]
        [else
         (collision-checker)]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; collision-checker : -> Void
    ;; Checks for collision for the given vehicle with the vehicle in its front
    ;; moves the car accordingly.
    ;; Examples are with test cases
    ;; Function Composition
    (define/public (collision-checker)
      (if (>= (+ (send this front) (hvel) SAFE-DIST)
              (send vehicle-infront rear))
          (update-pos)
          (move-vehicle)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; move-vehicle : Void
    ;; Selects which vehicle to be moved and calls the movement-fn for that
    ;; vehicle type accordingly.
    ;; Examples are with tests.
    ;; Strategy : Structural Decomposition on Vehicle%
    (define/public (move-vehicle)
      (cond
        [(equal? vehicle-type CAR) (car-movement)]
        [(equal? vehicle-type TRUCK) (truck-movement)]
        [(equal? vehicle-type TRACTOR-TRAILER) (tractor-movement)]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; tractor-movement : -> Void
    ;; EFFECT : Checks for max limit of speed and accelerates the
    ;; tractor accordingly and moves the tractor.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (tractor-movement)
      (if (< truck-speed MAX-TRACTOR-SPEED)
          (accelerate-tractor)
          (set! x (+ x MAX-TRACTOR-SPEED))))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; accelerate-tractor : -> Void
    ;; EFFECT : Set the value for truck-speed based on the acceleration and
    ;; then move the truck with that speed.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (accelerate-tractor)
      (set! truck-speed (+ truck-speed truck-acc))
      (set! x (+ x truck-speed)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; truck-movement : -> Void
    ;; EFFECT : Assign speed with MAX-SPEED and move the tractor in that speed.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (truck-movement)
      (set! speed MAX-SPEED)
      (set! x (+ x MAX-SPEED)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; car-movement : -> Void
    ;; EFFECT : Assign speed with MAX-SPEED and move the car in that speed.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (car-movement)
      (set! x (+ x (get-speed)))
      (set! speed (get-speed)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-speed : -> Number
    ;; Checks as to car is in which half of street and returns the allowed
    ;; speed in that half of street.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (get-speed)
      (if (<= x MID-STREET-X)
          CAR-FIRST-HALF-SPEED
          CAR-SECOND-HALF-SPEED))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; update-pos : -> Void
    ;; EFFECT : Updates the position of the vehicle based on the rear of vehicle
    ;; in front and min distance allowed between cars.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (update-pos)
      (set! x (- (send vehicle-infront rear) SAFE-DIST (get-vehicle-half-width))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-vehicle-half-width : -> Void
    ;; Returns the  half-width value of the corresponding vehicle.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (get-vehicle-half-width)
      (cond
        [(equal? vehicle-type CAR) CAR-WIDTH-HALF]
        [(equal? vehicle-type TRUCK) TRUCK-WIDTH-HALF]
        [(equal? vehicle-type TRACTOR-TRAILER) TRACTOR-TRAILER-WIDTH-HALF]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; hvel : -> Number
    ;; Returns the  speed of that vehicle.
    ;; Examples are with test cases.
    ;; Domain Knowledge
    (define/public (hvel)
      speed)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; front : -> Number
    ;; Returns the  front bumber value of that vehicle accordingly.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (front)
      (cond
        [(equal? vehicle-type CAR) (car-front)]
        [(equal? vehicle-type TRUCK) (truck-front)]
        [(equal? vehicle-type TRACTOR-TRAILER) (tractor-front)]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; car-front : -> Number
    ;; Returns the  front bumber value of car accordingly.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (car-front)
      (+ x CAR-WIDTH-HALF))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; truck-front : -> Number
    ;; Returns the  front bumber value of truck accordingly.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (truck-front)
      (+ x TRUCK-WIDTH-HALF))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; tractor-front : -> Number
    ;; Returns the  front bumber value of tractor-trailer accordingly.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (tractor-front)
      (+ x TRACTOR-TRAILER-WIDTH-HALF))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; rear : -> Number
    ;; Returns the rear bumber value of the vehicle accordingly.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (rear)
      (cond
        [(equal? vehicle-type CAR) (car-rear)]
        [(equal? vehicle-type TRUCK) (truck-rear)]
        [(equal? vehicle-type TRACTOR-TRAILER) (tractor-rear)]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; car-rear : -> Number
    ;; Returns the rear bumber value of car accordingly.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (car-rear)
      (- x CAR-WIDTH-HALF))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; truck-rear : -> Number
    ;; Returns the rear bumber value of truck-rear accordingly.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (truck-rear)
      (- x TRUCK-WIDTH-HALF))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; tractor-rear : -> Number
    ;; Returns the rear bumber value of tractor-trailer accordingly.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (tractor-rear)
      (- x TRACTOR-TRAILER-WIDTH-HALF))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-to-scene : Scene -> Scene
    ;; Adds the image of the vehicle to the given scene.
    ;; Examples are with test cases.
    ;; Structural Decomposition on Vehicle%
    (define/public (add-to-scene s)
      (place-image
       (send this get-image)
       x CANVAS-CENTER-Y s))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A StreetCar is (new StreetCar%
;; [x Number][vehicle-tpe String]
;; [vehicle-infront MaybeVehicle])
;; A streetCar is a type of vehicle and it inherits many methods and fields
;; from the Vehicle% class which implements StatefulWorldObj and Car interface.
(define StreetCar%
  (class* Vehicle% (StatefulWorldObj<%> Car<%>)

    ;; Fields to be inherited from Vehicle% class
    (inherit-field x vehicle-type vehicle-infront)

    (field [CAR-COLOR "yellow"])
    (field [CAR-IMG (rectangle CAR-WIDTH VEHICLE-HEIGHT VEHICLE-MODE CAR-COLOR)])

    (super-new)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-image : -> image
    ;; Returs the image of the car.
    ;; Examples are with tests.
    ;; Strategy :Domain Knowledge
    (define/public (get-image)
      CAR-IMG)

    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Truck is (new Truck%
;; [x Number][vehicle-tpe String]
;; [vehicle-infront MaybeVehicle])
;; A Truck is a type of vehicle and it inherits many methods and fields
;; from the Vehicle% class which implements StatefulWorldObj and Car interface.
(define Truck%
  (class* Vehicle% (StatefulWorldObj<%> Car<%>)

    ;; Fields to be inherited from Vehicle% class
    (inherit-field x vehicle-type vehicle-infront)

    (field [TRUCK-COLOR "Blue"])
    (field [TRUCK-IMG (rectangle TRUCK-WIDTH VEHICLE-HEIGHT VEHICLE-MODE TRUCK-COLOR)])

    (super-new)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-image : -> image
    ;; Returs the image of the truck.
    ;; Examples are with tests.
    ;; Strategy :Domain Knowledge
    (define/public (get-image)
      TRUCK-IMG)

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Tractor-Trailer is (new Tractor-Trailer%
;; [x Number][vehicle-tpe String]
;; [vehicle-infront MaybeVehicle])
;; A Tractor-Trailer is a type of vehicle and it inherits many methods and fields
;; from the Vehicle% class which implements StatefulWorldObj and Car interface.
(define Tractor-Trailer%
  (class* Vehicle% (StatefulWorldObj<%> Car<%>)

    ;; Fields to be inherited from Vehicle% class
    (inherit-field x vehicle-type vehicle-infront)

    (field [TRACTOR-COLOR "white"])
    (field [TRACTOR-IMG (rectangle TRACTOR-TRAILER-WIDTH VEHICLE-HEIGHT VEHICLE-MODE TRACTOR-COLOR)])

    (super-new)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-image : -> image
    ;; Returs the image of the tractor-trailer.
    ;; Examples are with tests.
    ;; Strategy :Domain Knowledge
    (define/public (get-image)
      TRACTOR-IMG)

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A TrafficLight is (new TrafficLight%
;; [x Number][y Number]
;; [selected? Boolean][on-street? Boolean])
;; A TrafficLight is a regular traffic light on the streets which implements
;; StatefulWorldObj interface.
;; On red the vehicles should not cross traffic light .i.e stop
;; and on Green vehicles can continue moving as before.
(define TrafficLight%
  (class* object% (StatefulWorldObj<%>)

    (init-field x y selected? on-street?)

    (field [color-change-interval 8])         ;; Time interval for light
    ;; to change colors between red and green

    (field [time-left color-change-interval])
    (field [colors (list "green" "red")])      ;; List of traffic light colors.

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; left-edge : -> Number
    ;; Returns the left edge of the square shaped trafic light.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (left-edge)
      (- x (/ TRAFFIC-LIGHT-SIZE TWO)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; right-edge : -> Number
    ;; Returns the right edge of the square shaped trafic light.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (right-edge)
      (+ x (/ TRAFFIC-LIGHT-SIZE TWO)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; top-edge : -> Number
    ;; Returns the top edge of the square shaped trafic light.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (top-edge)
      (- y (/ TRAFFIC-LIGHT-SIZE TWO)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; bottom-edge : -> Number
    ;; Returns the bottom edge of the square shaped trafic light.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (bottom-edge)
      (+ y (/ TRAFFIC-LIGHT-SIZE TWO)))

    (super-new)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; on-tick : -> Void
    ;; Returns the same traffic light after the tick.
    ;; Examples are with test cases.
    ;; Strategy : Domain knowledge
    (define/public (on-tick) this)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; get-selected? : -> Boolean
    ;; Returns the value of selected? field.
    ;; Examples are with test cases.
    ;; Strategy : Domain knowledge
    (define/public (get-selected?) selected?)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; on-mouse : Number Number MouseEvent -> Void
    ;; Checks which mouseevent has occured and calls that event handlers accordingly.
    ;; Ignores mouse events other than button-down,drag and button-up.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on MouseEvent.
    (define/public (on-mouse mouse-x mouse-y evt)
      (cond
        [(mouse=? evt "button-down") (button-down-handler mouse-x mouse-y)]
        [(mouse=? evt "drag") (drag-event-handler mouse-x mouse-y)]
        [(mouse=? evt "button-up") (button-up-handler mouse-x mouse-y)]
        [else this]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; button-down-handler : Number Number -> Void
    ;; Checks if the button down has happened inside the traffic light ,if true
    ;; it is selected else it is not selected.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (button-down-handler mouse-x mouse-y)
      (if (send this inside-this? mouse-x mouse-y)
          (button-down-handler-helper mouse-x mouse-y)
          (set! x x)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; button-down-handler-helper : Number Number -> Void
    ;; EFFECT : Sets the selected? field sa true and sets the value of center
    ;; x,y coordinate as mouse-x and mouse-y.
    ;; Does not change colours when in selected mode.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (button-down-handler-helper mouse-x mouse-y)
      (set! x mouse-x)
      (set! y mouse-y)
      (set! selected? true))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; drag-event-handler : Number Number -> Void
    ;; Check if drag is inside the traffic light and then enable it to change
    ;; position along with mouse positions else traffic light does not move.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (drag-event-handler mouse-x mouse-y)
      (if (and (send this inside-this? mouse-x mouse-y) selected?)
          (drag-handler-helper mouse-x mouse-y)
          (set! x x)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; drag-handler-helper : Number Number -> Void
    ;; EFFECT : Drags the traffic light along with the mouse cursor and
    ;; color of the traffic light does not change.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (drag-handler-helper mouse-x mouse-y)
      (set! x mouse-x)
      (set! y mouse-y)
      (set! selected? selected?))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; button-up-handler : Number Number -> Void
    ;; Checks if button-up has happened inside the traffic light ,if yes make
    ;; it unselected else do anything.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (button-up-handler mouse-x mouse-y)
      (if (inside-this? mouse-x mouse-y)
          (button-up-handler-helper mouse-x)
          (set! x x)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; button-up-handler-helper : Number -> Void
    ;; EFFECT : Sets the traffic light on the street center and makes it
    ;; unselected.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (button-up-handler-helper mouse-x)
      (set! on-street? true)
      (set! x mouse-x)
      (set! y CANVAS-CENTER-Y)
      (set! selected? false))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; inside-this? : Number Number -> Boolean
    ;; Checks if the mouse cursor is inside the square shaped traffic light,
    ;; if so returns true else returns false.
    ;; Examples are with test cases.
    ;; Strategy : Function Composition
    (define/public (inside-this? mouse-x mouse-y)
      (and
       (<= (left-edge) mouse-x (right-edge))
       (<= (top-edge) mouse-y (bottom-edge))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; change-traffic-colors : -> Void
    ;; Checks for the time remaining to change traffic colors and
    ;; also reduces the time-left field by one.
    ;; Examples are with test cases.
    ;; Strategy : Function Composition
    (define/public (change-traffic-colors)
      (if (zero? time-left)
          (change-colors)
          (set! time-left (sub1 time-left))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-to-scene : Scene -> Scene
    ;; Adds traffc lights image to the existing scene at required positions.
    ;; Examples are with test cases.
    ;; Strategy : Structural Decomposition on this
    (define/public (add-to-scene s)
      (if (and on-street? (not selected?))
          (change-traffic-colors)
          (set! colors colors))
      (place-image
       (square TRAFFIC-LIGHT-SIZE
               (if selected? "solid" "outline")
               (first colors))
       x y s))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; change-colors : -> Void
    ;; EFFECT : Set the colour of the traffic light at that tick and
    ;; updates the time-left field value.
    ;; Examples are with test cases.
    ;; Strategy : Domain Knowledge
    (define/public (change-colors)
      (set! colors (append (rest colors) (list (first colors))))
      (set! time-left color-change-interval))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (initial-world rate)
  (new World%
       [vehicles empty]
       [traffic-lights empty]
       [tick-count 0]
       [rate rate]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run frame-rate world)
  (big-bang
   world
   (on-tick
    (lambda (w) (send w on-tick) w)
    frame-rate)
   (on-draw
    (lambda (w) (send w add-to-scene EMPTY-CANVAS)))
   (on-key
    (lambda (w keyevt) (send w on-key keyevt) w))
   (on-mouse
    (lambda (w mouse-evt x y) (send w on-mouse mouse-evt x y) w))))ss
