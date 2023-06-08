#lang racket

;;;
;;; LIGHTS-OUT
;;;
;;; - Clicking a corner button will toggle the corner and its 3 surrounding lights.
;;; - Clicking an edge button will toggle the edge and its 2 surrounding neighbors.
;;; - Clicking the center button will toggle it and the edge lights.
;; - The game is won when all lights are out.
;;;

(require 2htdp/image
         2htdp/universe
         utils/2htdp/clicker
         utils/2htdp/text)

;; The world state.
;; inst? indicates whether we've already seen the game instructions.
;; lights are represented with a natural number betwwn 0 and 511.
(struct world (inst-seen? lights containers) #:mutable #:transparent)

;; Initial world state setup. Tells the program we haven't seen
;; instructions yet, and builds the containers framework that will
;; be initialized each time by new-world.
(define (init-world)  
  ;; Build the containers list used by clicker functions.
  ;; This builds the container, button, and label configuraiton
  ;; in a solved state, which is not used except for the basis
  ;; of button assignmennt by new-world.
  (define containers
    (for/list ([c (range 3)])
      (make-container (name c)
                      (y-offset (* c (current-container-button-height)))
                      (buttons (for/list ([b (range 3)])                               
                                 (make-button (name b)
                                              (label (make-label))))))))
  
  (world #f 0 containers))

;; Once instructions are seen this funciton will set up
;; the tile lighting for game play. This function is called
;; each time the spacebar is pressed. 
(define (new-world ws)
  ;; Mark instructions seen.
  (set-world-inst-seen?! ws #t)
  ;; Compute a natural number representing our lights game tiles.
  (set-world-lights! ws (random 1 (add1 (expt 2 9))))
  ;; Set the label values of the lights tiles on or off as indicated
  ;; by the bit values of the natural representing each tile position.
  (update-containers! ws)
  ws)

;; The spacebar is used to begin an ew game. It also sets
;; the initial game flag requesting instructions off.
(define (key-handler ws ke)
  (cond
    [(key=? ke " ") (new-world ws)]                       
    [else ws]))

;; Mouse events are handled by the clicke library according to the
;; definitions described by containers.
(define (mouse-handler ws x y evt)
  (cond [(or (false? (world-inst-seen? ws)) (solved? ws)) (void)]
        [else (process-containers (world-containers ws) ws x y evt)])  
  ws)

;;;
;;; TOGGLING
;;;
;;; The lights natural number represnts a binary vector of 9
;;; bits. Each game tile can be thought of as a bit in that 
;;; vector, with light on/off values represented by 1s and 0s.
;; 
;;; 0 1 2
;;; 3 4 5  => #(bit8 bit7 bit6 bit5 bit4 bit3 bit2 bit1 bit0)
;;; 6 7 8
;;;
;;; The tiles toggled by a mouse click can likewise be thought of
;;; as a bit vector of length 8 with the values reprenting the
;;; associated toggling tiles as 1s, the tiles not being toggled
;; having their bits set to 0s. To simplify matters this toggling
;;; bit vector is also converted to a natural number.
;;;
;;; The new lights bit vector resulting from the toggle can be done
;;; by mod-2 vector addition of the current lights bit vector and the
;;; toggling bit vector. To simplify things the program replaces mod-2
;;; addtion of bit vectors with the equivalent bitwise-xor of the lights
;;; natural number with the toggling natural number.
;;;

;; Converts the base 2 exponent list into a natural number.
(define (toggle-value-for . ns)
  (for/sum ([n ns])
    (expt 2 n)))

;; Defines a table of tile positions with the natural number
;; required to toggle it and its associated tiles on or off
;; according to the game rules. 
(define TOGGLE-HASH (for/hash ([n (range 9)])
                      (values n (case n
                                  [(0) (toggle-value-for 0 1 3 4)]
                                  [(1) (toggle-value-for 0 1 2)]
                                  [(2) (toggle-value-for 1 2 4 5)]
                                  [(3) (toggle-value-for 0 3 6)]
                                  [(4) (toggle-value-for 1 3 4 5 7)]
                                  [(5) (toggle-value-for 2 5 8)]
                                  [(6) (toggle-value-for 3 4 6 7)]
                                  [(7) (toggle-value-for 6 7 8)]
                                  [(8) (toggle-value-for 4 5 7 8)]))))

;; Toggles the lights associated with container button according to
;; the game rules and sets the new lights value.
;; Encodes the toggle as a bitwise-xor application.
(define (toggle-lights! c b ws x-pos y-pos)
  (define cname (container-name c))
  (define bname (button-name b))
  (define n (+ (* 3 cname) bname))
  (define t (hash-ref TOGGLE-HASH n))  
  (define lights (bitwise-xor (world-lights ws) t))
  (set-world-lights! ws lights)
  (update-containers! ws))

(define (update-containers! ws)
  (for ([n (range 9)])
    (define cname (quotient n 3))
    (define bname (modulo n 3))
    (define btn (second (find-container/button cname bname (world-containers ws))))
    (if (bitwise-bit-set? (world-lights ws) n)
        (set-button-label! btn LIGHT-ON)
        (set-button-label! btn LIGHT-OFF))))

;; Label defaults
(current-label-name " ")
(current-label-border? #t)
(current-label-font-color 'white)
(current-label-bg-color 'black)
(current-label-bo-color 'red)
(current-label-padding 12)

;; Button defaults
(current-button-up-action toggle-lights!)

;; Container defaults
(current-container-button-width 100)
(current-container-button-height 100)

(define LIGHT-ON (make-label (bg-color 'white)))
(define LIGHT-OFF (make-label (bg-color 'black)))

(define (solved? ws) (zero? (world-lights ws)))

;;;
;;; RENDERING
;;;

(define MT-WIDTH  300)
(define MT-HEIGHT 300)
(define MT (empty-scene MT-WIDTH MT-HEIGHT 'black))

(define INST-FONT-SIZE 22)
(define INST-FONT-COLOR 'white)
(define INST-IMG-WIDTH 290)
(define-values (INST1 INST-L1) (text-tok-wrap "Rules:"
                            INST-FONT-SIZE
                            INST-FONT-COLOR
                            INST-IMG-WIDTH))
(define-values (INST2 INST-L2)
  (text-tok-wrap "- Click a corner tile to toggle it and the 3 adjacent tiles." 
                 INST-FONT-SIZE
                 INST-FONT-COLOR
                 INST-IMG-WIDTH))
(define-values (INST3 INST-L3)
  (text-tok-wrap "- Click an edge tile to toggle it and the 2 surrounding corner tiles." 
                 INST-FONT-SIZE
                 INST-FONT-COLOR
                 INST-IMG-WIDTH))
(define-values (INST4 INST-L4)
  (text-tok-wrap "- Click the center tile to toggle it and the 4 edge tiles." 
                 INST-FONT-SIZE
                 INST-FONT-COLOR
                 INST-IMG-WIDTH))
(define-values (INST5 INST-L5)
  (text-tok-wrap "Game is won when all lights are out."
                 INST-FONT-SIZE
                 INST-FONT-COLOR
                 INST-IMG-WIDTH))
(define-values (INST6 INST-L6)
  (text-tok-wrap "Press SPACEBAR to begin a new Game."
                 INST-FONT-SIZE
                 INST-FONT-COLOR
                 INST-IMG-WIDTH))
(define INST
  (above/align "left"
             INST1 INST2 INST3 INST4 INST5 INST6))

;; Shows the puzzle in 1 of 3 states: instructions, unsolved, and solved.
(define (render ws)
  (cond
    [(false? (world-inst-seen? ws))
     (render-init ws)]
    [(solved? ws)
     (render-solved! ws)]
    [else (render-unsolved ws)]))

;; Show the puzzle instructions.
(define (render-init ws)
  (place-image INST
               (quotient MT-WIDTH 2)
               (quotient MT-HEIGHT 2)
               MT))

;; Show the unsolved puzzle state.
(define (render-unsolved ws)
  (place-containers (world-containers ws)
                    MT))

;; Show the solved puzzle. 
(define (render-solved! ws)
  (define img (text "SOLVED!" 80 'darkred))
  (define win-img (rotate 45 (overlay img
                                      (rectangle (+ 40 (image-width img))
                                                 (+ 4 (image-height img))
                                                 'solid 'gold))))
  (overlay
   win-img
   (render-unsolved ws)))

(big-bang (init-world)
  (on-mouse mouse-handler)
  (on-key key-handler)
  (to-draw render)
  (name "LIGHTS OUT PUZZLE"))