;; As a wizard's apprentice - explore his house

;; Solve the puzzles and win the magical donut

;; There are 3 different locations:
;;   - living-room
;;   - attic
;;   - garden

;; Move between these locations via a door, and
;; ladder to the attic

;; A simple di-graph...

;;         +-------------+
;;     -->| living-room |<--
;;     |  +-------------+  |
;;     |                   |
;;     | ladder       door |
;;     |                   |
;; +---V---+          +----V---+
;; | attic |          | garden |
;; +-------+          +--------+

;; The player needs to be able to:
;;   - look around
;;   - walk to different locations
;;   - pick up objects
;;   - perform actions on the picked up objects

;; When looking around the world, you will be able
;; to see: 
;;   - basic scenery
;;   - one or more paths to other locations
;;   - objects that you can pick up and manipluate

;; creates a golbal (top-level) hash (association list or alist)...
;; note the 'string's aren't stings - they are a list of symbols
(defparameter *nodes* '((living-room (you are in the living room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))


;; create a function that uses assoc to find the correct item in the list by key
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; describe the paths between locations
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

;; similar to describe-location...
;; uses quasiquoting (interpolation, sort of)
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; describe multiple paths...
;; this does several things:
;;    - find the edges
;;    - convert the edges to descriptions
;;    - join the descriptions
;; Note: #'<function-name> passes in functions as values (parameters) - lamdba
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; create a list of objects
(defparameter *objects* '(whiskey bucket frog chain))

;; track the location of each object
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

;; list the objects at a location
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; describe the objects at a location
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; keep track of player's current position
(defparameter *location* 'living-room)

;; pull everything together into a simple 'look' command
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;; walking around
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
  (if next
      (progn (setf *location* (car next))
             (look))
      '(you cannot go that way.))))

;; picking up things
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
           `(you are now carrying the object ,object))
         (t '(you cannot get that.))))

;; check your inventory
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

