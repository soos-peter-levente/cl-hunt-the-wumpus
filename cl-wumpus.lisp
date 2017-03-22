(in-package :cl-wumpus)

(defparameter *gs* nil)

(defstruct (game-state (:constructor make-game-state (k n arrows pits bats)))
  (n-of-tunnels k :type integer)
  (n-of-rooms n :type integer)
  (cave-hash-map nil)
  (n-of-arrows arrows :type integer)
  (in-quiver arrows :type integer)
  (range-of-arrows 4 :type integer)
  (n-of-pits pits :type integer)
  (n-of-bats bats :type integer)
  (pit-rooms (select-n-random-rooms pits n) :type list)
  (bat-rooms (select-n-random-rooms bats n) :type list)
  (wumpus-location 0 :type integer)
  (player-location 0 :type integer))

;;;; A BIT OF VOCABULARY FOR WHAT FOLLOWS
(defun n-of-bats ()
  (game-state-n-of-bats *gs*))

(defun n-of-pits ()
  (game-state-n-of-pits *gs*))

(defun n-of-rooms ()
  (game-state-n-of-rooms *gs*))

(defun arrow-range ()
  (game-state-range-of-arrows *gs*))

(defun is-bat-room (room)
  (member room (game-state-bat-rooms *gs*)))

(defun is-pit-room (room)
  (member room (game-state-pit-rooms *gs*)))

(defun n-of-tunnels ()
  (game-state-n-of-tunnels *gs*))

(defun cave-hash-map ()
  (game-state-cave-hash-map *gs*))

(defun is-wumpus-room (room)
  (= room (game-state-wumpus-location *gs*)))

(defun player-location ()
  (game-state-player-location *gs*))

(defun wumpus-location ()
  (game-state-wumpus-location *gs*))

(defun n-of-arrows ()
  (game-state-n-of-arrows *gs*))

(defun arrows-remaining ()
  (game-state-in-quiver *gs*))

(defun reset-quiver ()
  (setf (game-state-in-quiver *gs*) (n-of-arrows)))

(defun roll-the-dice (above)
  (< above (random 100)))

(defun is-quiver-empty ()
  (= (arrows-remaining) 0))

(defun use-up-one-arrow ()
  (decf (game-state-in-quiver *gs*)))

(defun set-player-location (to)
  (setf (game-state-player-location *gs*) to))

(defun set-wumpus-location (to)
  (setf (game-state-wumpus-location *gs*) to))

(defun is-hazardous-room (room)
  (or (is-pit-room room) (is-bat-room room) (is-wumpus-room room)))

(defun select-random-room (below &optional (predicate #'identity))
  (loop for room = (random below)
     while (not (funcall predicate room)) :finally (return room)))

(defun randomly-set-wumpus-location ()
  (setf (game-state-wumpus-location *gs*) (select-random-room (n-of-rooms))))

(defun randomly-set-player-location ()
  (setf (game-state-player-location *gs*)
        (select-random-room (n-of-rooms) (complement #'is-hazardous-room))))

(defun randomly-select-adjacent-room (room)
  (nth (random (n-of-tunnels)) (get-adjacent-rooms room)))

(defun move-wumpus-one-room ()
  (set-wumpus-location (randomly-select-adjacent-room (wumpus-location)))
  (txt 'hazard-wumpus-wake))

(defun select-n-random-rooms (n below &optional &key (no-same t))
  (labels ((collect-different (n below acc)
             (let ((this (select-random-room below)))
               (cond ((= 0 n) acc)
                     ((member this acc) (collect-different n below acc))
                     (t (collect-different (decf n) below (cons this acc))))))
           (collect-indifferent (n below)
             (loop repeat n collect (select-random-room below))))
    (cond (no-same (collect-different n below '()))
          (t (collect-indifferent n below)))))

;;;; LEVEL SETUP, GAME LOOP & RUNTIME
(defun start-game (&optional &key (k 3) (n 20) (arrows 4) (pits 3) (bats 3))
  (main-game-loop k n arrows pits bats))

(defun generate-random-cave (k n)
  (setf (game-state-cave-hash-map *gs*)
        (graph->hash-table (make-random-graph k n))))

(defun initialize-game-state (k n arrows bats pits &optional (same-cave nil))
  (unless same-cave
    (progn (setf *gs* (make-game-state k n arrows bats pits))
           (generate-random-cave k n)))
  (reset-quiver)
  (randomly-set-wumpus-location)
  (randomly-set-player-location))

(defun main-game-loop (k n arrows pits bats)
  (when (y-or-n-p (txt 'instruction-query))
    (txt 'instruction-welcome) (read-line))
  (loop :for first = t :then nil
     :for again = nil :then (y-or-n-p (txt 'play-again-query))
     :while (or first again)
     :do (initialize-game-state
          k n arrows bats pits (unless first (y-or-n-p (txt 'same-cave-query))))
     (in-game-loop)))

(defun in-game-loop ()
  ;; Iteration will continue until `game-over?` returns t.
  (txt 'game-start-intro
       (n-of-rooms) (n-of-tunnels) (n-of-bats) (n-of-pits) (arrows-remaining))
  (loop :for game-over = nil :then (in-game-round) :while (not game-over) :do
     (describe-player-location (player-location))))

(defun in-game-round ()
  (game-over? (handle-player-input (prompt-player))))

;;;; IN-GAME LOGIC
(defun game-over (by)
  ;; The player is done for when this is called. It invariably returns t and
  ;; prints a game over message associated with the parameter passed to  it.
  (txt by) t)

(defun game-over? (player-action-result)
  ;; a number in `player-action-value` is taken to mean that room-specific
  ;; hazards need to be checked. Symbols received are taken to mean that
  ;; something "out of turn" has happened. Although it defaults to to nil,
  ;; since the return values of most in-game functions percolate up here,
  ;; that branch is not entered in actuality.
  (if (numberp #1=player-action-result)
      (cond ((is-wumpus-room #1#)
             (game-over 'death-by-wumpus))
            ((is-bat-room #1#)
             (txt 'hazard-bats-carry)
             (game-over? (randomly-set-player-location)))
            ((is-pit-room #1#)
             (if (roll-the-dice 85)
                 (txt 'death-by-pit-avoided)
                 (game-over 'death-by-pit)))
            (t nil))
      (if (eq (type-of #1#) 'symbol)
          (cond ((eq #1# 'death-of-wumpus)
                 (game-over 'death-of-wumpus))
                ((eq #1# 'death-by-self-hit)
                 (game-over 'death-by-self-hit))
                (t nil))
          (when (is-quiver-empty)
            (game-over 'death-by-quiver-empty)))))

(defun describe-player-location (room)
  ;; Adjust zero indexing up 1 representationally. Player
  ;; input is adjusted down 1 accordingly in `prompt-player`,
  ; (print *gs*) ; for debugging
  (let ((adjacent-rooms (get-adjacent-rooms room)))
    (txt 'room-description (1+ room) (arrows-remaining))
    (when (some #'is-pit-room adjacent-rooms)
      (txt 'hazard-pits-nearby))
    (when (some #'is-bat-room adjacent-rooms)
      (txt 'hazard-bats-nearby))
    (when (member (wumpus-location)
                  (get-adjacent-rooms room 2))
      (txt 'hazard-wumpus-nearby))
    (txt 'room-pathways
         (mapcar #'1+ (butlast adjacent-rooms))
         (1+ (car (last adjacent-rooms))))))

(defun get-adjacent-rooms (room &optional n)
  (labels ((get-adjacent-for-this-room (in)
             (cond ((null in) '())
                   ((atom in) (gethash in (cave-hash-map)))
                   ((listp in)
                    (append (get-adjacent-for-this-room (car in))
                            (get-adjacent-for-this-room (cdr in))))))
           (get-adjacent-for-n-removed (in n acc)
             (cond ((= 0 n)
                    (remove-duplicates acc))
                   (t (let ((curr (get-adjacent-for-this-room in)))
                        (get-adjacent-for-n-removed curr (decf n)
                                                    (append curr acc)))))))
    (cond (n (get-adjacent-for-n-removed room n '()))
          (t (get-adjacent-for-this-room room)))))

(defun handle-player-input (parsed-input)
  (destructuring-bind (move-type move-rest)
      parsed-input (cond ((char= move-type #\m)
                          (move-player (first move-rest)))
                         ((char= move-type #\s)
                          (use-up-one-arrow)
                          (shoot-arrow (car move-rest)
                                       (cdr move-rest)
                                       (arrow-range))))))

(defun shoot-arrow (in targets range)
  (if (> range 0)
      (or (check-arrow-hit in range)
          (cond ((null targets)
                 (shoot-arrow (randomly-select-adjacent-room in) '() (decf range)))
                ((consp targets)
                 (shoot-arrow (car targets) (cdr targets) (decf range)))
                ((atom targets)
                 (shoot-arrow targets '() (decf range)))))
      (player-location)))

(defun check-arrow-hit (in range)
  (cond ((= in (wumpus-location)) 'death-of-wumpus)
        ((= in (player-location)) 'death-by-self-hit)
        ((and (= 0 range) (member (wumpus-location)
                                  (get-adjacent-rooms in)))
         (if (roll-the-dice 75) (move-wumpus-one-room)))
        (t nil)))

(defun move-player (to)
  (if (member to (get-adjacent-rooms (player-location)))
      (set-player-location to)
      (progn (txt 'hazard-wall-bump)
             (when (roll-the-dice 75)
               (move-wumpus-one-room))
             (player-location))))

;;;; PROMPT
(defun prompt-player () ; nag player until valid input is received.
  (loop for player-input = (%prompt (get-text 'player-prompt))
     do (destructuring-bind (move-type move-rest)
            (list (parse-move player-input)
                  (parse-directions player-input))
          (when move-type
           (unless move-rest
             (loop until move-rest do
                  (setf move-rest
                        (parse-directions (%prompt (get-text 'player-prompt-again)))))))
          (when (and move-type move-rest)
            ;; adjust direction indices down one.
            (return-from nil (list move-type (mapcar #'1- move-rest)))))))

(defun parse-move (input-string)
  (when (> (length input-string) 0)
    (let ((move-char (coerce (subseq input-string 0 1) 'character)))
      (when (member move-char '(#\m #\s)) move-char))))

(defun parse-directions (input-string)
  ;; negative and zero (= -1 after index adjustment) values are pruned from input.
  (remove-if-not #'(lambda (n) (and (numberp n) (/= 0 n) (= (abs n) n)))
    (mapcar #'(lambda (value) (parse-integer value :junk-allowed t))
            (loop for start = 0 then (1+ finish)
               for finish = (position #\space input-string :start start)
               collecting (subseq input-string start finish)
               until (null finish)))))

(defun %prompt (prompt-string)
  (format t prompt-string) (string-trim '(#\space) (read-line nil nil)))
