;;; ------------------------------
;;; CLASS DEFINITIONS
;;; ------------------------------

;;; A generic deck class.
(defclass deck () ())

;;; A shoe is composed of many decks.
(defclass shoe (deck) ())

;;; A deck that allows the user to select the next card.
(defclass debug-deck (deck) ())

;;; A deck has a set number of cards.
(defclass single-deck (deck)
  ((cards :accessor single-deck-cards :initarg :cards :initform (populate-deck))))

;;; A generic player class.
(defclass player ()
  ((hands :accessor player-hands :initform ())
   (bets :accessor player-bets :initform nil)
   (cash :accessor player-cash :initarg :cash :initform 0)))

;;; A human player.
(defclass human-player (player) ())

;;; A random player.
(defclass random-player (player) ())

;;; A heuristic player.
(defclass heuristic-player (player) ())

;;; A learning player.
(defclass learning-player (player)
  ((initial-rules :accessor learning-player-initial-rules :initarg :initial-rules :initform ())
   (subsequent-rules :accessor learning-player-subsequent-rules :initarg :subsequent-rules :initform ())
   ; set mode=t to turn on heuristic use
   (mode :accessor learning-player-mode :initarg :mode :initform nil)))

;;; A dealer.
;;; The dealer's first card is face-down.
(defclass dealer ()
  ((deck :accessor dealer-deck :initarg :deck :initform ())
   (hand :accessor dealer-hand :initarg :hand :initform ())))

;;; ------------------------------
;;; DECK METHODS
;;; ------------------------------

;;; Generate a standard deck of playing cards.
(defmethod populate-deck (&aux cards suits ranks)
  (setf suits '(hearts diamonds clubs spades))
  (setf ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))
  (dolist (suit suits)
    (dolist (value ranks)
      (setf cards (cons (cons value suit) cards))))
  cards)

;;; Draw a card from a single deck.
(defmethod draw ((d single-deck) &aux card)
  (setf card (car (single-deck-cards d)))
  (setf (single-deck-cards d) (cdr (single-deck-cards d)))
  card)

;;; Draw a card from a shoe.
(defmethod draw ((s shoe))
  (let
    ((suits '(hearts diamonds clubs spades))
    (ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace)))
      (cons
        (nth (random (length ranks)) ranks)
        (nth (random (length suits)) suits))))

;;; Return a random permutation of the given list lst.
(defun randomize (l)
  (cond
    ((null l) ())
    (t 
      (let ((e (nth (random (length l)) l)))
        (cons e (randomize (remove e l :count 1)))))))

;;; Shuffle the deck randomly.
(defmethod shuffle ((d single-deck))
  (setf (single-deck-cards d) (randomize (single-deck-cards d)))
  nil)

;;; Deal two cards.
(defmethod deal ((d deck))
  (list (draw d) (draw d)))

;;; Give a player another card.
(defmethod add-one-card ((d deck) (hand list))
  (cons (draw d) hand))

;;; Give a print representation of a card.
(defmethod card-to-string ((card list))
  (format nil "~A of ~A" (car card) (cdr card)))

;;; Allow the user to pick a card to draw.
(defmethod draw ((dd debug-deck))
  (format t "Select a card to draw.~%> ")
  (read))

;;; ------------------------------
;;; DEALER METHODS
;;; ------------------------------

(defmethod deal-to-player ((dl dealer) (pl player))
  (setf (player-hands pl) (list (deal (dealer-deck dl)))))

(defmethod deal-to-self ((dl dealer))
  (setf (dealer-hand dl) (deal (dealer-deck dl))))

(defmethod hit ((dl dealer) (pl player))
  (setf (first (player-hands pl)) (add-one-card (dealer-deck dl) (first (player-hands pl)))))

(defmethod draw-one ((dl dealer))
  (setf (dealer-hand dl) (add-one-card (dealer-deck dl) (dealer-hand dl))))

(defmethod display ((dl dealer))
  (format t "The dealer is showing: ~A~%~%" (card-to-string (second (dealer-hand dl)))))

;;; ------------------------------
;;; PLAYER METHODS
;;; ------------------------------

(defmethod display ((pl player) &aux type)
  (dolist (hand (player-hands pl))
    (progn
      (format t "Your cards:~%")
      ;(loop for card in (mapcar #'card-to-string hand) do
      ;  (format t "~A~%" card))
      (mapc (lambda (card)
              (princ card)
              (fresh-line))
            (mapcar #'card-to-string hand))
      (format t "That's a ")
      (setf type (evaluate-hand hand))
      (if (or (eq type 'soft) (eq type 'hard))
          (format t "~A ~A.~%" type (calculate-total hand))
          (format t "~A.~%" type))))
  (format t "Cash: ~A~%~%" (player-cash pl)))

(defmethod display1 ((pl player) &aux type)
  (format t "Your cards:~%")
  (dolist (card (mapcar #'card-to-string (player-current-hand pl)))
    (format t "~A~%" card))
  (format t "That's a ")
  (setf type (evaluate-hand (player-current-hand pl)))
  (if (or (eq type 'soft) (eq type 'hard))
      (format t "~A ~A.~%" type (calculate-total (player-current-hand pl)))
      (format t "~A.~%" type))
  (format t "Bet on hand: ~A.~%" (player-current-bet pl))
  (format t "Cash: ~A.~%~%" (player-cash pl)))

;;; Return the hand the player is currently playing.
(defmethod player-current-hand ((pl player))
  (first (player-hands pl)))

;;; Return the player's bet on the current hand.
(defmethod player-current-bet ((pl player))
  (first (player-bets pl)))

;;; ------------------------------
;;; GENERAL PLAY METHODS
;;; ------------------------------

;;; Set up some sample players.
(defmethod set-up ()
  (setf dl (make-instance 'dealer :deck (make-instance 'shoe)))
  (setf rmp (make-instance 'random-player :cash 100))
  (setf hup (make-instance 'human-player :cash 100))
  (setf hmp (make-instance 'heuristic-player :cash 100)))

;;; Play a game of blackjack.
(defmethod play-game ((dl dealer) (pl player) &aux starting-cash)
  (setf starting-cash (player-cash pl))
  (deal-to-player dl pl)
  (deal-to-self dl)
  (loop while (not (null (player-hands pl))) do
    (play-hand dl pl))
  (format t "Game over.~%")
  (format t "Cash remaining: ~A~%" (player-cash pl))
  (format t "Difference from starting cash: ~A~%"
    (- (player-cash pl) starting-cash))
  nil)

;;; Play a hand of blackjack.
(defmethod play-hand ((dl dealer) (pl player) &aux bet action result total dealer-total starting-cash)
  (setf starting-cash (player-cash pl))
  (when (= 1 (length (player-current-hand pl)))
    (hit dl pl))
  (display dl)
  (display1 pl)
  (make-bet pl)
  (format t "Betting ~A.~%" (player-current-bet pl))
  (setf action (pick-initial-action pl dl))
  ;; If the player doubles down, double its bet and end the hand.
  (when (eq action 'dd)
    (setf (player-bets pl) (cons (* 2 (player-current-bet pl)) (cdr (player-bets pl))))
    (setf (player-cash pl) (- (player-cash pl) (player-current-bet pl)))
    (hit dl pl))
  (when (eq action 'ht)
    (hit dl pl))
  (when (eq action 'sp)
    (setf (player-hands pl) (append (player-hands pl) (list (list (second (first (player-hands pl)))))))
    (setf (player-bets pl) (append (player-bets pl) (list (player-current-bet pl))))
    (setf (player-cash pl) (- (player-cash pl) (player-current-bet pl)))
    (hit dl pl))
  (setf result (evaluate-hand (first (player-hands pl))))
  (display1 pl)
  ;; Let the player continue hitting or end when it stands, busts, or gets a blackjack.
  (loop while (not (or (eq action 'dd) (eq action 'st) (eq result 'bust) (eq result 'blackjack))) do
    (setf action (pick-subsequent-action pl dl))
    (when (eq action 'ht)
      (hit dl pl))
    (setf result (evaluate-hand (first (player-hands pl))))
    (display1 pl))
  ;; The hand is now over.  Let the dealer draw and then calculate the result.
  (setf total (calculate-total (first (player-hands pl))))
  (setf dealer-total (calculate-total (dealer-hand dl)))
  (loop while (< dealer-total 17) do
    (progn
      (draw-one dl)
      (setf dealer-total (calculate-total (dealer-hand dl)))))
  (display1 pl)
  (format t "Dealer's total: ~A~%" dealer-total)
  (determine-outcome pl dl)
  (setf (player-cash pl) (+ (player-cash pl) (first (player-bets pl))))
  (setf (player-bets pl) (cdr (player-bets pl)))
  (setf (player-hands pl) (cdr (player-hands pl)))
  (format t "Player's cash after this hand: ~A~%" (player-cash pl))
  (format t "That's a difference of ~A.~%" (- (player-cash pl) starting-cash))
  nil)
  

;;; Calculate the total value of a hand.
(defmethod calculate-total ((hand list) &aux values total)
  (setf values (mapcar #'car hand))
  (setf values (substitute 10 'king values))
  (setf values (substitute 10 'queen values))
  (setf values (substitute 10 'jack values))
  (setf values (substitute 11 'ace values))
  (setf total (reduce #'+ values))
  (loop while (and (> total 21) (member 11 values)) do
    (progn
      (setf values (substitute 1 11 values :count 1))
      (setf total (reduce #'+ values))))
  total)

;;; Determine if a hand is soft.
;;; A hand is soft if it contains an Ace that can be counted as 11 without busting.
(defmethod softp ((hand list))
  (setf values (mapcar #'car hand))
  (setf values (substitute 10 'king values))
  (setf values (substitute 10 'queen values))
  (setf values (substitute 10 'jack values))
  (setf values (substitute 11 'ace values))
  (setf total (reduce #'+ values))
  (and
    (member 11 values)
    (<= total 21)))

;;; Determine if a hand is a pair, a blackjack, a bust, soft, or hard.
(defmethod evaluate-hand ((hand list) &aux ranks total)
  (setf ranks (mapcar #'car hand))
  (setf total (calculate-total hand))
  (cond
    ((and (= (length ranks) 2) (eq (first ranks) (second ranks)))
      'pair)
    ((or (eq ranks '(jack ace)) (eq ranks '(ace jack)))
      'blackjack)
    ((> total 21)
      'bust)
    ((softp hand)
      'soft)
    (t
      'hard)))

;;; Determine the result of a hand.
(defmethod determine-outcome ((pl player) (dl dealer) &aux total dealer-total)
  (setf total (calculate-total (first (player-hands pl))))
  (setf dealer-total (calculate-total (dealer-hand dl)))
  (cond
    ((eq (evaluate-hand (first (player-hands pl))) 'blackjack)
      (setf (player-bets pl)  (cons (* (/ 5 2) (player-current-bet pl) (cdr (player-bets pl))))))
    ((and (<= total 21) (> dealer-total 21))
      (setf (player-bets pl) (cons (* 2 (player-current-bet pl)) (cdr (player-bets pl)))))
    ((and (> total 21) (<= dealer-total 21))
      (setf (player-bets pl) (cons 0 (cdr (player-bets pl)))))
    ((> total dealer-total)
      (setf (player-bets pl) (cons (* 2 (player-current-bet pl)) (cdr (player-bets pl)))))
    ((< total dealer-total)
      (setf (player-bets pl) (cons 0 (cdr (player-bets pl))))))
  nil)

;;; ------------------------------
;;; HUMAN PLAY METHODS
;;; ------------------------------

;;; Set up a hand for a human to play.
(defmethod human-test ()
  (setf pl (make-instance 'human-player :cash 100))
  (setf dl (make-instance 'dealer :deck (make-instance 'shoe)))
  (play-game dl pl))

;;; Allow a human player to select a bet.
(defmethod make-bet ((pl human-player) &aux bet)
  (loop while (null bet) do
    (progn
      (format t "Select a bet between 5 and 100.~%> ")
      (setf bet (read))
      (if (numberp bet)
          (cond 
            ((or (< bet 5) (> bet 100))
              (format t "That is outside the betting range.~%"))
            ((> bet (player-cash pl))
              (format t "You don't have that much money.~%")))
          (progn
            (format t "That is not a number.~%")
            (setf bet nil)))))
      (setf (player-cash pl) (- (player-cash pl) bet))
      (setf (player-bets pl) (cons bet (player-bets pl))))

;;; Allow a human player to select its initial move.
(defmethod pick-initial-action ((pl human-player) (dl dealer) &aux moves action)
  (setf moves '(ht st dd))
  (when (eq (evaluate-hand (player-current-hand pl)) 'pair)
    (setf moves (cons 'sp moves)))
  (loop while (null action) do
    (format t "Select an action: ~A~%> " moves)
    (setf action (read))
    (when (not (member action moves))
      (format t "That's not an available action.~%")
      (setf action nil))
    ;; Can the player double-down?
    (when (and (eq action 'dd) (> (player-current-bet pl) (player-cash pl)))
      (format t "You don't have enough cash to double down.~%")
      (setf action nil)))
  action)

;;; Allow a human to pick actions after the first action.
(defmethod pick-subsequent-action ((pl human-player) (dl dealer) &aux moves action)
  (setf moves '(ht st))
  (loop while (null action) do
    (format t "Select an action: ~A~%> " moves)
    (setf action (read))
    (when (not (member action moves))
      (format t "That's not an available action.")
      (setf action nil)))
  action)

;;; ------------------------------
;;; RANDOM PLAY METHODS
;;; ------------------------------

;;; Set up a game with a random player.
(defmethod random-test ()
  (setf pl (make-instance 'random-player :cash 100))
  (setf dl (make-instance 'dealer :deck (make-instance 'shoe)))
  (play-game dl pl))

;;; Randomly choose an amount to bet.
(defmethod make-bet ((pl random-player) &aux bet)
  (setf bet (random (player-cash pl)))
  (setf (player-cash pl) (- (player-cash pl) bet))
  (setf (player-bets pl) (cons bet (player-bets pl))))

;;; Randomly choose a first action.
(defmethod pick-initial-action ((pl random-player) (dl dealer) &aux moves action)
  (setf moves '(st ht))
  (when (<= (player-current-bet pl) (player-cash pl))
    (setf moves (cons 'dd moves)))
  (setf action (nth (random (length moves)) moves))
  (format t "Action: ~A~%" action)
  action)

;;; Randomly choose a subsequent action.
(defmethod pick-subsequent-action ((pl random-player) (dl dealer) &aux action)
  (setf action (nth (random 2) '(st dd)))
  (format t "Action: ~A~%" action)
  action)

;;; ------------------------------
;;; HEURISTIC PLAY METHODS
;;; ------------------------------

;;; Set up a game with a heuristic player.
(defmethod heuristic-test ()
  (setf pl (make-instance 'heuristic-player :cash 100))
  (setf dl (make-instance 'dealer :deck (make-instance 'shoe)))
  (play-game dl pl))

;;; Let a heuristic player choose a bet.
(defmethod make-bet ((pl heuristic-player) &aux bet)
  (setf bet (/ (player-cash pl) 2))
  (setf (player-cash pl) (- (player-cash pl) bet))
  (setf (player-bets pl) (cons bet (player-bets pl))))

;;; Let a heuristic player pick its first move.
(defmethod pick-initial-action ((pl heuristic-player) (dl dealer) &aux action rank hand-type hand-total dealer-card)
  (setf hand-type (evaluate-hand (first (player-hands pl))))
  (setf hand-total (calculate-total (first (player-hands pl))))
  (setf dealer-card (first (first (dealer-hand dl))))
  (when (member dealer-card '(jack queen king))
    (setf dealer-card 10))
  (cond
    ((and (eq hand-type 'pair) (>= (player-cash pl) (player-current-bet pl)))
      (setf rank (first (first (first (player-hands pl)))))
      (when (member rank '(jack queen king))
        (setf rank 10))
      (cond
        ((eq rank 'ace) (setf action 'sp))
        ((= rank 10) (setf action 'st))
        ((= rank 9)
          (cond
            ((member dealer-card '(7 10 ace))
              (setf action 'st))
            (t (setf action 'sp))))
        ((= rank 8) (setf action 'sp))
        ((= rank 7)
          (cond
            ((member dealer-card '(8 9 10 'ace))
              (setf action 'ht))
            (t (setf action 'sp))))
        ((= rank 6) (setf action 'ht))
        ((= rank 5)
          (cond
            ((member dealer-card '(10 ace))
              (setf action 'ht))
            (t (setf action 'dd))))
        ((= rank 4)
          (cond
            ((member dealer-card '(5 6))
              (setf action 'sp))
            (t (setf action 'ht))))
        ((= rank 3)
          (cond
            ((member dealer-card '(8 9 10 ace))
              (setf action 'ht))
            (t (setf action 'sp))))
        ((= rank 2)
          (cond
            ((member dealer-card '(8 9 10 ace))
              (setf action 'ht))
            (t (setf action 'sp))))))
    ((eq hand-type 'hard)
      (cond
        ((>= hand-total 17) (setf action 'st))
        ((member hand-total '(13 14 15 16))
          (cond
            ((member dealer-card '(2 3 4 5 6)) (setf action 'st))
            (t (setf action 'ht))))
        ((= hand-total 12)
          (cond
            ((member dealer-card '(4 5 6)) (setf action 'st))
            (t (setf action 'ht))))
        ((= hand-total 11)
          (cond
            ((eq dealer-card 'ace) (setf action 'ht))
            (t (setf action 'dd))))
        ((= hand-total 10)
          (cond
            ((member dealer-card '(ace 10)) (setf action 'ht))
            (t (setf action 'dd))))
        ((= hand-total 9)
          (cond
            ((member dealer-card '(2 7 8 9 10 ace)) (setf action 'ht))
            (t (setf action 'dd))))
        (t (setf action 'ht))))
    ((eq hand-type 'soft)
      (cond
        ((member hand-total '(19 20))
          (cond
            ((member dealer-card '(2 3 4 5 6)) (setf action 'dd))
            (t (setf action 'st))))
        ((= hand-total 18)
          (cond
            ((member dealer-card '(2 3 4 5 6)) (setf action 'dd))
            ((member dealer-card '(7 8)) (setf action 'st))
            (t (setf action 'ht))))
        ((= hand-total 17)
          (cond
            ((member dealer-card '(3 4 5 6)) (setf action 'dd))
            (t (setf action 'ht))))
        ((member hand-total '(16 15))
          (cond
            ((member dealer-card '(4 5 6)) (setf action 'dd))
            (t (setf action 'ht))))
        (t
          (cond
            ((member dealer-card '(5 6)) (setf action 'dd))
            (t (setf action 'ht)))))))
  (when (and (eq action 'dd) (> (player-current-bet pl) (player-cash pl)))
    (setf action 'ht))
  action)

;;; Let a heuristic player pick another move.
(defmethod pick-subsequent-action ((pl heuristic-player) (dl dealer) &aux hand-type hand-total dealer-card)
  (setf hand-type (evaluate-hand (first (player-hands pl))))
  (setf hand-total (calculate-total (first (player-hands pl))))
  (setf dealer-card (first (first (dealer-hand dl))))
  (when (member dealer-card '(jack queen king))
    (setf dealer-card 10))
  (cond
    ((eq hand-type 'hard)
      (cond
        ((>= hand-total 17) (setf action 'st))
        ((member hand-total '(13 14 15 16))
          (cond
            ((member dealer-card '(2 3 4 5 6)) (setf action 'st))
            (t (setf action 'ht))))
        ((= hand-total 12)
          (cond
            ((member dealer-card '(4 5 6)) (setf action 'st))
            (t (setf action 'ht))))
        ((= hand-total 11)
          (cond
            ((eq dealer-card 'ace) (setf action 'ht))
            (t (setf action 'ht))))
        ((= hand-total 10)
          (cond
            ((member dealer-card '(ace 10)) (setf action 'ht))
            (t (setf action 'ht))))
        ((= hand-total 9)
          (cond
            ((member dealer-card '(2 7 8 9 10 ace)) (setf action 'ht))
            (t (setf action 'ht))))
        (t (setf action 'ht))))
    ((eq hand-type 'soft)
      (cond
        ((member hand-total '(19 20))
          (cond
            ((member dealer-card '(2 3 4 5 6)) (setf action 'ht))
            (t (setf action 'st))))
        ((= hand-total 18)
          (cond
            ((member dealer-card '(2 3 4 5 6)) (setf action 'ht))
            ((member dealer-card '(7 8)) (setf action 'st))
            (t (setf action 'ht))))
        ((= hand-total 17)
          (cond
            ((member dealer-card '(3 4 5 6)) (setf action 'ht))
            (t (setf action 'ht))))
        ((member hand-total '(16 15))
          (cond
            ((member dealer-card '(4 5 6)) (setf action 'ht))
            (t (setf action 'ht))))
        (t
          (cond
            ((member dealer-card '(5 6)) (setf action 'ht))
            (t (setf action 'ht)))))))
  action)

;;; ------------------------------
;;; LEARNING PLAY METHODS
;;; ------------------------------

(defun run-tests (show n)
  (let ((success nil) (successes 0))
    (learning-setup)
    (dotimes (x n)
      (when (play-with-learning m d)
        (setf success t))
      (when show (format t "~A: ~A~%" x success))
      (when success (setf successes (1+ successes)))
      (setf success nil))
    successes))

(defun test-heuristics (show n)
  (let ((success nil) (successes 0))
    (setf (learning-player-mode m) t)
    (dotimes (x n)
      (when (play-without-learning m d)
        (setf success t))
      (when show (format t "~A: ~A~%" x success))
      (when success (setf successes (1+ successes)))
      (setf success nil))
    successes))

(defun test-book (show n)
  (setf b (make-instance 'heuristic-player :cash 100))
  (setf d (make-instance 'dealer :deck (make-instance 'shoe)))
  (let ((success nil) (successes 0))
    (dotimes (x n)
      (when (play-without-learning b d)
        (setf success t))
      (when show (format t "~A: ~A~%" x success))
      (when success (setf successes (1+ successes)))
      (setf success nil))
    successes))

;;; Set up a game with a learning player.
(defmethod learning-test ()
  (setf pl (make-instance 'learning-player :cash 100))
  (setf (learning-player-initial-rules pl) (copy-tree (create-initial-rule-base)))
  (setf (learning-player-subsequent-rules pl) (copy-tree (create-subsequent-rule-base)))
  (setf dl (make-instance 'dealer :deck (make-instance 'shoe)))
  (play-game dl pl))

;;; Set up a game with a learning player and a dealer.
(defmethod learning-setup ()
  (setf m (make-instance 'learning-player
    :cash 100
    :mode nil
    :initial-rules (copy-tree (create-initial-rule-base))
    :subsequent-rules (copy-tree (create-subsequent-rule-base))))
  (setf d (make-instance 'dealer
    :deck (make-instance 'shoe)))
  nil)

;;; Create an initial rule base.
;;; A three-dimensional matrix represented as a triply-nested list.
;;; Outermost layer: each "row" represents the player's hand.
;;; Middle layer: each "column" represents the dealer's hand.
;;; Innermost layer: each... "third-dimensional-thingy" represents the action taken, of (ht st dd sp).
(defmethod create-initial-rule-base ()
  (make-list 28
    :initial-element (make-list 10
      :initial-element (make-list 4 :initial-element 0.25))))

(defmethod create-subsequent-rule-base ()
  (make-list 18
    :initial-element (make-list 10
      :initial-element (make-list 2 :initial-element 0.5))))

;;; Let a learning player choose a bet.
(defmethod make-bet ((pl learning-player) &aux bet)
  (setf bet (/ (player-cash pl) 2))
  (setf (player-cash pl) (- (player-cash pl) bet))
  (setf (player-bets pl) (cons bet (player-bets pl))))

;;; Given the player, return the index of the appropriate row in the rule base.
(defmethod initial-player-hand-index ((pl learning-player) &aux hand-type hand-total rank)
  (setf hand-type (evaluate-hand (first (player-hands pl))))
  (setf hand-total (calculate-total (first (player-hands pl))))
  (cond
    ((eq hand-type 'pair)
      (setf rank (first (first (first (player-hands pl)))))
      (when (member rank '(jack queen king))
        (setf rank 10))
      (cond
        ((eq rank 'ace) 0)
        ((= rank 10) 1)
        ((= rank 9) 2)
        ((= rank 8) 3)
        ((= rank 7) 4)
        ((= rank 6) 5)
        ((= rank 5) 6)
        ((= rank 4) 7)
        ((= rank 3) 8)
        ((= rank 2) 9)))
    ((eq hand-type 'hard)
      (cond
        ((>= hand-total 17) 10)
        ((= hand-total 16) 11)
        ((= hand-total 15) 12)
        ((= hand-total 14) 13)
        ((= hand-total 13) 14)
        ((= hand-total 12) 15)
        ((= hand-total 11) 16)
        ((= hand-total 10) 17)
        ((= hand-total 9) 18)
        ((<= hand-total 8) 19)))
    ((eq hand-type 'soft)
      (cond
        ((= hand-total 21) 10)
        ((= hand-total 20) 20)
        ((= hand-total 19) 21)
        ((= hand-total 18) 22)
        ((= hand-total 17) 23)
        ((= hand-total 16) 24)
        ((= hand-total 15) 25)
        ((= hand-total 14) 26)
        ((= hand-total 13) 27)))))

(defmethod subsequent-player-hand-index ((pl learning-player) &aux hand-type hand-total rank)
  (setf hand-type (evaluate-hand (first (player-hands pl))))
  (setf hand-total (calculate-total (first (player-hands pl))))
  (cond
    ((eq hand-type 'hard)
      (cond
        ((>= hand-total 17) 0)
        ((= hand-total 16) 1)
        ((= hand-total 15) 2)
        ((= hand-total 14) 3)
        ((= hand-total 13) 4)
        ((= hand-total 12) 5)
        ((= hand-total 11) 6)
        ((= hand-total 10) 7)
        ((= hand-total 9) 8)
        ((<= hand-total 8) 9)))
    ((eq hand-type 'soft)
      (cond
        ((= hand-total 21) 0)
        ((= hand-total 20) 10)
        ((= hand-total 19) 11)
        ((= hand-total 18) 12)
        ((= hand-total 17) 13)
        ((= hand-total 16) 14)
        ((= hand-total 15) 15)
        ((= hand-total 14) 16)
        ((= hand-total 13) 17)))))

;;; Given the dealer, return the index of the appropriate column of the rule base.
(defmethod dealer-card-index ((dl dealer) &aux dealer-card)
  (setf dealer-card (first (first (dealer-hand dl))))
  (when (member dealer-card '(jack queen king))
    (setf dealer-card 10))
  (cond
    ((eq dealer-card 'ace) 0)
    ((= dealer-card 10) 1)
    ((= dealer-card 9) 2)
    ((= dealer-card 8) 3)
    ((= dealer-card 7) 4)
    ((= dealer-card 6) 5)
    ((= dealer-card 5) 6)
    ((= dealer-card 4) 7)
    ((= dealer-card 3) 8)
    ((= dealer-card 2) 9)))

;;; Given an action, return the index of that action.
(defmethod action-index ((action symbol))
  (cond
    ((eq action 'ht) 0)
    ((eq action 'st) 1)
    ((eq action 'dd) 2)
    ((eq action 'sp) 3)))

;;; Given a list of four probabilities, pick the move with the highest one.
(defmethod initial-highest-of ((l list) &aux highest)
  (setf highest 'ht)
  (when (> (second l) (first l))
    (setf highest 'st))
  (when (> (third l) (second l))
    (setf highest 'dd))
  (when (> (fourth l) (third l))
    (setf highest 'sp))
  highest)

(defmethod initial-random-from ((l list) &aux rand)
  (setf rand (/ (float (random 100)) 100.0))
  (cond
    ((<= rand (first l)) 'ht)
    ((<= rand (+ (first l) (second l))) 'st)
    ((<= rand (+ (first l) (second l) (third l))) 'dd)
    (t 'sp)))

(defmethod subsequent-highest-of ((l list))
  (if (>= (first l) (second l))
      'ht
      'st))

(defmethod subsequent-random-from ((l list) &aux rand)
  (setf rand (/ (float (random 100)) 100.0))
  (if (<= rand (first l))
      'ht
      'st))

;;; Let a learning player pick its first move.
(defmethod pick-initial-action ((pl learning-player) (dl dealer) &aux moves action hand-type)
  (setf moves '(st ht))
  (setf hand-type (evaluate-hand (first (player-hands pl))))
  (when (<= (player-current-bet pl) (player-cash pl))
    (setf moves (cons 'dd moves)))
  (when (and (eq hand-type 'pair) (>= (player-cash pl) (player-current-bet pl)))
    (setf moves (cons 'sp moves)))
  (if (learning-player-mode pl)
      ;; mode set to t: play using heuristics
      (setf action 
            (initial-random-from (nth (dealer-card-index dl) (nth (initial-player-hand-index pl) (learning-player-initial-rules pl)))))
      ;; mode set to nil: play randomly
      (setf action (nth (random (length moves)) moves)))
  action)

;;; Let a learning player pick subsequent moves.
(defmethod pick-subsequent-action ((pl learning-player) (dl dealer))
  (setf moves '(ht st))
  (if (learning-player-mode pl)
      (setf action (subsequent-random-from (nth (dealer-card-index dl) (nth (subsequent-player-hand-index pl) (learning-player-subsequent-rules pl)))))
      (setf action (nth (random (length moves)) moves)))
  action)

;;; Let a learning player play a hand and modify its rule base.
(defmethod play-with-learning ((pl learning-player) (dl dealer) &aux successful starting-cash action result total dealer-total data success p-h-index d-c-index)
  (setf starting-cash (values (player-cash pl)))
  (deal-to-player dl pl)
  (deal-to-self dl)
  (make-bet pl)
  (setf p-h-index (initial-player-hand-index pl))
  (setf d-c-index (dealer-card-index dl))
  (setf action (pick-initial-action pl dl))
  ;; If the player doubles down, double its bet and end the hand.
  (when (eq action 'dd)
    (setf (player-cash pl) (- (values (player-cash pl)) (player-current-bet pl)))
    (setf (player-bets pl) (cons (* 2 (player-current-bet pl)) (cdr (player-bets pl))))
    (hit dl pl))
  (when (eq action 'ht)
    (hit dl pl))
  (when (eq action 'sp)
    (setf (player-hands pl) (append (player-hands pl) (list (list (second (first (player-hands pl)))))))
    (hit dl pl))
  (setf result (evaluate-hand (first (player-hands pl))))
  (setf data (list (list (action-index action) p-h-index d-c-index)))
  ;; Let the player continue hitting or end when it stands, busts, or gets a blackjack.
  (loop while (not (or (eq action 'dd) (eq action 'st) (eq result 'bust) (eq result 'blackjack))) do
    (setf p-h-index (subsequent-player-hand-index pl))
    (setf d-c-index (dealer-card-index dl))
    (setf action (pick-subsequent-action pl dl))
    (when (eq action 'ht)
      (hit dl pl))
    (setf result (evaluate-hand (first (player-hands pl))))
    (setf data (append data (list (list (action-index action) p-h-index d-c-index)))))
  (setf total (calculate-total (first (player-hands pl))))
  (setf dealer-total (calculate-total (dealer-hand dl)))
  (loop while (< dealer-total 17) do
    (progn
      (draw-one dl)
      (setf dealer-total (calculate-total (dealer-hand dl)))))
  (determine-outcome pl dl)
  (setf (player-cash pl) (+ (player-cash pl) (first (player-bets pl))))
  (if (>= (values (player-cash pl)) starting-cash)
      (setf successful t)
      (setf successful nil))
  (update-data successful data pl)
  (setf (player-cash pl) starting-cash)
  (setf (player-hands pl) ())
  (setf (player-bets pl) nil)
  (setf (dealer-hand dl) ())
  successful)

;;; Play with the learning player, without modifying the rules.
(defmethod play-without-learning ((pl player) (dl dealer) &aux successful starting-cash action result total dealer-total data success)
  (setf starting-cash (values (player-cash pl)))
  (deal-to-player dl pl)
  (deal-to-self dl)
  (make-bet pl)
  (setf action (pick-initial-action pl dl))
  ;; If the player doubles down, double its bet and end the hand.
  (when (eq action 'dd)
    (setf (player-cash pl) (- (values (player-cash pl)) (player-current-bet pl)))
    (setf (player-bets pl) (cons (* 2 (player-current-bet pl)) (cdr (player-bets pl))))
    (hit dl pl))
  (when (eq action 'ht)
    (hit dl pl))
  (when (eq action 'sp)
    (setf (player-hands pl) (append (player-hands pl) (list (list (second (first (player-hands pl)))))))
    (hit dl pl))
  (setf result (evaluate-hand (first (player-hands pl))))
  ;; Let the player continue hitting or end when it stands, busts, or gets a blackjack.
  (loop while (not (or (eq action 'dd) (eq action 'st) (eq result 'bust) (eq result 'blackjack))) do
    (setf action (pick-subsequent-action pl dl))
    (when (eq action 'ht)
      (hit dl pl))
    (setf result (evaluate-hand (first (player-hands pl)))))
  (setf total (calculate-total (first (player-hands pl))))
  (setf dealer-total (calculate-total (dealer-hand dl)))
  (loop while (< dealer-total 17) do
    (progn
      (draw-one dl)
      (setf dealer-total (calculate-total (dealer-hand dl)))))
  (determine-outcome pl dl)
  (setf (player-cash pl) (+ (player-cash pl) (first (player-bets pl))))
  (if (>= (values (player-cash pl)) starting-cash)
      (setf successful t)
      (setf successful nil))
  (setf (player-cash pl) starting-cash)
  (setf (player-hands pl) ())
  (setf (player-bets pl) nil)
  (setf (dealer-hand dl) ())
  successful)

;;; Update the learning player's rule base.
(defmethod update-data ((success symbol) (data list) (pl learning-player) &aux datum)
  (setf datum (first data))
  (update-initial-rules success datum pl)
  (setf data (cdr data))
  (when (not (null data))
    (loop for datum in data do
      (update-subsequent-rules success datum pl))))

;;; Update an individual cell in the player's initial rule base.
(defmethod update-initial-rules ((success symbol) (data list) (pl learning-player) &aux act ph dc rule cell)
  (setf act (first data))
  (setf ph (second data))
  (setf dc (third data))
  (setf cell (nth dc (nth ph (learning-player-initial-rules pl))))
  (when success
    (setf (nth act cell) (1+ (nth act cell)))
    (setf (nth dc (nth ph (learning-player-initial-rules pl)))
          (mapcar (lambda (x)
                    (/ x 2))
                  cell))))

;;; Update an individual cell in the player's subsequent rule base.
(defmethod update-subsequent-rules ((success symbol) (data list) (pl learning-player) &aux act ph dc rule cell)
  (setf act (first data))
  (setf ph (second data))
  (setf dc (third data))
  (setf cell (nth dc (nth ph (learning-player-subsequent-rules pl))))
  (when success
    (setf (nth act cell) (1+ (nth act cell)))
    (setf (nth dc (nth ph (learning-player-subsequent-rules pl))) (mapcar #'(lambda (x) (/ x 2)) cell))))
