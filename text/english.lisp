(in-package :cl-wumpus)

(defparameter *text-en*
  '((instruction-query "~&Instructions?")
    (play-again-query "~&Play again?")
    (same-cave-query "~&In the same cave?")
    (game-start-intro "~%You're in a cave with ~a rooms and ~a tunnels leading from each room.
There are ~a bats and ~a pits scattered throughout the cave, and your
quiver holds ~a custom super anti-evil Wumpus arrows.~%~%")
    (room-description "~%You are in room ~a of the cave and have ~a arrow left.")
    (room-pathways "~&There are tunnels to rooms ~{~a~^, ~} and ~a")
    (player-prompt "~&Move or shoot? (m-s) ")
    (unrecognized-move "~&Unrecognized move.")
    (player-prompt-again "~&Where? ")
    (hazard-bats-nearby "~%*rustle* *rustle* (must be bats nearby.)")
    (hazard-pits-nearby "~%*whooosh* (I feel a draft from some pits.)")
    (hazard-wumpus-nearby "~%*sniff* (I can smell the evil Wumpus nearby!)")
    (hazard-bats-carry "~&*flap*  *flap* (humongous bats pick you up and move you!)~%")
    (hazard-wall-bump "~&*ooph!* You bumped into a wall!")
    (hazard-wumpus-wake"~&Your colorful comments awaken the wumpus!")
    (death-by-pit-avoided "~&You fell into a pit, but held steady into a root!!")
    (death-by-pit "*AAAUUUUGGGGGHHHHHhhhhhhhhhh...*
The whistling sound and updraft as you walked into this room of the
cave apparently wasn't enough to clue you in to the presence of the
bottomless pit.  You have a lot of time to reflect on this error as
you fall many miles to the core of the earth.  Look on the bright
side; you can at least find out if Jules Verne was right...")
    (death-by-quiver-empty "You turn and look at your quiver, and realize with a sinking feeling
that you've just shot your last arrow (figuratively, too).  Sensing this
with its psychic powers, the evil Wumpus rampagees through the cave, finds
you, and with a mighty *ROAR* eats you alive!")
    (death-by-self-hit "*Thwack!*  A sudden piercing feeling informs you that the ricochet
of your wild arrow has resulted in it wedging in your side, causing
extreme agony.  The evil Wumpus, with its psychic powers, realizes this
and immediately rushes to your side, not to help, alas, but to EAT YOU!
\(*CHOMP*\)")
    (death-by-wumpus "~&*ROAR* *chomp* *snurfle* *chomp*!~%
Much to the delight of the Wumpus, you walked right into his mouth,
making you one of the easiest dinners he's ever had!  For you, however,
it's a rather unpleasant death.  The only good thing is that it's been
so long since the evil Wumpus cleaned his teeth that you immediately
passed out from the stench!")
    (death-of-wumpus "*thwock!* *groan* *crash*~%
A horrible roar fills the cave, and you realize, with a smile, that you
have slain the evil Wumpus and won the game!  You don't want to tarry for
long, however, because not only is the Wumpus famous, but the stench of
dead Wumpus is also quite well known, a stench plenty enough to slay the
mightiest adventurer at a single whiff!!")
    (instruction-welcome "  Welcome to the game of Hunt the Wumpus! 
~%The Wumpus typically lives in a cave of twenty rooms, with each room having
three tunnels connecting it to other rooms in the cavern.  Caves may vary,
however, depending on options specified when starting the game.
~%The game has the following hazards for intrepid adventurers to wind their
way through:
~%  Pits   -- If you fall into one of the bottomless pits, you find yourself
            slung back out on the far side of the Earth and in very poor
            shape to continue your quest since you're dead.
~%  Bats   -- As with any other cave, the Wumpus cave has bats in residence.
            These are a bit more potent, however, and if you stumble into
            one of their rooms they will rush up and carry you elsewhere in
            the cave.
~%  Wumpus -- If you happen to walk into the room the Wumpus is in you'll find
            that he has quite an appetite for young adventurous humans!  Not
            recommended.
~%The Wumpus, by the way, is not bothered by the hazards since he has sucker
feet and is too big for a bat to lift.  If you try to shoot him and miss,
there's also a chance that he'll up and move himself into another cave,
though by nature the Wumpus is a sedentary creature.
~%Each turn you may either move or shoot a crooked arrow.  Moving is done
simply by specifying \"m\" for move and the number of the room that you'd
like to move down a tunnel towards.  Shooting is done similarly; indicate
that you'd like to shoot one of your magic arrows with an \"s\" for shoot,
then list a set of connected room numbers through which the deadly shaft
should fly!
~%If your path for the arrow is incorrect, however, it will flail about in
the room it can't understand and randomly pick a tunnel to continue
through.  You might just end up shooting yourself in the foot if you're
not careful!  On the other hand, if you shoot the Wumpus you've WON!
~%Good luck.")))
