# cl-hunt-the-wumpus
The classic game of Hunt The Wumpus

Created under influence from fond memories, Land of Lisp and other sources. Depends on `cl-kreg`, a simple graph generator for level structure.

Clone the repository to your local directory where ASDF can find it. Load & start it with:

```
(asfd:load-system :cl-wumpus)
(cl-wumpus:start-game)
```

Depends on cl-kreg for level generation.

# todo

- some events that are present in the `bsdgames` version are not included. 
- The 3 tunnels/25 rooms default cannot result in a regular graph.
  Either:
  - add check to generate-random-cave and warn the player
  - add different level generation methods