Kruskal's Algorithm
-------------------

Given a non-directed, connected weighted graph, construct a minimal spanning tree


Sample Usage
------------

~~~
dow@dow-laptop ~/haskell/kruskal $ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :l Kruskal.hs 
[1 of 1] Compiling Kruskal          ( Kruskal.hs, interpreted )
Ok, modules loaded: Kruskal.

*Kruskal> txt <- readFile "ndg3.txt" 
*Kruskal> let g = fromText txt
*Kruskal> let tree = kruskal g

*Kruskal> tree
(["A","B","E","F","C","D","G","I","H","J","K"],
[(["I","J"],378.0),(["E","I"],375.0),(["J","K"],365.0),
(["B","C"],360.0),(["F","J"],350.0),(["G","K"],345.0),
(["A","B"],332.0),(["C","D"],330.0),(["H","K"],320.0),
(["A","F"],315.0)])

*Kruskal> totalWeight tree
3470.0
~~~
