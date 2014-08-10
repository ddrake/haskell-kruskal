module Kruskal
(
  Node,
  Edge,
  Graph,
  fromText,
  kruskal,
  totalWeight,
)
where

import Data.List
import System.IO

type Node = String
type Edge = ([Node], Float)
type Graph = ([Node], [Edge])

-- Get a weighted, non-directed graph from a multiline text string, 
-- where each line specifies two nodes and a weight
fromText :: String -> Graph
fromText strLines = 
  let readData [n1, n2, w] = ([n1, n2], read w :: Float)
      es = map (readData . words) $ lines strLines
  in fromList es

-- Construct a weighted graph from a list of pairs where the first element is a two-member list 
-- of nodes in any order and the second element is the weight for the edge connecting them.
fromList :: [([String], Float)] -> Graph
fromList es =
  let ns = nub . concatMap fst $ es
  in (ns, es)    

-- given a list of edges and a node, return a sublist of the edges which are incident on the node
incidentEdges :: [Edge] -> Node -> [Edge]
incidentEdges es n = filter (\(ns,_) -> n `elem` ns) es

-- given a list of all edges in a subgraph (not necessarily connected) and a list of connected nodes, 
-- return all edges in the connected component containing the nodes
connectedEdges :: [Edge] -> [Node] -> [Edge]
connectedEdges es ns = 
  let incidents = nub . concatMap (incidentEdges es) $ ns
      nodes = nodesInEdges incidents
  in  if length nodes == length ns then incidents
      else connectedEdges es nodes

-- check the edges of a connected component to see if it has any cycles
containsCycles :: [Edge] -> Bool
containsCycles es = length es >= length (nodesInEdges es) 

-- assumes the edge has already been added ro the list
containsCyclesWithEdge :: [Edge] -> Edge -> Bool
containsCyclesWithEdge es (ns,_) = 
    let cnEs = connectedEdges es ns
    in containsCycles cnEs

-- given a list of edges, return a list of the nodes they are incident on
nodesInEdges :: [Edge] -> [Node]
nodesInEdges = nub . concatMap (\(ns,_) -> ns)


-- Kruskal's algorithm
-- given a connected, weighted, non-directed graph, construct a minimum spanning tree
-- by starting with a new graph which has the original set of nodes but no edges
-- then sequentially adding minimal weight edges from the original graph, 
-- while ensuring at each step that no cycles are created.
-- the algorithm is complete when the number of edges is one less than the number of nodes.
kruskal :: Graph -> Graph
kruskal ([],_) = error "Graph contains no nodes"
kruskal g@(ns,es) = kruskal' g (ns,[])

kruskal' :: Graph -> Graph -> Graph
kruskal' gOrig gNew
  | nodeCt gNew == 1 + edgeCt gNew = gNew
  | otherwise =
      let gNew' = addEdge gOrig gNew
      in kruskal' gOrig gNew'

addEdge :: Graph -> Graph -> Graph
addEdge gOrig@(nsO, esO) gNew@(nsN, esN) =
  let edges = sortBy (\(_,w1) (_,w2) -> compare w1 w2) (esO \\ esN)
  in addFirstNonCycling edges gNew

addFirstNonCycling :: [Edge] -> Graph -> Graph
addFirstNonCycling (e@(ens,_):rest) g@(ns,es) = 
  let es' = e:es
      g' = (ns,es')
      cyclesWithEdge = containsCycles (connectedEdges es' ens)
  in  if cyclesWithEdge then addFirstNonCycling rest g
      else g'


nodeCt :: Graph -> Int
nodeCt (ns,_) = length ns

edgeCt :: Graph -> Int
edgeCt (_,es) = length es

totalWeight :: Graph -> Float
totalWeight (_,es) = sum . map (\(_,w) -> w) $ es
