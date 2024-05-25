import Data.List
import Data.Ord

type Vertex = Int
type Weight = Int
type Graph = [[Weight]]

inf :: Weight
inf = maxBound :: Int

adjMatrix :: Graph
adjMatrix =
    [ [0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
              , [0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
              , [0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
              , [10, 0, 0, 0, 3, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0]
              , [0, 5, 0, 3, 0, 6, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0]
              , [0, 0, 5, 0, 6, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0]
              , [0, 0, 0, 12, 0, 0, 0, 11, 0, 14, 0, 0, 0, 0, 0, 0]
              , [0, 0, 0, 0, 7, 0, 11, 0, 5, 0, 6, 0, 0, 0, 0, 0]
              , [0, 0, 0, 0, 0, 8, 0, 5, 0, 0, 0, 20, 0, 0, 0, 0]
              , [0, 0, 0, 0, 0, 0, 14, 0, 0, 0, 6, 0, 9, 0, 0, 0]
              , [0, 0, 0, 0, 0, 0, 0, 6, 0, 6, 0, 1, 0, 7, 0, 0]
              , [0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 1, 0, 0, 0, 15, 0]
              , [0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 3, 0, 4]
              , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 3, 0, 9, 8]
              , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15, 0, 9, 0, 2]
              , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 8, 2, 0]]

dijkstra :: Graph -> Vertex -> ([Weight], [Vertex])
dijkstra graph start = go [start] (initialDistances start) (initialPredecessors start)
  where
    vertices = [0..length graph - 1]
    
    initialDistances :: Vertex -> [Weight]
    initialDistances s = [if v == s then 0 else if w == 0 then inf else w | (v, w) <- zip vertices (graph !! s)]
    
    initialPredecessors :: Vertex -> [Vertex]
    initialPredecessors s = [if w /= 0 && v /= s then s else -1 | (v, w) <- zip vertices (graph !! s)]
    
    go :: [Vertex] -> [Weight] -> [Vertex] -> ([Weight], [Vertex])
    go visited dists preds
      | length visited == length graph = (dists, preds)
      | otherwise = go (next:visited) updatedDists updatedPreds
      where
        availableVertices = vertices \\ visited
        next = minimumBy (comparing (dists !!)) availableVertices
        (updatedDists, updatedPreds) = unzip [
            if graph !! next !! v == 0 
            then (dists !! v, preds !! v) 
            else let newDist = d + graph !! next !! v
                 in if newDist < dists !! v 
                    then (newDist, next) 
                    else (dists !! v, preds !! v) 
            | v <- vertices, let d = dists !! next ]

getPath :: Vertex -> Vertex -> [Vertex] -> [Vertex]
getPath start end preds
    | start == end = [start]
    | otherwise = getPath start (preds !! end) preds ++ [end]

main :: IO ()
main = do
    let (dists0, preds0) = dijkstra adjMatrix 0
    print dists0
    print $ getPath 0 15 preds0
    
    let (dists1, preds1) = dijkstra adjMatrix 1
    print dists1
    print $ getPath 1 15 preds1

    let (dists2, preds2) = dijkstra adjMatrix 2
    print dists2
    print $ getPath 2 15 preds2