type Point = (Int, Int)

type Polyomino = [Point]

main :: IO ()  
main = do
    putStrLn "Zliczanie poliomin"
    putStrLn "Podaj z ilu klockow ma skladac sie poliomino: "
    input <- getLine
    if((read input :: Int) >= 0) then let 
        solution = generatePolys(read input :: Int)
        in print $ length solution
    else
        putStrLn "Zla dana"
    
generatePolys :: Int -> [Polyomino]
generatePolys 0 = []
generatePolys 1 = [[(0,0)]]
generatePolys n =  removeDuplicates (foldr (\x acc ->  createPolys x ++ acc ) [] (generatePolys (n-1)))

numberOfPolys :: Int -> Int
numberOfPolys n = length (generatePolys n)

createPolys :: Polyomino -> [Polyomino]
createPolys p = removeDuplicates (foldr(\point acc -> translate(point:p):acc) [] $ correctNeighbours p)

neighbours :: Point -> [Point] 
neighbours point =
    let up = (0,1)
        down = (0,-1)
        right = (1,0)
        left = (-1,0)
    in [addTuples point up, addTuples point down , addTuples point right, addTuples point left]

    
correctNeighbours :: Polyomino -> [Point] 
correctNeighbours p = removeDuplicates [x | x <- (foldr (\point acc -> acc ++ neighbours point)) [] p , not (isInPoly p x) ]


translate :: Polyomino -> Polyomino
translate p = let vec = findVector p
              in qsort (foldr (\(x,y) acc  -> acc ++ [(subtract  (fst vec) x  , subtract (snd vec) y )]) [] p)
              

findVector :: Polyomino -> Point
findVector = foldl (\(accX,accY)  (currX, currY) -> (min currX accX , min currY accY)) (0,0) 

removeDuplicates ::(Ord a) => [a] -> [a]
removeDuplicates = foldr (\el acc  -> if elem el acc then acc else [el] ++ acc ) []

isInPoly :: Polyomino -> Point -> Bool 
isInPoly [] el = False 
isInPoly (x:xs) el = x == el || isInPoly xs el


addTuples :: Point -> Point -> Point
addTuples (a,b) (c,d) = (a+c,b+d)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ equal ++ qsort larger
   where
     smaller = [y | y<-xs, y<x]
     equal   = [y | y<-xs, y==x] ++ [x]
     larger = [y | y<-xs, y>x]


    
    

