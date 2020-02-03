{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import qualified Data.Set as S

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = EmptyNode {depth :: Int} | Node {stare :: s, actiune :: a, parent :: (Node s a), depth :: Int, copii :: [(a, s)]}

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState node = stare node 

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}
createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace initState = Node {stare = initState,
                                   actiune = fst (head (successors initState)),
                                   parent = EmptyNode {depth = -1},
                                   depth = 0,
                                   copii = successors initState} 

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace = undefined


{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}  
{- 
  --> helper - va primi ca argumente maxDepth, o pereche cu un set in care am pus nodurile
  deja vizitate si o lista cu acele noduri (could not deduce Ord(Node s a)) => folosesc foldl pt lista)
  si un nod nu care trebuie inserat si va returna setul care contine nodurile vizitate
  --> garzi:
  daca s-a atins adancimea maxima => set
  daca starea nodului se afla in set(deci e deja vizitat) => set
  altfel apelez recursiv helper pentru fiecare copil al unui nod pentru a construi setul final
  
-}

helper :: (ProblemState s a, Ord s) => Int -> (S.Set s, [Node s a]) -> Node s a -> (S.Set s, [Node s a])
helper maxDepth (set, noduriVizitate) node 
    --doar pentru a evita infinite loop
    | maxDepth > 50 = undefined
    | ((depth node) > maxDepth) == True = (set, noduriVizitate)
    | (S.member (nodeState node) set) = (set, noduriVizitate)
    | otherwise = foldl (helper maxDepth) (S.insert (nodeState node) set, node : noduriVizitate) (succesori node)
    where succesori n = [Node {stare = snd x,
                                  actiune = fst x,
                                  parent = n,
                                  depth = (depth n) + 1,
                                  copii = successors (snd x)}
                            | x <- (copii n)]
                            
limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs initialNode maxDepth = reverse $ snd (helper maxDepth (S.empty, []) initialNode)
{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

helper1 :: (ProblemState s a, Ord s) => Node s a -> Int -> Int -> (Node s a, Int) 
helper1 node currentDepth nr 
    | (depth n) >= 0 = (n, index + nr)
    | otherwise = helper1 node (currentDepth + 1) (nr + (length noduriStari))
    where noduriStari = limitedDfs node currentDepth
          (n, index) = myFilter (\x -> isGoal(nodeState x)) noduriStari 0

--returneaza starea corespunzatoare gasita si ii asociaza un index           
myFilter ::(ProblemState s a, Ord s) => (Node s a -> Bool) -> [Node s a] -> Int -> (Node s a, Int)
myFilter fct noduriStari startIndex 
    | fct (head noduriStari) = (head noduriStari, startIndex)
    | null (tail noduriStari) = (EmptyNode{depth = -1}, 0)
    | otherwise = myFilter fct (tail noduriStari) (startIndex + 1)
    
iterativeDeepening :: (ProblemState s a, Ord s)
           => Node s a         -- Nodul stării inițiale
           -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening initialNode = helper1 initialNode 0 0

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
getPath :: Node s a -> [(a, s)]
getPath initialNode 
    | (depth initialNode == 0) = []
    | otherwise = (actiune initialNode, stare initialNode) : (getPath (parent initialNode))
    
    
extractPath :: Node s a -> [(a, s)]
extractPath initialNode = reverse (getPath initialNode)  
     
{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve initState False = extractPath (fst (iterativeDeepening (createStateSpace initState)))

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
