{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState
import Data.List

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

--
data Cell = Empty | Soft | Hard | Switch | Winning | Block deriving (Eq, Ord)

instance Show Cell where
    show Empty = [emptySpace]
    show Soft = [softTile]
    show Hard = [hardTile]
    show Switch = [switch]
    show Winning = [winningTile]
    show Block = [block]

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}
type Harta = A.Array Position Cell
data Level = Level {activatedTiles :: [Position], low_right :: Position, blockPos :: (Position, Position) , harta :: Harta, wonGame :: Bool, lostGame :: Bool}  
    deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

instance Show Level where
    show lvl 
        | wonGame lvl == True = "\n" ++ (unlines (helper (A.assocs h))) ++ "Congrats! You won!\n"
        | lostGame lvl == True = "\n" ++ (unlines (helper (A.assocs h))) ++ "Game Over\n"
        | otherwise = "\n" ++ (unlines (helper (A.assocs h)))
        where helper l = [concat(map show(map snd e))| e <- (groupBy (\a b -> fst (fst a) == fst (fst b)) l)]
              h = (harta lvl) A.// ([(x,y)| let x = fst (blockPos lvl), let y = Block] ++ [(x,y)| let x = snd (blockPos lvl), let y = Block]) 
-- h e harta la care adaug blocul pentru a face show
{-  
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hărții și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}


-- lr == low-right => max i, max j pentru coordonate
emptyLevel :: Position -> Position -> Level
emptyLevel  lr blkPos = Level {low_right = lr,
                               blockPos = (blkPos, blkPos),
                               harta = (A.listArray ((0, 0), lr) (replicate ((fst lr + 1)*(snd lr + 1)) Empty)),
                               wonGame = False,
                               lostGame = False,
                               activatedTiles = []}

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile tile tilePos currentLvl 
    | tile == 'H' = currentLvl {harta = (harta currentLvl) A.// [(tilePos, Hard)]}
    | tile == 'S' = currentLvl {harta = (harta currentLvl) A.// [(tilePos, Soft)]}
    | tile == 'W' = currentLvl {harta = (harta currentLvl) A.// [(tilePos, Winning)]} 
    | otherwise = undefined
    
{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch switchPos positions currentLvl = currentLvl {harta = (harta currentLvl) A.// [(switchPos, Switch)] , activatedTiles = positions}

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

activate :: (Position, Position) -> Level -> Level
activate tile currentLvl
    | ((h A.! (fst tile)) == Switch || (h A.! (snd tile)) == Switch) && (h A.! (head (activatedTiles currentLvl)) == Empty) == True 
        = currentLvl {harta = (harta currentLvl) A.// [(x, y)| x <- (activatedTiles currentLvl), let y = Hard]}
    | ((h A.! (fst tile)) == Switch || (h A.! (snd tile)) == Switch) && (h A.! (head (activatedTiles currentLvl)) == Hard) == True 
        = currentLvl {harta = (harta currentLvl) A.// [(x, y)| x <- (activatedTiles currentLvl), let y = Empty]}
    | ((h A.! (fst tile)) == Empty || (h A.! (snd tile)) == Empty) == True = currentLvl {lostGame = True}
    | ((h A.! (fst tile)) == Winning || (h A.! (snd tile)) == Winning) && (fst tile == snd tile) == True = currentLvl {wonGame = True}
    | ((h A.! (fst tile)) == Soft || (h A.! (snd tile)) == Soft) && (fst tile == snd tile) == True = currentLvl {lostGame = True}
    | otherwise = currentLvl
    where h = harta currentLvl
{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}
-- daca blocul e verical, oricum l-as muta o sa fie orizontal
-- daca blocul e .., trebuie sa il mut west/east ca sa fie vertical
-- daca blocul e :, trebuie sa il mut north/south ca sa fie vertical
getCoordinates :: Directions -> (Position, Position) -> (Position, Position)
getCoordinates dir tiles 
    | (dir == North) && (tile1X < tile2X) && (tile1Y == tile2Y) = ((tile1X - 1, tile1Y), (tile1X - 1, tile1Y))
    | (dir == North) && (tile1X > tile2X) && (tile1Y == tile2Y) = ((tile2X - 1, tile2Y), (tile2X - 1, tile2Y))
    | (dir == South) && (tile1X < tile2X) && (tile1Y == tile2Y) = ((tile2X + 1, tile2Y), (tile2X + 1, tile2Y))
    | (dir == South) && (tile1X > tile2X) && (tile1Y == tile2Y) = ((tile1X + 1, tile1Y), (tile1X + 1, tile1Y))
    | (dir == West) && (tile1Y < tile2Y) && (tile1X == tile2X) = ((tile1X, tile1Y - 1), (tile1X, tile1Y - 1))
    | (dir == West) && (tile1Y > tile2Y) && (tile1X == tile2X) = ((tile2X, tile2Y - 1), (tile2X, tile2Y - 1))
    | (dir == East) && (tile1Y < tile2Y) && (tile1X == tile2X) = ((tile2X, tile2Y + 1), (tile2X, tile2Y + 1))
    | (dir == East) && (tile1Y > tile2Y) && (tile1X == tile2X) = ((tile1X, tile1Y + 1), (tile1X, tile1Y + 1))
    | otherwise = undefined
    where tile1X = fst(fst tiles)
          tile1Y = snd(fst tiles)
          tile2X = fst(snd tiles)
          tile2Y = snd(snd tiles)
    
move :: Directions -> Level -> Level
move direction currentLvl
    | (wonGame currentLvl == True) = currentLvl
    | (lostGame currentLvl == True) = currentLvl
    | (direction == North) && (tile2 == tile1) == True
        = activate  ((tile1X - 1, tile1Y), (tile1X - 2, tile1Y)) (currentLvl {blockPos = ((tile1X - 1, tile1Y), (tile1X - 2, tile1Y))})
    | (direction == North) && (tile2 /= tile1) && (tile1Y == tile2Y) == True
        = activate (getCoordinates North bPos) (currentLvl {blockPos = getCoordinates North bPos})
    | (direction == North) && (tile2 /= tile1) && (tile1X == tile2X) == True
        = activate ((tile1X - 1, tile1Y) , (tile2X - 1, tile2Y)) (currentLvl {blockPos = ((tile1X - 1, tile1Y), (tile2X - 1, tile2Y))})
    
    | (direction == South) && (tile2 == tile1) == True
        = activate ((tile1X + 1, tile1Y), (tile1X + 2, tile1Y)) (currentLvl {blockPos = ((tile1X + 1, tile1Y), (tile1X + 2, tile1Y))})
    | (direction == South) && (tile2 /= tile1) && (tile1Y == tile2Y) == True
        = activate (getCoordinates South bPos) (currentLvl {blockPos = getCoordinates South bPos})
    | (direction == South) && (tile2 /= tile1) && (tile1X == tile2X) == True
        = activate ((tile1X + 1, tile1Y), (tile2X + 1, tile2Y)) (currentLvl {blockPos = ((tile1X + 1, tile1Y), (tile2X + 1, tile2Y))})
    
    | (direction == West) && (tile2 == tile1) == True
        = activate ((tile1X, tile1Y - 1), (tile1X, tile1Y - 2)) (currentLvl {blockPos = ((tile1X, tile1Y - 1), (tile1X, tile1Y - 2))})
    | (direction == West) && (tile2 /= tile1) && (tile1X == tile2X) == True
        = activate (getCoordinates West bPos) (currentLvl {blockPos = getCoordinates West bPos})
    | (direction == West) && (tile2 /= tile1) && (tile1Y == tile2Y) == True
        = activate ((tile1X, tile1Y - 1), (tile2X, tile2Y - 1)) (currentLvl {blockPos = ((tile1X, tile1Y - 1), (tile2X, tile2Y - 1))})
        
    | (direction == East) && (tile2 == tile1) == True
        = activate ((tile1X, tile1Y + 1), (tile1X, tile1Y + 2)) (currentLvl {blockPos = ((tile1X, tile1Y + 1), (tile1X, tile1Y + 2))})
    | (direction == East) && (tile2 /= tile1) && (tile1X == tile2X) == True
        = activate (getCoordinates East bPos) (currentLvl {blockPos = getCoordinates East bPos})
    | (direction == East) && (tile2 /= tile1) && (tile1Y == tile2Y) == True
        = activate ((tile1X, tile1Y + 1), (tile2X, tile2Y + 1)) (currentLvl {blockPos = ((tile1X, tile1Y + 1), (tile2X, tile2Y + 1))})
    |otherwise = undefined
    where tile1X = fst (fst(blockPos currentLvl))
          tile1Y = snd (fst(blockPos currentLvl))
          tile2X = fst (snd(blockPos currentLvl))
          tile2Y = snd (snd(blockPos currentLvl))
          tile1 = fst(blockPos currentLvl)
          tile2 = snd(blockPos currentLvl)
          bPos = blockPos currentLvl
          
{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame currentLvl 
    | (((wonGame currentLvl) == False) && ((lostGame currentLvl)== False)) = True
    | otherwise = False

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors level = [(dir, move dir level) | dir <- [North, South, West, East], (lostGame (move dir level)) == False]

    isGoal = wonGame

    -- Doar petru BONUS
    -- heuristic = undefined
