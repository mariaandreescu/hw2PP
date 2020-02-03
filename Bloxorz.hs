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
type Harta = A.Array Position [Char]
data Level = Level {low_right :: Position, blockPos :: Position , harta :: Harta, wonGame :: Bool, lostGame :: Bool}  
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
        | wonGame lvl == True = "\n" ++ "ab" ++ "\n" ++ "Congrats! You won!\n"
        | lostGame lvl == True = "\n" ++ "ab" ++ "\n" ++ "Game Over\n"
        | otherwise = "\n" ++ (unlines (helper (A.assocs (harta lvl))))
        where helper l = [concat(map snd l)| l <- (groupBy (\a b -> fst (fst a) == fst (fst b)) l)]
{-  
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hărții și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

-- lr == low-right => max i, max j pentru coordonate
emptyLevel :: Position -> Position -> Level
emptyLevel  lr blkPos = Level {low_right = lr,
                               blockPos = blkPos,
                               harta = (A.listArray ((0, 0), lr) (replicate ((fst lr + 1)*(snd lr + 1)) (show Empty))) A.// [(blkPos, (show Block))],
                               wonGame = False,
                               lostGame = False}

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
    | tilePos == (blockPos currentLvl) = currentLvl
    | tile == 'H' = currentLvl {harta = (harta currentLvl) A.// [(tilePos, (show Hard))]}
    | tile == 'S' = currentLvl {harta = (harta currentLvl) A.// [(tilePos, (show Soft))]}
    | tile == 'W' = currentLvl {harta = (harta currentLvl) A.// [(tilePos, (show Winning))]} 
    
    
{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch switchPos positions currentLvl 
    | switchPos == (blockPos currentLvl) = currentLvl
    | otherwise = currentLvl {harta = (harta currentLvl) A.// [(switchPos, (show Switch))]}

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

activate :: Cell -> Level -> Level
activate = undefined

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

move :: Directions -> Level -> Level
move = undefined

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame = undefined

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors = undefined

    isGoal = undefined

    -- Doar petru BONUS
    -- heuristic = undefined
