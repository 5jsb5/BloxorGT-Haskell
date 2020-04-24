module Bloxorgt where

import Data.Maybe
import Data.Char (ord)
import Data.Char
import Data.Typeable
import System.IO

-- Tipus Board
type Tauler = [[Casella]]

-- Tipus Square
type Casella = Maybe Terra

-- Objecte Ground que ve format pel seu Status.
data Terra = Terra Estat Objectiu deriving (Show)

-- Objecte Status que pot ser Empty o Busy.
data Estat = Buit | Ocupat deriving (Show)

-- Objecte Goal que ens diu si Square és l'objectiu a arribar o no.
data Objectiu = Goal | NoGoal deriving (Show)

-- String que conté la informació inicial de Board.
taulerInicial :: String
taulerInicial = unlines ["1110000000","1S11110000","1S11111110","0111111111","0000011G11","0000001110"]

-- Donat l'String taulerInicial es llegeixen els Square
llegirTauler :: String -> Tauler
llegirTauler = map readRow . lines
 where readRow = map llegirCasella
 
-- Donat un caracter retornem la Casella corresponent(Terra que hi ha, si hi és).
llegirCasella :: Char -> Casella
llegirCasella '0' = Nothing
llegirCasella t = (llegirTerra t)

-- Donat un caracter retorna la Terra que correspon.
llegirTerra :: Char -> Maybe Terra
llegirTerra '1' = Just(Terra Buit NoGoal)
llegirTerra 'S' = Just(Terra Ocupat NoGoal)
llegirTerra 'G' = Just(Terra Buit Goal)
llegirTerra 'B' = Just(Terra Ocupat NoGoal)
llegirTerra '0' = Nothing

-- Donada una Terra mostrem el caracter corresponent a les seves caracteristiques (depenent de color i tipus).
mostraTerra :: Terra -> Char
mostraTerra(Terra Buit NoGoal) = '1'
mostraTerra(Terra Buit Goal) = 'G'
mostraTerra(Terra Ocupat NoGoal) = 'B'

taulerFinal = llegirTauler taulerInicial

--Mostrem Casella per Casella el Tauler entrat.
mostraTauler :: Tauler -> String
mostraTauler = unwords . map showRow
 where showRow = map mostraCasella

-- Metode per tranformar la taula a mostrar de un simple String a un llistat de Strings (format vertical).
extreuSalts :: String -> [String]
extreuSalts s = [take 10 s] ++ [take 10(drop 11 s)] ++ [take 10 (drop 22 s)] ++ [take 10 (drop 33 s)] ++ [take 10 (drop 44 s)] ++ [take 10 (drop 55 s)]

-- Donada una Casella ens mostra el caracter de la Terra corresponent.
mostraCasella :: Casella -> Char
mostraCasella = maybe '.' mostraTerra

-- Donat un llistat de Strings i un enter (comptador) mostra per pantalla els elements de la llista.
printElements :: [String] ->IO()
printElements [] = return ()
printElements (x:xs) = do 
 putStrLn ("|" ++ x ++ "|")
 printElements xs
 
mostraTaulerFinal :: Tauler -> IO()
mostraTaulerFinal t = do
 putStrLn(" ==========")
 printElements (extreuSalts(mostraTauler(t)))
 putStrLn(" ==========")
 
-- Donat un enter, et retorna un altre enter que indica la fila on es troba.
fila :: Int -> Int
fila x = div x 10

-- Donat un enter, et retorna un altre enter que indica la casella on es troba.
casellaFila :: Int -> Int
casellaFila x = mod x 10

partida :: Tauler -> Int -> Int -> IO()
partida t x y = do
 putStrLn(" ")
 putStrLn("Moviments en horitzontal: " ++ (show x))
 putStrLn("Moviments en vertical: " ++ (show y))
 putStrLn(" ")
 putStrLn("On mous la peça? (WASD) ")
 putStrLn(" ")
 mov <- getLine
 let movChar = map toUpper mov
 --let pecaAra = llistaPeces t 60 []
 --let possiblesJugades = possiblesTaulers t pecaAra 0
 putStrLn(" ")
 mostraTaulerFinal(movimentBloc t (llistaPeces t 60 []) 0 (movChar!!0))
 partida (movimentBloc t (llistaPeces t 60 []) 0 (movChar!!0)) x y
 --mostraTaulerFinal (mostraTaulerActualitzat possiblesJugades (movChar!!0))
 --if(trobaGoal (mostraTaulerActualitzat possiblesJugades (movChar!!0)) 60 == 60)
 --then putStrLn("Partida finalitzada")
 --if (taulerIgual t (mostraTaulerActualitzat possiblesJugades (movChar!!0)) 60)
 --then putStrLn("Has sortit del limit")
 --else if (movChar!!0 == 'W' ||movChar!!0 =='S')
 --then partida (mostraTaulerActualitzat possiblesJugades (movChar!!0)) x (y+1)
 --else if (movChar!!0 == 'A' ||movChar!!0 =='D')
 --then partida (mostraTaulerActualitzat possiblesJugades (movChar!!0)) (x+1) y
 --else partida t x y
 
-- Donat un tauler, dos enters (reprenten la Casella), retorna un nou Tauler on la Terra de la primera Casella sha mogut a la segona Casella.
moviment :: Tauler -> Int -> Int -> Tauler
moviment t x y = moviment2 (moviment1 t x) y (casella t (fila x) (casellaFila x))

-- Donat un Tauler, una Terra i un enter que representa la Casella retorna un nou Tauler on ???.
moviment2 :: Tauler -> Int -> Maybe Terra -> Tauler
moviment2 t x p
 | fila x == 0 = [(movimentFilaTerra t (truncate(fromIntegral( fila x))) (casellaFila x) (llegirTerra 'B'))] ++ tail t
 | fila x /= 0 = fst(splitAt (fila x) t) ++ [(movimentFilaTerra t (truncate(fromIntegral( fila x))) (casellaFila x) p )] ++ drop 1(snd(splitAt (fila x) t))

-- Donat un Tauler i un enter que representa la Casella retorna un nou Tauler on ???.
moviment1 :: Tauler -> Int -> Tauler
moviment1 t x 
 | fila x == 0 = [(movimentFilaBuit t (truncate(fromIntegral( fila x))) (casellaFila x))] ++ tail t
 | fila x /= 0 = fst(splitAt (fila x) t) ++ [(movimentFilaBuit t (truncate(fromIntegral( fila x))) (casellaFila x))] ++ drop 1(snd(splitAt (fila x) t))
 

-- Donada una posicio (x,y) i un tauler retorna la Terra corresponent.
casella :: Tauler -> Int -> Int -> Maybe Terra
casella tauler x y = (tauler !! x) !! y
 
-- Donada una posicio (x,y) i un Tauler retorna llistat de Caselles on ???.
movimentFilaBuit :: Tauler -> Int -> Int -> [Casella]
movimentFilaBuit t x y 
 | y /= 0 = init(fst(splitAt (y+1)(t !! x))) ++ [Just(Terra Buit NoGoal)] ++ (snd(splitAt (y+1)(t !! x)))
 | y == 0 = [Just(Terra Buit NoGoal)] ++ tail(t !! x)

-- Donada una posicio (x,y), una Terra i un Tauler retorna llistat de Caselles on ???.
movimentFilaTerra :: Tauler -> Int -> Int -> Maybe Terra -> [Casella]
movimentFilaTerra t x y piece
 | y /= 0 = init(fst(splitAt (y+1)(t !! x))) ++ [piece] ++ (snd(splitAt (y+1)(t !! x)))
 | y == 0 = [piece] ++ tail(t !! x)
 
trobaPeca :: Tauler -> Int -> Int
trobaPeca t x
 |mostraCasella((t!!(fila x))!!(casellaFila x)) == 'B' = x
 |mostraCasella((t!!(fila x))!!(casellaFila x)) /= 'B' = trobaPeca t (x+1)
 
trobaGoal :: Tauler -> Int -> Int
trobaGoal t x
 |(x > 0) && mostraCasella((t!!(fila (x-1)))!!(casellaFila (x-1))) == 'G' = (x-1)
 |(x > 0) && mostraCasella((t!!(fila (x-1)))!!(casellaFila (x-1))) /= 'G' = trobaGoal t (x-1)
 |(x == 0) && mostraCasella((t!!(fila 0))!!(casellaFila 0)) == 'G' = x
 |(x == 0) && mostraCasella((t!!(fila 0))!!(casellaFila 0)) /= 'G' = 60

pecaMoviment :: Int -> Char -> Int
pecaMoviment x m 
 |m == 'W' = (x-10)
 |m == 'A' = (x-1)
 |m == 'S' = (x+10)
 |m == 'D' = (x+1)
 
-- Programa principal que es crida per començar la partida
main = do
 putStrLn("Comença la partida")
 putStrLn(" ")
 mostraTaulerFinal taulerFinal
 partida taulerFinal 0 0 

llistaPeces :: Tauler -> Int -> [Int] -> [Int]
llistaPeces t n x
 |(n > 0) && mostraCasella((t!!(fila (n-1)))!!(casellaFila (n-1))) == 'B' = llistaPeces t (n-1) (x++[(n-1)])
 |(n > 0) && mostraCasella((t!!(fila (n-1)))!!(casellaFila (n-1))) /= 'B' = llistaPeces t (n-1) (x++[])
 |(n == 0) = x
 
possiblesTaulers :: Tauler -> [Int] -> Int -> [Tauler]
possiblesTaulers t x i
 |(i==0) && (fila(x!!0)) == 0 = possiblesTaulers t x (i+1) ++ [t]
 |(i==1) && (casellaFila(x!!0)) == 0 = possiblesTaulers t x (i+1) ++ [t]
 |(i==2) && (fila(x!!0)) == 5 = possiblesTaulers t x (i+1) ++ [t]
 |(i==3) && (casellaFila(x!!0)) == 9 = [t]
 |(i==0) && mostraCasella((t!!(fila ((x!!0)-10)))!!(casellaFila ((x!!0)-10))) == '1' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)-10)]
 |(i==1) && mostraCasella((t!!(fila ((x!!0)-1)))!!(casellaFila ((x!!0)-1))) == '1' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)-1)]
 |(i==2) && mostraCasella((t!!(fila ((x!!0)+10)))!!(casellaFila ((x!!0)+10))) == '1' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)+10)]
 |(i==3) && mostraCasella((t!!(fila ((x!!0)+1)))!!(casellaFila ((x!!0)+1))) == '1' = [moviment t (x!!0) ((x!!0)+1)]
 |(i==0) && mostraCasella((t!!(fila ((x!!0)-10)))!!(casellaFila ((x!!0)-10))) == 'G' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)-10)]
 |(i==1) && mostraCasella((t!!(fila ((x!!0)-1)))!!(casellaFila ((x!!0)-1))) == 'G' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)-1)]
 |(i==2) && mostraCasella((t!!(fila ((x!!0)+10)))!!(casellaFila ((x!!0)+10))) == 'G' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)+10)]
 |(i==3) && mostraCasella((t!!(fila ((x!!0)+1)))!!(casellaFila ((x!!0)+1))) == 'G' = [moviment t (x!!0) ((x!!0)+1)]
 |(i==0) && mostraCasella((t!!(fila ((x!!0)-10)))!!(casellaFila ((x!!0)-10))) /= '1' = possiblesTaulers t x (i+1) ++ [t]
 |(i==1) && mostraCasella((t!!(fila ((x!!0)-1)))!!(casellaFila ((x!!0)-1))) /= '1' = possiblesTaulers t x (i+1) ++ [t]
 |(i==2) && mostraCasella((t!!(fila ((x!!0)+10)))!!(casellaFila ((x!!0)+10))) /= '1' = possiblesTaulers t x (i+1) ++ [t]
 |(i==3) && mostraCasella((t!!(fila ((x!!0)+1)))!!(casellaFila ((x!!0)+1))) /= '1' = [t]
 
 
mostraTaulerActualitzat :: [Tauler] -> Char -> Tauler
mostraTaulerActualitzat x m
 |m == 'W' = x!!3
 |m == 'A' = x!!2
 |m == 'S' = x!!1
 |m == 'D' = x!!0

taulerIgual :: Tauler -> Tauler -> Int -> Bool
taulerIgual tauler1 tauler2 x
 | x == 0 && mostraCasella((tauler1!!(fila (x-1)))!!(casellaFila (x-1))) == mostraCasella((tauler2!!(fila (x-1)))!!(casellaFila (x-1))) = True
 | mostraCasella((tauler1!!(fila (x-1)))!!(casellaFila (x-1))) == mostraCasella((tauler2!!(fila (x-1)))!!(casellaFila (x-1))) = taulerIgual tauler1 tauler2 (x-1)
 | mostraCasella((tauler1!!(fila (x-1)))!!(casellaFila (x-1))) == mostraCasella((tauler2!!(fila (x-1)))!!(casellaFila (x-1))) = False
 
movimentBloc :: Tauler -> [Int] -> Int -> Char -> Tauler
movimentBloc t x i m 
 | i < (length x) = movimentBloc (moviment t (x!!i) (pecaMoviment (x!!i) m)) x (i+1) m
 | i == (length x) = t
