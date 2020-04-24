module Bloxorgt where

import Data.Maybe
import Data.Char (ord)
import Data.Char
import Data.Typeable
import System.IO
import Control.Monad


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

data Solucio = Solucio { moviments :: String
                     , tauler :: Tauler
                     } deriving Show

-- String que conté la informació inicial de Board.
taulerInicial :: String
taulerInicial = unlines ["11111111","11SSS111","11SSS111","11SSS111","11111111","11111111","11111111","11111111"]

-- String que conté la informació inicial de Board.
taulerInicial2 :: String
taulerInicial2 = unlines ["11011111","11SSS111","11SSS111","11SSS111","11111111","11111111","11111111","11111111"]

-- String que conté la informació inicial de Board.
taulerInicial3 :: String
taulerInicial3 = unlines ["11011111","11S11111","11SSS111","11SSS111","11111111","11111111","11111111","11111111"]


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
taulerFinal2 = llegirTauler taulerInicial2
taulerFinal3 = llegirTauler taulerInicial3

--Mostrem Casella per Casella el Tauler entrat.
mostraTauler :: Tauler -> String
mostraTauler = unwords . map showRow
 where showRow = map mostraCasella

-- Metode per tranformar la taula a mostrar de un simple String a un llistat de Strings (format vertical).
extreuSalts :: String -> Int -> Int -> Int -> [String]
extreuSalts s files columnes count
 | files == 0 = []
 | otherwise = [take columnes (drop ((columnes + 1) * count) s)] ++ extreuSalts s (files - 1) columnes (count + 1)

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
 putStrLn (" " ++ creaLinea (length (t!!0)))
 printElements (extreuSalts (mostraTauler t) (length t) (length (t!!0)) 0)
 putStrLn (" " ++ creaLinea (length (t!!0)))

creaLinea :: Int -> String
creaLinea i
 | i > 0 = ['='] ++ creaLinea (i-1)
 | i == 0 = [' ']

 
-- Donat un enter, et retorna un altre enter que indica la fila on es troba.
fila :: Int -> Int -> Int
fila x col = div x col

-- Donat un enter, et retorna un altre enter que indica la casella on es troba.
casellaFila :: Int -> Int -> Int
casellaFila x col = mod x col

partida :: Tauler -> Tauler -> Int -> Int -> Int -> Int -> Int -> IO()
partida taulerJoc plantilla x y z files columnes = do
 putStrLn(" ")
 putStrLn(" ")
 putStrLn("On mous la peça? (WASD) ")
 putStrLn(" ")
 mov <- getLine
 let movChar = (map toUpper mov)!!0
 putStrLn(" ")
 let movimentEsPossible = esPossible x y z movChar files columnes (llistaPosicions taulerJoc 0)
 if (movimentEsPossible) then do
  let llistaPosicionsNouBloc = calculaNouBloc x y z (llistaPosicions taulerJoc 0) movChar (length (taulerJoc!!0))
  if (blocNouPossible llistaPosicionsNouBloc plantilla (length llistaPosicionsNouBloc) columnes) then do
   let taulerActualitzat = (pintaTauler plantilla (length llistaPosicionsNouBloc) llistaPosicionsNouBloc (troba1 taulerJoc 0))
   mostraTaulerFinal taulerActualitzat
   if(not (blocNouFinal llistaPosicionsNouBloc plantilla (length llistaPosicionsNouBloc) columnes)) then do
    if(movChar == 'W' || movChar == 'S')then partida taulerActualitzat plantilla x z y files columnes
    else partida taulerActualitzat plantilla z y x files columnes
   else putStrLn("Molt bé, has guanyat!") 
  else putStrLn("Has mort! Partida finalitzada :(")
 else putStrLn("Has mort! Partida finalitzada :(")

-- Busca tauler dins de llista
buscaTauler :: [Tauler] -> Tauler -> Int -> Int -> Int -> Bool
buscaTauler llista tauler i files columnes
 | i > 0 && (comparaTaulers tauler (llista!!(i-1)) files columnes) = True
 | i > 0 && not(comparaTaulers tauler (llista!!(i-1)) files columnes) = buscaTauler llista tauler (i-1) files columnes
 | i == 0 = False

-- Compara tauler1 i tauler2 i diu si son iguals
comparaTaulers :: Tauler -> Tauler -> Int -> Int -> Bool
comparaTaulers tauler1 tauler2 files columnes
 | files > 0 && (comparaFila tauler1 tauler2 (files-1) columnes) = comparaTaulers tauler1 tauler2 (files-1) columnes
 | files > 0 && not(comparaFila tauler1 tauler2 (files-1) columnes) = False
 | files == 0 = True

-- Compara la fila i de tauler1 i tauler2 i diu si son iguals
comparaFila :: Tauler -> Tauler -> Int -> Int -> Bool
comparaFila tauler1 tauler2 fila casella
 | casella > 0 && (mostraCasella ((tauler1!!fila)!!(casella-1)) == mostraCasella ((tauler2!!fila)!!(casella-1))) = comparaFila tauler1 tauler2 fila (casella-1)
 | casella > 0 && (mostraCasella ((tauler1!!fila)!!(casella-1)) /= mostraCasella ((tauler2!!fila)!!(casella-1))) = False
 | casella == 0 = True
 
esPossible :: Int -> Int -> Int -> Char -> Int -> Int -> [Int] -> Bool
esPossible x y z m files columnes bloc
 | m == 'W' = (fila((agafaPrimeraFila bloc x)!!0) columnes) - z >= 0
 | m == 'A' = (casellaFila((agafaColumna bloc x)!!0)columnes) - z >= 0
 | m == 'S' = (fila((agafaUltimaFila bloc x)!!0) columnes) + z < files
 | m == 'D' = (casellaFila((agafaColumna (drop (x-1) bloc) x)!!0)columnes) + z < columnes
 
blocNouPossible :: [Int] -> Tauler -> Int -> Int ->Bool
blocNouPossible bloc t i columnes
 | i > 0 && mostraCasella(t!!(fila (bloc!!(i-1)) columnes)!!(casellaFila (bloc!!(i-1)) columnes)) /= '.' = blocNouPossible bloc t (i-1) columnes
 | i > 0 && mostraCasella(t!!(fila (bloc!!(i-1)) columnes)!!(casellaFila (bloc!!(i-1)) columnes)) == '.' = False
 | i == 0 = True

blocNouFinal :: [Int] -> Tauler -> Int -> Int ->Bool
blocNouFinal bloc t i columnes
 | i > 0 && mostraCasella(t!!(fila (bloc!!(i-1)) columnes)!!(casellaFila (bloc!!(i-1)) columnes)) == 'G' = blocNouFinal bloc t (i-1) columnes
 | i > 0 && mostraCasella(t!!(fila (bloc!!(i-1)) columnes)!!(casellaFila (bloc!!(i-1)) columnes)) /= 'G' = False
 | i == 0 = True


-- Donat un tauler, dos enters (reprenten la Casella), retorna un nou Tauler on la Terra de la primera Casella sha mogut a la segona Casella.
moviment :: Tauler -> Int -> Int -> Tauler
moviment t x y = moviment2 (moviment1 t x) y (casella t (fila x col) (casellaFila x col))
 where col = (length (t!!0))

moviment0 :: Tauler -> Int -> Int -> Tauler
moviment0 t x y = moviment3 (moviment1 t x) y (casella t (fila x col) (casellaFila x col))
 where col = (length (t!!0))
 
-- Donat un Tauler, una Terra i un enter que representa la Casella retorna un nou Tauler on ???.
moviment3 :: Tauler -> Int -> Maybe Terra -> Tauler
moviment3 t x p
 | fila x col == 0 = [(movimentFilaTerra t (truncate(fromIntegral(fila x col))) (casellaFila x col) (llegirTerra '1'))] ++ tail t
 | fila x col /= 0 = fst(splitAt (fila x col) t) ++ [(movimentFilaTerra t (truncate(fromIntegral(fila x col))) (casellaFila x col) (llegirTerra '1') )] ++ drop 1(snd(splitAt (fila x col) t))
 where col = (length (t!!0))

-- Donat un Tauler, una Terra i un enter que representa la Casella retorna un nou Tauler on ???.
moviment2 :: Tauler -> Int -> Maybe Terra -> Tauler
moviment2 t x p
 | fila x col == 0 = [(movimentFilaTerra t (truncate(fromIntegral(fila x col))) (casellaFila x col) (llegirTerra 'B'))] ++ tail t
 | fila x col /= 0 = fst(splitAt (fila x col) t) ++ [(movimentFilaTerra t (truncate(fromIntegral(fila x col))) (casellaFila x col) (llegirTerra 'B') )] ++ drop 1(snd(splitAt (fila x col) t))
 where col = (length (t!!0))

-- Donat un Tauler i un enter que representa la Casella retorna un nou Tauler on ???.
moviment1 :: Tauler -> Int -> Tauler
moviment1 t x 
 | fila x col == 0 = [(movimentFilaBuit t (truncate(fromIntegral(fila x col))) (casellaFila x col))] ++ tail t
 | fila x col /= 0 = fst(splitAt (fila x col) t) ++ [(movimentFilaBuit t (truncate(fromIntegral(fila x col))) (casellaFila x col))] ++ drop 1(snd(splitAt (fila x col) t))
 where col = (length (t!!0))

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
 |mostraCasella((t!!(fila x col))!!(casellaFila x col)) == 'B' = x
 |mostraCasella((t!!(fila x col))!!(casellaFila x col)) /= 'B' = trobaPeca t (x+1)
 where col = (length (t!!0))
 
troba1:: Tauler -> Int -> Int
troba1 t x
 |mostraCasella((t!!(fila x col))!!(casellaFila x col)) == '1' = x
 |mostraCasella((t!!(fila x col))!!(casellaFila x col)) /= '1' = troba1 t (x+1)
 where col = (length (t!!0))

trobaGoal :: Tauler -> Int -> Int
trobaGoal t x
 |(x > 0) && mostraCasella((t!!(fila (x-1) col))!!(casellaFila (x-1) col)) == 'G' = (x-1)
 |(x > 0) && mostraCasella((t!!(fila (x-1) col))!!(casellaFila (x-1) col)) /= 'G' = trobaGoal t (x-1)
 |(x == 0) && mostraCasella((t!!(fila 0 col))!!(casellaFila 0 col)) == 'G' = x
 |(x == 0) && mostraCasella((t!!(fila 0 col))!!(casellaFila 0 col)) /= 'G' = (length t)*col
 where col = (length (t!!0))

pecaMoviment :: Tauler -> Int -> Char -> Int
pecaMoviment t x m
 |m == 'W' = (x-col)
 |m == 'A' = (x-1)
 |m == 'S' = (x+col)
 |m == 'D' = (x+1)
 where col = length (t!!0)

readTaulerFromFile :: [String] -> Tauler
readTaulerFromFile words = llegirTauler (unlines words)

-- Parse String to Int
llegirEnter :: String -> Int
llegirEnter x = read x

castIntToString :: Int -> String
castIntToString x = show x

-- Programa principal que es crida per començar la partida
main = do
 content <- readFile "C:\\Users\\Linux\\Desktop\\u1940048\\3r GEINF\\Paradigmes i Llenguatges de Programació\\2a Convocatòria\\src\\Tauler3.txt"

 let dimensioZ = llegirEnter (take 1 content)
 let files = llegirEnter (take 1 (drop 2 content))
 let columnes = llegirEnter (take 1 (drop 4 content))
 
 let tauler = readTaulerFromFile (words (drop 6 content))

 let llista = (llistaPosicions tauler 0)
 
 let taulerSenseBlocs = taulerSenseBloc tauler (length (llistaPosicions tauler 0)) (llistaPosicions tauler 0)
 
 let nouTauler = pintaTauler taulerSenseBlocs (length (llistaPosicions tauler 0)) (llistaPosicions tauler 0) (troba1 tauler 0)

 let dimensioX = getDimensioX tauler llista (fila (llista!!0) columnes)
 let dimensioY = getDimensioY tauler llista (casellaFila (llista!!0) columnes)

 putStrLn("X: " ++ (show dimensioX))
 putStrLn("Y: " ++ (show dimensioY))
 putStrLn("Z: " ++ (show dimensioZ))

 putStrLn("Comença la partida")
 putStrLn(" ")
 mostraTaulerFinal nouTauler
 partida nouTauler taulerSenseBlocs dimensioX dimensioY dimensioZ files columnes


forEach :: String -> Int
forEach [] = 0
forEach (char:string) = 1 + (forEach string)

-- Donat un tauler i un iterador, retorna una llista de les posicions (cardinals) que ocupa el bloc
llistaPosicions :: Tauler -> Int -> [Int]
llistaPosicions t it
 | (it == (col * (length t))) = []
 | mostraCasella((t!!(fila it col))!!(casellaFila it col)) == 'B' = [it] ++ llistaPosicions t (it+1)
 | otherwise = llistaPosicions t (it+1)
 where col = (length (t!!0))

-- Donat un tauler, una llista que conte les posicions del bloc i una fila, retorna el numero de posicions que hi ha en aquesta fila
getDimensioX :: Tauler -> [Int] -> Int -> Int
getDimensioX t [] fil = 0
getDimensioX t (x:xs) fil = if fila x (length (t!!0)) == fil then 1 + getDimensioX t xs fil else getDimensioX t xs fil

-- Donat un tauler, una llista que conte les posicions del bloc i una columna, retorna el numero de posicions que hi ha en aquesta columna
getDimensioY :: Tauler -> [Int] -> Int -> Int
getDimensioY t [] col = 0
getDimensioY t (x:xs) col = if casellaFila x (length (t!!0)) == col then 1 + getDimensioY t xs col else getDimensioY t xs col

possiblesTaulers :: Tauler -> [Int] -> Int -> [Tauler]
possiblesTaulers t x i
 |(i==0) && (fila (x!!0) col) == 0 = possiblesTaulers t x (i+1) ++ [t]
 |(i==1) && (casellaFila (x!!0) col) == 0 = possiblesTaulers t x (i+1) ++ [t]
 |(i==2) && (fila (x!!0) col) == (length t)-1 = possiblesTaulers t x (i+1) ++ [t]
 |(i==3) && (casellaFila (x!!0) col) == (col-1) = [t]
 |(i==0) && mostraCasella((t!!(fila ((x!!0)-col) col))!!(casellaFila ((x!!0)-col) col)) == '1' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)-col)]
 |(i==1) && mostraCasella((t!!(fila ((x!!0)-1) col))!!(casellaFila ((x!!0)-1) col)) == '1' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)-1)]
 |(i==2) && mostraCasella((t!!(fila ((x!!0)+col) col))!!(casellaFila ((x!!0)+col) col)) == '1' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)+col)]
 |(i==3) && mostraCasella((t!!(fila ((x!!0)+1) col))!!(casellaFila ((x!!0)+1) col)) == '1' = [moviment t (x!!0) ((x!!0)+1)]
 |(i==0) && mostraCasella((t!!(fila ((x!!0)-col) col))!!(casellaFila ((x!!0)-col) col)) == 'G' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)-col)]
 |(i==1) && mostraCasella((t!!(fila ((x!!0)-1) col))!!(casellaFila ((x!!0)-1) col)) == 'G' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)-1)]
 |(i==2) && mostraCasella((t!!(fila ((x!!0)+col) col))!!(casellaFila ((x!!0)+col) col)) == 'G' = possiblesTaulers t x (i+1) ++ [moviment t (x!!0) ((x!!0)+col)]
 |(i==3) && mostraCasella((t!!(fila ((x!!0)+1) col))!!(casellaFila ((x!!0)+1) col)) == 'G' = [moviment t (x!!0) ((x!!0)+1)]
 |(i==0) && mostraCasella((t!!(fila ((x!!0)-col) col))!!(casellaFila ((x!!0)-col) col)) /= '1' = possiblesTaulers t x (i+1) ++ [t]
 |(i==1) && mostraCasella((t!!(fila ((x!!0)-1) col))!!(casellaFila ((x!!0)-1) col)) /= '1' = possiblesTaulers t x (i+1) ++ [t]
 |(i==2) && mostraCasella((t!!(fila ((x!!0)+col) col))!!(casellaFila ((x!!0)+col) col)) /= '1' = possiblesTaulers t x (i+1) ++ [t]
 |(i==3) && mostraCasella((t!!(fila ((x!!0)+1) col))!!(casellaFila ((x!!0)+1) col)) /= '1' = [t]
 where col = (length (t!!0))

mostraTaulerActualitzat :: [Tauler] -> Char -> Tauler
mostraTaulerActualitzat x m
 |m == 'W' = x!!3
 |m == 'A' = x!!2
 |m == 'S' = x!!1
 |m == 'D' = x!!0

taulerIgual :: Tauler -> Tauler -> Int -> Bool
taulerIgual tauler1 tauler2 x
 | x == 0 && mostraCasella((tauler1!!(fila (x-1) col))!!(casellaFila (x-1) col)) == mostraCasella((tauler2!!(fila (x-1) col))!!(casellaFila (x-1) col)) = True
 | mostraCasella((tauler1!!(fila (x-1) col))!!(casellaFila (x-1) col)) == mostraCasella((tauler2!!(fila (x-1) col))!!(casellaFila (x-1) col)) = taulerIgual tauler1 tauler2 (x-1)
 | mostraCasella((tauler1!!(fila (x-1) col))!!(casellaFila (x-1) col)) == mostraCasella((tauler2!!(fila (x-1) col))!!(casellaFila (x-1) col)) = False
 where col = (length (tauler1!!0))

movimentBloc :: Tauler -> [Int] -> Int -> Char -> Tauler
movimentBloc t x i m
 | i < (length x) = movimentBloc (moviment t (x!!i) (pecaMoviment t (x!!i) m)) x (i+1) m
 | i == (length x) = t
 
taulerSenseBloc :: Tauler -> Int -> [Int] -> Tauler
taulerSenseBloc t i x 
 | i > 0 = taulerSenseBloc (moviment0 t (x!!(i-1)) (x!!(i-1))) (i-1) x
 | i == 0 = t
 
pintaTauler :: Tauler -> Int -> [Int] -> Int -> Tauler
pintaTauler t i x y
 | i > 0 = pintaTauler (moviment t (y) (x!!(i-1))) (i-1) x y
 | i == 0 = t

agafaUltimaFila :: [Int] -> Int -> [Int]
agafaUltimaFila l x = drop ((length l)-x) l

agafaPrimeraFila :: [Int] -> Int -> [Int]
agafaPrimeraFila l x = take x l

-- Retorna la columna del primer element de la llista
agafaColumna :: [Int] -> Int -> [Int]
agafaColumna [] x = []
agafaColumna (i:is) x = [i] ++ (agafaColumna (drop (x-1) is) x)

-- Llista, iterador, z, col, res
 
calculaPosicionsW :: [Int] -> Int -> Int -> Int -> [Int]
calculaPosicionsW [] it z col = []
calculaPosicionsW (x:xs) it z col
 | it == 0 = calculaPosicionsW xs z z col
 | otherwise = [x-(col*it)] ++ (calculaPosicionsW ([x] ++ xs) (it-1) z col)
 
calculaPosicionsA :: [Int] -> Int -> Int -> Int -> [Int]
calculaPosicionsA [] it z col = []
calculaPosicionsA (x:xs) it z col
 | it == 0 = calculaPosicionsA xs z z col
 | otherwise = [x-it] ++ (calculaPosicionsA ([x] ++ xs) (it-1) z col)
 
calculaPosicionsS :: [Int] -> Int -> Int -> Int -> [Int]
calculaPosicionsS [] it z col = []
calculaPosicionsS (x:xs) it z col
 | it == 0 = calculaPosicionsS xs z z col
 | otherwise = [x+(col*it)] ++ (calculaPosicionsS ([x] ++ xs) (it-1) z col)
 
calculaPosicionsD :: [Int] -> Int -> Int -> Int -> [Int]
calculaPosicionsD [] it z col = []
calculaPosicionsD (x:xs) it z col
 | it == 0 = calculaPosicionsD xs z z col
 | otherwise = [x+it] ++ (calculaPosicionsD ([x] ++ xs) (it-1) z col)
  
calculaNouBloc :: Int -> Int -> Int -> [Int] -> Char ->  Int -> [Int]
calculaNouBloc x y z b m col
 | m == 'W' = calculaPosicionsW (agafaPrimeraFila b x) z  z col
 | m == 'A' = calculaPosicionsA (agafaColumna b x) z z col
 | m == 'S' = calculaPosicionsS (agafaUltimaFila b x) z  z col
 | m == 'D' = calculaPosicionsD (agafaColumna (drop (x-1) b) x) z z col
 
trobaCami :: Solucio -> [Solucio] -> [Tauler] -> Tauler -> Int -> Int -> String 
trobaCami actual perVisitar visitats plantilla files columnes = do
 -- COMPROVEM SI LA SOLUCIÓ ACTUAL ÉS FINAL
 let taulerActual = tauler actual
 let llistaPosicionsBloc = llistaPosicions taulerActual 0
 if(blocNouFinal llistaPosicionsBloc plantilla (length llistaPosicionsBloc) columnes) then moviments actual
 else moviments actual




