module Chess where

import Data.Maybe
import Data.Char (ord)
import Data.Char
import Data.Typeable
import System.IO

-- Tipus Tauler.
type Tauler = [[Casella]]

-- Tipus Casella.
type Casella = Maybe Peca

-- Objecte Peca que ve formada per el seu color i tipus.
data Peca = Peca Color Tipus deriving (Show)

-- Objecte colo que pot ser Blanc o Negra.
data Color = Blanca | Negra deriving (Show)

-- Objecte Tipus que inclous cada tipus de les peces d'Escacs.
data Tipus = Peo | Cavall | Alfil | Torre | Dama | Rei deriving (Show)

-- String que conté la informació inicial del Tauler.
taulerInicial :: String
taulerInicial = unlines ["tcadract","pppppppp","........","........","........","........","PPPPPPPP","TCADRACT"]

-- Donat l'String taulerInicial es llegeixen i creen les diferents Caselles.
llegirTauler :: String -> Tauler
llegirTauler = map readRow . lines
 where readRow = map llegirCasella

-- Tauler que es va modificant.
taulerFinal = llegirTauler taulerInicial
 
--Mostrem Casella per Casella el Tauler entrat.
mostraTauler :: Tauler -> String
mostraTauler = unwords . map showRow
 where showRow = map mostraCasella
 
-- Metode per tranformar la taula a mostrar de un simple String a un llistat de Strings (format vertical).
extreuSalts :: String -> [String]
extreuSalts s = [take 8 s] ++ [take 8(drop 9 s)] ++ [take 8 (drop 18 s)] ++ [take 8 (drop 27 s)] ++ [take 8 (drop 36 s)] ++ [take 8 (drop 45 s)] ++ [take 8 (drop 54 s)] ++ [take 8 (drop 63 s)]

-- Donada una Casella ens mostra el caracter de la Peca corresponent.
mostraCasella :: Casella -> Char
mostraCasella = maybe '.' mostraPeca

-- Donat un caracter retornem la Casella corresponent(peca que hi ha, si hi es).
llegirCasella :: Char -> Casella
llegirCasella '.' = Nothing
llegirCasella p = (llegirPeca p)

-- Donada una Peca mostrem el caracter corresponent a les seves caracteristiques (depenent de color i tipus).
mostraPeca :: Peca -> Char
mostraPeca (Peca Blanca Peo) = 'P'
mostraPeca (Peca Blanca Cavall) = 'C'
mostraPeca (Peca Blanca Alfil) = 'A'
mostraPeca (Peca Blanca Torre) = 'T'
mostraPeca (Peca Blanca Dama) = 'D'
mostraPeca (Peca Blanca Rei) = 'R'
mostraPeca (Peca Negra Peo) = 'p'
mostraPeca (Peca Negra Cavall) = 'c'
mostraPeca (Peca Negra Alfil) = 'a'
mostraPeca (Peca Negra Torre) = 't'
mostraPeca (Peca Negra Dama) = 'd'
mostraPeca (Peca Negra Rei) = 'r'

-- Donat un caracter retorna la peca que correspont.
llegirPeca :: Char -> Maybe Peca
llegirPeca 'P' = Just(Peca Blanca Peo)
llegirPeca 'C' = Just(Peca Blanca Cavall)
llegirPeca 'A' = Just(Peca Blanca Alfil)
llegirPeca 'T' = Just(Peca Blanca Torre)
llegirPeca 'D' = Just(Peca Blanca Dama)
llegirPeca 'R' = Just(Peca Blanca Rei) 
llegirPeca 'p' = Just(Peca Negra Peo)
llegirPeca 'c' = Just(Peca Negra Cavall)
llegirPeca 'a' = Just(Peca Negra Alfil)
llegirPeca 't' = Just(Peca Negra Torre) 
llegirPeca 'd' = Just(Peca Negra Dama)
llegirPeca 'r' = Just(Peca Negra Rei)
llegirPeca _ = Nothing

-- Donat un String que representa el tauler el mostra.
taulerString :: String -> String
taulerString t = mostraTauler $ (llegirTauler t)

-- Donada una posicio (x,y) i un tauler retorna la Peca corresponent.
casella :: Tauler -> Int -> Int -> Maybe Peca
casella tauler x y = (tauler !! x) !! y

-- Donada una posicio (x,y) i un Tauler retorna llistat de Caselles on ???.
movimentFilaBuit :: Tauler -> Int -> Int -> [Casella]
movimentFilaBuit t x y 
 | y /= 0 = init(fst(splitAt (y+1)(t !! x))) ++ [Nothing] ++ (snd(splitAt (y+1)(t !! x)))
 | y == 0 = [Nothing] ++ tail(t !! x)

-- Donada una posicio (x,y), una Peca i un Tauler retorna llistat de Caselles on ???.
movimentFilaPeca :: Tauler -> Int -> Int -> Maybe Peca -> [Casella]
movimentFilaPeca t x y piece
 | y /= 0 = init(fst(splitAt (y+1)(t !! x))) ++ [piece] ++ (snd(splitAt (y+1)(t !! x)))
 | y == 0 = [piece] ++ tail(t !! x)
 
-- Donat un enter, et retorna un altre enter que indica la fila on es troba.
fila :: Int -> Int
fila x = div x 8

-- Donat un enter, et retorna un altre enter que indica la casella on es troba.
casellaFila :: Int -> Int
casellaFila x = mod x 8

-- Donat un Tauler i un enter que representa la Casella retorna un nou Tauler on ???.
moviment1 :: Tauler -> Int -> Tauler
moviment1 t x 
 | fila x == 0 = [(movimentFilaBuit t (truncate(fromIntegral( fila x))) (casellaFila x))] ++ tail t
 | fila x /= 0 = fst(splitAt (fila x) t) ++ [(movimentFilaBuit t (truncate(fromIntegral( fila x))) (casellaFila x))] ++ drop 1(snd(splitAt (fila x) t))
 
-- Donat un Tauler, una Peca i un enter que representa la Casella retorna un nou Tauler on ???.
moviment2 :: Tauler -> Int -> Maybe Peca -> Tauler
moviment2 t x p
 | fila x == 0 = [(movimentFilaPeca t (truncate(fromIntegral( fila x))) (casellaFila x) p )] ++ tail t
 | fila x /= 0 = fst(splitAt (fila x) t) ++ [(movimentFilaPeca t (truncate(fromIntegral( fila x))) (casellaFila x) p )] ++ drop 1(snd(splitAt (fila x) t))

-- Donat un tauler, dos enters (reprenten la Casella), retorna un nou Tauler on la Peca de la primera Casella sha mogut a la segona Casella.
moviment :: Tauler -> Int -> Int -> Tauler
moviment t x y
 | movimentCorrecte (mostraCasella(casella t (fila x) (casellaFila x))) x y False == True = moviment2 (moviment1 t x) y (casella t (fila x) (casellaFila x))
 | otherwise = t

-- Donat un Caracter i un enter (p.e.: "e" i 2) retorna un enter que representa la casella e2.
convertirCasellaInt :: Char -> Int -> Int
convertirCasellaInt c x = ((x-1)*8) + ((ord c) - 97)

-- Array de jugades a realitzar.
jugades = ["Pe2e4","Pe7e5","Af1c4","Cb8c6","Dd1h5","Cg8f6"]
jugades2 = ["Pe2e4","Pe7e5","Pf2f4","Pe5xf4","Af1c4","Dd8h4","Pg2g3","Pb7b5","Pg3g4","Dh4xg4"]
jugades3 = ["Pe2e4","Pe7e5","Pf2f4","Pe5xf4","Af1c4","Dd8h4","Pg2g3","Dh4xg3","Ph2g3","Pf4f3"]

--taulerModificat = moviment taulerFinal (convertirCasellaInt 'e' 2) (convertirCasellaInt 'e' 4)

-- Donat un llistat de Strings i un enter (comptador) mostra per pantalla els elements de la llista.
printElements :: [String] -> Int ->IO()
printElements [] n = return ()
printElements (x:xs) n = do 
 putStrLn (show(n) ++ "|" ++ x ++ "|")
 printElements xs (n+1)
 
-- Donat un caracter (tipus de peca), 2 enters (x,y) que representen la Casella i un boolea indicant si es mata o no, 
-- retorna un boolea indicant si es un moviment correcte o no.
movimentCorrecte :: Char -> Int -> Int -> Bool -> Bool
movimentCorrecte p x y mata
 | p == 'p' || p == 'P' = (fila(x) == 1 && p == 'p' && (fila(y) == 2 || fila(y) == 3 && casellaFila(x) == casellaFila(y))) || (fila(x) == 6 && p == 'P' && (fila(y) == 5 || fila(y) == 4) && casellaFila(x) == casellaFila(y)) || (p == 'P' && ((fila(x) - 1) == fila(y)) && mata == False && casellaFila(x) == casellaFila(y)) || (p == 'p' && ((fila(x) + 1) == fila(y)) && mata == False && casellaFila(x) == casellaFila(y)) || (mata == True && p == 'P' && ((fila(x) - 1) == fila(y)) && ((casellaFila(x) + 1 == casellaFila(y)) || (casellaFila(x) - 1 == casellaFila(y)))) 
 | p == 't' || p == 'T' = ((fila(x) == fila (y)) && (casellaFila(x) /= casellaFila(y))) || ((fila(x) /= fila (y)) && (casellaFila(x) == casellaFila(y)))
 | p == 'c' || p == 'C' = True
 | p == 'a' || p == 'A' = True
 | p == 'd' || p == 'D' = True
 | p == 'r' || p == 'R' = True

-- Donat un Tauler, el mostra per pantalla amb le format especificat al enunciat 
mostraTaulerFinal :: Tauler -> IO()
mostraTaulerFinal t = do
 putStrLn("  ========")
 printElements (extreuSalts(mostraTauler(t))) 1
 putStrLn("  ========")
 putStrLn("  ABCDEFGH")

 -- Donat un llistat de String (jugades a realitzar) i un enter (comptador de cada jugada), va mostrant per pantalla el Tauler resultant per cada jugada
partida :: [String] -> Tauler -> Int -> IO()
partida [] t n= return ()
partida (x:xs) t n = do
 if(mod n 2 == 0) 
 then putStrLn ("  Jugada " ++ (show (div n 2)))
 else putStrLn (" ")
 let taulerAra = moviment t (convertirCasellaInt (x !! 1) (digitToInt(x !! 2))) (convertirCasellaInt (x !! 3) (digitToInt(x !! 4)))
 if(mod n 2 == 0) 
 then mostraTaulerFinal taulerAra
 else putStrLn (" ")
 partida xs taulerAra (n+1)

-- Programa principal que es crida per començar la partida
main = do
 putStrLn("Comença la partida")
 putStrLn(" ")
 partida jugades taulerFinal 1
 putStrLn(" ")
 putStrLn(" ")
 putStrLn("Partida finalitzada")