-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2018li1g138 where

import LI11819
import Tarefa1_2018li1g138
import Data.List
import Data.Char

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [(Estado (mapaInicial (32,32)) [Jogador (2,3) D 382 841 813, Jogador (17,16) E 812 312 2] [DisparoChoque 0 5, DisparoLaser 1 (14,14) D, DisparoCanhao 1 (18,18) E]),
            (Estado (mapaInicial (30,30)) [Jogador (2,3) C 40 41 13, Jogador (19,30) E 12 312 2, Jogador (13,30) B 400 2 2] [DisparoChoque 1 5, DisparoLaser 0 (1,3) C, DisparoCanhao 1 (19,29) E]),
            (Estado (mapaInicial (25,25)) [Jogador (12,12) D 100 0 1, Jogador (20,20) E 10 10 10] [DisparoLaser 1 (2,14) D, DisparoCanhao 1 (20,18) C, DisparoChoque 0 5]),
            (Estado (mapaInicial (50,50)) [Jogador (2,3) C 40 41 13, Jogador (17,32) E 12 312 2, Jogador (19,37) E 100 4 4, Jogador (20,20) C 140 44 10] [DisparoChoque 0 5, DisparoLaser 0 (1,3) C, DisparoCanhao 2 (19,29) E])]
-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -> String
comprime (Estado m j d) = (codMapa m) ++ (codJogadores j) ++ (codDisparos d)

-- | Comprime um 'Mapa' para formato textual.
--codMapa :: Mapa -- ^ O 'Mapa' a comprimir em formato textual.
--        -> String -- ^ A 'String' textual resultante.
codMapa :: Mapa -> String
codMapa [] = "V"
codMapa (x:xs) = (codLinhaMapa x) ++ (codMapa xs)

-- | Comprime uma 'Peca' para formato textual.
--codLinhaMapa :: [Peca] -- ^ O 'Peca' a comprimir em formato textual.
--             -> String -- ^ A 'String' textual resultante.
codLinhaMapa :: [Peca] -> String
codLinhaMapa [] = "L"
codLinhaMapa (Vazia:xs) = 'v':(codLinhaMapa xs)
codLinhaMapa (Bloco Destrutivel:xs) = 'd':(codLinhaMapa xs)
codLinhaMapa (Bloco Indestrutivel:xs) = 'i':(codLinhaMapa xs)

-- | Comprime uma lista de "Jogador" para formato textual.
--codJogadores :: [Jogador] -- ^ A lista de "Jogador" a comprimir em formato textual.
--             -> String -- ^ A 'String' textual resultante.
codJogadores :: [Jogador] -> String
codJogadores [] = "J"
codJogadores ((Jogador (x,y) dj vida laser choques):xs) = 'P':(show (x+10)) ++ (show (y+10)) ++ (show dj) ++ (show (vida+100)) ++ (show (laser+100)) ++ (show (choques+100)) ++ (codJogadores xs)

-- | Comprime uma lista de "Disparo" para formato textual.
--codDisparos :: [Disparo] -- ^ A lista de "Disparo" a comprimir em formato textual.
--            -> String -- ^ A 'String' textual resultante.
codDisparos :: [Disparo] -> String
codDisparos [] = "T"
codDisparos ((DisparoCanhao jog (x,y) d):xs) = 'Z':(show jog) ++ (show (x+10)) ++ (show (y+10)) ++ (show d) ++ (codDisparos xs)
codDisparos ((DisparoLaser jog (x,y) d):xs) = 'X':(show jog) ++ (show (x+10)) ++ (show (y+10)) ++ (show d) ++ (codDisparos xs)
codDisparos ((DisparoChoque jog ticks):xs) = 'Y':(show jog) ++ (show ticks) ++ (codDisparos xs)

-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime e = Estado (descompMapa (lengthline e) e) (descompJogadores e) (descompDisparos e)

-- | Percorre uma "String" e descobre o numero de elementos que cada linha do mapa possui.
--descompLinha :: String -- ^ A "String" textual a percorrer.
--             -> Int -- ^ O valor 'Int' que representa o comprimento de cada linha.
lengthline :: String -> Int
lengthline (x:xs) = if x /= 'L' then 1 + lengthline xs else 0

-- | Descomprime uma "String" no formato textual para uma "Peca" utilizada pela função 'codLinhaMapa'.
--descompLinha :: String -- ^ A "String" textual a descomprimir.
--             -> [Peca] -- ^ A a lista de "Peca" resultante.
descompLinha :: String -> [Peca]
descompLinha [] = []
descompLinha ('v':xs) = Vazia:(descompLinha xs)
descompLinha ('d':xs) = (Bloco Destrutivel):(descompLinha xs)
descompLinha ('i':xs) = (Bloco Indestrutivel):(descompLinha xs)

-- | Descomprime uma "String" no formato textual para um 'Mapa'.
--descompMapa :: Int -- ^ valor inteiro representativo do numero de colunas do mapa (lengthline)
--            -> String -- ^ A "String" textual a descomprimir.
--            -> Mapa -- ^ O 'Mapa' resultante.
descompMapa :: Int -> String -> Mapa
descompMapa y ('V':xs) = []
descompMapa y m = descompLinha (take y m):(descompMapa y (drop (y+1) m))

-- | Descomprime uma "String" no formato textual para uma lista de 'Jogador'.
--descompJogadores :: String -- ^ A "String" textual a descomprimir.
--                 -> [Jogador] -- ^ A a lista de "Jogador" resultante.
descompJogadores :: String -> [Jogador]
descompJogadores ('J':xs) = []
descompJogadores ('P':z:x:y:w:v:u:t:s:r:q:p:o:n:m:xs) = (Jogador (((read (z:[x])::Int)-10),((read (y:[w])::Int)-10)) (descompDirecao v) ((read (u:t:[s])::Int)-100) ((read (r:q:[p])::Int)-100) ((read (o:n:[m])::Int)-100)) : (descompJogadores xs)
descompJogadores (x:xs) = descompJogadores xs

-- | Descomprime um 'Char' no formato textual para uma 'Direcao'.
--descompDirecao :: Char -- ^ O 'Char' textual a descomprimir.
--               -> Direcao -- ^ A 'Direcao' resultante.
descompDirecao :: Char -> Direcao
descompDirecao 'C' = C
descompDirecao 'D' = D
descompDirecao 'B' = B
descompDirecao 'E' = E

-- | Descomprime uma 'String' no formato textual para uma lista 'Disparo'.
--descompDisparos :: String -- ^ A "String" textual a descomprimir.
--                -> [Disparo] -- ^ A a lista de "Disparo" resultante.
descompDisparos :: String -> [Disparo]
descompDisparos ('T':xs) = []
descompDisparos ('Z':z:x:y:w:v:u:xs) = (DisparoCanhao (digitToInt z) (((read (x:[y])::Int)-10),((read (w:[v])::Int)-10)) (descompDirecao u)) : (descompDisparos xs)
descompDisparos ('X':z:x:y:w:v:u:xs) = (DisparoLaser (digitToInt z) (((read (x:[y])::Int)-10),((read (w:[v])::Int)-10)) (descompDirecao u)) : (descompDisparos xs)
descompDisparos ('Y':z:t:xs) = (DisparoChoque (digitToInt z) (digitToInt t)) : (descompDisparos xs)
descompDisparos (x:xs) = descompDisparos xs