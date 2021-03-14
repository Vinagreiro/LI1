-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g138 where

import LI11819
import Tarefa0_2018li1g138
import Tarefa1_2018li1g138
import Tarefa2_2018li1g138

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [(Estado (mapaInicial (20,20)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5]),
            (Estado (mapaInicial (20,20)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 1]),
            (Estado (mapaInicial (20,20)) [(Jogador (2,2) B 3 3 3),(Jogador (6,3) D 3 3 3),(Jogador (12,2) B 3 3 3)] [DisparoLaser 0 (3,2) B]),
            (Estado (mapaInicial (20,20)) [(Jogador (2,2) B 3 3 3),(Jogador (6,5) D 3 3 3),(Jogador (12,5) B 3 3 3)] [(DisparoLaser 0 (3,2) B),(DisparoCanhao 1 (5,2) C),(DisparoCanhao 1 (6,3) C)]),
            (Estado (mapaInicial (20,20)) [(Jogador (2,2) B 3 3 3),(Jogador (6,5) D 3 3 3),(Jogador (12,5) B 3 3 3)] [(DisparoCanhao 0 (3,2) B),(DisparoCanhao 1 (11,5) B)]),
            (Estado (mapaInicial (20,20)) [(Jogador (2,2) B 3 3 3),(Jogador (6,5) D 3 3 3),(Jogador (12,5) B 3 3 3)] [(DisparoCanhao 0 (5,4) B),(DisparoCanhao 1 (11,6) B)]),
            (Estado (mapaInicial (20,20)) [(Jogador (2,2) B 3 3 3),(Jogador (6,5) D 3 3 3),(Jogador (12,5) B 3 3 3)] [(DisparoCanhao 0 (8,9) B),(DisparoCanhao 1 (10,9) C)]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [(Jogador (1,2) B 3 3 3),(Jogador (5,2) D 3 3 3)] [DisparoLaser 0 (2,2) B]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [(Jogador (1,2) B 3 3 3),(Jogador (5,2) D 3 3 3)] [DisparoLaser 0 (2,2) B]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [(Jogador (1,2) B 3 3 3),(Jogador (5,2) D 3 3 3)] [DisparoLaser 0 (2,2) B]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [(Jogador (1,2) B 3 3 3),(Jogador (5,2) D 3 3 3)] [DisparoCanhao 0 (3,2) B]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [(Jogador (1,2) B 3 3 3),(Jogador (5,3) D 3 3 3)] [DisparoLaser 0 (2,2) B]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [(Jogador (1,2) B 3 3 3),(Jogador (5,3) D 3 3 3)] [DisparoLaser 0 (2,2) B]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [(Jogador (1,2) B 3 3 3),(Jogador (5,3) D 3 3 3)] [DisparoCanhao 0 (4,2) B]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [(Jogador (1,2) B 3 3 3),(Jogador (5,3) D 3 3 3)] [(DisparoCanhao 0 (4,2) B),(DisparoChoque 1 1)]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [(Jogador (1,2) B 3 3 3),(Jogador (5,3) D 3 3 3)] [DisparoLaser 0 (2,2) B])]

-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers (Estado m l disp) = (Estado (verificaMapaL (alcanceLasers' disp m) m) (jogLaser (alcanceLasers' disp m) l) (actuLaser disp (alcanceLasersG' disp m) m))

-- | Utilizando uma lista de posiçoes que sao afectadas pelo laser actualiza o mapa após os disparos de laser avançarem 1 tick.
--verificaMapaL :: [Posicao] -- ^ A lista de posiçoes afectadas pelo laser.
--              -> Mapa -- ^ O 'Mapa' anterior.
--              -> Mapa -- ^ O 'Mapa' actualizado.
verificaMapaL :: [Posicao] -> Mapa -> Mapa
verificaMapaL [] m = m
verificaMapaL ((x,y):xs) m | (encontraPosicaoMatriz (x,y) m)==(Bloco Destrutivel) = verificaMapaL xs (atualizaPosicaoMatriz (x,y) Vazia m)
                           | (encontraPosicaoMatriz (x,y) m)==(Bloco Indestrutivel) = verificaMapaL xs m
                           | (encontraPosicaoMatriz (x,y) m)==Vazia = verificaMapaL xs m

-- | Utilizando uma lista de posiçoes que sao afectadas pelo laser actualiza a lista de 'Jogador' após os disparos de laser avançarem 1 tick.
--verificaJogadorAtingeLaser :: [Posicao] -- ^ A lista de "Posicao" afectadas pelo laser.
--                           -> [Jogador] -- ^ A lista de "Jogador" anterior.
--                           -> [Jogador] -- ^ A lista de "Jogador" actualizada.
jogLaser :: [Posicao] -> [Jogador] -> [Jogador]
jogLaser [] j = j
jogLaser (p:ps) [] = []
jogLaser (p:ps) (x:xs) = (verificaJogadorAtingeLaser (p:ps) x):(jogLaser (p:ps) xs)

-- | Utilizando uma lista de posiçoes que sao afectadas pelo laser actualiza um 'Jogador' após os disparos de laser avançarem 1 tick.
--verificaJogadorAtingeLaser :: [Posicao] -- ^ A lista de "Posicao" afectadas pelo laser.
--                           -> Jogador -- ^ O "Jogador" anterior.
--                           -> Jogador -- ^ O "Jogador" actualizado.
verificaJogadorAtingeLaser :: [Posicao] -> Jogador -> Jogador
verificaJogadorAtingeLaser [] j = j
verificaJogadorAtingeLaser (p:ps) (Jogador (x,y) dj vida laser choques) | p/=(x,y) && p/=(x,y-1) && p/=(x-1,y) && p/=(x-1,y-1) = verificaJogadorAtingeLaser ps (Jogador (x,y) dj vida laser choques)
                                                                        | otherwise = (Jogador (x,y) dj (vida-1) laser choques)

-- | Percorre a lista de Disparo e actualiza-a após o jogo avançar 1 tick.
--actuLaser :: [Disparo] -- ^ A lista de "Disparo" anterior.
--          -> [Disparo] -- ^ A lista de "Disparo" actualizada.
actuLaser :: [Disparo] -> [PosicaoGrelha] -> Mapa -> [Disparo]
actuLaser [] (p:ps) m = []
actuLaser ((DisparoLaser jog (x,y) d):disp) (p:ps) m = (actuLaser disp (p:ps) m)
actuLaser (x:disp) [] m = (x:disp)
actuLaser ((DisparoCanhao jog (x,y) d):disp) (p:ps) m | (eliminaCanhao (DisparoCanhao jog (x,y) d) (p:ps))==True = actuLaser disp (p:ps) m
                                                      | otherwise = (DisparoCanhao jog (x,y) d):(actuLaser disp (p:ps) m)
actuLaser (x:disp) (p:ps) m = x:(actuLaser disp (p:ps) m)


-- | Verifica os disparos de canhao que se encontram nas posiçoes grelha afectadas pelos disparos laser para depois os eliminar.
--actuLaser :: Disparo -- ^ O "Disparo" de canhao a verificar.
--          -> [Disparo] -- ^ A lista de "PosicaoGrelha" afectadas pelos lasers.
--          -> Bool -- ^ A resposta boliana
eliminaCanhao :: Disparo -> [PosicaoGrelha] -> Bool
eliminaCanhao (DisparoCanhao jog (x,y) d) [] = False
eliminaCanhao (DisparoCanhao jog (x,y) d) (p:ps) | (x,y)/=p = (eliminaCanhao (DisparoCanhao jog (x,y) d) ps)
                                                 | otherwise = True

-- | Verica todas as posiçoes que sao afectadas por um disparo de laser tendo em conta o mapa onde o jogo decorre.
--alcanceLaser :: Disparo -- ^ O 'Disparo' a ter em conta.
--             -> Mapa -- ^ A 'Mapa' onde o jogo decorre.
--             -> [Posicao] -- ^ A lista de "Posicao" afectadas.
alcanceLaser :: Disparo -> Mapa -> [Posicao]
alcanceLaser (DisparoLaser jog (x,y) C) m | (verificaMapaD (x,y) C m)==False = [(x,y),(x,y+1)]++(alcanceLaser (DisparoLaser jog (x-1,y) C) m)
                                          | otherwise = [(x,y),(x,y+1)]
alcanceLaser (DisparoLaser jog (x,y) D) m | (verificaMapaD (x,y) D m)==False = [(x,y),(x+1,y)]++(alcanceLaser (DisparoLaser jog (x,y+1) D) m)
                                          | otherwise = [(x,y),(x+1,y)]
alcanceLaser (DisparoLaser jog (x,y) B) m | (verificaMapaD (x,y) B m)==False = [(x,y),(x,y+1)]++(alcanceLaser (DisparoLaser jog (x+1,y) B) m)
                                          | otherwise = [(x,y),(x,y+1)]
alcanceLaser (DisparoLaser jog (x,y) E) m | (verificaMapaD (x,y) E m)==False = [(x,y),(x+1,y)]++(alcanceLaser (DisparoLaser jog (x,y-1) E) m)
                                          | otherwise = [(x,y),(x+1,y)]
alcanceLaser x m = []

-- | Verica todas as posiçoes grelha que sao afectadas por um disparo de laser tendo em conta o mapa onde o jogo decorre.
--alcanceLaser :: Disparo -- ^ O 'Disparo' a ter em conta.
--             -> Mapa -- ^ A 'Mapa' onde o jogo decorre.
--             -> [PosicaoGrelha] -- ^ A lista de "PosicaoGrelha" afectadas.
alcanceLaserG :: Disparo -> Mapa -> [PosicaoGrelha]
alcanceLaserG (DisparoLaser jog (x,y) C) m | (verificaMapaD (x,y) C m)==False = [(x,y)]++(alcanceLaserG (DisparoLaser jog (x-1,y) C) m)
                                           | otherwise = [(x,y)]
alcanceLaserG (DisparoLaser jog (x,y) D) m | (verificaMapaD (x,y) D m)==False = [(x,y)]++(alcanceLaserG (DisparoLaser jog (x,y+1) D) m)
                                           | otherwise = [(x,y)]
alcanceLaserG (DisparoLaser jog (x,y) B) m | (verificaMapaD (x,y) B m)==False = [(x,y)]++(alcanceLaserG (DisparoLaser jog (x+1,y) B) m)
                                           | otherwise = [(x,y)]
alcanceLaserG (DisparoLaser jog (x,y) E) m | (verificaMapaD (x,y) E m)==False = [(x,y)]++(alcanceLaserG (DisparoLaser jog (x,y-1) E) m)
                                           | otherwise = [(x,y)]
alcanceLaserG x m = []

-- | Verica todas as posiçoesgrelha que sao afectadas por uma lista de disparo de laser tendo em conta o mapa onde o jogo decorre.
--alcanceLaser :: [Disparo] -- ^ A lista de 'Disparo' a ter em conta.
--             -> Mapa -- ^ A 'Mapa' onde o jogo decorre.
--             -> [PosicaoGrelha] -- ^ A lista de "Posicao" afectadas.
alcanceLasersG' :: [Disparo] -> Mapa -> [PosicaoGrelha]
alcanceLasersG' [] m = []
alcanceLasersG' (l:disp) m = (alcanceLaserG l m)++(alcanceLasersG' disp m)

-- | Verica todas as posiçoes que sao afectadas por uma lista de disparo de laser tendo em conta o mapa onde o jogo decorre.
--alcanceLaser :: [Disparo] -- ^ A lista de 'Disparo' a ter em conta.
--             -> Mapa -- ^ A 'Mapa' onde o jogo decorre.
--             -> [Posicao] -- ^ A lista de "Posicao" afectadas.
alcanceLasers' :: [Disparo] -> Mapa -> [Posicao]
alcanceLasers' [] m = []
alcanceLasers' (l:disp) m = (alcanceLaser l m)++(alcanceLasers' disp m)

-- | Verifica se a posiçao do mapa corresponde a um bloco 'Indestrutivel' sendo que assim o laser pára nessa posicao.
--verificaMapaD :: PosicaoGrelha -- ^ A 'PosicaoGrelha' actual.
--              -> Direcao -- ^ A 'Direcao' a aplicar.
--              -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--              -> Bool -- ^ A resposta em relaçao a ocupaçao da 'Posicao' por um 'Bloco Indestrutivel'
verificaMapaD :: PosicaoGrelha -> Direcao -> Mapa -> Bool
verificaMapaD (x1,y1) d m = encontraPosicaoMatriz b1 m == (Bloco Indestrutivel) || encontraPosicaoMatriz b2 m == (Bloco Indestrutivel)
                             where b1 | d==C = (x1-1,y1)
                                      | d==D = (x1,y1+1)
                                      | d==B = (x1+1,y1+1)
                                      | d==E = (x1+1,y1-1)
                                   b2 | d==C = (x1-1,y1+1)
                                      | d==D = (x1+1,y1+1)
                                      | d==B = (x1+1,y1)
                                      | d==E = (x1,y1-1)

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes (Estado m l disp) = (Estado (actualizaMapaC (alcanceCanhao disp m) m) (actuListJog disp l) (actuCanhao disp l m))

-- | Utilizando uma lista de posiçoes que sao afectadas pelo Canhao actualiza o mapa após os disparos de Canhao avançarem 1 tick.
--verificaMapaC :: [Posicao] -- ^ A lista de posiçoes afectadas pelos disparos Canhao.
--              -> Mapa -- ^ O 'Mapa' anterior.
--              -> Mapa -- ^ O 'Mapa' actualizado.
actualizaMapaC :: [Posicao] -> Mapa -> Mapa
actualizaMapaC [] m = m
actualizaMapaC ((x,y):xs) m | (encontraPosicaoMatriz (x,y) m)==(Bloco Destrutivel) = (actualizaMapaC xs (atualizaPosicaoMatriz (x,y) Vazia m))
                            | (encontraPosicaoMatriz (x,y) m)==(Bloco Indestrutivel) = actualizaMapaC xs m
                            | (encontraPosicaoMatriz (x,y) m)==Vazia = actualizaMapaC xs m

-- | Actualiza a lista de disparo depois de verificar se os tiros de canhao permanecem nesta, ou seja se a sua nova posição é desempedida de jogadores ou blocos.
--actuCanhao :: [Disparo] -- ^ A lista de 'Disparo' anterior.
--           -> [Jogador] -- ^ A lista de 'Jogador' a percorrer
--           -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--           -> [Disparo] -- ^ A lista de 'Disparo' actualizada
actuCanhao :: [Disparo] -> [Jogador] -> Mapa -> [Disparo]
actuCanhao [] j m = []
actuCanhao ((DisparoCanhao jog (x,y) d):disp) ((Jogador (x1,y1) dj vida laser choques):xs) m | d==C && (verificaMapaC (x,y) d m) && (verificaJogLivreC (DisparoCanhao jog (x,y) d) ((Jogador (x1,y1) dj vida laser choques):xs)) && (colideTiroC (DisparoCanhao jog (x,y) d) ((DisparoCanhao jog (x,y) d):disp)) = (DisparoCanhao jog (x-1,y) d):(actuCanhao disp ((Jogador (x1,y1) dj vida laser choques):xs) m)
                                                                                             | d==D && (verificaMapaC (x,y) d m) && (verificaJogLivreC (DisparoCanhao jog (x,y) d) ((Jogador (x1,y1) dj vida laser choques):xs)) && (colideTiroC (DisparoCanhao jog (x,y) d) ((DisparoCanhao jog (x,y) d):disp)) = (DisparoCanhao jog (x,y+1) d):(actuCanhao disp ((Jogador (x1,y1) dj vida laser choques):xs) m)
                                                                                             | d==B && (verificaMapaC (x,y) d m) && (verificaJogLivreC (DisparoCanhao jog (x,y) d) ((Jogador (x1,y1) dj vida laser choques):xs)) && (colideTiroC (DisparoCanhao jog (x,y) d) ((DisparoCanhao jog (x,y) d):disp)) = (DisparoCanhao jog (x+1,y) d):(actuCanhao disp ((Jogador (x1,y1) dj vida laser choques):xs) m)
                                                                                             | d==E && (verificaMapaC (x,y) d m) && (verificaJogLivreC (DisparoCanhao jog (x,y) d) ((Jogador (x1,y1) dj vida laser choques):xs)) && (colideTiroC (DisparoCanhao jog (x,y) d) ((DisparoCanhao jog (x,y) d):disp)) = (DisparoCanhao jog (x,y-1) d):(actuCanhao disp ((Jogador (x1,y1) dj vida laser choques):xs) m)
                                                                                             | otherwise = (actuCanhao disp ((Jogador (x1,y1) dj vida laser choques):xs) m)
actuCanhao (x:xs) j m = x:(actuCanhao xs j m)

-- | Verifica se o disparo canhao colide com outro sendo que assim se anulam.
--colideTiroC :: Disparo -- ^ O 'Disparo' a verificar'.
--            -> [Disparo] -- ^ A lista de "Disparo" a percorrer.
--            -> Bool -- ^ A resposta em bool á questao se a posicao grelha é segura para o movimento.
colideTiroC :: Disparo -> [Disparo] -> Bool
colideTiroC (DisparoCanhao jog (x,y) d) [] = True
colideTiroC (DisparoCanhao jog1 (x1,y1) d1) ((DisparoCanhao jog (x,y) d):disp) | d1==C && (x1,y1)==(x+1,y) && d/=C = False
                                                                               | d1==D && (x1,y1)==(x,y-1) && d/=D = False
                                                                               | d1==B && (x1,y1)==(x-1,y) && d/=B = False
                                                                               | d1==E && (x1,y1)==(x,y+1) && d/=E = False
                                                                               | otherwise = (colideTiroC (DisparoCanhao jog1 (x1,y1) d1) disp)
colideTiroC (DisparoCanhao jog1 (x1,y1) d1) (x:disp) = (colideTiroC (DisparoCanhao jog1 (x1,y1) d1) disp)

-- | Actulaliza uma lista de jogadores depois de todos os tiros de canhao avançarem 1 tick.
--actuListJog :: [Disparo] -- ^ A lista de "Disparo" a percorrer.
--            -> [Jogador] -- ^ A lista de "Jogador" anterior.
--            -> [Jogador] -- ^ A lista de "Jogador" actualizada.
actuListJog :: [Disparo] -> [Jogador] -> [Jogador]
actuListJog disp [] = []
actuListJog disp ((Jogador (x1,y1) dj vida laser choques):xs) = (actuJog disp (Jogador (x1,y1) dj vida laser choques)):(actuListJog disp xs)

-- | Percorre a lista de disparos de modo a actualizar as vidas de um jogador sempre que este tenha sido atingido.
--actuJog :: [Disparo] -- ^ A lista de 'Disparo' a percorrer.
--        -> Jogador -- ^ O 'Jogador' anterior.
--        -> Jogador -- ^ O lista de 'Jogador' actualizada.
actuJog :: [Disparo] -> Jogador -> Jogador
actuJog [] l = l
actuJog ((DisparoCanhao jog (x,y) d):disp) (Jogador (x1,y1) dj vida laser choques) | d==C && ((x-1,y)/=(x1,y1) && (x-1,y)/=(x1,y1-1) && (x-1,y)/=(x1,y1+1)) || vida==0 = actuJog disp (Jogador (x1,y1) dj vida laser choques)
                                                                                   | d==D && ((x,y+1)/=(x1,y1) && (x,y+1)/=(x1+1,y1) && (x,y+1)/=(x1-1,y1)) || vida==0 = actuJog disp (Jogador (x1,y1) dj vida laser choques)
                                                                                   | d==B && ((x+1,y)/=(x1,y1) && (x+1,y)/=(x1,y1-1) && (x+1,y)/=(x1,y1+1)) || vida==0 = actuJog disp (Jogador (x1,y1) dj vida laser choques)
                                                                                   | d==E && ((x,y-1)/=(x1,y1) && (x,y-1)/=(x1+1,y1) && (x,y-1)/=(x1-1,y1)) || vida==0 = actuJog disp (Jogador (x1,y1) dj vida laser choques)
                                                                                   | otherwise = actuJog disp (Jogador (x1,y1) dj (vida-1) laser choques)
actuJog (x:xs) j = actuJog xs j

-- | Percorre a lista de jogadores para verificar se determinado disparo de canhao nao atinge nenhum dos jogadores.
--verificaJogLivreC :: Disparo -- ^ O "Disparo" a verificar.
--                  -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--                  -> Bool -- ^ A resposta em bolianos á pergunta colocada.
verificaJogLivreC :: Disparo -> [Jogador] -> Bool
verificaJogLivreC disp [] = True
verificaJogLivreC (DisparoCanhao jog (x,y) d) ((Jogador (x1,y1) dj vida laser choques):xs) | d==C && ((x-1,y)/=(x1,y1) && (x-1,y)/=(x1,y1-1) && (x-1,y)/=(x1,y1+1)) || vida==0 = verificaJogLivreC (DisparoCanhao jog (x,y) d) xs
                                                                                           | d==D && ((x,y+1)/=(x1,y1) && (x,y+1)/=(x1+1,y1) && (x,y+1)/=(x1-1,y1)) || vida==0 = verificaJogLivreC (DisparoCanhao jog (x,y) d) xs
                                                                                           | d==B && ((x+1,y)/=(x1,y1) && (x+1,y)/=(x1,y1-1) && (x+1,y)/=(x1,y1+1)) || vida==0 = verificaJogLivreC (DisparoCanhao jog (x,y) d) xs
                                                                                           | d==E && ((x,y-1)/=(x1,y1) && (x,y-1)/=(x1+1,y1) && (x,y-1)/=(x1-1,y1)) || vida==0 = verificaJogLivreC (DisparoCanhao jog (x,y) d) xs
                                                                                           | otherwise = False

-- | Verica todas as posiçoes que sao afectadas pelos disparos de canhao tendo em conta o mapa onde o jogo decorre.
--alcanceCanhao :: [Disparo] -- ^ A lista de 'Disparo' a ter em conta.
--              -> Mapa -- ^ A 'Mapa' onde o jogo decorre.
--              -> [Posicao] -- ^ A lista de "Posicao" afectadas.
alcanceCanhao :: [Disparo] -> Mapa -> [Posicao]
alcanceCanhao [] m = []
alcanceCanhao ((DisparoCanhao jog (x,y) d):disp) m | d==C = (alcanceCanhao disp m)++[(x,y),(x,y+1),(x-1,y),(x-1,y+1)]
                                                   | d==D = (alcanceCanhao disp m)++[(x,y),(x+1,y),(x,y+1),(x+1,y+1)]
                                                   | d==B = (alcanceCanhao disp m)++[(x,y),(x,y+1),(x+1,y),(x+1,y+1)]
                                                   | d==E = (alcanceCanhao disp m)++[(x,y),(x+1,y),(x,y-1),(x+1,y-1)]
alcanceCanhao (c:disp) m = alcanceCanhao disp m

-- | Verifica se a posiçao do mapa corresponde a um bloco 'Vazia' sendo assim possivel o deslocamento do disparo.
--verificaMapaC :: PosicaoGrelha -- ^ A 'PosicaoGrelha' actual do disparo.
--              -> Direcao -- ^ A 'Direcao' a aplicar.
--              -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--              -> Bool -- ^ A resposta em relaçao á nao ocupaçao da 'Posicao' por um 'bloco'.
verificaMapaC :: PosicaoGrelha -> Direcao -> Mapa -> Bool
verificaMapaC (x1,y1) d m = encontraPosicaoMatriz b1 m == Vazia && encontraPosicaoMatriz b2 m == Vazia
                            where b1 | d==C = (x1-1,y1)
                                     | d==D = (x1,y1+1)
                                     | d==B = (x1+1,y1+1)
                                     | d==E = (x1+1,y1-1)
                                  b2 | d==C = (x1-1,y1+1)
                                     | d==D = (x1+1,y1+1)
                                     | d==B = (x1+1,y1)
                                     | d==E = (x1,y1-1)

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques (Estado m l disp) = (Estado m l (actualizaChoq disp))

-- | Percorre a lista de Disparo e actualiza-a após o jogo avançar 1 tick.
--actuLaser :: [Disparo] -- ^ A lista de "Disparo" anterior.
--          -> [Disparo] -- ^ A lista de "Disparo" actualizada.
actualizaChoq  :: [Disparo] -> [Disparo]
actualizaChoq [] = []
actualizaChoq ((DisparoChoque jog ticks):disp) | ticks>0 = (DisparoChoque jog (ticks-1)):(actualizaChoq disp)
                                               | ticks==0 = actualizaChoq disp
actualizaChoq (x:xs) = x:(actualizaChoq xs)