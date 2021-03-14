-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g138 where

import LI11819
import Tarefa0_2018li1g138
import Tarefa1_2018li1g138
-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(1,(Movimenta D),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (1,(Movimenta E),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (0,(Movimenta C),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (0,(Movimenta D),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (0,(Dispara Canhao),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (0,(Dispara Laser),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (2,(Dispara Laser),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (2,(Dispara Laser),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 0 3)] [DisparoChoque 1 5])),
            (2,(Movimenta B),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (2,(Movimenta D),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (2,(Dispara Choque),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 1)] [DisparoChoque 1 5])),
            (1,(Dispara Choque),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 0),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (2,(Movimenta B),(Estado (mapaInicial (15,15)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (12,12) B 3 3 3)] [DisparoChoque 1 5])),
            (2,(Movimenta B),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3),(Jogador (15,14) E 3 3 3)] [DisparoChoque 1 5])),
            (1,(Dispara Canhao),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3)] [DisparoChoque 1 5])),
            (2,(Dispara Laser),(Estado (mapaInicial (15,15)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (12,12) B 3 3 3)] [DisparoChoque 1 5])),
            (2,(Movimenta B),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 3 3 3),(Jogador (15,14) E 0 3 3)] [DisparoChoque 1 5])),
            (2,(Dispara Laser),(Estado (mapaInicial (25,25)) [(Jogador (2,2) C 3 3 3),(Jogador (3,4) D 3 3 3),(Jogador (14,14) B 0 3 3)] [DisparoChoque 1 5]))]

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada jog (Movimenta d) (Estado m l disp) = if temVidas jog l then jogadaMovimenta jog d (Estado m l disp) else (Estado m l disp)
jogada jog (Dispara t) (Estado m l disp) = jogadaDisparo jog t (Estado m l disp)

-- | Actualiza um estado após uma jogada de movimento nomeadamente a lista de "Jogador".
--jogadaMovimenta :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--                -> Direcao -- ^ A 'Direcao' do movimento aplicado.
--                -> Estado -- ^ O 'Estado' anterior.
--                -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogadaMovimenta :: Int -> Direcao -> Estado -> Estado
jogadaMovimenta jog d (Estado m l disp) | dentroAreaChoque jog jog (Estado m l disp) = (Estado m (rodaJogador jog d l) disp)
                                        | otherwise = (Estado m (qualJog jog d l m) disp)

-- | Actualiza uma lista após uma jogada de movimento nomeadamente a lista de "Jogador" (serve para o caso em que o jogador em questão se encontra dentro da área choque mesmo assim efectuar a rotação para a 'Direção' desejada).
--jogadaMovimenta :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--                -> Direcao -- ^ A 'Direcao' do movimento aplicado.
--                -> [Jogador] -- ^ A lista de 'Jogador' anterior.
--                -> [Jogador] -- ^ A lista de 'Jogador' resultante após o jogador efetuar a jogada.
rodaJogador :: Int -> Direcao -> [Jogador] -> [Jogador]
rodaJogador jog d ((Jogador pos dj vida laser choques):xs) | jog>0 = (Jogador pos dj vida laser choques):(rodaJogador (jog-1) d xs)
                                                           | jog==0 = (Jogador pos d vida laser choques):xs

-- | Corre a lista de "Jogador" até chegar ao jogador que vai efectuar o movimento.
--qualJog :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--        -> Direcao -- ^ A 'Direcao' do movimento aplicado.
--        -> [Jogador] -- ^ O lista de "Jogador" anterior.
--        -> Mapa -- ^ O 'Mapa' onde o jogo decorre e que limita as jogadas possiveis.
--        -> [Jogador] -- ^ A lista de "Jogador" actualizada.
qualJog :: Int -> Direcao -> [Jogador] -> Mapa -> [Jogador]
qualJog jog d ((Jogador pos dj vida laser choques):xs) m | jog>0 = (Jogador pos dj vida laser choques):(qualJog (jog-1) d xs m)
                                                         | jog==0 = (posNew jog (Jogador pos dj vida laser choques) d m xs):xs

-- | Actualiza a 'Posicao' do jogador que efectua a jogada.
--posNew :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--       -> Jogador -- ^ O Jogador antes de aplicar o movimento.
--       -> Direcao -- ^ A 'Direcao' do movimento a aplicar.
--       -> Mapa -- ^ O 'Mapa' onde o jogo decorre e que limita as jogadas possiveis.
--       -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--       -> Jogador -- ^ O Jogador com a sua 'Posicao' actualizada.
posNew :: Int -> Jogador -> Direcao -> Mapa -> [Jogador] -> Jogador
posNew jog (Jogador (x,y) dj vida laser choques) m mapa l | m==C && dj==m && x>1 && verificaPosLivre jog m (x-1,y) mapa l = (Jogador (x-1,y) dj vida laser choques)
                                                          | m==D && dj==m && y<((length(head mapa))-1) && verificaPosLivre jog m (x,y+1) mapa l = (Jogador (x,y+1) dj vida laser choques)
                                                          | m==B && dj==m && x<((length mapa)-1) && verificaPosLivre jog m (x+1,y) mapa l = (Jogador (x+1,y) dj vida laser choques)
                                                          | m==E && dj==m && y>1 && verificaPosLivre jog m (x,y-1) mapa l = (Jogador (x,y-1) dj vida laser choques)
                                                          | dj/=m = (Jogador (x,y) m vida laser choques)
                                                          | otherwise = (Jogador (x,y) m vida laser choques)

-- | Verifica se a posiçao do mapa para a qual é suposto o movimento ser efectuada está livre.
--verificaPosLivre :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--                 -> Direcao -- ^ A 'Direcao' do movimento aplicado.
--                 -> PosicaoGrelha -- ^ A 'PosicaoGrelha' que se quer verificar estar livre.
--                 -> Mapa -- ^ O 'Mapa' onde o jogo decorre e que limita as jogadas possiveis.
--                 -> [Jogador] -- ^ A lista de "Jogador" a ser percorrida para verificar a 'Posicao' de todos os jogadores.
--                 -> Bool -- ^ A resposta em relaçao a ocupaçao da 'Posicao'
verificaPosLivre :: Int -> Direcao -> PosicaoGrelha -> Mapa -> [Jogador] -> Bool
verificaPosLivre jog d (x,y) m [] = (verificaPosMapa (x,y) d m)
verificaPosLivre jog d (x,y) m ((Jogador (x1,y1) dj vida laser choques):xs) | d==C && ((x,y) /= (x1,y1) && (x,y) /= (x1,y1-1) && (x,y) /= (x1,y1+1) && (x,y) /= (x1+1,y1-1) && (x,y) /= (x1+1,y1+1) && (x,y) /= (x1+1,y1)) || vida==0 = verificaPosLivre jog d (x,y) m xs
                                                                            | d==D && ((x,y) /= (x1,y1) && (x,y) /= (x1+1,y1) && (x,y) /= (x1-1,y1) && (x,y) /= (x1+1,y1-1) && (x,y) /= (x1-1,y1-1) && (x,y) /= (x1,y1-1)) || vida==0 = verificaPosLivre jog d (x,y) m xs
                                                                            | d==B && ((x,y) /= (x1,y1) && (x,y) /= (x1,y1-1) && (x,y) /= (x1,y1+1) && (x,y) /= (x1-1,y1-1) && (x,y) /= (x1-1,y1+1) && (x,y) /= (x1-1,y1)) || vida==0 = verificaPosLivre jog d (x,y) m xs
                                                                            | d==E && ((x,y) /= (x1,y1) && (x,y) /= (x1+1,y1) && (x,y) /= (x1-1,y1) && (x,y) /= (x1+1,y1+1) && (x,y) /= (x1-1,y1+1) && (x,y) /= (x1,y1+1)) || vida==0 = verificaPosLivre jog d (x,y) m xs
                                                                            | otherwise = False

-- | Verifica se a posiçao do mapa corresponde a um bloco 'Vazia' sendo assim possivel o movimento.
--verificaPosMapa :: PosicaoGrelha -- ^ A 'PosicaoGrelha' a verificar.
--                -> Direcao -- ^ A 'Direcao' a aplicar.
--                -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--                -> Bool -- ^ A resposta em relaçao a ocupaçao da 'Posicao' por um 'bloco'
verificaPosMapa :: PosicaoGrelha -> Direcao -> Mapa -> Bool
verificaPosMapa (x1,y1) d m = encontraPosicaoMatriz b1 m == Vazia && encontraPosicaoMatriz b2 m == Vazia
                                                               where b1 | d==C = (x1-1,y1)
                                                                        | d==D = (x1,y1+2)
                                                                        | d==B = (x1+2,y1+1)
                                                                        | d==E = (x1+1,y1-1)
                                                                     b2 | d==C = (x1-1,y1+1)
                                                                        | d==D = (x1+1,y1+2)
                                                                        | d==B = (x1+2,y1)
                                                                        | d==E = (x1,y1-1)

--jogadaDisparo :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--              -> Arma -- ^ A 'Arma' usada no disparo efectuado.
--              -> Estado -- ^ O 'Estado' anterior.
--              -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogadaDisparo :: Int -> Arma -> Estado -> Estado
jogadaDisparo jog Canhao (Estado m l disp) = if temVidas jog l then (Estado m (actualizaListaJ jog Canhao l) ((DisparoCanhao jog (encontraPosTiro jog l) (encontraDirTiro jog l)):disp)) else (Estado m l disp)
jogadaDisparo jog Laser (Estado m l disp) = if temLaser jog l && temVidas jog l then (Estado m (actualizaListaJ jog Laser l) ((DisparoLaser jog (encontraPosTiro jog l) (encontraDirTiro jog l)):disp)) else (Estado m l disp)
jogadaDisparo jog Choque (Estado m l disp) = if temChoque jog l && temVidas jog l then (Estado m (actualizaListaJ jog Choque l) ((DisparoChoque jog 5):disp)) else (Estado m l disp)

-- | Actualiza a lista de jogadores após um disparo.
--actualizaListaJ :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--               -> Arma -- ^ A 'Arma' usada no disparo efectuado.
--               -> [Jogador] -- ^ A lista de "Jogador" anterior.
--               -> [Jogador] -- ^ A lista de "Jogador" actualizada.
actualizaListaJ :: Int -> Arma -> [Jogador] -> [Jogador]
actualizaListaJ jog t ((Jogador (x,y) dj vida laser choques):xs) | jog>0 = (Jogador (x,y) dj vida laser choques):(actualizaListaJ (jog-1) t xs)
                                                                 | jog==0 && t==Canhao = ((Jogador (x,y) dj vida laser choques):xs)
                                                                 | jog==0 && t==Laser = ((Jogador (x,y) dj vida (laser-1) choques):xs)
                                                                 | jog==0 && t==Choque = ((Jogador (x,y) dj vida laser (choques-1)):xs)

-- | Descobre qual a posiçao de um tiro dado por determinado jogador.
--encontraPosTiro :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--                -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--                -> PosicaoGrelha -- ^ A 'PosicaoGrelha' do tiro efectuado.
encontraPosTiro :: Int -> [Jogador] -> PosicaoGrelha
encontraPosTiro jog ((Jogador (x,y) dj vida laser choques):xs) | jog>0 = encontraPosTiro (jog-1) xs
                                                               | jog==0 && dj==C = (x-1,y)
                                                               | jog==0 && dj==D = (x,y+1)
                                                               | jog==0 && dj==B = (x+1,y)
                                                               | jog==0 && dj==E = (x,y-1)

-- | Descobre qual a direcao de um tiro dado por determinado jogador.
--encontraDirTiro :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--                -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--                -> Direcao -- ^ A 'Direcao' do jogador que efectua a jogada.
encontraDirTiro :: Int -> [Jogador] -> Direcao
encontraDirTiro jog ((Jogador (x,y) dj vida laser choques):xs) | jog>0 = encontraDirTiro (jog-1) xs
                                                               | jog==0 = dj

-- | Serve para descobrir a vizinhança de uma determinada posicao (area que o choque afecta).
--areaChoque :: PosicaoGrelha -- ^ As 'Posicao' a testar.
--           -> PosicaoGrelha -- ^ A 'Posicao' central.
--           -> Bool -- ^ A resposta á pergunta se as posiçoes estao dentro da area pretendida.
areaChoque :: PosicaoGrelha -> PosicaoGrelha -> Bool
areaChoque (x,y) (x1,y1) | (x <= (x1+2) && x >= (x1-2)) && (y <= (y1+2) && y >= (y1-2)) = True
                         | otherwise = False

-- | Serve para descobrir se o jogador que efectua a jogada tem algum jogador na sua area de efeito.
--verificaArea :: Int -- ^ O identificador do 'Jogador' que queremos verificar se está na area de efeito do choque.
--             -> Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--             -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--             -> Bool -- ^ A resposta em bolianos á pergunta se existe algum jogador na area de efeito do choque.
verificaArea :: Int -> Int -> [Jogador] -> Bool
verificaArea jog j (x:xs) = areaChoque posJog posJ
                             where posJog = encontraPosJog jog (x:xs)
                                   posJ = encontraPosJog j (x:xs)

-- | Serve para descobrir 'Posicao' de um determinado jogador.
--verificaArea :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--             -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--             -> PosicaoGrelha -- ^ A 'Posicao' do jogador pretendido.
encontraPosJog :: Int -> [Jogador] -> PosicaoGrelha
encontraPosJog jog ((Jogador (x,y) dj vida laser choques):xs) | jog>0 = encontraPosJog (jog-1) xs
                                                              | jog==0 = (x,y)

-- | Serve para Verificar se o jogador que efectua a jogada se encontra sobre o efeito de algum choque.
--verificaArea :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--             -> Int -- ^ O identificador do 'Jogador' que queremos testar se tem um choque activo.
--             -> Estado -- ^ O 'Estado a percorrer.
--             -> Bool -- ^ A resposta em bolianos á pergunta se o jogador que efectua a jogada se encontra sobre o efeito de algum choque.
dentroAreaChoque :: Int -> Int -> Estado -> Bool
dentroAreaChoque jog j (Estado m l disp) | jog>0 = dentroAreaChoque (jog-1) j (Estado m l disp)
                                         | jog==0 = choqueAtivo j l disp
                                         | otherwise = False
                                                       where choqueAtivo :: Int -> [Jogador] -> [Disparo] -> Bool
                                                             choqueAtivo j l [] = False
                                                             choqueAtivo j l ((DisparoCanhao jog (x,y) d):xs) = choqueAtivo j l xs
                                                             choqueAtivo j l ((DisparoLaser jog (x,y) d):xs) = choqueAtivo j l xs
                                                             choqueAtivo j l ((DisparoChoque jog ticks):xs) | jog==j = choqueAtivo j l xs
                                                                                                            | jog/=j && verificaArea j jog l = True
                                                                                                            | otherwise = choqueAtivo j l xs

-- | Serve para descobrir se o jogagador em questao possui muições na arma 'Choque'.
--temChoque :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--          -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--          -> Bool -- ^ A resposta em bolianos relativamente á posse de munições na arma 'Choque'.
temChoque :: Int -> [Jogador] -> Bool
temChoque jog ((Jogador pos dj vida laser choques):xs) | jog>0 = temChoque (jog-1) xs
                                                       | jog==0 && choques>0 = True
                                                       | otherwise = False

-- | Serve para descobrir se o jogagador em questao possui muições na arma 'Laser'.
--temLaser :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--         -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--         -> Bool -- ^ A resposta em bolianos relativamente á posse de munições na arma 'Laser'.
temLaser :: Int -> [Jogador] -> Bool
temLaser jog ((Jogador pos dj vida laser choques):xs) | jog>0 = temLaser (jog-1) xs
                                                      | jog==0 && laser>0 = True
                                                      | otherwise = False

-- | Serve para descobrir se o jogagador em questao possui vidas.
--temVidas :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--         -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--         -> Bool -- ^ A resposta em bolianos relativamente á posse de 'vida'.
temVidas :: Int -> [Jogador] -> Bool
temVidas jog ((Jogador pos dj vida laser choques):xs) | jog>0 = temVidas (jog-1) xs
                                                      | jog==0 && vida>0 = True
                                                      | otherwise = False