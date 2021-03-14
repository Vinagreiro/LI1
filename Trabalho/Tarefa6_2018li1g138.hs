-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2018li1g138 where

import LI11819
import Tarefa0_2018li1g138
import Tarefa1_2018li1g138
import Tarefa2_2018li1g138
import Tarefa4_2018li1g138

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot jog (Estado m l disp) | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (saiDaBorda (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (desviaTiroCnear disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (desviaTiroCnear disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (desviaTiroCnear disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (desviaTiroCnear disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (laserProtector disp (playerJogador jog l) m) && (temLaser jog l) = Just (Dispara Laser)
                          | (laserProtector disp (playerJogador jog l) m) && (temLaser jog l)==False = Just (Dispara Canhao)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (desviaTiroC disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (desviaTiroProo disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe3Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (desviaTiroProo disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe3Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (desviaTiroProo disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe3Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (desviaTiroProo disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe3Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (desviaTiroProo disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe3Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (desviaTiroProo disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe3Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (desviaTiroProo disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe3Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (desviaTiroProo disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe3Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (canhaoDefende disp (playerJogador jog l) m) = Just (Dispara Canhao)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==C && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==D && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==B && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (linha3X disp (playerJogador jog l) m) && (dirJogador jog l)==E && (tirosAdjacenteSafe2Casas jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (naoTeMexas disp (playerJogador jog l) m) = Just (Dispara Canhao)
                          | (eliminaLinha jog l (posJogador jog l) (dirJogador jog l) m) && (temLaser jog l) = Just (Dispara Laser)
                          | (disparaCanhaoNoob l (posJogador jog l) (dirJogador jog l) m) = Just (Dispara Canhao)
                          | (multiDisparo l (posJogador jog l) (dirJogador jog l) m) = Just (Dispara Canhao)
                          | (eliminaLinha jog l (posJogador jog l) (dirJogador jog l) m) && (temLaser jog l)==False = Just (Dispara Canhao)
                          | (choquessss (posJogador jog l) (qualPlayer jog l)) && (temChoque jog l) && (choqueAct jog disp)==False = Just (Dispara Choque)
                          | (demolidor (playerJogador jog l) m) = Just (Dispara Canhao)
                          | (verificaMapaIndestr (posJogador jog l) C m) && (verificaPosLivre jog D (encontraPosicJog jog l D) m l) = Just (Movimenta D)
                          | (verificaMapaIndestr (posJogador jog l) C m) && (verificaPosLivre jog B (encontraPosicJog jog l B) m l) = Just (Movimenta B)
                          | (verificaMapaIndestr (posJogador jog l) C m) && (verificaPosLivre jog E (encontraPosicJog jog l E) m l) = Just (Movimenta E)
                          | (verificaMapaIndestr (posJogador jog l) D m) && (verificaPosLivre jog C (encontraPosicJog jog l C) m l) = Just (Movimenta C)
                          | (verificaMapaIndestr (posJogador jog l) D m) && (verificaPosLivre jog B (encontraPosicJog jog l B) m l) = Just (Movimenta B)
                          | (verificaMapaIndestr (posJogador jog l) D m) && (verificaPosLivre jog E (encontraPosicJog jog l E) m l) = Just (Movimenta E)
                          | (verificaMapaIndestr (posJogador jog l) B m) && (verificaPosLivre jog C (encontraPosicJog jog l C) m l) = Just (Movimenta C)
                          | (verificaMapaIndestr (posJogador jog l) B m) && (verificaPosLivre jog D (encontraPosicJog jog l D) m l) = Just (Movimenta D)
                          | (verificaMapaIndestr (posJogador jog l) B m) && (verificaPosLivre jog E (encontraPosicJog jog l E) m l) = Just (Movimenta E)
                          | (verificaMapaIndestr (posJogador jog l) E m) && (verificaPosLivre jog C (encontraPosicJog jog l C) m l) = Just (Movimenta C)
                          | (verificaMapaIndestr (posJogador jog l) E m) && (verificaPosLivre jog D (encontraPosicJog jog l D) m l) = Just (Movimenta D)
                          | (verificaMapaIndestr (posJogador jog l) E m) && (verificaPosLivre jog B (encontraPosicJog jog l B) m l) = Just (Movimenta B)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[C] && (verificaPosLivre jog C (encontraPosicJog jog l C) m l) = Just (Movimenta C)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[C] && (dirJogador jog l)/=C && (tirosAdjacenteDest jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[C] && (dirJogador jog l)==C && (tirosAdjacenteDest jog C (playerJogador jog l) disp l m) = Just (Dispara Canhao)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[C] && (tirosAdjacenteSafe jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[C] && (tirosAdjacenteSafe jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[C] && (tirosAdjacenteSafe jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[D] && (verificaPosLivre jog D (encontraPosicJog jog l D) m l) = Just (Movimenta D)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[D] && (dirJogador jog l)/=D && (tirosAdjacenteDest jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[D] && (dirJogador jog l)==D && (tirosAdjacenteDest jog D (playerJogador jog l) disp l m) = Just (Dispara Canhao)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[D] && (tirosAdjacenteSafe jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[D] && (tirosAdjacenteSafe jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[D] && (tirosAdjacenteSafe jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[B] && (verificaPosLivre jog B (encontraPosicJog jog l B) m l) = Just (Movimenta B)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[B] && (dirJogador jog l)/=B && (tirosAdjacenteDest jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[B] && (dirJogador jog l)==B && (tirosAdjacenteDest jog B (playerJogador jog l) disp l m) = Just (Dispara Canhao)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[B] && (tirosAdjacenteSafe jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[B] && (tirosAdjacenteSafe jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[B] && (tirosAdjacenteSafe jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[E] && (verificaPosLivre jog E (encontraPosicJog jog l E) m l) = Just (Movimenta E)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[E] && (dirJogador jog l)/=E && (tirosAdjacenteDest jog E (playerJogador jog l) disp l m) = Just (Movimenta E)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[E] && (dirJogador jog l)==E && (tirosAdjacenteDest jog E (playerJogador jog l) disp l m) = Just (Dispara Canhao)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[E] && (tirosAdjacenteSafe jog B (playerJogador jog l) disp l m) = Just (Movimenta B)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[E] && (tirosAdjacenteSafe jog C (playerJogador jog l) disp l m) = Just (Movimenta C)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[E] && (tirosAdjacenteSafe jog D (playerJogador jog l) disp l m) = Just (Movimenta D)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[C,D] && (verificaPosLivre jog D (encontraPosicJog jog l D) m l) = Just (Movimenta D)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[C,D] && (verificaPosLivre jog C (encontraPosicJog jog l C) m l) = Just (Movimenta C)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[C,E] && (verificaPosLivre jog E (encontraPosicJog jog l E) m l) = Just (Movimenta E)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[C,E] && (verificaPosLivre jog C (encontraPosicJog jog l C) m l) = Just (Movimenta C)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[B,D] && (verificaPosLivre jog B (encontraPosicJog jog l B) m l) = Just (Movimenta B)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[B,D] && (verificaPosLivre jog D (encontraPosicJog jog l D) m l) = Just (Movimenta D)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[B,E] && (verificaPosLivre jog B (encontraPosicJog jog l B) m l) = Just (Movimenta B)
                          | (cacaKill (procuraSoro (qualPlayer jog l)) (playerJogador jog l) m)==[B,E] && (verificaPosLivre jog E (encontraPosicJog jog l E) m l) = Just (Movimenta E)
                          | otherwise = Just (Dispara Canhao)

-- | Verifica se a posiçao do mapa corresponde a um bloco 'Indestrutivel' sendo assim impossivel o destruir o bloco.
--verificaMapaIndestr :: PosicaoGrelha -- ^ A 'PosicaoGrelha' actual.
--                    -> Direcao -- ^ A 'Direcao' a aplicar.
--                    -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--                    -> Bool -- ^ A resposta em relaçao á nao ocupaçao da 'Posicao' por um 'Bloco Indestrutivel'.
verificaMapaIndestr :: PosicaoGrelha -> Direcao -> Mapa -> Bool
verificaMapaIndestr (x1,y1) d m = encontraPosicaoMatriz b1 m == (Bloco Indestrutivel) || encontraPosicaoMatriz b2 m == (Bloco Indestrutivel)
                                    where b1 | d==C = (x1-1,y1)
                                             | d==D = (x1,y1+2)
                                             | d==B = (x1+2,y1+1)
                                             | d==E = (x1+1,y1-1)
                                          b2 | d==C = (x1-1,y1+1)
                                             | d==D = (x1+1,y1+2)
                                             | d==B = (x1+2,y1)
                                             | d==E = (x1,y1-1)

-- | Verifica se existem Blocos destrutiveis a obstruir o caminho.
--demolidor :: Jogador -- ^ O 'Jogador' a ter em conta.
--          -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--          -> Bool -- ^ A resposta em relaçao á necessidade de usar o canhao para destruir os blocos.
demolidor :: Jogador -> Mapa -> Bool
demolidor (Jogador (x,y) dj vida laser choques) m | dj==C && (verificaMapaDestr (x,y) dj m) = True
                                                  | dj==D && (verificaMapaDestr (x,y) dj m) = True
                                                  | dj==B && (verificaMapaDestr (x,y) dj m) = True
                                                  | dj==E && (verificaMapaDestr (x,y) dj m) = True
                                                  | otherwise = False

-- | Verifica se a posiçao do mapa para a qual é suposto o movimento ser efectuada é destruitivel.
--verificaPosDestr :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--                 -> Direcao -- ^ A 'Direcao' do movimento aplicado.
--                 -> PosicaoGrelha -- ^ A 'PosicaoGrelha' que se quer verificar estar livre.
--                 -> Mapa -- ^ O 'Mapa' onde o jogo decorre e que limita as jogadas possiveis.
--                 -> [Jogador] -- ^ A lista de "Jogador" a ser percorrida para verificar a 'Posicao' de todos os jogadores.
--                 -> Bool -- ^ A resposta em relaçao a ocupaçao da 'Posicao' somente por blocos destruitiveis.
verificaPosDestr :: Int -> Direcao -> PosicaoGrelha -> Mapa -> [Jogador] -> Bool
verificaPosDestr jog d (x,y) m [] = (verificaMapaDestr (x,y) d m)
verificaPosDestr jog d (x,y) m ((Jogador (x1,y1) dj vida laser choques):xs) | d==C && ((x,y) /= (x1,y1) && (x,y) /= (x1,y1-1) && (x,y) /= (x1,y1+1) && (x,y) /= (x1+1,y1-1) && (x,y) /= (x1+1,y1+1) && (x,y) /= (x1+1,y1)) || vida==0 = verificaPosDestr jog d (x,y) m xs
                                                                            | d==D && ((x,y) /= (x1,y1) && (x,y) /= (x1+1,y1) && (x,y) /= (x1-1,y1) && (x,y) /= (x1+1,y1-1) && (x,y) /= (x1-1,y1-1) && (x,y) /= (x1,y1-1)) || vida==0 = verificaPosDestr jog d (x,y) m xs
                                                                            | d==B && ((x,y) /= (x1,y1) && (x,y) /= (x1,y1-1) && (x,y) /= (x1,y1+1) && (x,y) /= (x1-1,y1-1) && (x,y) /= (x1-1,y1+1) && (x,y) /= (x1-1,y1)) || vida==0 = verificaPosDestr jog d (x,y) m xs
                                                                            | d==E && ((x,y) /= (x1,y1) && (x,y) /= (x1+1,y1) && (x,y) /= (x1-1,y1) && (x,y) /= (x1+1,y1+1) && (x,y) /= (x1-1,y1+1) && (x,y) /= (x1,y1+1)) || vida==0 = verificaPosDestr jog d (x,y) m xs
                                                                            | otherwise = False

-- | Verifica se a posicaogrelha para a qual pretendemos efectuar o movimento é segura, ou seja livre de disparos proximos com essa mesma direcao, blocos indestruiveis ou jogadores adversarios.
--tirosAdjacenteDest :: Int -- ^ O identificador do 'Jogador' pretendido.
--                   -> Direcao -- ^ A 'Direcao a aplicar'.
--                   -> Jogador -- ^ O 'Jogador' que efectua a jogada
--                   -> [Disparo] -- ^ A lista de "Disparo" a percorrer.
--                   -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--                   -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--                   -> Bool -- ^ A resposta em bool á questao se a posicao grelha é segura para o movimento.
tirosAdjacenteDest :: Int -> Direcao -> Jogador -> [Disparo] -> [Jogador] -> Mapa -> Bool
tirosAdjacenteDest jog d (Jogador (x,y) dj vida laser choques) disp l m | d==C && x>1 && (verificaPosDestr jog d (x-1,y) m l) && (existeTiro (x-1,y) disp) = True
                                                                        | d==D && y<((length(head m))-1) && (verificaPosDestr jog d (x,y+1) m l) && (existeTiro (x,y+1) disp) = True
                                                                        | d==B && x<((length m)-1) && (verificaPosDestr jog d (x+1,y) m l) && (existeTiro (x+1,y) disp) = True
                                                                        | d==E && y>1 && (verificaPosDestr jog d (x,y-1) m l) && (existeTiro (x,y-1) disp) = True
                                                                        | otherwise = False                                                                        

-- | Verifica se a posiçao do mapa corresponde a um bloco 'Vazia' ou 'Destrutivel' sendo assim possivel o destruir o bloco.
--verificaMapaDestr :: PosicaoGrelha -- ^ A 'PosicaoGrelha' actual.
--                  -> Direcao -- ^ A 'Direcao' a aplicar.
--                  -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--                  -> Bool -- ^ A resposta em relaçao á nao ocupaçao da 'Posicao' por um 'Bloco Destrutivel'.
verificaMapaDestr :: PosicaoGrelha -> Direcao -> Mapa -> Bool
verificaMapaDestr (x1,y1) d m = encontraPosicaoMatriz b1 m == (Bloco Destrutivel) && encontraPosicaoMatriz b2 m == (Bloco Destrutivel)
                                    where b1 | d==C = (x1-1,y1)
                                             | d==D = (x1,y1+2)
                                             | d==B = (x1+2,y1+1)
                                             | d==E = (x1+1,y1-1)
                                          b2 | d==C = (x1-1,y1+1)
                                             | d==D = (x1+1,y1+2)
                                             | d==B = (x1+2,y1)
                                             | d==E = (x1,y1-1)

-- | Verifica se a posicaogrelha para a qual pretendemos efectuar o movimento é livre de disparos canhao para prever colisao.
--existeTiroC :: PosicaoGrelha -- ^ A 'PosicaoGrelha' para a qual o movimento é pretendido'.
--            -> [Disparo] -- ^ A lista de "Disparo" a percorrer.
--            -> Bool -- ^ A resposta em bool á questao se a posicao grelha é segura para o movimento.
existeTiroC :: PosicaoGrelha -> [Disparo] -> Bool
existeTiroC (x1,y1) [] = True
existeTiroC (x1,y1) ((DisparoCanhao jog (x,y) d):disp) | d==C && (x,y)==(x1+1,y1) = False
                                                       | d==D && (x,y)==(x1,y1-1) = False
                                                       | d==B && (x,y)==(x1-1,y1) = False
                                                       | d==E && (x,y)==(x1,y1+1) = False
                                                       | otherwise = (existeTiroC (x1,y1) disp)
existeTiroC (x1,y1) (x:disp) = (existeTiroC (x1,y1) disp)

-- | Descobre se o jogador que efectua a jogada tem algum adveersário na sua área de choque.
--choquessss :: PosicaoGrelha -- ^ A 'PosicaoGrelha' do 'Jogador' que efetua a jogada.
--           -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--           -> Bool -- ^ A resposta em bolianos á questao se o 'Jogador' tem algum adversário na sua área de choque.
choquessss :: PosicaoGrelha -> [Jogador] -> Bool
choquessss (x,y) [] = False
choquessss (x,y) ((Jogador (x1,y1) dj vida laser choques):xs) | (areaChoque (x1,y1) (x,y)) && vida/=0 = True
                                                              | otherwise = (choquessss (x,y) xs)

-- | Descobre se o jogador que efectua a jogada já tem algum disparo choque activo.
--choqueAct :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--          -> [Disparo] -- ^ A lista de "Disparo" a percorrer.
--          -> Bool -- ^ A resposta em bolianos á questao se o 'Jogador' que efectua a jogada ja tem algum choque ativo.
choqueAct :: Int -> [Disparo] -> Bool
choqueAct j [] = False
choqueAct j ((DisparoCanhao jog (x,y) d):xs) = choqueAct j xs
choqueAct j ((DisparoLaser jog (x,y) d):xs) = choqueAct j xs
choqueAct j ((DisparoChoque jog ticks):xs) | jog/=j = choqueAct j xs
                                           | jog==j = True

-- | Descobre a suposta posicao de um jogador após este realizar um movimento numa determinada direcao.
--encontraPosicJog :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
--                 -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--                 -> Direcao -- ^ A 'Direcao a aplicar'.
--                 -> PosicaoGrelha -- ^ A 'PosicaoGrelha' do tiro efectuado.
encontraPosicJog :: Int -> [Jogador] -> Direcao -> PosicaoGrelha
encontraPosicJog jog ((Jogador (x,y) dj vida laser choques):xs) d | jog>0 = encontraPosTiro (jog-1) xs
                                                                  | jog==0 && d==C = (x-1,y)
                                                                  | jog==0 && d==D = (x,y+1)
                                                                  | jog==0 && d==B = (x+1,y)
                                                                  | jog==0 && d==E = (x,y-1)

-- | Verifica se o jogador se encontra numa bora e voltado para esta.
--saiDaBorda :: Jogador -- ^ O 'Jogador' a ter em conta.
--           -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--           -> Bool -- ^ A resposta em relaçao á posicao do jogador.
saiDaBorda :: Jogador -> Mapa -> Bool
saiDaBorda (Jogador (x,y) dj vida laser choques) m | dj==C && x==1 = True
                                                   | dj==D && y==((length(head m))-2) = True
                                                   | dj==B && x==((length m)-2) = True
                                                   | dj==E && y==1 = True
                                                   | otherwise = False

-- | Verifica se existem disparos com a direçao do jogador.
--linha3X :: [Disparo] -- ^ A lista de "Disparo" a percorrrer.
--           -> Jogador -- ^ O 'Jogador' a ter em conta.
--           -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--           -> Bool -- ^ A resposta em relaçao á necessidade de usar o canhao defensivamente.
linha3X :: [Disparo] -> Jogador -> Mapa -> Bool
linha3X [] (Jogador (x,y) dj vida laser choques) m = False
linha3X ((DisparoCanhao jog (x,y) d):disp) (Jogador (x1,y1) dj vida laser choques) m | d==C && (x,y)==(x1+3,y1) && (verificaMapaC (x,y) d m) && (verificaMapaC (x-1,y) d m) = True
                                                                                     | d==D && (x,y)==(x1,y1-3) && (verificaMapaC (x,y) d m) && (verificaMapaC (x,y+1) d m) = True
                                                                                     | d==B && (x,y)==(x1-3,y1) && (verificaMapaC (x,y) d m) && (verificaMapaC (x+1,y) d m) = True
                                                                                     | d==E && (x,y)==(x1,y1+3) && (verificaMapaC (x,y) d m) && (verificaMapaC (x,y-1) d m) = True
                                                                                     | otherwise = (linha3X disp (Jogador (x1,y1) dj vida laser choques) m)
linha3X (x:disp) (Jogador (x1,y1) dj vida laser choques) m = (linha3X disp (Jogador (x1,y1) dj vida laser choques) m)

-- | Verifica se nas posicçoes frontais do jogador existem disparos com a sua direçao para os quais é possivel defender-se com um tiro de canhao.
--canhaoDefende :: [Disparo] -- ^ A lista de "Disparo" a percorrrer.
--              -> Jogador -- ^ O 'Jogador' a ter em conta.
--              -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--              -> Bool -- ^ A resposta em relaçao á necessidade de usar o canhao defensivamente.
canhaoDefende :: [Disparo] -> Jogador -> Mapa -> Bool
canhaoDefende [] (Jogador (x,y) dj vida laser choques) m = False
canhaoDefende ((DisparoCanhao jog (x,y) d):disp) (Jogador (x1,y1) dj vida laser choques) m | d==C && dj==B && (x,y)==(x1+3,y1) && (verificaMapaC (x,y) d m) && (verificaMapaC (x-1,y) d m) = True
                                                                                           | d==D && dj==E && (x,y)==(x1,y1-3) && (verificaMapaC (x,y) d m) && (verificaMapaC (x,y+1) d m) = True
                                                                                           | d==B && dj==C && (x,y)==(x1-3,y1) && (verificaMapaC (x,y) d m) && (verificaMapaC (x+1,y) d m) = True
                                                                                           | d==E && dj==D && (x,y)==(x1,y1+3) && (verificaMapaC (x,y) d m) && (verificaMapaC (x,y-1) d m) = True
                                                                                           | otherwise = (canhaoDefende disp (Jogador (x1,y1) dj vida laser choques) m)
canhaoDefende (x:disp) (Jogador (x1,y1) dj vida laser choques) m = (canhaoDefende disp (Jogador (x1,y1) dj vida laser choques) m)

-- | Verifica se a posicaogrelha para a qual pretendemos efectuar o movimento é segura, ou seja livre de disparos proximos com essa mesma direcao, blocos ou jogadores adversarios.
--tirosAdjacenteSafe2Casas :: Int -- ^ O identificador do 'Jogador' pretendido.
--                         -> Direcao -- ^ A 'Direcao a aplicar'.
--                         -> Jogador -- ^ O 'Jogador' que efectua a jogada
--                         -> [Disparo] -- ^ A lista de "Disparo" a percorrer.
--                         -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--                         -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--                         -> Bool -- ^ A resposta em bool á questao se a posicao grelha é segura para o movimento.
tirosAdjacenteSafe2Casas :: Int -> Direcao -> Jogador -> [Disparo] -> [Jogador] -> Mapa -> Bool
tirosAdjacenteSafe2Casas jog d (Jogador (x,y) dj vida laser choques) disp l m | d==C && x>2 && (verificaPosLivre jog d (x-1,y) m l) && (verificaPosLivre jog d (x-2,y) m l) && (existeTiro (x-2,y) disp) = True
                                                                              | d==D && y<((length(head m))-2) && (verificaPosLivre jog d (x,y+1) m l) && (verificaPosLivre jog d (x,y+2) m l) && (existeTiro (x,y+2) disp) = True
                                                                              | d==B && x<((length m)-2) && (verificaPosLivre jog d (x+1,y) m l) && (verificaPosLivre jog d (x+2,y) m l) && (existeTiro (x+2,y) disp) = True
                                                                              | d==E && y>2 && (verificaPosLivre jog d (x,y-1) m l) && (verificaPosLivre jog d (x,y-2) m l) && (existeTiro (x,y-2) disp) = True
                                                                              | otherwise = False

-- | Verifica se a posicaogrelha para a qual pretendemos efectuar o movimento é segura, ou seja livre de disparos proximos com essa mesma direcao, blocos ou jogadores adversarios.
--tirosAdjacenteSafe :: Int -- ^ O identificador do 'Jogador' pretendido.
--                   -> Direcao -- ^ A 'Direcao a aplicar'.
--                   -> Jogador -- ^ O 'Jogador' que efectua a jogada
--                   -> [Disparo] -- ^ A lista de "Disparo" a percorrer.
--                   -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--                   -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--                   -> Bool -- ^ A resposta em bool á questao se a posicao grelha é segura para o movimento.
tirosAdjacenteSafe :: Int -> Direcao -> Jogador -> [Disparo] -> [Jogador] -> Mapa -> Bool
tirosAdjacenteSafe jog d (Jogador (x,y) dj vida laser choques) disp l m | d==C && x>1 && (verificaPosLivre jog d (x-1,y) m l) && (existeTiro (x-1,y) disp) = True
                                                                        | d==D && y<((length(head m))-1) && (verificaPosLivre jog d (x,y+1) m l) && (existeTiro (x,y+1) disp) = True
                                                                        | d==B && x<((length m)-1) && (verificaPosLivre jog d (x+1,y) m l) && (existeTiro (x+1,y) disp) = True
                                                                        | d==E && y>1 && (verificaPosLivre jog d (x,y-1) m l) && (existeTiro (x,y-1) disp) = True
                                                                        | otherwise = False

-- | Verifica se a posicaogrelha para a qual pretendemos efectuar o movimento é segura, ou seja livre de disparos proximos com essa mesma direcao, blocos ou jogadores adversarios.
--tirosAdjacenteSafe3Casas :: Int -- ^ O identificador do 'Jogador' pretendido.
--                         -> Direcao -- ^ A 'Direcao a aplicar'.
--                         -> Jogador -- ^ O 'Jogador' que efectua a jogada
--                         -> [Disparo] -- ^ A lista de "Disparo" a percorrer.
--                         -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--                         -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--                         -> Bool -- ^ A resposta em bool á questao se a posicao grelha é segura para o movimento.
tirosAdjacenteSafe3Casas :: Int -> Direcao -> Jogador -> [Disparo] -> [Jogador] -> Mapa -> Bool
tirosAdjacenteSafe3Casas jog d (Jogador (x,y) dj vida laser choques) disp l m | d==C && x>3 && (verificaPosLivre jog d (x-1,y) m l) && (verificaPosLivre jog d (x-2,y) m l) && (verificaPosLivre jog d (x-3,y) m l) && (existeTiro (x-1,y) disp) && (existeTiro (x-2,y) disp) && (existeTiro (x-3,y) disp) = True
                                                                              | d==D && y<((length(head m))-3) && (verificaPosLivre jog d (x,y+1) m l) && (verificaPosLivre jog d (x,y+2) m l) && (verificaPosLivre jog d (x,y+3) m l) && (existeTiro (x,y+1) disp) && (existeTiro (x,y+2) disp) && (existeTiro (x,y+3) disp) = True
                                                                              | d==B && x<((length m)-3) && (verificaPosLivre jog d (x+1,y) m l) && (verificaPosLivre jog d (x+2,y) m l) && (verificaPosLivre jog d (x+3,y) m l) && (existeTiro (x+1,y) disp) && (existeTiro (x+2,y) disp) && (existeTiro (x+3,y) disp) = True
                                                                              | d==E && y>3 && (verificaPosLivre jog d (x,y-1) m l) && (verificaPosLivre jog d (x,y-2) m l) && (verificaPosLivre jog d (x,y-3) m l) && (existeTiro (x,y-1) disp) && (existeTiro (x,y-2) disp) && (existeTiro (x,y-3) disp) = True
                                                                              | otherwise = False

-- | Verifica se existem disparos em determinadas posiçoes circundantes do jogador para que ele reaja de determinada forma.
--desviaTiroProo :: [Disparo] -- ^ A lista de "Disparo" a percorrrer.
--               -> Jogador -- ^ O 'Jogador' a ter em conta.
--               -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--               -> Bool -- ^ A resposta em relaçao á necessidade de movimento.
desviaTiroProo :: [Disparo] -> Jogador -> Mapa -> Bool
desviaTiroProo [] (Jogador (x,y) dj vida laser choques) m = False
desviaTiroProo ((DisparoCanhao jog (x,y) d):disp) (Jogador (x1,y1) dj vida laser choques) m | d==C && dj==B && ((x,y)==(x1+3,y1-1) || (x,y)==(x1+3,y1+1)) && (verificaMapaC (x,y) d m) && (verificaMapaC (x-1,y) d m) = True
                                                                                            | d==D && dj==E && ((x,y)==(x1-3,y1-1) || (x,y)==(x1+3,y1-1)) && (verificaMapaC (x,y) d m) && (verificaMapaC (x,y+1) d m) = True
                                                                                            | d==B && dj==C && ((x,y)==(x1-3,y1-1) || (x,y)==(x1-3,y1+1)) && (verificaMapaC (x,y) d m) && (verificaMapaC (x+1,y) d m) = True
                                                                                            | d==E && dj==D && ((x,y)==(x1-3,y1+1) || (x,y)==(x1+3,y1+1)) && (verificaMapaC (x,y) d m) && (verificaMapaC (x,y-1) d m) = True
                                                                                            | otherwise = (desviaTiroProo disp (Jogador (x1,y1) dj vida laser choques) m)
desviaTiroProo (x:disp) (Jogador (x1,y1) dj vida laser choques) m = (desviaTiroProo disp (Jogador (x1,y1) dj vida laser choques) m)

-- | Verifica se nas posicçoes circundantes do jogador existem disparos com a sua direçao sendo assim preciso o movimento.
--desviaTiroCnear :: [Disparo] -- ^ A lista de "Disparo" a percorrrer.
--                -> Jogador -- ^ O 'Jogador' a ter em conta.
--                -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--                -> Bool -- ^ A resposta em relaçao á necessidade de movimento.
desviaTiroCnear :: [Disparo] -> Jogador -> Mapa -> Bool
desviaTiroCnear [] (Jogador (x,y) dj vida laser choques) m = False
desviaTiroCnear ((DisparoCanhao jog (x,y) d):disp) (Jogador (x1,y1) dj vida laser choques) m | d==C && ((x,y)==(x1+1,y1-1) || (x,y)==(x1+1,y1+1)) = True
                                                                                             | d==D && ((x,y)==(x1-1,y1-1) || (x,y)==(x1+1,y1-1)) = True
                                                                                             | d==B && ((x,y)==(x1-1,y1-1) || (x,y)==(x1-1,y1+1)) = True
                                                                                             | d==E && ((x,y)==(x1-1,y1+1) || (x,y)==(x1+1,y1+1)) = True
                                                                                             | otherwise = (desviaTiroCnear disp (Jogador (x1,y1) dj vida laser choques) m)
desviaTiroCnear (x:disp) (Jogador (x1,y1) dj vida laser choques) m = (desviaTiroCnear disp (Jogador (x1,y1) dj vida laser choques) m)

-- | Verifica se nas posicçoes circundantes do jogador existem disparos com a sua direçao sendo assim preciso o movimento.
--desviaTiroC :: [Disparo] -- ^ A lista de "Disparo" a percorrrer.
--               -> Jogador -- ^ O 'Jogador' a ter em conta.
--               -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--               -> Bool -- ^ A resposta em relaçao á necessidade de movimento.
desviaTiroC :: [Disparo] -> Jogador -> Mapa -> Bool
desviaTiroC [] (Jogador (x,y) dj vida laser choques) m = False
desviaTiroC ((DisparoCanhao jog (x,y) d):disp) (Jogador (x1,y1) dj vida laser choques) m | d==C && ((x,y)==(x1+2,y1) || (x,y)==(x1+2,y1-1) || (x,y)==(x1+2,y1+1)) && (verificaMapaC (x,y) d m) = True
                                                                                         | d==D && ((x,y)==(x1,y1-2) || (x,y)==(x1-1,y1-2) || (x,y)==(x1+1,y1-2)) && (verificaMapaC (x,y) d m) = True
                                                                                         | d==B && ((x,y)==(x1-2,y1) || (x,y)==(x1-2,y1-1) || (x,y)==(x1-2,y1+1)) && (verificaMapaC (x,y) d m) = True
                                                                                         | d==E && ((x,y)==(x1,y1+2) || (x,y)==(x1-1,y1+2) || (x,y)==(x1+1,y1+2)) && (verificaMapaC (x,y) d m) = True
                                                                                         | otherwise = (desviaTiroC disp (Jogador (x1,y1) dj vida laser choques) m)
desviaTiroC (x:disp) (Jogador (x1,y1) dj vida laser choques) m = (desviaTiroC disp (Jogador (x1,y1) dj vida laser choques) m)

-- | Verifica se nas posicçoes circundantes do jogador existem disparos com a sua direçao sendo que se existirem é mais seguro nao se movimentar.
--naoTeMexas :: [Disparo] -- ^ A lista de "Disparo" a percorrrer.
--              -> Jogador -- ^ O 'Jogador' a ter em conta.
--              -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--              -> Bool -- ^ A resposta em relaçao á nao necessidade de movimento.
naoTeMexas :: [Disparo] -> Jogador -> Mapa -> Bool
naoTeMexas [] (Jogador (x1,y1) dj vida laser choques) m = False
naoTeMexas ((DisparoCanhao jog (x,y) d):disp) (Jogador (x1,y1) dj vida laser choques) m | d==C && ((x,y)==(x1+1,y1-2) || (x,y)==(x1+1,y1+2)) = True
                                                                                        | d==D && ((x,y)==(x1-2,y1-1) || (x,y)==(x1+2,y1-1)) = True
                                                                                        | d==B && ((x,y)==(x1-1,y1-2) || (x,y)==(x1-1,y1+2)) = True
                                                                                        | d==E && ((x,y)==(x1-2,y1+1) || (x,y)==(x1+2,y1+1)) = True
                                                                                        | otherwise = (naoTeMexas disp (Jogador (x1,y1) dj vida laser choques) m)
naoTeMexas (x:disp) (Jogador (x1,y1) dj vida laser choques) m = (naoTeMexas disp (Jogador (x1,y1) dj vida laser choques) m)

-- | Verifica se nas posicçoes frontais do jogador existem disparos com a sua direçao para os quais é impossivel desviar-se sendo que neste caso usará o laser como defesa.
--laserProtector :: [Disparo] -- ^ A lista de "Disparo" a percorrrer.
--               -> Jogador -- ^ O 'Jogador' a ter em conta.
--               -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--               -> Bool -- ^ A resposta em relaçao á necessidade de usar laser defensivamente.
laserProtector :: [Disparo] -> Jogador -> Mapa -> Bool
laserProtector [] (Jogador (x1,y1) dj vida laser choques) m = False
laserProtector ((DisparoCanhao jog (x,y) d):disp) (Jogador (x1,y1) dj vida laser choques) m | d==C && dj==B && (x,y)==(x1+2,y1) && (verificaMapaC (x,y) d m) = True
                                                                                            | d==D && dj==E && (x,y)==(x1,y1-2) && (verificaMapaC (x,y) d m) = True
                                                                                            | d==B && dj==C && (x,y)==(x1-2,y1) && (verificaMapaC (x,y) d m) = True
                                                                                            | d==E && dj==D && (x,y)==(x1,y1+2) && (verificaMapaC (x,y) d m) = True
                                                                                            | otherwise = (laserProtector disp (Jogador (x1,y1) dj vida laser choques) m)
laserProtector (x:disp) (Jogador (x1,y1) dj vida laser choques) m = (laserProtector disp (Jogador (x1,y1) dj vida laser choques) m)
                                                                                           
-- | Verifica se nas posicçoes frontais ao jogador que realiza a jogada existe um adversário e verifica se este se encontra virado numa direcao oposta.
--disparaCanhaoNoob :: [Jogador] -- ^ A lista de "Jogador" a percorrrer.
--                  -> PosicaoGrelha -- ^ A 'PosicaoGrelha' do jogador que realiza a jogada.
--                  -> Direcao -- ^ A 'Direcao' do jogador que realiza a jogada.
--                  -> Bool -- ^ A resposta em relaçao á nessecidade de disparar.
disparaCanhaoNoob :: [Jogador] -> PosicaoGrelha -> Direcao -> Mapa -> Bool
disparaCanhaoNoob [] (x1,y1) d m = False
disparaCanhaoNoob ((Jogador (x,y) dj vida laser choques):xs) (x1,y1) d m | d==C && dj/=B && vida>0 && ((x,y)==(x1-2,y1) || (x,y)==(x1-2,y1+1) || (x,y)==(x1-2,y1-1)) && (verificaMapaC (x1,y1) d m) = True
                                                                         | d==D && dj/=E && vida>0 && ((x,y)==(x1,y1+2) || (x,y)==(x1+1,y1+2) || (x,y)==(x1-1,y1+2)) && (verificaMapaC (x1,y1) d m) = True
                                                                         | d==B && dj/=C && vida>0 && ((x,y)==(x1+2,y1) || (x,y)==(x1+2,y1+1) || (x,y)==(x1+2,y1-1)) && (verificaMapaC (x1,y1) d m) = True
                                                                         | d==E && dj/=D && vida>0 && ((x,y)==(x1,y1-2) || (x,y)==(x1+1,y1-2) || (x,y)==(x1-1,y1-2)) && (verificaMapaC (x1,y1) d m) = True
                                                                         | otherwise = (disparaCanhaoNoob xs (x1,y1) d m)

-- | Verifica se nas posicçoes frontais ao jogador que realiza a jogada existe um adversario e se mapa que os separa está livre de blocos indestrutiveis.
--multiDisparo :: [Jogador] -- ^ A lista de "Jogador" a percorrrer.
--             -> PosicaoGrelha -- ^ A 'PosicaoGrelha' do jogador que realiza a jogada.
--             -> Direcao -- ^ A 'Direcao' do jogador que realiza a jogada.
--             -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--             -> Bool -- ^ A resposta em relaçao á nessecidade de disparar.
multiDisparo :: [Jogador] -> PosicaoGrelha -> Direcao -> Mapa -> Bool
multiDisparo [] (x1,y1) d m = False
multiDisparo ((Jogador (x,y) dj vida laser choques):xs) (x1,y1) d m | d==C && vida>0 && ((x,y)==(x1-3,y1) || (x,y)==(x1-3,y1+1) || (x,y)==(x1-3,y1-1) || (x,y)==(x1-4,y1) || (x,y)==(x1-4,y1+1) || (x,y)==(x1-4,y1-1)) && (verificaMapaD (x1,y1) d m)==False && (verificaMapaD (x1-1,y1) d m)==False = True
                                                                    | d==D && vida>0 && ((x,y)==(x1,y1+3) || (x,y)==(x1+1,y1+3) || (x,y)==(x1-1,y1+3) || (x,y)==(x1,y1+4) || (x,y)==(x1+1,y1+4) || (x,y)==(x1-1,y1+4)) && (verificaMapaD (x1,y1) d m)==False && (verificaMapaD (x1,y1+1) d m)==False = True
                                                                    | d==B && vida>0 && ((x,y)==(x1+3,y1) || (x,y)==(x1+3,y1+1) || (x,y)==(x1+3,y1-1) || (x,y)==(x1+4,y1) || (x,y)==(x1+4,y1+1) || (x,y)==(x1+4,y1-1)) && (verificaMapaD (x1,y1) d m)==False && (verificaMapaD (x1+1,y1) d m)==False = True
                                                                    | d==E && vida>0 && ((x,y)==(x1,y1-3) || (x,y)==(x1+1,y1-3) || (x,y)==(x1-1,y1-3) || (x,y)==(x1,y1-4) || (x,y)==(x1+1,y1-4) || (x,y)==(x1-1,y1-4)) && (verificaMapaD (x1,y1) d m)==False && (verificaMapaD (x1,y1-1) d m)==False = True
                                                                    | otherwise = (multiDisparo xs (x1,y1) d m)

-- | Recebe uma posicaogrelha do jogador que tem menos vida e de acordo com a posicao do jogador que efectua a jogada dá uma lista de direçoes possiveis para o alcançar.
--cacaKill :: PosicaoGrelha -- ^ A 'PosicaoGrelha do jogador com menos vida (Alvo).
--            -> Jogador -- ^ O 'Jogador' que realiza a jogada.
--            -> Mapa -- ^ Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--            -> [Direcao] -- ^ A lista de "Direcao" para o qual o movimento é ideal.
cacaKill :: PosicaoGrelha -> Jogador -> Mapa -> [Direcao]
cacaKill (x,y) (Jogador (x1,y1) dj vida laser choques) m | x>x1 && y==y1 = [B]
                                                         | x<x1 && y==y1 = [C]
                                                         | x==x1 && y>y1 = [E]
                                                         | x==x1 && y<y1 = [D]
                                                         | x>x1 && y>y1 = [B,E]
                                                         | x>x1 && y<y1 = [B,D]
                                                         | x<x1 && y>y1 = [C,E]
                                                         | x<x1 && y<y1 = [C,D]

-- | Percorre a lista de jogadores e retira desta o jogador que efectua a jogada.
--qualPlayer :: Int -- ^ O identificador do 'Jogador' pretendido.
--           -> [Jogador] -- ^ A lista de "Jogador" a percorrrer.
--           -> [Jogador] -- ^ A lista de "Jogador" a pretendida.
qualPlayer :: Int -> [Jogador] -> [Jogador]
qualPlayer n [] = []
qualPlayer 3 l = (init l)
qualPlayer 0 ((Jogador (x,y) dj vida laser choques):[]) = []
qualPlayer n (j:js) | n>0 = j:(qualPlayer (n-1) js)
                    | n==0 = js

-- | Percorre a lista de jogadores e devolve a posicaogrelha daquele com menos vidas.
--procuraSoro :: [Jogador] -- ^ A lista de "Jogador" a percorrrer.
--            -> PosicaoGrelha -- ^ A 'PosicaoGrelha' do jogador com menos vida.
procuraSoro :: [Jogador] -> PosicaoGrelha
procuraSoro ((Jogador (x,y) dj vida laser choques):[]) = (x,y)
procuraSoro (j:k:js) | (comparaVidas j)>0 && ((comparaVidas j)<=(comparaVidas k)) = (procuraSoro (j:js))
                     | otherwise = (procuraSoro (k:js))
                                    where comparaVidas :: Jogador -> Int
                                          comparaVidas (Jogador (x1,y1) dj vida laser choques) = vida

-- | Verifica se existem jogadores na mesma coluna ou linha que o jogador que vai efectuar a jogada e o jogador está com a direcao certa para os atingir com um disparo laser tendo em conta o mapa de jogo .
--eliminaLinha :: [Jogador] -- ^ A lista de "Jogador" a percorrrer.
--             -> PosicaoGrelha -- ^ A 'PosicaoGrelha' do jogador que efectua a jogada.
--             -> Direcao -- ^ A 'Direcao' do jogador que efectua a jogada.
--             -> Mapa -- ^ O 'Mapa' onde se efectua o jogo.
--             -> Bool -- ^ A resposta em relaçao á possiblidade de usar laser.
eliminaLinha :: Int -> [Jogador] -> PosicaoGrelha -> Direcao -> Mapa -> Bool
eliminaLinha _ [] _ _ _ = False
eliminaLinha jog ((Jogador (x1,y1) dj vida laser choques):xs) (x,y) d m | x==x1 && y>y1 && d==E && vida>0 && (laserAtinge (alcanceLaser (simulaDisparoL jog (x,y) d) m) (Jogador (x1,y1) dj vida laser choques)) = True
                                                                        | x==x1 && y<y1 && d==D && vida>0 && (laserAtinge (alcanceLaser (simulaDisparoL jog (x,y) d) m) (Jogador (x1,y1) dj vida laser choques)) = True
                                                                        | x>x1 && y==y1 && d==C && vida>0 && (laserAtinge (alcanceLaser (simulaDisparoL jog (x,y) d) m) (Jogador (x1,y1) dj vida laser choques)) = True
                                                                        | x<x1 && y==y1 && d==B && vida>0 && (laserAtinge (alcanceLaser (simulaDisparoL jog (x,y) d) m) (Jogador (x1,y1) dj vida laser choques)) = True
                                                                        | otherwise = (eliminaLinha jog xs (x,y) d m)

-- | Verifica determinado jogador é afectado por um disparo laser atraves de uma lista de posicoes.
--laserAtinge :: [Posicao] -- ^ A lista de "Posicao" que o laser afecta.
--            -> Jogador -- ^ O 'Jogador a verificar.
--            -> Bool -- ^ A resposta em bool á questao se o jogador é atingido.                                                                    
laserAtinge :: [Posicao] -> Jogador -> Bool
laserAtinge [] (Jogador (x,y) dj vida laser choque) = False
laserAtinge (p:ps) (Jogador (x,y) dj vida laser choque) | p/=(x,y) && p/=(x,y-1) && p/=(x-1,y) && p/=(x-1,y-1) = (laserAtinge ps (Jogador (x,y) dj vida laser choque))
                                                        | otherwise = True

-- | Verifica se a posicaogrelha para a qual pretendemos efectuar o movimento é segura, ou seja livre de disparos proximos com essa mesma direcao .
--existeTiro :: PosicaoGrelha -- ^ A 'PosicaoGrelha' para a qual o movimento é pretendido'.
--             -> [Disparo] -- ^ A lista de "Disparo" a percorrer.
--             -> Bool -- ^ A resposta em bool á questao se a posicao grelha é segura para o movimento.
existeTiro :: PosicaoGrelha -> [Disparo] -> Bool
existeTiro (x1,y1) [] = True
existeTiro (x1,y1) ((DisparoCanhao jog (x,y) d):disp) | d==C && ((x,y)==(x1+1,y1) || (x,y)==(x1+1,y1-1) || (x,y)==(x1+1,y1+1)) = False
                                                      | d==D && ((x,y)==(x1,y1-1) || (x,y)==(x1+1,y1-1) || (x,y)==(x1-1,y1-1)) = False
                                                      | d==B && ((x,y)==(x1-1,y1) || (x,y)==(x1-1,y1-1) || (x,y)==(x1-1,y1+1)) = False
                                                      | d==E && ((x,y)==(x1,y1+1) || (x,y)==(x1+1,y1+1) || (x,y)==(x1-1,y1+1)) = False
                                                      | otherwise = (existeTiro (x1,y1) disp)
existeTiro (x1,y1) (x:disp) = (existeTiro (x1,y1) disp)

-- | Percorre uma lista de jogador e devolve o jogador que efectua a jogada.
--playerJogador :: Int -- ^ O identificador do 'Jogador' pretendido.
--             -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--             -> Jogador -- ^ O 'Jogador' que efectua a jogada.
playerJogador :: Int -> [Jogador] -> Jogador
playerJogador n ((Jogador (x1,y1) dj vida laser choques):xs) | n>0 = (playerJogador (n-1) xs)
                                                             | otherwise = (Jogador (x1,y1) dj vida laser choques)

-- | Percorre uma lista de jogador e devolve a posicaogrelha do jogador que efectua a jogada.
--posJogador :: Int -- ^ O identificador do 'Jogador' pretendido.
--           -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--           -> PosicaoGrelha -- ^ A 'PosicaoGrelha' do jogador que efectua a jogada.
posJogador :: Int -> [Jogador] -> PosicaoGrelha
posJogador n ((Jogador (x1,y1) dj vida laser choques):xs) | n>0 = (posJogador (n-1) xs)
                                                          | otherwise = (x1,y1)

-- | Percorre uma lista de jogador e devolve a direcao do jogador que efectua a jogada.
--dirJogador :: Int -- ^ O identificador do 'Jogador' pretendido.
--           -> [Jogador] -- ^ A lista de "Jogador" a percorrer.
--           -> Direcao -- ^ A 'Direcao' do jogador que efectua a jogada.
dirJogador :: Int -> [Jogador] -> Direcao
dirJogador n ((Jogador (x1,y1) dj vida laser choques):xs) | n>0 = (dirJogador (n-1) xs)
                                                          | otherwise = dj

-- | Atraves da posicao e da direcao do jogador que efectua a jogada simula um disparo laser nessas condiçoes.
--simulaDisparoL :: PosicaoGrelha -- ^ A 'PosicaoGrelha' do jogador que efectua a jogada.
--               -> Direcao -- ^ A 'Direcao' do jogador que efectua a jogada.
--               -> Disparo -- ^ O 'Disparo' laser pretendido.
simulaDisparoL :: Int -> PosicaoGrelha -> Direcao -> Disparo
simulaDisparoL jog (x,y) d | d==C = (DisparoLaser jog (x-1,y) C)
                           | d==D = (DisparoLaser jog (x,y+1) D)
                           | d==B = (DisparoLaser jog (x+1,y) B)
                           | d==E = (DisparoLaser jog (x,y-1) E)