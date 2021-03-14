-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g138 where

import LI11819
import Tarefa0_2018li1g138
-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Move B,Move D,Move B,Move D,Move D,MudaTetromino,MudaParede,Desenha,Move E,Move E,Move C,MudaTetromino,MudaTetromino,Move B,Roda,MudaParede,Desenha,Move D,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Roda,Roda,Roda,Roda,Move C,MudaParede,Move B,Desenha]]

-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao MudaTetromino (Editor (x,y) d t p m) = (Editor (x,y) d (if t==Z then I else succ t) p m)
instrucao MudaParede (Editor (x,y) d t p m) = (Editor (x,y) d t (if p==Destrutivel then Indestrutivel else Destrutivel) m)
instrucao (Move direcao) (Editor (x,y) d t p m) = (Editor (x1,y1) d t p m)
                                                       where (x1,y1) | direcao==C = (x-1,y) 
                                                                     | direcao==D = (x,y+1) 
                                                                     | direcao==B = (x+1,y) 
                                                                     | direcao==E = (x,y-1)
instrucao Roda (Editor (x,y) d t p m) = (Editor (x,y) (if d==E then C else succ d) t p m)
instrucao Desenha (Editor (x,y) d t p m) = (Editor (x,y) d t p (inseresMatrizMapa 0 (x,y) (tetre t d) m p))

-- | Pega num tetronimo aplica-lhe uma determinada posiçao e tranforma-o numa raiz de bolianos
--tetre :: Tetronimo -- ^ O 'Tipo' de Tetromino.
--      -> Direcao -- ^ A 'Direcao' a aplicar.
--      -> Matriz Bool -- ^ A 'Matriz Bool' resultante.
tetre :: Tetromino -> Direcao -> Matriz Bool
tetre t d | d==D = rodaMatriz(tetrominoParaMatriz t)
          | d==B = rodaMatriz(rodaMatriz(tetrominoParaMatriz t))
          | d==E = rodaMatriz(rodaMatriz(rodaMatriz(tetrominoParaMatriz t)))
          | d==C = tetrominoParaMatriz t

-- | Pega numa 'Matriz Bool' e num determinado tipo de parede e tranforma-a numa lista de 'Peca' que é um 'Mapa'
--paredeLinhaTetre :: Parede -- ^ O 'Tipo' de Parede.
--                 -> Matriz Bool -- ^ A matriz de "Bool" a tranformar em 'Mapa'.
--                 -> Mapa -- ^ O 'Mapa' resultante.
paredeTetreMatriz :: Parede -> Matriz Bool -> Mapa
paredeTetreMatriz p [] = []
paredeTetreMatriz p ((x:xs):y) = [(paredeLinhaTetre p (x:xs))] ++ paredeTetreMatriz p y

-- | Pega numa linha de bolianos e num determinado tipo de parede e tranforma-a numa linha de blocos
--paredeLinhaTetre :: Parede -- ^ O 'Tipo' de Parede.
--                 -> Bool -- ^ A lista de "Bool" a tranformar em "Peca".
--                 -> [Peca] -- ^ A "Peca" resultante.
paredeLinhaTetre :: Parede -> [Bool] -> [Peca]
paredeLinhaTetre p [] = []
paredeLinhaTetre p (m:ms) | p==Destrutivel = if m==True then (Bloco Destrutivel):(paredeLinhaTetre p ms) else Vazia:(paredeLinhaTetre p ms)
                          | p==Indestrutivel = if m==True then (Bloco Indestrutivel):(paredeLinhaTetre p ms) else Vazia:(paredeLinhaTetre p ms)

-- | A função pega no mapa formado pelo nosso tetronimo e insere-o no mapa inicial
--inseresMatrizMapa :: Int -- ^ O inteiro que serve para encontramos a linha certa para inserir o 'Tetromino'.
--                  -> Posicao -- ^ A 'Posicao' onde pretendemos inserir o 'Tetromino'.
--                  -> Matriz Bool -- ^ A 'Matriz Bool' que representa o 'Tetromino'.
--                  -> Mapa -- ^ O 'Mapa' inicial.
--                  -> Parede -- ^ O tipo de 'Parede' a atribuir ao á 'Matriz Bool' que representa o 'Tetromino'.
--                  -> Mapa -- ^ A 'Mapa' Resultante
inseresMatrizMapa :: Int -> Posicao -> Matriz Bool -> Mapa -> Parede -> Mapa
inseresMatrizMapa _ (_,_) [] m _ = m
inseresMatrizMapa _ (_,_) _ [] _ = []
inseresMatrizMapa n (x,y) (b:bs) (m:ms) p | x>n = m:(inseresMatrizMapa (n+1) (x,y) (b:bs) ms p)
                                          | x==n = (actualizaIndiceMatriz 0 y b m (Bloco p)) : (inseresMatrizMapa (n+1) ((x+1),y) bs ms p)

-- | A Funcao pega na linha onde é pretendido inserir o tetromino e procura a coluna indicada onde faz a substituição dos blocos excepto no caso destes serem vazios.
--actualizaIndiceMatriz :: Int -- ^ O 'Int' que serve como contador para chegarmos á coluna correta.
--                      -> Int -- ^ O 'Int' que representa a coluna onde vamos inserir o 'Tetromino'.
--                      -> [Bool] -- ^ A lista de "Bool" para verificar se os blocos sao vazios ou temos uma parede do tetromino.
--                      -> [Peca] -- ^ A "Peca" inicial.
--                      -> Peca -- ^ O tipo de 'Peca' a inserir
--                      -> [Peca] -- ^ A "Peca" resultante.
actualizaIndiceMatriz :: Int -> Int -> [Bool] -> [Peca] -> Peca -> [Peca]
actualizaIndiceMatriz _ _ [] m _ = m
actualizaIndiceMatriz _ _ _ [] _ = []
actualizaIndiceMatriz n y (x:xs) (m:ms) b | y>n = m:(actualizaIndiceMatriz (n+1) y (x:xs) ms b)
                                          | y==n && x==True = b:(actualizaIndiceMatriz (n+1) (y+1) xs ms b)
                                          | otherwise = m:(actualizaIndiceMatriz (n+1) (y+1) xs ms b)

-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes [] e = e
instrucoes (MudaParede:xs) e = instrucoes xs (instrucao MudaParede e)
instrucoes (MudaTetromino:xs) e = instrucoes xs (instrucao MudaTetromino e)
instrucoes (Roda:xs) e = instrucoes xs (instrucao Roda e)
instrucoes ((Move d):xs) e = instrucoes xs (instrucao (Move d) e)
instrucoes (Desenha:xs) e = instrucoes xs (instrucao Desenha e)

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (x,y) | x<3 || y<3 = []
                  | otherwise = i ++ (replicaa (x-2) h) ++ i
                                where i = [replicate y (Bloco Indestrutivel)]
                                      h = (Bloco Indestrutivel):(replicate (y-2) Vazia) ++ [Bloco Indestrutivel]
                                      replicaa :: Int -> [a] -> [[a]]
                                      replicaa n l | n==1 = [l]
                                                   | n>1 = [l] ++ (replicaa (n-1) l)

-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial is = (Editor pos d t p m)
                    where pos = posicaoInicial is
                          d = C
                          t = I
                          p = Indestrutivel
                          m = mapaInicial(dimensaoInicial is)
                          
-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi is = editMap (instrucoes is(editorInicial is))
              where editMap :: Editor -> Mapa
                    editMap (Editor (x,y) d t p m) = m
