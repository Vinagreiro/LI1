-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g138 where

    import LI11819
    import Data.List
    
    -- * Funções não-recursivas.
    
    -- | Um 'Vetor' é uma 'Posicao' em relação à origem.
    type Vetor = Posicao
    -- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>
    
    -- ** Funções sobre vetores
    
    -- *** Funções gerais sobre 'Vetor'es.
    
    -- | Soma dois 'Vetor'es.
    somaVetores :: Vetor -> Vetor -> Vetor
    somaVetores (x1,y1) (x2,y2) = (x1+x2, y1+y2)
    
    -- | Subtrai dois 'Vetor'es.
    subtraiVetores :: Vetor -> Vetor -> Vetor
    subtraiVetores (x1,y1) (x2,y2) = (x1-x2, y1-y2)
    
    -- | Multiplica um escalar por um 'Vetor'.
    multiplicaVetor :: Int -> Vetor -> Vetor
    multiplicaVetor k (x,y) = (k*x, k*y)
    
    -- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distânciaa à origem).
    --
    -- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
    rodaVetor :: Vetor -> Vetor
    rodaVetor (0,y) = (y,0)
    rodaVetor (x,0) = (0,-x)
    rodaVetor (x,y) = (y,-x)

    -- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
    --
    -- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
    inverteVetorH :: Vetor -> Vetor
    inverteVetorH (x,0) = (x,0)
    inverteVetorH (x,y) = (x,-y)
    
    -- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
    --
    -- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
    inverteVetorV :: Vetor -> Vetor
    inverteVetorV (0,y) = (0,y)
    inverteVetorV (x,y) = (-x,y)
    
    -- *** Funções do trabalho sobre 'Vetor'es.
    
    -- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
    direcaoParaVetor :: Direcao -> Vetor
    direcaoParaVetor C = (-1,0)
    direcaoParaVetor B = (1,0)
    direcaoParaVetor D = (0,1)
    direcaoParaVetor E = (0,-1)
    
    -- ** Funções sobre listas
    
    -- *** Funções gerais sobre listas.
    --
    -- Funções não disponíveis no 'Prelude', mas com grande utilidade.
    
    -- | Verifica se o indice pertence à lista.
    eIndiceListaValido :: Int -> [a] -> Bool
    eIndiceListaValido x1 [] = False
    eIndiceListaValido x1 (l:ls) = (x1 >= 0) && x1 < (length (l:ls))
    
    -- ** Funções sobre matrizes.
    
    -- *** Funções gerais sobre matrizes.
    
    -- | Uma matriz é um conjunto de elementos a duas dimensões.
    --
    -- Em notação matemática, é geralmente representada por:
    --
    -- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
    type Matriz a = [[a]]
    
    -- | Calcula a dimensão de uma matriz.
    dimensaoMatriz :: Matriz a -> Dimensao
    dimensaoMatriz [] = (0,0)
    dimensaoMatriz (m:ms) | null m = dimensaoMatriz ms
                          | not (null m) = (length (m:ms),length m)
    
    -- | Verifica se a posição pertence à matriz.
    ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
    ePosicaoMatrizValida (x1,y1) [] = False
    ePosicaoMatrizValida (x1,y1) (m:ms) = if x1 > ((length(m:ms))-1) || x1<0 then False
                                                                             else y1 <= ((length(head(m:ms)))-1) && y1 >= 0
    
    -- | Verifica se a posição está numa borda da matriz.
    eBordaMatriz :: Posicao -> Matriz a -> Bool
    eBordaMatriz (x1,y1) [] = False
    eBordaMatriz (x1,y1) (m:ms) | x1==0 || x1==(length(m:ms)-1) = True
                                | y1==0 || y1==(length(head(m:ms))-1) = True
                                | otherwise = False

    -- *** Funções do trabalho sobre matrizes.
    
    -- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
    --
    -- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
    tetrominoParaMatriz :: Tetromino -> Matriz Bool
    tetrominoParaMatriz I = [[False,True,False,False],[False,True,False,False],[False,True,False,False],[False,True,False,False]]
    tetrominoParaMatriz J = [[False,True,False],[False,True,False],[True,True,False]]
    tetrominoParaMatriz L = [[False,True,False],[False,True,False],[False,True,True]]
    tetrominoParaMatriz O = [[True,True],[True,True]]
    tetrominoParaMatriz S = [[False,True,True],[True,True,False],[False,False,False]]
    tetrominoParaMatriz T = [[False,False,False],[True,True,True],[False,True,False]]
    tetrominoParaMatriz Z = [[True,True,False],[False,True,True],[False,False,False]]
    -- * Funções recursivas.
    
    -- ** Funções sobre listas.
    --
    -- Funções não disponíveis no 'Prelude', mas com grande utilidade.
    
    -- | Devolve o elemento num dado índice de uma lista.
    encontraIndiceLista :: Int -> [a] -> a
    encontraIndiceLista n (x:xs) | n==0 = x
                                 | n>0 = encontraIndiceLista (n-1) xs 
                                 | otherwise = error "so n positivos"     
    
    -- | Modifica um elemento num dado índice.
    --
    -- __NB:__ Devolve a própria lista se o elemento não existir.
    atualizaIndiceLista :: Int -> a -> [a] -> [a]
    atualizaIndiceLista _ _ [] = []
    atualizaIndiceLista n a (x:xs) | n==0 = (a:xs)
                                   | otherwise = x:(atualizaIndiceLista (n-1) a xs)
                    
    -- ** Funções sobre matrizes.
    
    -- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
    --
    -- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
    rodaMatriz :: Matriz a -> Matriz a
    rodaMatriz m = transpose (reverse' m)
                   where reverse' :: [a] -> [a]
                         reverse' [] = []
                         reverse' (x:xs) = (reverse' xs)++[x]

    -- | Inverte uma 'Matriz' na horizontal.
    --
    -- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
    inverteMatrizH :: Matriz a -> Matriz a
    inverteMatrizH [] = []
    inverteMatrizH (x:[]) = [reverse x]
    inverteMatrizH (m:ms) = [reverse m] ++ (inverteMatrizH ms)
                                  
    -- | Inverte uma 'Matriz' na vertical.
    --
    -- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
    inverteMatrizV :: Matriz a -> Matriz a
    inverteMatrizV [] = []
    inverteMatrizV (x:[]) = [x]
    inverteMatrizV (x:xs) = (inverteMatrizV xs) ++ [x]

    -- | Cria uma nova 'Matriz' com o mesmo elemento.
    criaMatriz :: Dimensao -> a -> Matriz a
    criaMatriz (0,y) _ = [[]]
    criaMatriz (x,0) _ = [[]]
    criaMatriz (1,y) n = [replicate y n]
    criaMatriz (x,y) n = (l1 ++ l2)
                         where l1 = [replicate y n]
                               l2 = criaMatriz (x-1,y) n

    -- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
    encontraPosicaoMatriz :: Posicao -> Matriz a -> a
    encontraPosicaoMatriz (x,y) ((m:n):ms) | x==0 && y==0 = m
                                           | x==0 && y>0 = encontraPosicaoMatriz (x,y-1) [n]
                                           | x>0 = (encontraPosicaoMatriz (x-1,y) ms)

    -- | Modifica um elemento numa dada 'Posicao'
    --
    -- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
    atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
    atualizaPosicaoMatriz (_,_) _ [] = []
    atualizaPosicaoMatriz (x,y) a ((m:n):ms) | x==0 && y==0 = ((a:n):ms)
                                             | x==0 && y/=0 = ((m:(mudaPos (y-1) a n)):ms)
                                             | x>0 = [(m:n)] ++ (atualizaPosicaoMatriz (x-1,y) a ms)
                                                     where mudaPos _ _ [] = []
                                                           mudaPos n a (x:xs) | n==0 = (a:xs)
                                                                              | n>0 = x:(mudaPos (n-1) a xs)
                                                                              | n<0 = error "so n negativos"
