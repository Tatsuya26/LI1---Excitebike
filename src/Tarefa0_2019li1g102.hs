-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g102 where

-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving (Eq, Show)

-- | Um ângulo em graus.
type Angulo = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- | Muda o valor para Degree
degrees :: Double -> Double
degrees x = x/pi*180

-- | Muda o valor para Radians
radians :: Double -> Double
radians x = x*(pi/180)

-- | Posição X de um Ponto
posx :: Ponto -> Double
posx (Cartesiano x1 y1) = x1
posx (Polar r1 ang1) = r1*(cos(radians ang1))

-- | Posição X de um Ponto 
posy :: Ponto -> Double
posy (Cartesiano x1 y1) = y1
posy (Polar r1 ang1) = r1*(sin(radians ang1))

-- | Raio de um Vetor
raio :: Vetor -> Double
raio (Cartesiano x y) = sqrt(x^2+y^2) 
raio (Polar r ang) = r

-- | Angulo de um vetor com o eixo dos x positivo
angulo :: Vetor -> Angulo
angulo (Cartesiano x y) = degrees (atan (y/x))
angulo (Polar r ang) = ang 

-- | Muda o tipo de vetor
mudaVetor :: Vetor -> Vetor
mudaVetor (Cartesiano x1 y1) = Polar (raio(Cartesiano x1 y1)) (angulo(Cartesiano x1 y1))
mudaVetor (Polar r1 ang1) = Cartesiano (posx(Polar r1 ang1)) (posy(Polar r1 ang1))

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores v1 v2 = Cartesiano (posx v1 + posx v2) (posy v1 + posy v2)

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores v1 v2 = Cartesiano (posx v1 - posx v2) (posy v1 - posy v2)


-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor a v1 = Cartesiano (a*(posx v1)) (a*(posy v1))

-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam (p1,p2) (p3,p4) = 
    let x1 = posx p1;
        x2 = posx p2;
        x3 = posx p3;
        x4 = posx p4;
        y1 = posy p1;
        y2 = posy p2;
        y3 = posy p3;
        y4 = posy p4;
        ta = ((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3)) / ((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3));
        tb = ((y1-y2)*(x1-x3)+(x2-x1)*(y1-y3)) / ((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3));
    in 0<=ta && ta<=1 && 0<=tb && tb<= 1 
         
-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao (p1,p2) (p3,p4) = 
    let x1 = posx p1;
        x2 = posx p2;
        x3 = posx p3;
        x4 = posx p4;
        y1 = posy p1;
        y2 = posy p2;
        y3 = posy p3;
        y4 = posy p4;
        ta = ((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3)) / ((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3));
    in  somaVetores p1  (multiplicaVetor ta (subtraiVetores p2 p1))
             
-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i l = i<length l && i>=0


-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz mtx = if all (null) mtx then (0,0)
                     else (length mtx, (length (head mtx)))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (m,n) mtx = m<m1 && n<n1 && n>=0 && m>=0
               where (m1, n1) = dimensaoMatriz mtx
-- * Funções recursivas.

-- ** Funções sobre ângulos

-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo ang | ang < 360 && ang >= 0 = ang
                    | ang < 0               = normalizaAngulo (ang+360)
                    | otherwise             = normalizaAngulo (ang-360)

-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (h:t) = h
encontraIndiceLista i (h:t) = encontraIndiceLista (i-1) t

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista i x [] = []
atualizaIndiceLista 0 x (h:t) = x:t
atualizaIndiceLista i x (h:t) = h : atualizaIndiceLista (i-1) x t

-- ** Funções sobre matrizes.
-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (0,n) (h:t) = encontraIndiceLista n h
encontraPosicaoMatriz (m,n) (h:t) = encontraPosicaoMatriz ((m-1),n) t

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (0,n) x (h:t) = (atualizaIndiceLista n x h) : t
atualizaPosicaoMatriz (m,n) x (h:t) = h : (atualizaPosicaoMatriz ((m-1),n) x t) 
    