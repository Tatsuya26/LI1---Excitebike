-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g102 where
import Tarefa0_2019li1g102
import LI11920
import Data.List
import System.Random

-- * Testes
-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(2,5,1), ((1),(1),(1)), (0,5,(-100000)), (30,30,30)]

-- * Funções pré-definidas da Tarefa 1.
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.
-- | Gera um 'Mapa' aleatorio com um dado numero de pistas e comprimento, usando uma seed de aleatoriedade
gera :: Int -> Int -> Int -> Mapa
gera npistas comp smn | comp > 1  = let r = geraAleatorios (2*npistas*(comp-1)) smn
                                        l = geraListaPar r
                                        m = geraMatrizPares (comp-1) l
                                    in auxGera m
                      | comp == 1 = replicate npistas [(Recta Terra 0)]
-- | Auxiliar para gerar cada 'Pista' do 'Mapa'
auxGera :: [[(Int,Int)]] -> Mapa
auxGera [] = []
auxGera (h:t) = (geraPista h):(auxGera t)

-- | Transforma uma lista de Int em uma lista de pares de Int
geraListaPar :: [Int] -> [(Int,Int)]
geraListaPar [] = []
geraListaPar (h:x:t) = (h,x):(geraListaPar t)

-- | Transforma uma lista de pares de Int em uma Matriz
geraMatrizPares :: Int -> [(Int,Int)] ->[[(Int,Int)]]
geraMatrizPares _ [] = []
geraMatrizPares comp p = frst:(geraMatrizPares comp resto) 
                    where (frst,resto) = splitAt comp p

-- | Retorna o 'Piso' correspondente ao Int dado.
geraTerreno :: Int -> Piso -> Piso
geraTerreno i p | i==0 || i==1 = Terra
                | i==2 || i==3 = Relva
                | i == 4       = Lama
                | i == 5       = Boost
                | otherwise    = p

-- | Calcula a altura de uma 'Rampa' conforme a subida ou descida
alturaPeca :: Int -> Int -> Int
alturaPeca b h | b==0 || b==1                 = h+b+1 
               | b==2 || b==3 || b==4 || b==5 = if (h-(b-1)>0) then h-(b-1) else 0
               | otherwise                    = h

-- | Transforma uma lista de pares de Int 
geraPista :: [(Int,Int)] -> Pista
geraPista l = (Recta Terra 0):(geraPistaAux l Terra 0)

-- | Funçao auxiliar para a geraPista que recebe o 'Piso' e a altura anteriores
geraPistaAux :: [(Int,Int)] -> Piso -> Int -> Pista
geraPistaAux [] _ _ = []
geraPistaAux (c:cs) p1 h1 = (aux c h1):(geraPistaAux cs piso alt)
                       where aux :: (Int,Int) -> Int -> Peca
                             aux (a,b) h | b==0 || b==1                 = Rampa piso h alt
                                         | b==2 || b==3 || b==4 || b==5 = if (alt == h) then (Recta piso h) 
                                                                                        else (Rampa piso h alt)
                                         | otherwise                    = Recta piso h
                             piso = geraTerreno (fst c) p1
                             alt = alturaPeca (snd c) h1 