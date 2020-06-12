-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g102 where
import Tarefa0_2019li1g102
import Tarefa1_2019li1g102
import Tarefa2_2019li1g102
import Tarefa3_2019li1g102
import Tarefa4_2019li1g102
import LI11920
import Data.List 
-- * __Relatório T6__
-- ** Introdução
-- $intro 
-- A Tarefa 6 da como desafio criar um bot que faça jogadas nas quais numa disputa o mesmo chegue em primeiro ao fim da pista
-- Na nossa ótica o essencial desta tarefa seria fazer com que o jogador decidi-se o melhor piso para acelarar, disparar cola nos melhos pisos e ajustar a sua posição no ar para cair direito numa peça se morrer

-- ** Objetivos
-- $intro
-- A maneira de como abordamos esse desafio foi através de um conjunto de condições que tem de ser tomadas para que o bot faça uma determinada jogada
-- o bot faz a jogada dispara caso este nao esteja na pista em que iniciou
-- o bot faz a jogada Acelera se não estiver numa zona de travessia e estiver no chão
-- o bot faz a jogada Movimenta(Cima e Baixo) se estiver no chão e numa travessia. Ele analisa as pecas em que o atrito é menor. caso nao esteja na pista dessa peca ele move para a pista da mesma, caso contrário ele acelera.
-- o bot faz a jogada Movimenta (Esquerda e Direita) se estiver no ar. o mesmo calcula a inclinação da peça em que vai aterrar e tenta com as jogadas ficar minimamente com inclinação da peca sendo o parametro de comparação aceitavel o bot ficar entre (incPeca - 5º) e (incPeca + 5º) 
-- O bot nunca desacelera 

-- ** Discussão e conclusão
-- $conc
-- O bot é funcional e faz todas as jogadas principais e lógicas.


-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n (Estado m (j:js)) | mudouPista j1 m                     = Just Dispara
                        | not(travessia j1) && estaNoChao j1  = Just Acelera
                        | travessia j1  && estaNoChao j1      = (if posAtrito == (pista j1) then Just Acelera
                                                                     else (if (posAtrito > pista j1) then Just (Movimenta B )
                                                                            else Just (Movimenta C)))
                        | estaNoAr j1                         = (if inc <= inclinacaoPeca (pecaSeguinte j m) - 5 && inc >= inclinacaoPeca(pecaSeguinte j m) + 5.0 then Nothing 
                                                                else (if inc < inclinacaoPeca (pecaSeguinte j m) then Just (Movimenta E)
                                                                     else Just (Movimenta D)))
                           where 
                            j1        = encontraIndiceLista n (j:js)          
                            posAtrito = pSeguinte j1 m 
                            inc       = inclinacaoJog j1 


-- | Retorna as coordenadas no Ar de um 'Jogador'
coordenAr:: Jogador -> Vetor
coordenAr (Jogador _ d _ _ (Ar alt _ _ )) =  Cartesiano d alt


-- | Função que calcula se um jogador mudou de pista 
mudouPista :: Jogador -> Mapa -> Bool 
mudouPista j@(Jogador p _ _ _ _) m = if p /= pistaAtual then True
                                     else False
                         where 
                         pistaAtual = x 
                         (x,y) = coordenadaJogador j 


-- |inclinação do jogador no ar
inclinacaoJog :: Jogador -> Double
inclinacaoJog (Jogador _ _ _ _ (Ar _ inc _)) = inc 


-- | Funçao que encontra um jogador no mapa
encontraJogadorNoMapa :: Int -> Estado -> Peca 
encontraJogadorNoMapa n (Estado m (j:js)) =  encontraPosicaoMatriz (coordenadaJogador j1) m 
                           where 
                            j1= encontraIndiceLista n (j:js)


-- | Posição da peça seguinte com menor atrito
pSeguinte :: Jogador -> Mapa -> Double
pSeguinte j m = atritoList (colunaMapa j m)


-- | Função que devolve uma coluna do Mapa
colunaMapa ::Jogador -> Mapa -> [Peca] 
colunaMapa (Jogador _ b _ _ _)  m = encontraColunaMapa ((floor b)+1) (transpose m) 


-- | Função que encontra uma coluna de um mapa apos receber o seu numero
encontraColunaMapa ::  Int -> Mapa -> [Peca]
encontraColunaMapa 0 (h:t) = h 
encontraColunaMapa n (h:t) = encontraColunaMapa (n-1) t


-- | Função que calcula na lista de pecas seguintes qual a peça com menor atrito e devolve a pista da mesma 
atritoList ::  [Peca] -> Double
atritoList (h:t) = pMenor (map atrito (h:t))


-- | Função que da a posição do menor elemento numa lista
pMenor :: Ord a => [a] -> Double
pMenor [h] = 0
pMenor (h:t)= if h == aux (h:t) then 0.0
              else 1.0+pMenor t
              where
              aux [x] = x 
              aux (x1:x2:xs)= if x1<x2 then aux (x1:xs)
                              else aux (x2:xs)


-- | Função que calcula se o jogador esta a passar de uma peca para outra (sendo o intervalo entre 0.69 e 0.99 da percentagem de peca percorrida)
travessia :: Jogador -> Bool
travessia (Jogador _ b _ _ _ ) = if  (b-realToFrac(floor b) > 0.69) && (b-realToFrac(floor b)< 0.99) then True
                                 else False


-- | Função que da a pista em que um jogador se encontra 
pista :: Jogador -> Double
pista (Jogador a _ _ _ _ ) = fromIntegral a 


-- | Função que calcula a peça em que o jogador vai aterrar 
pecaSeguinte :: Jogador -> Mapa -> Peca
pecaSeguinte j@(Jogador p d v c (Ar alt inc g)) m =  encontraPosicaoMatriz pontoDeAterragem m 
                                where 
                                  pontoDeAterragem  = arredondaReta(intersecao retaDoJogador (retaPista m j ))
                                  retaDoJogador     = (coordenAr j, (Cartesiano distN altN))
                                  pecaAtual         = encontraPosicaoMatriz (x,y) m
                                  (x,y)             = coordenadaJogador j
                                  altN              = alt+(posy vetorArF)
                                  distN             = d+(posx vetorArF)
                                  vetorArF          = somaVetores (vetorVel j m) (vetorGrav j)


-- | Função que dada um jogador da a reta da pista dele
retaPista :: Mapa-> Jogador -> Reta 
retaPista m j@(Jogador p d _ _ _) = ((Cartesiano 0 (realToFrac p)),(Cartesiano (realToFrac(length(head m))) (realToFrac p)))  


-- | Função que arredonda coordenadas de reta 
arredondaReta :: Ponto-> (Int,Int)
arredondaReta (Cartesiano x y) = (fromIntegral(floor x),fromIntegral(floor y))


