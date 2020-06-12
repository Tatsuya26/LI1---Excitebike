-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where
import Tarefa0_2019li1g102
import Tarefa1_2019li1g102
import Tarefa2_2019li1g102
import Tarefa3_2019li1g102
import Tarefa4_2019li1g102
import Tarefa6_2019li1g102  
import LI11920
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Data.List
-- * __Relatório T5__
-- ** Introdução
-- $intro 
-- A Tarefa 5 lança como objetivo a construção da parte grafica do jogo como menus,mapa e jogadores.
-- Na nossa ótica o principal desafio desta tarefa era desenhar o mapa em movimento e colocar os jogadores a moverem se nesse mapa de forma correta e alguns menus para o jogo.

-- ** Objetivos
-- $intro 
-- Primeiramente o grande desafio foi desenhar o mapa do jogo onde optamos por desenhar pecas base como retas e rampas num editor de imagem e depois importar as imagens no codigo através do gloss juicy e tentando desta maneira fazer um jogo mais apelativo.
-- As pecas do mapa com altura superior a zero sao desenhadas a determinada altura delas e depois sende desenhadas por baixo blocos da mesma textura para que ficassem preenchidos esses espaços.
-- As dimensões das peças são a wPeca que define a largura da peca e hpeca que define a altura.
-- As dimensões do mapa sao a width(define em que posicao x do ecra se começa a desenhar o mapa) e a height(define em que posicao y do ecra se começa a desenhar o mapa).
-- O mapa em movimento e dado pela constante soma da distancia do jogador da lista de jogadores à width do mesmo.
-- São desenhados quatro jogadores e o numero maximo de jogadores que o nosso jogo suporta.
-- O jogo desenha varios estados ao longo do seu percurso sendo o primeiro o menu inicial com uma breve introdução,um segundo menu com dicas de jogo e um terceiro menu que permite escolher um mapa de jogo.
-- Ao longo do movimento da pista os jogadores reagem ao tempo com a função passo da Tarefa 4
-- Os jogadores podem fazer as jogadas Movimenta,Acelera,Desacelera e dispara
-- Chegando ao fim da pista o jogo encerra 

-- ** Discussão e conclusão
-- $conc
-- É evidente que esta tarefa poderia ser melhorada e acrescentar muitos extras. Contudo conseguimos um jogo funcional,divertido e desafiador.



-- | Função principal da Tarefa 5. 
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
-- | 
window :: Display
window = FullScreen

-- |Cor do backGround
background :: Color
background = orange


-- | FrameRate adequado para o jogo ser jogavel pelo utilizador,nao sendo nem muito rapido nem muito lento
frameRate :: Int
frameRate = 4


-- | Tipo de estado defenido para o utilizador atualizar a lista de jogadores ao longo do tempo, passar sempre a mesma lista de pictures e um inteiro que define qual o menu a desenhar
type StatusGloss = (Estado,[[Picture]],Int)


-- | Define em que posicao x do ecra se começa a desenhar o mapa
width :: Float
width = -600


-- | Define em que posicao y do ecra se começa a desenhar o mapa
height :: Float
height = 50


-- | Largura das peças do mapa
wPeca :: Float
wPeca = 200


-- | altura base das peças com altura = 0
hPeca :: Float
hPeca = 40 

-- | Primeiro estado do jogo onde e inserido o primeiro mapa
gameState1 :: Estado
gameState1 = (Estado map1 lj)

-- | Segundo estado do jogo onde e inserido o segundo mapa
gameState2 :: Estado
gameState2 = (Estado map2 lj)


-- | Terceiro estado do jogo onde e inserido o terceiro mapa
gameState3 :: Estado
gameState3 = (Estado map3 lj)


-- | Quarto estado do jogo onde e inserido o quarto mapa
gameState4 :: Estado
gameState4 = (Estado map4 lj)

-- | Primeiro Mapa
map1 :: Mapa
map1 = [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Rampa Boost 0 2,Recta Terra 2,Recta Terra 2,Recta Lama 2,Recta Terra 2,Recta Terra 2,Recta Boost 2,Rampa Lama 2 1,Recta Terra 1, Rampa Terra 1 0,Recta Terra 0,Recta Boost 0,Recta Lama 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],
         [Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Rampa Terra 0 2,Recta Boost 2,Recta Terra 2,Recta Boost 2,Recta Terra 2,Recta Terra 2,Recta Boost 2,Rampa Lama 2 1,Recta Terra 1,Rampa Terra 1 0,Recta Terra 0, Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Cola 0],
         [Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0, Rampa Terra 0 2,Recta Terra 2,Recta Lama 2,Recta Lama 2,Recta Terra 2,Recta Terra 2,Recta Lama 2,Rampa Lama 2 1,Recta Boost 1,Rampa Terra 1 0,Recta Terra 0, Recta Lama 0, Recta Terra 0 ,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Relva 0],
         [Recta Terra 0,Recta Lama 0,Recta Boost 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0, Rampa Boost 0 2,Recta Boost 2,Recta Lama 2,Recta Boost 2,Recta Lama 2,Recta Terra 2,Recta Terra 2,Rampa Lama 2 1,Recta Terra 1,Rampa Terra 1 0,Recta Terra 0, Recta Boost 0,Recta Boost 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Lama 0]]
       

-- | Segundo Mapa
map2 :: Mapa
map2 = [[Recta Terra 0, Recta Relva 0,Recta Lama 0, Rampa Boost 0 1, Rampa Lama  1 0, Recta Lama 0,Recta Lama 0,Rampa Boost 0 1,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Boost 0 1,Recta Lama 1,Rampa Boost 0 1,Recta Boost 0, Rampa Boost 0 2,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Terra 0],
        [Recta Terra 0, Recta Relva 0,Recta Lama 0, Rampa Relva 0 1, Rampa Boost 1 0,Recta Lama 0,Recta Lama 0,Rampa Boost 0 1,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 1,Recta Lama 1,Rampa Boost 0 1, Recta Terra 0, Rampa Boost 0 2,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Cola 0],
        [Recta Terra 0, Recta Relva 0,Recta Lama 0, Rampa Lama  0 1, Rampa Terra 1 0,Recta Lama 0,Recta Lama 0,Rampa Boost 0 1,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Cola 0 1,Recta Lama 1,Rampa Boost 0 1, Recta Terra 0, Rampa Boost 0 2,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0, Recta Relva 0 ],
        [Recta Terra 0, Recta Relva 0,Recta Lama 0, Rampa Relva 0 1, Rampa Terra 1 0,Recta Lama 0,Recta Lama 0,Rampa Boost 0 1,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Recta Lama 1,Rampa Boost 0 1,Recta Terra 0, Rampa Boost 0 2,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Lama 0]]


-- | Terceiro Mapa 
map3 :: Mapa
map3 = [[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Boost 0,Rampa Terra 0 2 ,Recta Terra 2,Rampa Lama 2 1,Recta Terra 1,Recta Terra 1, Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],
        [Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Rampa Terra 0 2 ,Recta Terra 2,Rampa Relva 2 1,Recta Terra 1,Recta Relva 1, Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Cola 1,Rampa Terra 1 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Cola 0],
        [Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Rampa Terra 0 2 ,Recta Terra 2,Rampa Relva 2 1,Recta Terra 1,Recta Boost 1, Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Terra 1, Recta Relva 1,Rampa Terra 1 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Relva 0],
        [Recta Terra 0,Recta Lama  0,Recta Cola 0,Recta Terra  0,Recta Boost 0,Rampa Terra 0 2 ,Recta Terra 2,Rampa Terra 2 1,Recta Terra 1,Recta Lama 1, Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Recta Lama 1,Rampa Terra 1 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Lama 0]]


-- | Quarto Mapa
map4 :: Mapa
map4 = [[Recta Terra 0,Recta Lama 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0 ,Recta Terra 0,Recta Terra 0,Recta Lama 0,Rampa Terra 0 1,Rampa Terra 1 0,Rampa Boost 0 2,Recta Terra 2,Recta Terra 2, Rampa Terra 2 0,Recta Terra 0,Recta Boost 0,Recta Lama 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],
        [Recta Relva 0,Recta Relva 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Rampa Terra 0 1,Rampa Terra 1 0,Rampa Terra 0 2,Recta Terra 2,Recta Terra 2,Rampa Terra 2 0,Recta Terra 0, Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Cola 0],
        [Recta Terra 0,Recta Lama 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Rampa Terra 0 1,Rampa Terra 1 0,Rampa Terra 0 2,Recta Terra 2,Recta Terra 2,Rampa Terra 2 0,Recta Terra 0, Recta Lama 0, Recta Terra 0 ,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Relva 0],
        [Recta Terra 0,Recta Lama 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Rampa Terra 0 1,Rampa Terra 1 0,Rampa Terra 0 2,Recta Terra 2,Recta Terra 2,Rampa Terra 2 0,Recta Terra 0, Recta Boost 0,Recta Boost 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0, Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Lama 0]]

-- | Lista de Jogadores de todos os mapas
lj :: [Jogador]
lj = [(Jogador 0 0 0 4 (Chao True)),(Jogador 1 0 0.2 4 (Chao True)),(Jogador 2 0 0 4 (Chao True)),(Jogador 3 0 0 4 (Chao True))]


-- | Estado inical do jogo em que a função ao receber os parametros das textures e o inteiro (indicador de menu) da o estado gloss inicial
initialState :: [[Picture]]-> Int -> StatusGloss
initialState lt i  = (gameState1,lt,i)


-- | Função que ao receber um estado do jogo,as texturas e o indicador do menu retorna todo o ambiente grafico desenhado com o mapa em movimento e os jogadores. O mapa movimenta-se ao somar constantemente a distancia do jogador multiplicado pela largura da peça
drawEstado ::  Estado-> [[Picture]] -> Int -> Picture
drawEstado e@(Estado m lj) lt i = Pictures ((drawMapa m (width-d1) height lt) ++  (drawJogadores e lt))
                                          where d1 = realToFrac (distanciaJogador (head lj))*200

-- | Função que desenha todos os menus
drawmenu :: [[Picture]] -> Picture
drawmenu [[menu]]    = Translate 1280 800 menu 
drawmenu [[escMenu]] = Translate 1280 800 escMenu
drawmenu [[dicas]]   = Translate 1280 800 dicas

-- | Função que desenha o mapa todo onde desenha pista a pista sendo sempre subtraido ao eixo y das coordenadas a hpeca
drawMapa ::  Mapa  -> Float->  Float -> [[Picture]] -> [Picture]
drawMapa (p:ps)  x y lt = (drawPista p x y lt)++(drawMapa ps x (y-hPeca) lt) 
drawMapa _ _ _ _        = []


-- | Função que desenha uma pista sendo recursivamente desenhada peca a peca ate ao seu fim
drawPista :: Pista -> Float -> Float -> [[Picture]] -> [Picture]
drawPista (p:ps) x y lt = (drawPeca p x y lt):(drawPista ps (x+wPeca) y lt)
drawPista [] _ _ _      = []


-- | Função que desenha cada tipo de peça mais o seu respetivo bloco abaixo do material que a peça for. Foi subdividada em funcoes para desenhar retas, subidas(caso a diferanca de alturas seja positiva) e descidas(caso diferenca de alturas seja negativa)
drawPeca :: Peca -> Float -> Float -> [[Picture]] -> Picture
drawPeca (Recta p i1) x y (_:[rtTerra,rtCola,rtRelva,rtBoost,rtLama]:_:_:lb:[]) = 
                                       case p of 
                                         Terra -> Pictures ((Translate x (y+px) (Scale 1 0.5 rtTerra)):(drawBlock (p,i1) x (y+px-100) lb))
                                         Cola  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rtCola)):(drawBlock (p,i1) x (y+px-100) lb))
                                         Relva -> Pictures ((Translate x (y+px) (Scale 1 0.5 rtRelva)):(drawBlock (p,i1) x (y+px-100) lb))
                                         Boost -> Pictures ((Translate x (y+px) (Scale 1 0.5 rtBoost)):(drawBlock (p,i1) x (y+px-100) lb))
                                         Lama  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rtLama)):(drawBlock (p,i1) x (y+px-100) lb))
                                        where px = (realToFrac i1)*100
drawPeca r@(Rampa p i1 i2) x y lt | i1-i2 > 0 = drawDescida r x y lt
                                  | i1-i2 < 0 = drawSubida r x y lt


-- | Função que desenha uma descida onde os seus parametros x e y são o sao passados nas funcoes acima. O px é a quantidade de pixeis qe vai ser colocada cada peca e o dif a diferança de alturas entre a altura final e inicial de uma rampa
drawDescida :: Peca -> Float -> Float -> [[Picture]] -> Picture
drawDescida (Rampa p i1 i2) x y (_:_:_:[rD1Terra,rD2Terra,rD3Terra,rD4Terra,rD1Cola,rD2Cola,rD3Cola,rD4Cola,rD1Relva,rD2Relva,rD3Relva,rD4Relva,rD1Boost,rD2Boost,rD3Boost,rD4Boost,rD1Lama,rD2Lama,rD3Lama,rD4Lama]:lb:[]) = 
                                       case (p,dif) of 
                                         (Terra,1)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD1Terra)):(drawBlock (p,i2) x (y+px-100) lb))
                                         (Cola,1)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD1Cola)):(drawBlock (p,i2) x (y+px-100) lb))
                                         (Relva,1)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD1Relva)):(drawBlock (p,i2) x (y+px-100) lb))
                                         (Boost,1)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD1Boost)):(drawBlock (p,i2) x (y+px-100) lb))
                                         (Lama,1)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD1Lama)):(drawBlock (p,i2) x (y+px-100) lb))
                                         (Terra,2)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD2Terra)):(drawBlock (p,i2) x (y+px-150) lb))
                                         (Cola,2)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD2Cola)):(drawBlock (p,i2) x (y+px-150) lb))
                                         (Relva,2)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD2Relva)):(drawBlock (p,i2) x (y+px-150) lb))
                                         (Boost,2)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD2Boost)):(drawBlock (p,i2) x (y+px-150) lb))
                                         (Lama,2)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD2Lama)):(drawBlock (p,i2) x (y+px-150) lb))
                                         (Terra,3)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD3Terra)):(drawBlock (p,i2) x (y+px-200) lb))
                                         (Cola,3)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD3Cola)):(drawBlock (p,i2) x (y+px-200) lb))
                                         (Relva,3)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD3Relva)):(drawBlock (p,i2) x (y+px-200) lb))
                                         (Boost,3)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD3Boost)):(drawBlock (p,i2) x (y+px-200) lb))
                                         (Lama,3)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD3Lama)):(drawBlock (p,i2) x (y+px-200) lb))
                                         (Terra,4)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD4Terra)):(drawBlock (p,i2) x (y+px-250) lb))
                                         (Cola,4)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD4Cola)):(drawBlock (p,i2) x (y+px-250) lb))
                                         (Relva,4)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD4Relva)):(drawBlock (p,i2) x (y+px-250) lb))
                                         (Boost,4)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD4Boost)):(drawBlock (p,i2) x (y+px-250) lb))
                                         (Lama,4)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rD4Lama)):(drawBlock (p,i2) x (y+px-250) lb))
                                       where px  = realToFrac (50*(dif-1)+(i2*100))
                                             dif = i1-i2 


-- | Função que desenha uma subida onde os seus parametros x e y são o sao passados nas funcoes acima. O px é a quantidade de pixeis qe vai ser colocada cada peca e o dif a diferança de alturas entre a altura final e inicial de uma rampa
drawSubida :: Peca -> Float -> Float -> [[Picture]] -> Picture 
drawSubida (Rampa p i1 i2) x y (_:_:[rU1Terra,rU2Terra,rU1Cola,rU2Cola,rU1Relva,rU2Relva,rU1Boost,rU2Boost,rU1Lama,rU2Lama]:_:lb:[]) = 
                                       case (p,dif) of 
                                         (Terra,1)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rU1Terra)):(drawBlock (p,i1) x (y+px-100) lb))
                                         (Cola,1)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rU1Cola)):(drawBlock (p,i1) x (y+px-100) lb))
                                         (Relva,1)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rU1Relva)):(drawBlock (p,i1) x (y+px-100) lb))
                                         (Boost,1)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rU1Boost)):(drawBlock (p,i1) x (y+px-100) lb))
                                         (Lama,1)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rU1Lama)):(drawBlock (p,i1) x (y+px-100) lb))
                                         (Terra,2)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rU2Terra)):(drawBlock (p,i1) x (y+px-150) lb))
                                         (Cola,2)   -> Pictures ((Translate x (y+px) (Scale 1 0.5 rU2Cola)):(drawBlock (p,i1) x (y+px-150) lb))
                                         (Relva,2)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rU2Relva)):(drawBlock (p,i1) x (y+px-150) lb))
                                         (Boost,2)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rU2Boost)):(drawBlock (p,i1) x (y+px-150) lb))
                                         (Lama, 2)  -> Pictures ((Translate x (y+px) (Scale 1 0.5 rU2Lama)):(drawBlock (p,i1) x (y+px-150) lb))
                                       where px =  realToFrac (50*(dif-1)+(i1*100))
                                             dif = i2-i1 


-- | Função que desenha imediatamente abaixo da peça com altura superior a o seu respetivo bloco da mesma textura
drawBlock :: (Piso,Int) -> Float -> Float -> [Picture] -> [Picture]
drawBlock (_,0) _ _ _ = []
drawBlock (p,i) x y lt@[bTerra,bCola,bRelva,bBoost,bLama] = 
                             case p of
                              Terra -> (Translate x y (Scale 1 0.5 bTerra)):(drawBlock (p,(i-1)) x (y-100) lt)
                              Cola  -> (Translate x y (Scale 1 0.5 bCola)):(drawBlock (p,(i-1)) x (y-100) lt)
                              Relva -> (Translate x y (Scale 1 0.5 bRelva)):(drawBlock (p,(i-1)) x (y-100) lt)
                              Boost -> (Translate x y (Scale 1 0.5 bBoost)):(drawBlock (p,(i-1)) x (y-100) lt) 
                              Lama  -> (Translate x y (Scale 1 0.5 bLama)):(drawBlock (p,(i-1)) x (y-100) lt)


-- | Função que desenha a lista de jogadores passada no estado de jogo
drawJogadores :: Estado -> [[Picture]] -> [Picture]
drawJogadores (Estado _ []) _     = []
drawJogadores (Estado m (h:q:r:t:[])) lt = [(drawJogadorAmarelo h m lt),(drawJogadorred q m lt),(drawJogadorOrange r m lt),(drawJogadorGreen t m lt)]


-- | Função que desenha o jogador Vermelho onde as coordenadas sao resultados da tarefa4 adaptados à interface Gráfica
drawJogadorAmarelo :: Jogador -> Mapa -> [[Picture]] -> Picture
drawJogadorAmarelo j@(Jogador p d v c e) m s@([p1,p2,p3,p4,p1M]:_:_:_:_:[])  = case e of 
                              Chao _   -> Translate (posxJogador j m) (posyJogador j m) (Rotate (realToFrac inc) ((Scale 0.75 0.75 p1)))   
                              Ar a i _ -> Translate (posxJogador j m) ((posyJogador j m)+(realToFrac a*50)) (Rotate (realToFrac i) (Scale 0.75 0.75 p1))
                              Morto _  -> Translate (posxJogador j m) (posyJogador j m) (Scale 0.75 0.75 p1M)
                              where
                                (alt,inc) = altInc j m


-- | Função que desenha o jogador Vermelho onde as coordenadas sao resultados da tarefa4 adaptados à interface Gráfica
drawJogadorred :: Jogador -> Mapa -> [[Picture]] -> Picture
drawJogadorred j@(Jogador p d v c e) m s@([p1,p2,p3,p4,p1M]:_:_:_:_:[])  = case e of 
                              Chao _   -> Translate (posxJogador j m) (posyJogador j m) (Rotate (realToFrac inc) ((Scale 0.75 0.75 p2)))  
                              Ar a i _ -> Translate (posxJogador j m) ((posyJogador j m)+(realToFrac a*50)) (Rotate (realToFrac i) (Scale 0.75 0.75 p2))
                              Morto _  -> Translate (posxJogador j m) (posyJogador j m) (Scale 0.75 0.75 p1M)
                              where
                                (alt,inc) = altInc j m


-- | Função que desenha o jogador Vermelho onde as coordenadas sao resultados da tarefa4 adaptados à interface Gráfica
drawJogadorOrange:: Jogador -> Mapa -> [[Picture]] -> Picture
drawJogadorOrange j@(Jogador p d v c e) m s@([p1,p2,p3,p4,p1M]:_:_:_:_:[])  = case e of 
                              Chao _   -> Translate (posxJogador j m) (posyJogador j m) (Rotate (realToFrac inc) ((Scale 0.75 0.75 p3)))   
                              Ar a i _ -> Translate (posxJogador j m) ((posyJogador j m)+(realToFrac a*50)) (Rotate (realToFrac i) (Scale 0.75 0.75 p3))
                              Morto _  -> Translate (posxJogador j m) (posyJogador j m) (Scale 0.75 0.75 p1M)
                              where
                                (alt,inc) = altInc j m


-- | Função que desenha o jogador Vermelho onde as coordenadas sao resultados da tarefa4 adaptados à interface Gráfica
drawJogadorGreen :: Jogador -> Mapa -> [[Picture]] -> Picture
drawJogadorGreen j@(Jogador p d v c e) m s@([p1,p2,p3,p4,p1M]:_:_:_:_:[])  = case e of 
                              Chao _   -> Translate (posxJogador j m) (posyJogador j m) (Rotate (realToFrac inc) ((Scale 0.75 0.75 p4)))   
                              Ar a i _ -> Translate (posxJogador j m) ((posyJogador j m)+(realToFrac a*50)) (Rotate (realToFrac i) (Scale 0.75 0.75 p4))
                              Morto _  -> Translate (posxJogador j m) (posyJogador j m) (Scale 0.75 0.75 p1M)
                              where
                                (alt,inc) = altInc j m

-- | Função que coloca o jogador na sua respetiva posição no eixo dos x
posxJogador :: Jogador -> Mapa -> Float
posxJogador j@(Jogador p d v c e) m = (width - d1)
                                      where d1 =realToFrac (distanciaJogador (head lj))*100                        


-- | Função que coloca o jogador na sua respetiva posição no eixo dos y
posyJogador :: Jogador-> Mapa -> Float
posyJogador j@(Jogador p d v c e) m  = height - (realToFrac (p-1)*hPeca) + px
                                     where
                                       (alt,inc) = altInc j m
                                       difPeca   = (snd (alturasPeca pecaJ)) - (fst (alturasPeca pecaJ))
                                       pecaJ     = encontraPosicaoMatriz (coordenadaJogador j) m
                                       px        = realToFrac (50*(difPeca-1)+(alt*100))


-- | Função que desenha um estado gloss onde são ordenados desenhar todos os menus e mapas
drawStatus :: StatusGloss -> Picture
drawStatus (e,(m:b:r),10) = Pictures [mu]
                where 
                    mu = head m
drawStatus (e,(m:b:r),30) = Pictures [escMu]  
                where 
                    escMu = encontraIndiceLista 1 m
drawStatus (e,(m:b:r),20) = Pictures [dicas]  
                where 
                    dicas = encontraIndiceLista 2 m                
drawStatus (e,(m:b:r),1) = Pictures [bg,st]               
                where
                    bg = head b
                    st =  (drawEstado e r 1 )
drawStatus (e,(m:b:r),2) = Pictures [bg,st]               
                where
                    bg = head b
                    st =  (drawEstado e r 2 )
drawStatus (e,(m:b:r),3) = Pictures [bg,st]               
                where
                    bg = head b
                    st =  (drawEstado e r 3 )
drawStatus (e,(m:b:r),4) = Pictures [bg,st]               
                where
                    bg = head b
                    st = (drawEstado e r 4)


-- | Função que reage a teclas premidas no teclado desde jogadas do jogador e  os menus para mudar (10-menu inicial) (20-Dicas) (30-Escolher menu) (1- Mapa 1) (2- Mapa 2) (3- Mapa 3) (4- Mapa 4) 
reactEvent :: Event -> StatusGloss -> StatusGloss
reactEvent (EventKey (SpecialKey KeyF1)    Down _ _) (e@(Estado map1 lj),lp,10) =  (e,lp,20)
reactEvent (EventKey (SpecialKey KeyF2)    Down _ _) (e@(Estado map1 lj),lp,20) =  (e,lp,30)
reactEvent (EventKey (Char '1')            Down _ _) (e@(Estado map1 lj),lp,30) = (gameState1,lp,1)
reactEvent (EventKey (Char '2')            Down _ _) (e@(Estado map1 lj),lp,30) = (gameState2,lp,2)
reactEvent (EventKey (Char '3')            Down _ _) (e@(Estado map1 lj),lp,30) = (gameState3,lp,3)
reactEvent (EventKey (Char '4')            Down _ _) (e@(Estado map1 lj),lp,30) = (gameState4,lp,4)
reactEvent (EventKey (SpecialKey KeyUp)    Down _ _) (e@(Estado map1 lj),lp,1 ) = (jogada 0 (Movimenta C) e,lp,1)
reactEvent (EventKey (SpecialKey KeyDown)  Down _ _) (e@(Estado map1 lj),lp,1 ) = (jogada 0 (Movimenta B) e,lp,1)
reactEvent (EventKey (SpecialKey KeyLeft)  Down _ _) (e@(Estado map1 lj),lp,1 ) = (jogada 0 (Movimenta E) e,lp,1)
reactEvent (EventKey (SpecialKey KeyRight) Down _ _) (e@(Estado map1 lj),lp,1 ) = (jogada 0 (Movimenta D) e,lp,1)
reactEvent (EventKey (Char 'e')            Down _ _) (e@(Estado map1 lj),lp,1 ) = (jogada 0 Dispara e,lp,1)
reactEvent (EventKey (Char 'q')            Down _ _) (e@(Estado map1 lj),lp,1 ) = (jogada 0 Acelera e,lp,1)
reactEvent (EventKey (Char 'w')            Down _ _) (e@(Estado map1 lj),lp,1 ) = (jogada 0 Desacelera e ,lp ,2)
reactEvent (EventKey (SpecialKey KeyUp)    Down _ _) (e@(Estado map2 lj),lp,2 ) = (jogada 0 (Movimenta C) e,lp,2)
reactEvent (EventKey (SpecialKey KeyDown)  Down _ _) (e@(Estado map2 lj),lp,2 ) = (jogada 0 (Movimenta B) e,lp,2)
reactEvent (EventKey (SpecialKey KeyLeft)  Down _ _) (e@(Estado map2 lj),lp,2 ) = (jogada 0 (Movimenta E) e,lp,2)
reactEvent (EventKey (SpecialKey KeyRight) Down _ _) (e@(Estado map2 lj),lp,2 ) = (jogada 0 (Movimenta D) e,lp,2)
reactEvent (EventKey (Char 'e')            Down _ _) (e@(Estado map2 lj),lp,2 ) = (jogada 0 Dispara e,lp,2)
reactEvent (EventKey (Char 'q')            Down _ _) (e@(Estado map2 lj),lp,2 ) = (jogada 0 Acelera e,lp,2)
reactEvent (EventKey (Char 'w')            Down _ _) (e@(Estado map2 lj),lp,2 ) = (jogada 0 Desacelera e ,lp ,2)
reactEvent (EventKey (SpecialKey KeyUp)    Down _ _) (e@(Estado map3 lj),lp,3 ) = (jogada 0 (Movimenta C) e,lp,3)
reactEvent (EventKey (SpecialKey KeyDown)  Down _ _) (e@(Estado map3 lj),lp,3 ) = (jogada 0 (Movimenta B) e,lp,3)
reactEvent (EventKey (SpecialKey KeyLeft)  Down _ _) (e@(Estado map3 lj),lp,3 ) = (jogada 0 (Movimenta E) e,lp,3)
reactEvent (EventKey (SpecialKey KeyRight) Down _ _) (e@(Estado map3 lj),lp,3 ) = (jogada 0 (Movimenta D) e,lp,3)
reactEvent (EventKey (Char 'e')            Down _ _) (e@(Estado map3 lj),lp,3 ) = (jogada 0 Dispara e,lp,3)
reactEvent (EventKey (Char 'q')            Down _ _) (e@(Estado map3 lj),lp,3 ) = (jogada 0 Acelera e,lp,3)
reactEvent (EventKey (Char 'w')            Down _ _) (e@(Estado map3 lj),lp,3 ) = (jogada 0 Desacelera e ,lp ,3)
reactEvent (EventKey (SpecialKey KeyUp)    Down _ _) (e@(Estado map4 lj),lp,4 ) = (jogada 0 (Movimenta C) e,lp,4)
reactEvent (EventKey (SpecialKey KeyDown)  Down _ _) (e@(Estado map4 lj),lp,4 ) = (jogada 0 (Movimenta B) e,lp,4)
reactEvent (EventKey (SpecialKey KeyLeft)  Down _ _) (e@(Estado map4 lj),lp,4 ) = (jogada 0 (Movimenta E) e,lp,4)
reactEvent (EventKey (SpecialKey KeyRight) Down _ _) (e@(Estado map4 lj),lp,4 ) = (jogada 0 (Movimenta D) e,lp,4)
reactEvent (EventKey (Char 'e')            Down _ _) (e@(Estado map4 lj),lp,4 ) = (jogada 0 Dispara e,lp,4)
reactEvent (EventKey (Char 'q')            Down _ _) (e@(Estado map4 lj),lp,4 ) = (jogada 0 Acelera e,lp,4)
reactEvent (EventKey (Char 'w')            Down _ _) (e@(Estado map4 lj),lp,4 ) = (jogada 0 Desacelera e ,lp ,4)
reactEvent _ s = s -- ignora qualquer outro evento


-- | Função que reage ao tempo aplicando em todos os estados de jogo a função passo a lista de jogadores
reactTime :: Float -> StatusGloss -> StatusGloss
reactTime  n ((Estado m lj) ,lt,1) = ((Estado m ljn),lt,1)
                                      where
                                       ljn = map (passo 0.2 m ) lj
reactTime  n ((Estado m lj) ,lt,2) = ((Estado m ljn),lt,2)
                                      where
                                       ljn = map (passo 0.5 m ) lj
reactTime  n ((Estado m lj) ,lt,3) = ((Estado m ljn),lt,3)
                                      where
                                       ljn = map (passo 0.5 m ) lj
reactTime  n ((Estado m lj) ,lt,4) = ((Estado m ljn),lt,4)
                                      where
                                       ljn = map (passo 0.5 m ) lj
reactTime  n (e,lt,10) = (e,lt,10)
reactTime  n (e,lt,20) = (e,lt,20)
reactTime  n (e,lt,30) = (e,lt,30)



-- | Função principal onde são carregadas todas as imagens do mapa e onde o mapa e menus são mostrados no ecra e reage ao tempo,ou seja, o a função que tem como output o encadeamento de todas as funções acima 
main :: IO ()
main = do Just bg1      <- loadJuicy "../textures/bg/bg1.png"
          Just rtTerra  <- loadJuicy "../textures/map/reta/terra.png"
          Just rtCola   <- loadJuicy "../textures/map/reta/cola.png"
          Just rtRelva  <- loadJuicy "../textures/map/reta/relva.png"
          Just rtBoost  <- loadJuicy "../textures/map/reta/boost.png"
          Just rtLama   <- loadJuicy "../textures/map/reta/lama.png"
          Just rU1Terra <- loadJuicy "../textures/map/rampa/up/terra1.png"
          Just rU2Terra <- loadJuicy "../textures/map/rampa/up/terra2.png"   
          Just rU1Cola  <- loadJuicy "../textures/map/rampa/up/cola1.png"
          Just rU2Cola  <- loadJuicy "../textures/map/rampa/up/cola2.png"
          Just rU1Relva <- loadJuicy "../textures/map/rampa/up/relva1.png"
          Just rU2Relva <- loadJuicy "../textures/map/rampa/up/relva2.png"
          Just rU1Boost <- loadJuicy "../textures/map/rampa/up/boost1.png"
          Just rU2Boost <- loadJuicy "../textures/map/rampa/up/boost2.png"
          Just rU1Lama  <- loadJuicy "../textures/map/rampa/up/lama1.png"
          Just rU2Lama  <- loadJuicy "../textures/map/rampa/up/lama2.png"
          Just rD1Terra <- loadJuicy "../textures/map/rampa/down/terra1.png"
          Just rD2Terra <- loadJuicy "../textures/map/rampa/down/terra2.png"
          Just rD3Terra <- loadJuicy "../textures/map/rampa/down/terra3.png"
          Just rD4Terra <- loadJuicy "../textures/map/rampa/down/terra4.png"  
          Just rD1Cola  <- loadJuicy "../textures/map/rampa/down/cola1.png"
          Just rD2Cola  <- loadJuicy "../textures/map/rampa/down/cola2.png"
          Just rD3Cola  <- loadJuicy "../textures/map/rampa/down/cola3.png"
          Just rD4Cola  <- loadJuicy "../textures/map/rampa/down/cola4.png"
          Just rD1Relva <- loadJuicy "../textures/map/rampa/down/relva1.png"
          Just rD2Relva <- loadJuicy "../textures/map/rampa/down/relva2.png"
          Just rD3Relva <- loadJuicy "../textures/map/rampa/down/relva3.png"
          Just rD4Relva <- loadJuicy "../textures/map/rampa/down/relva4.png"
          Just rD1Boost <- loadJuicy "../textures/map/rampa/down/boost1.png"
          Just rD2Boost <- loadJuicy "../textures/map/rampa/down/boost2.png"
          Just rD3Boost <- loadJuicy "../textures/map/rampa/down/boost3.png"
          Just rD4Boost <- loadJuicy "../textures/map/rampa/down/boost4.png"
          Just rD1Lama  <- loadJuicy "../textures/map/rampa/down/lama1.png"
          Just rD2Lama  <- loadJuicy "../textures/map/rampa/down/lama2.png"
          Just rD3Lama  <- loadJuicy "../textures/map/rampa/down/lama3.png"  
          Just rD4Lama  <- loadJuicy "../textures/map/rampa/down/lama4.png"
          Just bTerra   <- loadJuicy "../textures/map/reta/blockterra.png"
          Just bCola    <- loadJuicy "../textures/map/reta/blockcola.png"
          Just bRelva   <- loadJuicy "../textures/map/reta/blockrelva.png"
          Just bBoost   <- loadJuicy "../textures/map/reta/blockboost.png"
          Just bLama    <- loadJuicy "../textures/map/reta/blocklama.png"
          Just p1       <- loadJuicy "../textures/players/p1.png"  
          Just p2       <- loadJuicy "../textures/players/p2.png"
          Just p3       <- loadJuicy "../textures/players/p3.png"
          Just p4       <- loadJuicy "../textures/players/p4.png"
          Just p1M      <- loadJuicy "../textures/players/p1M.png" 
          Just menu     <- loadJuicy "../textures/Menu/menu.png"
          Just escMenu  <- loadJuicy "../textures/Menu/EscolheMapa.png"
          Just dicas    <- loadJuicy "../textures/Menu/dicas.png"
          play window
               background
               frameRate
               (initialState  [[menu,escMenu,dicas],
                              [bg1],
                              [p1,p2,p3,p4,p1M],
                              [rtTerra,rtCola,rtRelva,rtBoost,rtLama],
                              [rU1Terra,rU2Terra,rU1Cola,rU2Cola,rU1Relva,rU2Relva,rU1Boost,rU2Boost,rU1Lama,rU2Lama],
                              [rD1Terra,rD2Terra,rD3Terra,rD4Terra,rD1Cola,rD2Cola,rD3Cola,rD4Cola,rD1Relva,rD2Relva,rD3Relva,rD4Relva,rD1Boost,rD2Boost,rD3Boost,rD4Boost,rD1Lama,rD2Lama,rD3Lama,rD4Lama],
                              [bTerra,bCola,bRelva,bBoost,bLama]
                              ]
                              10
                              )
               drawStatus
               reactEvent
               reactTime