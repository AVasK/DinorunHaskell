module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
--import System.Random

width, height, offset :: Int
fps :: Int

width = 1000
height = 400
offset = 100
fps = 60

window :: Display
window = InWindow "Dino Run" (width, height) (offset, offset)

background :: Color
background = white

g :: Float
g = 1700

-- How can we store positions of obstacles???
data Obstacle = Big | Small deriving (Show, Eq)
type Position = Float

-- | My very strangely implemented pseudo-random generator, but does it work??
type Seed = Int
start_seed = 5694355 :: Seed
coinToss :: Seed -> (Bool, Seed)
coinToss seed = if seed `mod` 2 == 0 then (True, newSeed) else (False, newSeed)
    where
        newSeed = seed * 42214221 + (seed ^ 2)
        
------------------- mb delete all of the above? -------------------------------------

addObst :: [(Obstacle, Position)] -> Obstacle -> Float -> [(Obstacle, Position)]
addObst list newObst x = list ++ [(newObst, fromIntegral width + x)]

removeOutdated :: [(Obstacle, Position)] -> [(Obstacle, Position)]
removeOutdated = filter (\(obst, pos) -> if pos <= - fromIntegral width / 2 then False else True)

currentDistance :: [(Obstacle, Position)] -> Float
currentDistance [] = 800
currentDistance [(_, pos)] = (fromIntegral width / 2) - pos
currentDistance ((_, pos) : xs) = min ((fromIntegral width / 2) - pos) (currentDistance xs)

updateObstacles :: GameState -> GameState
updateObstacles game = game { obstacles = obstacles' }
    where
        list = obstacles game
        obstacles' = if currentDistance list <= 200 then
            list
        else
            list'
            where
                --list' = list
                --list' = if addP == 0 then addObst obstType else list
                list' = addObst list obstType x
                x = fromIntegral $ 600 - round (speed game * fromIntegral (tick game) ^ 2) `mod` 800
                --obstP = randomRIO (0, 1)
                obstType = if btf then Big else Small
                btf_counter = round (fromIntegral (tick game) ^ 2) `mod` 6
                btf = if btf_counter < 3 then True else False
                --addP = randomRIO (0, upper)
                --upper = (currentDistance list - 200) `div` 100
        
                
-- Clouds:

addCloud :: [(Position, Position)] -> Float -> Float -> [(Position, Position)]
addCloud list x y = list ++ [(fromIntegral width + x, y)]

removeClouds :: [(Position, Position)] -> [(Position, Position)]
removeClouds = filter (\(pos, _) -> if pos <= - fromIntegral width / 2 then False else True)

-- | Horizontal distance
cloudDist :: [(Position, Position)] -> Float 
cloudDist [] = 0
cloudDist [(pos, _)] = (fromIntegral width / 2) - pos
cloudDist ((pos, _) : xs) = min ((fromIntegral width / 2) - pos) (cloudDist xs)

updateClouds :: GameState -> GameState
updateClouds game = game { clouds = clouds' }
    where
        list = clouds game
        clouds' = if cloudDist list <= 400 then
            list
        else
            list'
            where
                list' = addCloud list x y
                x = fromIntegral $ 600 - round (speed game * fromIntegral (tick game)) `mod` 500
                y = fromIntegral $ 100 - round (speed game * fromIntegral (tick game) ^ 2) `mod` 200

data GameState = Game 
    { acceleration :: Float
    , dinoY :: Float
    , dinoV :: Float
    , maxV :: Float
    , speed :: Float
    , endgame :: Bool
    , obstacles :: [(Obstacle, Position)]
    , clouds :: [(Position, Position)]
    , tick :: Int
    , score :: Int
    } deriving Show
    
-- | Desctibes the initial state of the Pong game
initialState :: GameState
initialState = Game
    { acceleration = 200
    , dinoY = 0
    , dinoV = 460
    , maxV = 1000
    , speed = 8
    , endgame = False
    , obstacles = [(Big, 100), (Small, 300), (Big, 900)]
    , clouds = [(100, -100), (340, 20), (560, -200)]
    , tick = 0
    , score = 0
    }

render :: GameState -> Picture
render game = 
    pictures 
    [ drawClouds (clouds game)
    , scene
    , drawObstacles (obstacles game)
    , placeDino (dinoY game)
    , interface
    , keys
    , gameOver
    ]
    where
        gameOver = 
            if endgame game then
                pictures 
                [ translate (-fromIntegral width / 2 + 150) 0 $ text "Game Over"
                , translate (-fromIntegral width / 2 + 150) 1 $ text "Game Over"
                , translate (-fromIntegral width / 2 + 150) 2 $ text "Game Over"
                , translate (-fromIntegral width / 2 + 151) 2 $ text "Game Over"
                , translate (-fromIntegral width / 2 + 149) 1 $ text "Game Over"
                ]
            else
                pictures []
        
        interface = translate (fromIntegral width / 2 - 100) (fromIntegral height / 2 - 50) $
            pictures 
            [ color (light black) $ rectangleSolid w h
            , color (counterColor) $ rectangleSolid (w-10) (h-10)
            , translate (-w/2 + 10) (-20) $ scale 0.3 0.3 $ text graphScore
            ]
            -- Key bindings
        keys =  
            if endgame game then 
                translate (fromIntegral width / 2 - 100) (fromIntegral height / 2 - 50) $
                pictures
                -- Retry key:
                [ translate 43 (-80) $ color (light black) $ rectangleSolid 50 50
                , translate 43 (-80) $ color (applyN 4 light black) $ rectangleSolid 40 40
                , translate 33 (-90) $ scale 0.2 0.2 $ text "R"
                , translate (-130) (-90) $ scale 0.2 0.2 $ text "New game: "
                -- Jump key:
                , translate 43 (-140) $ color (light black) $ rectangleSolid 50 50
                , translate 43 (-140) $ color (applyN 4 light black) $ rectangleSolid 40 40
                , translate 33 (-150) $ scale 0.2 0.2 $ text "W"
                , translate (-80) (-150) $ scale 0.2 0.2 $ text "Jump: "
                -- Score:
                , translate (-400) (-20) $ scale 0.3 0.3 $ text "Score ->"
                ]
            else
                pictures []
        w = 140
        h = 60   
        graphScore = 
            if score game > 90000 then
                show (score game `div` 10000) ++ "k..." 
            else 
                show $ score game
                
        counterColor = 
            if endgame game then
                dark red
            else
                applyN 4 light black
            
        
            
        parseObstacles = map (\(obstType, obstPos) -> translate obstPos 0 $ chooseObst obstType)
        chooseObst :: Obstacle -> Picture
        drawObstacles arg = pictures (parseObstacles arg)
        drawClouds arg = pictures (map (\(x, y) -> translate x y $ cloud_pic) arg)
        chooseObst Big = obstacle_big
        chooseObst Small = obstacle_small
        placeDino y = translate 0 y $ dinoPic
        dinoPic = 
            if endgame game 
                then dino_gameover 
            else if dinoV game > 0 
                    then dino_jump
                else 
                    dino
        
    
updateSpeed :: GameState -> GameState
updateSpeed game = game { speed = v', acceleration = a', maxV = maxV' }
    where
        v = speed game
        v' = if endgame game then v else v * 1.0001 
        -- x = v0*t - (g+a)*t^2 / 2
        -- v = v0 - (g+a) * t 
        -- v0 = (g+a) * t, t = time to reach max height
        -- t = v0 / (g+a)
        -- x = v0^2 / 2*(g+a) , where x = cactus height + some reserve
        a = acceleration game
        a' = if endgame game then a else a * 1.0002
        maxV' = sqrt (2 * height * (g+a))
        height = 250 + 50
        
        
updateScore :: GameState -> GameState
updateScore game = game { score = score' }
    where
        v = round $ speed game
        score' = if endgame game then score game else (score game + v)
        
        
updateTime :: GameState -> GameState
updateTime game = game { tick = newTime }
    where
        gameover = endgame game 
        time = tick game
        newTime = if gameover then time else time + 1
        
moveDino :: Float -> GameState -> GameState
moveDino t@seconds game = game { dinoY = y', dinoV = v' }
    where
        y = dinoY game
        v = dinoV game
        y' = if endgame game then y else y + v*t - ((g + a)*t^2) / 2
        v' = if endgame game then v else v - (g + a) * t
        a = acceleration game
        
        
dinoGroundCollision :: GameState -> GameState
dinoGroundCollision game = game { dinoY = y', dinoV = v'}
    where
        y = dinoY game
        v = dinoV game
        (y', v') = if y <= 0 then (0,0) else (y,v)
        
        
moveSurroundings :: GameState -> GameState
moveSurroundings game = game { obstacles = obstacles', clouds = clouds' }
    where
        v = speed game
        move = map (\(t, pos) -> (t, pos - v))
        moveClouds = map (\(t, pos) -> (t - v/2, pos))
        obstacles' = if endgame game then obstacles game else move (obstacles game)
        clouds' = if endgame game then clouds game else moveClouds (clouds game)
        

collides :: Float -> Float -> (Obstacle, Position) -> Bool
collides x y (Big, obstacle_x) = 
    if abs (x - obstacle_x) <= 75 && y <= 175 then
        True
    else
        False
        
collides x y (Small, obstacle_x) = 
    if abs (x - obstacle_x) <= 50 && y <= 100 then
        True
    else
        False
        
        
checkCollisions :: GameState -> GameState
checkCollisions game = game { endgame = collision }
    where
        x = (-400)
        y = dinoY game
        collision = any (collides x y) (obstacles game)
        
    
update seconds = checkCollisions . updateClouds . updateScore . updateSpeed . updateTime . updateObstacles . moveSurroundings . dinoGroundCollision . moveDino seconds
        
        
handler :: Event -> GameState -> GameState
handler (EventKey (Char 'w') _ _ _) game = game { dinoV = res }
    where
        v = dinoV game
        v' = maxV game
        res = if v == 0 then v' else v
        
handler (EventKey (Char 'r') _ _ _) game = 
    game { endgame = False, 
           dinoV = 0, 
           dinoY = 0,
           acceleration = 200,
           speed = 8,
           obstacles = [], --[(Big, 100), (Small, 200), (Big, 950)],
           score = 0
         }

handler _ game = game
        

scene :: Picture
scene = pictures 
    [ translate 0 (- fromIntegral height / 2 + 5) $ color (light $ light black) $ rectangleSolid (fromIntegral width) 10
    , translate 0 (- fromIntegral height / 2 + 15) $ color (light black) $ rectangleSolid (fromIntegral width) 2
    ]
    
dino :: Picture
dino = translate (-350) (- fromIntegral height / 2 + 120) $ scale 0.7 0.7 $ pictures 
    [ --translate (-400) (- fromIntegral height / 2 + 105) $ color red $ rectangleSolid 80 200
    -- | Dino
      color black $ rectangleSolid 80 50
    , translate 0 (-5) $ rectangleSolid 100 40
    , translate 0 (-40) $ color black $ rectangleSolid 60 10
    , translate (-30) (-15) $ color black $ rectangleSolid 40 60
    , translate (-30) 0 $ color white $ rectangleSolid 5 5 -- eye
    , translate (-20) (-70) $ color black $ rectangleSolid 30 8 -- arms
    , translate (-5) (-73) $ color black $ rectangleSolid 8 15 -- arms
    , translate (-40) (-70) $ color black $ rectangleSolid 40 60
    , translate (-55) (-85) $ color black $ rectangleSolid 45 70
    , translate (-65) (-90) $ color black $ rectangleSolid 60 50
    , translate (-75) (-95) $ color black $ rectangleSolid 60 50
    , translate (-115) (-95) $ color black $ rectangleSolid 25 30
    , translate (-125) (-90) $ color black $ rectangleSolid 15 30
    , translate (-130) (-85) $ color black $ rectangleSolid 10 30
    , translate (-135) (-80) $ color black $ rectangleSolid 10 30
    , translate (-140) (-70) $ color black $ rectangleSolid 8 35
    , translate (-90) (-135) $ color black $ rectangleSolid 10 40 -- Leg Left
    , translate (-80) (-125) $ color black $ rectangleSolid 10 30
    , translate (-70) (-115) $ color black $ rectangleSolid 10 30
    , translate (-85) (-160) $ color black $ rectangleSolid 20 10 -- Leg Left Base
    , translate (-50) (-135) $ color black $ rectangleSolid 10 40 -- Leg Right
    , translate (-55) (-115) $ color black $ rectangleSolid 10 30
    , translate (-45) (-160) $ color black $ rectangleSolid 20 10 -- Leg Right Base
    ]
    
    
dino_jump :: Picture
dino_jump = translate (-350) (- fromIntegral height / 2 + 120) $ scale 0.7 0.7 $ pictures 
    [ --translate (-400) (- fromIntegral height / 2 + 105) $ color red $ rectangleSolid 80 200
    -- | Dino
      color black $ rectangleSolid 80 50
    , translate 0 (-5) $ rectangleSolid 100 40
    , translate 0 (-40) $ color black $ rectangleSolid 60 10
    , translate (-30) (-15) $ color black $ rectangleSolid 40 60
    , translate (-30) 0 $ color white $ rectangleSolid 5 5 -- eye
    , translate (-20) (-70) $ color black $ rectangleSolid 30 8 -- arms
    , translate (-5) (-73) $ color black $ rectangleSolid 8 15 -- arms
    , translate (-40) (-70) $ color black $ rectangleSolid 40 60
    , translate (-55) (-85) $ color black $ rectangleSolid 45 70
    , translate (-65) (-90) $ color black $ rectangleSolid 60 50
    , translate (-75) (-95) $ color black $ rectangleSolid 60 50
    , translate (-115) (-95) $ color black $ rectangleSolid 25 30
    , translate (-125) (-90) $ color black $ rectangleSolid 15 30
    , translate (-130) (-85) $ color black $ rectangleSolid 10 30
    , translate (-135) (-80) $ color black $ rectangleSolid 10 30
    , translate (-140) (-70) $ color black $ rectangleSolid 8 35
    , translate (-90) (-135) $ color black $ rectangleSolid 10 20 -- Leg Left
    , translate (-80) (-125) $ color black $ rectangleSolid 10 20
    , translate (-70) (-115) $ color black $ rectangleSolid 10 20
    , translate (-85) (-140) $ color black $ rectangleSolid 20 10 -- Leg Left Base
    , translate (-50) (-135) $ color black $ rectangleSolid 10 20 -- Leg Right
    , translate (-55) (-115) $ color black $ rectangleSolid 10 20
    , translate (-45) (-140) $ color black $ rectangleSolid 20 10 -- Leg Right Base
    ]
    
    
dino_gameover :: Picture
dino_gameover = translate (-350) (- fromIntegral height / 2 + 120) $ scale 0.7 0.7 $ pictures 
    [ --translate (-400) (- fromIntegral height / 2 + 105) $ color red $ rectangleSolid 80 200
    -- | Dino
      color black $ rectangleSolid 80 50
    , translate 0 (-5) $ rectangleSolid 100 40
    , translate 0 (-40) $ color black $ rectangleSolid 60 10
    , translate (-30) (-15) $ color black $ rectangleSolid 40 60
    , translate (-30) 0 $ color white $ rectangleSolid 10 10 -- eye
    , translate (-30) (-3) $ color black $ rectangleSolid 3 3 -- eye
    , translate (-20) (-70) $ color black $ rectangleSolid 30 8 -- arms
    , translate (-5) (-73) $ color black $ rectangleSolid 8 15 -- arms
    , translate (-40) (-70) $ color black $ rectangleSolid 40 60
    , translate (-55) (-85) $ color black $ rectangleSolid 45 70
    , translate (-65) (-90) $ color black $ rectangleSolid 60 50
    , translate (-75) (-95) $ color black $ rectangleSolid 60 50
    , translate (-115) (-95) $ color black $ rectangleSolid 25 30
    , translate (-125) (-90) $ color black $ rectangleSolid 15 30
    , translate (-130) (-85) $ color black $ rectangleSolid 10 30
    , translate (-135) (-80) $ color black $ rectangleSolid 10 30
    , translate (-140) (-70) $ color black $ rectangleSolid 8 35
    , translate (-90) (-135) $ color black $ rectangleSolid 10 40 -- Leg Left
    , translate (-80) (-125) $ color black $ rectangleSolid 10 30
    , translate (-70) (-115) $ color black $ rectangleSolid 10 30
    , translate (-85) (-160) $ color black $ rectangleSolid 20 10 -- Leg Left Base
    , translate (-50) (-135) $ color black $ rectangleSolid 10 40 -- Leg Right
    , translate (-55) (-115) $ color black $ rectangleSolid 10 30
    , translate (-45) (-160) $ color black $ rectangleSolid 20 10 -- Leg Right Base
    ]
    
-- | Small Rectangle-Art cactus
obstacle_small :: Picture
obstacle_small = translate 0 (-160) $ pictures 
    [ --color red $ rectangleSolid 100 100 -- hitBox
      color black $ rectangleSolid 30 100
    , translate (-20) 0 $ color black $ rectangleSolid 40 20
    , translate (-40) 20 $ color black $ rectangleSolid 20 40
    , translate 20 (-10) $ color black $ rectangleSolid 30 15
    , translate 35 10 $ color black $ rectangleSolid 15 30
    ]
    
-- | Big rectangle-art cactus
obstacle_big :: Picture
obstacle_big = scale 0.7 0.7 $ translate 0 (-150) $ pictures 
    [ --translate (-20) 0 $ color red $ rectangleSolid 180 250 -- hitBox
      color black $ rectangleSolid 50 250
    , translate (-40) 0 $ color black $ rectangleSolid 80 40
    , translate (-60) 40 $ color black $ rectangleSolid 40 120
    , translate 45 (-20) $ color black $ rectangleSolid 60 30
    , translate (-90) (-10) $ color black $ rectangleSolid 40 20
    , translate (-100) 30 $ color black $ rectangleSolid 20 80
    , translate 35 10 $ color black $ rectangleSolid 15 30
    , translate 60 20 $ color black $ rectangleSolid 30 60
    -- , color red $ rectangleSolid 100 250 -- hitbox
    ]
    
applyN :: (Num n, Eq n) => n -> (a->a) -> a -> a
applyN 1 f x = f x
applyN n f x = f (applyN (n-1) f x)
    
cloud_pic :: Picture
cloud_pic = pictures 
        [ color (applyN 4 light black) $ rectangleSolid 50 50
        , translate 0 (-10) $ color (applyN 4 light black) $ rectangleSolid 100 40
        ]
    

    
drawing = pictures 
    [scene,
    obstacle_big, 
    obstacle_small,
    dino
    ]

{-

main :: IO ()
main = animate window background frame
    where
        frame :: Float -> Picture
        frame seconds = render $ dinoCollision (moveDino seconds initialState)
        
-}

main :: IO ()
main = play window background fps initialState render handler update