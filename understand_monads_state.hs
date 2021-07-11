module MonadState where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import System.Random

-- from https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State

data TurnstileState = Locked
                   | Unlocked
                   deriving (Eq, Show)

data TurnstileOutput = Thank
                    | Open
                    | Tut
                    deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Unlocked = (Open, Locked)
push Locked = (Tut, Locked)


monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
  in ([a1,a2,a3,a4,a5], s5)


regularPerson, distractedPerson, hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)

regularPerson s =
  let (a1, s1) = coin s
      (a2, s2) = push s1
  in ([a1,a2], s2)

distractedPerson s =
  let (a1, s1) = coin s
  in ([a1], s1)

hastyPerson s =
  let (a1, s1) = push s
  in ([a1], s1)

tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s =
  let (as1, s1) = regularPerson s
      (as2, s2) = hastyPerson s1
      (as3, s3) = distractedPerson s2
      (as4, s4) = hastyPerson s3
  in (as1 ++ as2 ++ as3 ++ as4, s4)

luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair b s =
  let
    f = if b then regularPerson else distractedPerson
    (a1, s1) = f s
  in
      case s1 of
        Locked -> let (a2, s2) = hastyPerson s1
                  in (False, s2)
        Unlocked -> let (a2', s2') = hastyPerson s1
                    in (True, s2')

-- coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coinS, pushS :: State TurnstileState TurnstileOutput

-- coinS = state coin
-- pushS = state push

coinS = do
  put Unlocked
  return Thank

pushS = do
  s <- get
  put Locked
  case s of
    Locked -> return Tut
    Unlocked -> return Open



mondayS :: State TurnstileState [TurnstileOutput]
mondayS = sequence [coinS, pushS, pushS, coinS, pushS]

regularPersonS, distractedPersonS, hastyPersonS :: State TurnstileState [TurnstileOutput]

regularPersonS = mapM turnS [Coin, Push]
distractedPersonS = mapM turnS [Coin]
hastyPersonS = mapM turnS [Push]

luckyPairS :: Bool -> State TurnstileState Bool

luckyPairS b = state $
  \s ->
    let p = if b then regularPersonS else distractedPersonS
        (o1, s1) = runState p s -- ignore output, transfer state s -> s1
        (o2, s2) = runState hastyPersonS s1 -- save with above
    in case s1 of
      Locked -> (False, s2)
      Unlocked -> (True, s2)

helper :: State TurnstileState Bool
helper = state $ \s ->
                   let b = case s of
                         Locked -> False
                         Unlocked -> True
                   in (b, s)

testTurnstile :: State TurnstileState Bool
testTurnstile = do
  os <- get
  put Locked
  check1 <- pushS
  put Unlocked
  check2 <- pushS
  put Locked
  put Unlocked
  coinS
  check3 <- pushS
  put Locked
  coinS
  check4 <- pushS
  put os
  return (check1 == Tut && check2 == Open && check3 == Open && check4 == Open)

modify :: (s -> s) -> State s ()
modify f = do
  os <- get
  put (f os)

gets :: (s -> a) -> State s a
gets f = do
  cs <- get
  return (f cs)

data TurnstileInput = Coin | Push
  deriving (Eq, Show)

turnS :: TurnstileInput -> State TurnstileState TurnstileOutput
turnS = state . turn where
  turn Coin _ = (Thank, Unlocked)
  turn Push Unlocked = (Open, Locked)
  turn Push Locked = (Tut, Locked)

getsThroughS :: TurnstileInput -> State TurnstileState Bool
getsThroughS input = do
  output <- turnS input
  return $ output == Open

countOpens :: [TurnstileInput] -> State TurnstileState Int
countOpens = foldM incIfOpens 0 where
  incIfOpens :: Int -> TurnstileInput -> State TurnstileState Int
  incIfOpens n i = do
    g <- getsThroughS i
    if g then return (n+1) else return n


tuesdayS :: State TurnstileState [TurnstileOutput]
tuesdayS =  mapM h [regularPersonS, hastyPersonS, distractedPersonS, hastyPersonS]
  where
    h s = do as <- s
             return (head as)

saveCoinS :: [TurnstileInput] -> State TurnstileState Int
saveCoinS = liftM snd . foldM record (False, 0)
  where
    record :: (Bool, Int) -> TurnstileInput -> State TurnstileState (Bool, Int)
    record (hasPreThank, n) input = do
      o <- turnS input
      if o == Thank
        then
        return (True, if hasPreThank then n+1 else n)
        else
        return (False, n)

-- sequenceUntil :: (a -> Bool) -> [State s a] -> State s [a]
sequenceUntil :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntil _ [] = return []
sequenceUntil p (s:ss) = do
  a <- s
  if p a
    then return [a]
    else do as <- sequenceUntil p ss
            return (a:as)

rollPair :: StdGen -> ((Int, Int), StdGen)
rollPair s0 = let (r1, s1) = randomR (1,6) s0
                  (r2, s2) = randomR (1,6) s1
              in ((r1,r2), s2)

rollSix :: StdGen -> ([Int], StdGen)
rollSix s0 = pick s0 6 []
  where
    pick :: StdGen -> Int -> [Int] -> ([Int], StdGen)
    pick s n r
      | n == 0 = (r, s)
      | otherwise = let (a1,s1) = randomR (1,6) s
                    in pick s1 (n-1) (r ++ [a1])


rollN :: Int -> StdGen -> ([Int], StdGen)
rollN n s = runState (pickN n) s

pick :: State StdGen Int
pick = state $ random

pickN :: Int -> State StdGen [Int]
pickN n
  | n == 0 = return []
  | otherwise = do k <- pick
                   ks <- pickN (n-1)
                   return (k:ks)

rollDieS :: State StdGen Int
rollDieS = state $ randomR (1, 6)

rollPairS :: State StdGen (Int, Int)
rollPairS = liftA2 (,) rollDieS rollDieS
{-
rollPairS = do
  r1 <- rollDieS
  r2 <- rollDieS
  return (r1,r2)
-}

rollDieDoubleS :: State StdGen Int
--rollDieDoubleS =
--  r <- rollDieS
--  return (r * 2)
rollDieDoubleS = fmap (*2) rollDieS


rollSixS :: State StdGen [Int]
rollSixS = do
  r1 <- rollDieS
  r2 <- rollDieS
  r3 <- rollDieS
  r4 <- rollDieS
  r5 <- rollDieS
  r6 <- rollDieS
  return (r1:r2:r3:r4:r5:r6:[])


rollNS :: Int -> State StdGen [Int]
rollNS n = replicateM n rollDieS

luckyDoubleS :: State StdGen Int
luckyDoubleS = do r1 <- rollDieS
                  if r1 == 6
                    then rollDieS
                    else return r1

rollTwoSummedS :: State StdGen Int
--rollTwoSummedS = (+) <$> rollDieS <$> rollDieS
rollTwoSummedS = liftA2 (+) rollDieS rollDieS

happyDoubleS :: State StdGen Int
happyDoubleS = f <$> rollDieS <*> rollDieS
  where
    f a b
      | a == 6 = 2 * (a + b)
      | otherwise = a + b
{-
happyDoubleS = do
  r1 <- rollDieS
  r2 <- rollDieS
  if r1 == 6
    then return $ (r1 + r2) * 2
    else return (r1 + r2)
-}


randomElt :: [a] -> State StdGen a
randomElt l = do i <- state $ randomR (0, len-1)
                 return (l!!i)
  where len = length l
{-
randomElt l = do s0 <- get
                 let (i, s1) = randomR (0, (length l) - 1) s0
                 put s1
                 return (l!!i)
-}

getRandomS :: Random a => State StdGen a
getRandomS = state random

randomInputS :: State StdGen TurnstileInput
randomInputS = do b <- getRandomS
                  return $ if b then Coin else Push

randomTurnS :: State (StdGen, TurnstileState) TurnstileOutput
randomTurnS = do inp <- processing fstL randomInputS
                 out <- processing sndL (turnS inp)
                 return out

{-
randomTurnS = do inp <- processingFst randomInputS
                 out <- processingSnd (turnS inp)
                 return out
randomTurnS = do (g0,s0) <- get
                 let (inp, g1) = runState randomInputS g0
                     (out, s1) = runState (turnS inp) s0
                     ss = (g1, s1)
                   in do put ss
                         return out
-}


processingFst :: State a o -> State (a,b) o
processingFst = processing fstL
{-
processingFst m = do
  (s1, s2) <- get
  let (o, s1') = runState m s1
  put (s1', s2)
  return o
-}

processingSnd :: State b o -> State (a,b) o
processingSnd = processing sndL
{-
processingSnd m = do
  (s1,s2) <- get
  let (o,s2') = runState m s2
  put (s1,s2')
  return o
-}

data Lens cmb sub = Lens { view :: cmb -> sub
                         , set :: cmb -> sub -> cmb
                         }

fstL :: Lens (a,b) a
fstL = Lens fst (\(_,y) x -> (x,y))

sndL :: Lens (a,b) b
sndL = Lens snd (\(x,_) y -> (x, y))

processing :: Lens cmb sub -> State sub o -> State cmb o
processing lens st = do c <- get
                        let s = (view lens c)
                            (o, s') = runState st s
                          in do put (set lens c s')
                                return o


