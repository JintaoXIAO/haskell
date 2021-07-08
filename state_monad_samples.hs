{-#LANGUAGE BlockArguments#-}
import Prelude

import Control.Monad.State
import Control.Monad

{-
string compressing
"aaabbddd" -> "a3b2d3"
"hello" -> "h1e1l2o1"
-}

p1 :: String -> Maybe (Char, Int, String) -> String
p1 [] Nothing = []
p1 [] (Just (c,i,s)) = s <> (c:(show i))
p1 (c:cs) Nothing = p1 cs (Just (c, 1, ""))
p1 (c:cs) (Just (c1, i, s))
  | c == c1 = p1 cs (Just (c,i+1,s))
  | otherwise = p1 cs (Just (c,1, (s <> (c1:(show i)))))

data CompressState = CompressState { previousChar :: Maybe Char
                                   , count :: Int
                                   , result :: String
                                   } deriving Show

initState = CompressState Nothing 0 ""

p :: String -> State CompressState String
p s = case s of
        [] -> do (CompressState mc i rs) <- get
                 case mc of
                   Nothing -> return []
                   Just c -> put (CompressState Nothing 0 (rs <> (c:(show i)))) >> return []
        (c:cs) -> do (CompressState mc i rs) <- get
                     case mc of
                       Nothing -> put (CompressState (Just c) 1 rs) >> return cs
                       Just c' -> if c == c'
                                  then put (CompressState (Just c) (i+1) rs) >> return cs
                                  else put (CompressState (Just c) 1 (rs <> (c':(show i)))) >> return cs

ps :: String -> State CompressState String
ps s = p s >>= \ns -> case ns of
                        [] -> get >>= \(CompressState mc i rs) ->
                                 case mc of
                                   Nothing -> return []
                                   (Just c) -> put (CompressState Nothing 0 (rs <> (c:(show i)))) >> return []
                        cs -> ps cs

solution :: String -> String
solution s = result $ execState (ps s) initState
