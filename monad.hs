import Text.Read (readMaybe)
import Data.Either ()


foo :: IO  Int
foo =  do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")
  return (length name) 

bar :: IO ()
bar = getLine >>= \s ->
      getLine >>= \t ->
          putStrLn (s ++ t)


foow :: String  -> String  -> String -> Maybe Int
foow x y z = case readMaybe x of
    Nothing -> Nothing
    Just k -> case readMaybe y of
        Nothing -> Nothing
        Just l -> case  readMaybe z of
            Nothing -> Nothing
            Just m -> Just (k + l + m)


bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x

foo' :: String  -> String -> String -> Maybe Int
foo' x y z = readMaybe x   `bindMaybe` \k ->
             readMaybe y `bindMaybe` \l ->
             readMaybe z `bindMaybe` \m ->
             Just(k + l + m)



--readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of 
    Nothing -> Left $ "can't parse: " ++ s
     Just a -> right a


data Writer a = Writer a [String]
     deriving Show 

--number :: Int -> Writer Int
--number :: Writer n    


instance Functor Writer where
   -- fmap = liftM
instance Applicative Writer where
    pure = return
    
instance Monad Writer where
         return a = Writer a []
        -- (>>=) = bindWriter


threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz = 
    mz >>= \k ->
    mx >>= \l ->
    mx >>= \m ->
        let s = k + l + m in return s


threeInts' :: Monad m => m Int -> m Int -> m Int ->m Int
threeInts' mx mz my = do
    k <- mx
    l <- mz
    m <- my
    return $ k + l + m
    
    
foo'' :: String  -> String -> String -> Maybe Int
foo'' x y z = readMaybe x   `bindMaybe` \k ->
             readMaybe y `bindMaybe` \l ->
             readMaybe z `bindMaybe` \m ->
             Just(k + l + m)
