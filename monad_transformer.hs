
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return :: Monad m => a -> t m a
  return = MaybeT . return Just
  (>>=) :: Monad m => t m a -> (a -> t m b) -> t m b
  x >>= f = MaybeT $ do -- f :: a -> MaybeT m a
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeT return (f y)
