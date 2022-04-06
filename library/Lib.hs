{-# LANGUAGE LambdaCase #-}
module Lib where

data Singleton a = Singleton a deriving (Eq, Show)
data Productish a b = Productish a b deriving (Eq, Show)
data Summish a b = First a | Second b deriving (Eq, Show)
data Optional a = NoValue | HasValue a deriving (Eq, Show)
data NotQuiteList a = Value a | Layer (NotQuiteList a) deriving (Eq, Show)
data NotEmpty a = LastValue a | MidValue a (NotEmpty a) deriving (Eq, Show)

-- Singleton

instance Functor Singleton where
  -- TODO
  -- fmap f (Singleton a) = Singleton (f a)
  fmap f (Singleton a) = Singleton $ f a

instance Applicative Singleton  where
  -- TODO
  pure = Singleton
  (Singleton a) <*> (Singleton b) = Singleton (a b)
  -- (Singleton a) (<*>) x = f <$> x

instance Monad Singleton where
  -- TODO
 (Singleton x) >>= f = f x

instance Foldable Singleton where
  -- TODO
    foldMap f (Singleton x) = f x
    -- или
    -- foldr = undefined

instance Traversable Singleton where
  -- TODO
  sequenceA (Singleton x) = Singleton <$> x
  -- или
  -- traverse = undefined

-- Productish

instance Functor (Productish x) where
  -- TODO
  fmap f (Productish a b) = Productish a $ f b

instance (Monoid a) => Applicative (Productish a) where
  -- TODO
  pure x = Productish mempty x
  (Productish a f) <*> (Productish b c) =  Productish (a <> b) (f c)

instance (Monoid a) => Monad (Productish a) where
  -- TODO
  (Productish u a) >>= k = case k a of (Productish v b) -> Productish (u <> v) b;

instance Foldable (Productish a) where
  -- TODO
    foldMap f (Productish _ b) = f b
    -- или
    -- foldr = undefined

instance Traversable (Productish a) where
  -- TODO
  sequenceA (Productish a b) =  Productish a <$> b
  -- или
  -- traverse = undefined

-- Summish

instance Functor (Summish a) where
  -- TODO
  fmap f = \case
              First x -> First x
              Second y -> Second (f y)

instance Applicative (Summish a) where
  -- TODO
  pure = Second
  First a <*> _ = First a
  Second f <*> b = fmap f b

instance Monad (Summish a) where
  -- TODO
  First a >>= _ = First a
  Second b >>= c = c b

instance Foldable (Summish a) where
  -- TODO
    foldMap _ (First _) = mempty 
    foldMap f (Second b) = f b
    -- или
    -- foldr = undefined

instance Traversable (Summish a) where
  -- TODO
  -- sequenceA = undefined
  sequenceA (First x) = pure (First x)
  sequenceA (Second b) = Second <$> b
  -- или
  -- traverse = undefined


-- Optional

instance Functor Optional where
  -- TODO
  fmap _ NoValue = NoValue
  fmap f (HasValue a) = HasValue (f a)

instance Applicative Optional where
  -- TODO
  pure = HasValue
  NoValue <*> _ = NoValue
  HasValue f <*> a = fmap f a

instance Monad Optional where
  -- TODO
  NoValue >>= _ = NoValue 
  (HasValue a) >>= k = k a

instance Foldable Optional where
  -- TODO
    foldMap _ NoValue = mempty 
    foldMap f (HasValue x) = f x
    -- или
    -- foldr = undefined

instance Traversable Optional where
  -- TODO
  sequenceA NoValue =  pure NoValue
  sequenceA (HasValue a) = HasValue <$> a
  -- или
  -- traverse = undefined


-- NotQuiteList

instance Functor NotQuiteList where
  -- TODO
  fmap f (Value a) = Value $ f a
  fmap f (Layer a) = Layer (fmap f a)

instance Applicative NotQuiteList where
  -- TODO
  pure x = Value x
  (Value f) <*> (Value a) = Value (f a)
  (Value f) <*> (Layer a) = Layer (f <$> a)
  (Layer f) <*> (Layer a) = Layer (f <*> a)
  (Layer f) <*> (Value a) = Layer (f <*> Value a)


instance Monad NotQuiteList where
  -- TODO
  (>>=) = undefined

instance Foldable NotQuiteList where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable NotQuiteList where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined

-- NotEmpty

instance Functor NotEmpty where
  -- TODO
  fmap = undefined

instance Applicative NotEmpty where
  -- TODO
  pure = undefined
  (<*>) = undefined

instance Monad NotEmpty where
  -- TODO
  (>>=) = undefined

instance Foldable NotEmpty where
  -- TODO
    foldMap = undefined
    -- или
    -- foldr = undefined

instance Traversable NotEmpty where
  -- TODO
  sequenceA = undefined
  -- или
  -- traverse = undefined
