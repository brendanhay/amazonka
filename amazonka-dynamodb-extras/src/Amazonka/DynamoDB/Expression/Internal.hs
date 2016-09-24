{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

-- |
-- Module      : Amazonka.DynamoDB.Expression.Internal
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Internal types for building expressions.
--
-- /Note:/ While these types endeavour to be correct by construction, it should
-- be considered unsafe to use the constructors directly, as well as the risk
-- of the internal representation changing without warning between releases.
module Amazonka.DynamoDB.Expression.Internal where

import Amazonka.DynamoDB.Item (Value)

import Control.Applicative (liftA2, many, (<|>))
import Control.Monad       (ap)

import Data.Attoparsec.Text (char, decimal, endOfInput, takeWhile1)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable        (foldl')
import Data.Function        (on)
import Data.Hashable        (Hashable)
import Data.List.NonEmpty   (NonEmpty (..))
import Data.Semigroup       (Semigroup (..))
import Data.Sequence        (Seq, (<|))
import Data.String          (IsString (..))
import Data.Text            (Text)

import Network.AWS.Data.Text

import Numeric.Natural (Natural)

import qualified Data.Sequence as Seq

newtype Name = Name { fromName :: Text }
    deriving (Eq, Show, Ord, Hashable, IsString, ToText, FromText)

-- | A path that can access top-level attribute names in addition to individual
-- nested elements within any document type attribute.
data Path a
    = Attr   a
    | Index  (Path a) Natural
    | Nested (Path a) (Path a)
      deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Path where
    pure  = return
    (<*>) = ap

instance Monad Path where
    return = Attr

    Attr   x   >>= f = f x
    Index  a n >>= f = Index  (a >>= f) n
    Nested a b >>= f = Nested (a >>= f) (b >>= f)

instance Semigroup (Path a) where
    (<>) = Nested

-- | Correctly handles attribute dereferencing via @.@ and @[Natural]@.
instance FromText (Path Name) where
    parser = path0 <* endOfInput
      where
        path0 = Nested <$> path1 <*> (char '.' *> path1)
            <|> path1

        path1 = foldl' Index <$> path2 <*> many (char '[' *> decimal <* char ']')

        path2 = Attr . Name <$> takeWhile1 (not . deref)

        deref c = c == '.' || c == '['

name :: Text -> Path Name
name = Attr . Name
{-# INLINE name #-}

index :: Path a -> Natural -> Path a
index = Index
{-# INLINE index #-}

-- A top-level attribute name, such as Id, Title, Description or ProductCategory
-- A document path that references a nested attribute
data Operand p v
    = Path  (Path p)
    | Value v
      deriving (Eq, Show)

instance Functor (Operand p) where
    fmap = bimap id

instance Foldable (Operand p) where
    foldMap = bifoldMap (const mempty)

instance Traversable (Operand p) where
    traverse = bitraverse (pure . id)

instance Bifunctor Operand where
    bimap f g = \case
        Path  p -> Path  (f <$> p)
        Value v -> Value (g v)

instance Bifoldable Operand where
    bifoldMap f g = \case
        Path  p -> foldMap f p
        Value v -> g v

instance Bitraversable Operand where
    bitraverse f g = \case
        Path  p -> Path  <$> traverse f p
        Value v -> Value <$> g v

-- | Denotes a valid condition for hash types such as partition keys.
data Hash

-- | Denotes a valid condition for range types such as sort keys.
data Range

-- | Denotes a valid condition for non-key types.
data Term

-- | A condition is a singular sub-expression that can be used or combined
-- to form a 'ConditionExpression'.
--
-- Any function signature that has an 'IsCondition' constraint, accepts
-- a 'Condition' as a parameter.
data Condition a p v where
    -- Comparators
    Equal          :: Operand p v -> Operand p v -> Condition Hash  p v
    NotEqual       :: Operand p v -> Operand p v -> Condition Term  p v
    Less           :: Operand p v -> Operand p v -> Condition Range p v
    LessOrEqual    :: Operand p v -> Operand p v -> Condition Range p v
    Greater        :: Operand p v -> Operand p v -> Condition Range p v
    GreaterOrEqual :: Operand p v -> Operand p v -> Condition Range p v

    -- Functions
    Exists         :: Path p                -> Condition Term  p v
    NotExists      :: Path p                -> Condition Term  p v
    IsType         :: Path p -> v           -> Condition Term  p v
    Contains       :: Path p -> Operand p v -> Condition Term  p v
    Size           :: Path p                -> Condition Term  p v
    BeginsWith     :: Path p -> v           -> Condition Range p v

    -- Ranges
    Between        :: Operand p v
                   -> (Operand p v, Operand p v)
                   -> Condition Range p v

    In             :: Operand p v
                   -> NonEmpty (Operand p v)
                   -> Condition Term p v

deriving instance (Eq   p, Eq   v) => Eq   (Condition a p v)
deriving instance (Show p, Show v) => Show (Condition a p v)

instance Functor (Condition a p) where
    fmap = bimap id

instance Foldable (Condition a p) where
    foldMap = bifoldMap (const mempty)

instance Traversable (Condition a p) where
    traverse = bitraverse (pure . id)

instance Bifunctor (Condition a) where
    bimap f g = \case
        Equal          a b -> Equal          (bimap f g a) (bimap f g b)
        NotEqual       a b -> NotEqual       (bimap f g a) (bimap f g b)
        Less           a b -> Less           (bimap f g a) (bimap f g b)
        LessOrEqual    a b -> LessOrEqual    (bimap f g a) (bimap f g b)
        Greater        a b -> Greater        (bimap f g a) (bimap f g b)
        GreaterOrEqual a b -> GreaterOrEqual (bimap f g a) (bimap f g b)

        Exists     p   -> Exists     (fmap f p)
        NotExists  p   -> NotExists  (fmap f p)
        Size       p   -> Size       (fmap f p)
        Contains   p o -> Contains   (fmap f p) (bimap f g o)
        IsType     p t -> IsType     (fmap f p) (g t)
        BeginsWith p x -> BeginsWith (fmap f p) (g x)

        Between a (b, c) -> Between (bimap f g a) (bimap f g b, bimap f g c)
        In      x xs     -> In      (bimap f g x) (bimap f g <$> xs)

instance Bifoldable (Condition a) where
    bifoldr f g z = \case
        Equal          a b -> bifoldr f g (bifoldr f g z b) a
        NotEqual       a b -> bifoldr f g (bifoldr f g z b) a
        Less           a b -> bifoldr f g (bifoldr f g z b) a
        LessOrEqual    a b -> bifoldr f g (bifoldr f g z b) a
        Greater        a b -> bifoldr f g (bifoldr f g z b) a
        GreaterOrEqual a b -> bifoldr f g (bifoldr f g z b) a

        Exists     p   -> foldr f z p
        NotExists  p   -> foldr f z p
        Size       p   -> foldr f z p
        Contains   p o -> foldr f (bifoldr f g z o) p
        IsType     p t -> foldr f (g t z) p
        BeginsWith p x -> foldr f (g x z) p

        Between a (b, c) -> bifoldr f g (bifoldr f g (bifoldr f g z c) b) a
        In      x xs     -> bifoldr f g (foldr (flip (bifoldr f g)) z xs) x

instance Bitraversable (Condition a) where
    bitraverse f g = \case
        Equal          a b ->
            Equal          <$> bitraverse f g a <*> bitraverse f g b
        NotEqual       a b ->
            NotEqual       <$> bitraverse f g a <*> bitraverse f g b
        Less           a b ->
            Less           <$> bitraverse f g a <*> bitraverse f g b
        LessOrEqual    a b ->
            LessOrEqual    <$> bitraverse f g a <*> bitraverse f g b
        Greater        a b ->
            Greater        <$> bitraverse f g a <*> bitraverse f g b
        GreaterOrEqual a b ->
            GreaterOrEqual <$> bitraverse f g a <*> bitraverse f g b

        Exists     p   -> Exists     <$> traverse f p
        NotExists  p   -> NotExists  <$> traverse f p
        Size       p   -> Size       <$> traverse f p
        Contains   p o -> Contains   <$> traverse f p <*> bitraverse f g o
        IsType     p t -> IsType     <$> traverse f p <*> g t
        BeginsWith p x -> BeginsWith <$> traverse f p <*> g x

        Between a (b, c) ->
            Between <$> bitraverse f g a
                    <*> liftA2 (,) (bitraverse f g b) (bitraverse f g c)

        In x xs -> In <$> bitraverse f g x <*> traverse (bitraverse f g) xs

-- | A compound logical expression consisting of sub-expressions and conditions
-- that can be used as part of a filter expression for 'Query' and 'Scan'
-- operations.
--
-- Any function signature that has an 'IsCondition' constraint, accepts
-- an 'Expression' as a parameter.
data ConditionExpression p v where
    CondE  :: Condition a p v
           -> ConditionExpression p v

    AndE   :: ConditionExpression p v
           -> ConditionExpression p v
           -> ConditionExpression p v

    OrE    :: ConditionExpression p v
           -> ConditionExpression p v
           -> ConditionExpression p v

    NotE   :: ConditionExpression p v
           -> ConditionExpression p v

deriving instance (Show p, Show v) => Show (ConditionExpression p v)

instance Functor (ConditionExpression p) where
    fmap = bimap id

instance Foldable (ConditionExpression p) where
    foldMap = bifoldMap (const mempty)

instance Traversable (ConditionExpression p) where
    traverse = bitraverse (pure . id)

instance Bifunctor ConditionExpression where
    bimap f g = \case
        CondE c   -> CondE (bimap f g c)
        AndE  a b -> AndE  (bimap f g a) (bimap f g b)
        OrE   a b -> OrE   (bimap f g a) (bimap f g b)
        NotE  a   -> NotE  (bimap f g a)

instance Bifoldable ConditionExpression where
    bifoldr f g z = \case
        CondE c   -> bifoldr f g z c
        AndE  a b -> bifoldr f g (bifoldr f g z b) a
        OrE   a b -> bifoldr f g (bifoldr f g z b) a
        NotE  a   -> bifoldr f g z a

instance Bitraversable ConditionExpression where
    bitraverse f g = \case
        CondE c   -> CondE <$> bitraverse f g c
        AndE  a b -> AndE  <$> bitraverse f g a <*> bitraverse f g b
        OrE   a b -> OrE   <$> bitraverse f g a <*> bitraverse f g b
        NotE  a   -> NotE  <$> bitraverse f g a

-- | The associative operation corresponds to conjunction using 'and'.
instance Semigroup (ConditionExpression p v) where
    (<>) = AndE

-- | A restricted expression that can be used as a @KeyConditionExpression@
-- for 'Query' requests, to provide a specific value the partition key must
-- match and an optional sort key condition.
data KeyConditionExpression p v
    = Partition (Condition Hash p v)
    | Sort      (Condition Hash p v) (Condition Range p v)
      deriving (Show)

instance Functor (KeyConditionExpression p) where
    fmap = bimap id

instance Foldable (KeyConditionExpression p) where
    foldMap = bifoldMap (const mempty)

instance Traversable (KeyConditionExpression p) where
    traverse = bitraverse (pure . id)

instance Bifunctor KeyConditionExpression where
    bimap f g = \case
        Partition h   -> Partition (bimap f g h)
        Sort      h r -> Sort      (bimap f g h) (bimap f g r)

instance Bifoldable KeyConditionExpression where
    bifoldr f g z = \case
        Partition h   -> bifoldr f g z h
        Sort      h r -> bifoldr f g (bifoldr f g z r) h

instance Bitraversable KeyConditionExpression where
    bitraverse f g = \case
        Partition h   -> Partition <$> bitraverse f g h
        Sort      h r -> Sort      <$> bitraverse f g h <*> bitraverse f g r

data Update p v
    = Operand     (Operand p v)
    | Plus        (Path p)     (Update p v)
    | Minus       (Path p)     (Update p v)
    | IfNotExists (Path p)     (Update p v)
    | ListAppend  (Update p v) (Update p v)
      deriving (Eq, Show)

instance Functor (Update p) where
    fmap = bimap id

instance Foldable (Update p) where
    foldMap = bifoldMap (const mempty)

instance Traversable (Update p) where
    traverse = bitraverse (pure . id)

instance Bifunctor Update where
    bimap f g = \case
        Operand     o   -> Operand     (bimap f g o)
        Plus        p u -> Plus        (fmap f p)    (bimap f g u)
        Minus       p u -> Minus       (fmap f p)    (bimap f g u)
        IfNotExists p u -> IfNotExists (fmap f p)    (bimap f g u)
        ListAppend  a b -> ListAppend  (bimap f g a) (bimap f g b)

instance Bifoldable Update where
    bifoldr f g z = \case
        Operand     o   -> bifoldr f g z o
        Plus        p u -> foldr f (bifoldr f g z u) p
        Minus       p u -> foldr f (bifoldr f g z u) p
        IfNotExists p u -> foldr f (bifoldr f g z u) p
        ListAppend  a b -> bifoldr f g (bifoldr f g z b) a

instance Bitraversable Update where
    bitraverse f g = \case
        Operand     o   -> Operand     <$> bitraverse f g o
        Plus        p u -> Plus        <$> traverse f p <*> bitraverse f g u
        Minus       p u -> Minus       <$> traverse f p <*> bitraverse f g u
        IfNotExists p u -> IfNotExists <$> traverse f p <*> bitraverse f g u
        ListAppend  a b -> ListAppend  <$> bitraverse f g a <*> bitraverse f g b

-- | An update expression consists of sections. Each section begins with a SET,
-- REMOVE or DELETE keyword. You can include any of these sections in an
-- update expression in any order. However, each section keyword can appear only
-- once.
--
-- You can combine multiple 'UpdateExpression's using the 'Semigroup'
-- instance. Ordering of actions is preserved, but the specific top-level action types
-- will always be compiled in the following order:
--
-- @
-- SET action[, ...] REMOVE action[, ...] ADD action[, ...] DELETE action[, ...]
-- @
--
data UpdateExpression p v = UnsafeUpdateExpression
    { _unsafeSet    :: Seq (Path p, Update p v)
    , _unsafeRemove :: Seq (Path p)
    , _unsafeAdd    :: Seq (Path p, v)
    , _unsafeDelete :: Seq (Path p, v)
    } deriving (Eq, Show)
    -- Assumes the invariant that an UpdateExpression cannot be empty is met,
    -- despite internally using sequences for concat/fold reasons.

instance Semigroup (UpdateExpression p v) where
    (<>) a b = UnsafeUpdateExpression
        { _unsafeSet    = on (<>) _unsafeSet    a b
        , _unsafeRemove = on (<>) _unsafeRemove a b
        , _unsafeAdd    = on (<>) _unsafeAdd    a b
        , _unsafeDelete = on (<>) _unsafeDelete a b
        }

instance Functor (UpdateExpression p) where
    fmap = bimap id

instance Foldable (UpdateExpression p) where
    foldMap = bifoldMap (const mempty)

instance Traversable (UpdateExpression p) where
    traverse = bitraverse (pure . id)

instance Bifunctor UpdateExpression where
    bimap f g UnsafeUpdateExpression{..} =
        UnsafeUpdateExpression
            { _unsafeSet    = bimap (fmap f) (bimap f g) <$> _unsafeSet
            , _unsafeRemove =        fmap f              <$> _unsafeRemove
            , _unsafeAdd    = bimap (fmap f) g           <$> _unsafeAdd
            , _unsafeDelete = bimap (fmap f) g           <$> _unsafeDelete
            }

instance Bifoldable UpdateExpression where
    bifoldr f g z UnsafeUpdateExpression{..} =
        let del' = foldr (\(p, v) a -> (flip (foldr f)) p (g v a)) z _unsafeDelete
            add' = foldr (\(p, v) a -> (flip (foldr f)) p (g v a)) del' _unsafeAdd
            rem' = foldr (flip (foldr f)) add' _unsafeRemove
            set' = foldr (\(p, v) a -> (flip (foldr f)) p (bifoldr f g a v)) rem' _unsafeSet
         in set'

instance Bitraversable UpdateExpression where
    bitraverse f g UnsafeUpdateExpression{..} =
        UnsafeUpdateExpression
            <$> traverse (bitraverse (traverse f) (bitraverse f g)) _unsafeSet
            <*> traverse (traverse f) _unsafeRemove
            <*> traverse (bitraverse (traverse f) g) _unsafeAdd
            <*> traverse (bitraverse (traverse f) g) _unsafeDelete

newtype ProjectionExpression p = ProjectionExpression
    { _project :: (Path p, Seq (Path p))
    } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative ProjectionExpression where
    pure = ProjectionExpression . (,mempty) . pure

    ProjectionExpression (f, fs) <*> ProjectionExpression (x, xs)
        = ProjectionExpression (f <*> x, Seq.zipWith (<*>) fs xs)

instance Semigroup (ProjectionExpression p) where
    (<>) (ProjectionExpression (x, xs)) (ProjectionExpression (y, ys))
        = ProjectionExpression (x, xs <> (y <| ys))

class IsOperand a where
    -- | Lift a path or a value to an operand.
    liftO :: a -> Operand Name Value

instance IsOperand (Operand Name Value) where
    liftO = id
    {-# INLINE liftO #-}

instance IsOperand (Path Name) where
    liftO = Path
    {-# INLINE liftO #-}

instance IsOperand Value where
    liftO = Value
    {-# INLINE liftO #-}

class IsCondition a where
    -- | Lift a condition or sub-expression to a top-level condition expression.
    liftC :: a p v -> ConditionExpression p v

instance IsCondition (Condition a) where
    liftC = CondE
    {-# INLINE liftC #-}

instance IsCondition ConditionExpression where
    liftC = id
    {-# INLINE liftC #-}

class IsUpdate a where
    -- | Lift an operand, path, or value to a top-level update sub-expression.
    liftU :: a -> Update Name Value

instance IsUpdate (Update Name Value) where
    liftU = id
    {-# INLINE liftU #-}

instance IsUpdate (Operand Name Value) where
    liftU = Operand
    {-# INLINE liftU #-}

instance IsUpdate (Path Name) where
    liftU = Operand . liftO
    {-# INLINE liftU #-}

instance IsUpdate Value where
    liftU = Operand . liftO
    {-# INLINE liftU #-}
