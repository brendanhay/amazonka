{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Amazonka.DynamoDB.Expression.Internal where

import Amazonka.DynamoDB.Item (DynamoType, Value)

import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid        (Monoid (..))
import Data.Semigroup     (Semigroup (..))
import Data.Text          (Text)

import Numeric.Natural (Natural)

data Path
    = Name   Text
    | Nested Path Path
    | Index  Path !Natural

instance Semigroup Path where
    (<>) = Nested

name :: Text -> Path
name = Name
{-# INLINE name #-}

index :: Path -> Natural -> Path
index = Index
{-# INLINE index #-}

-- A top-level attribute name, such as Id, Title, Description or ProductCategory
-- A document path that references a nested attribute
data Operand
    = Path  Path
    | Value Value

class IsOperand a where
    liftO :: a -> Operand

instance IsOperand Operand where
    liftO = id
    {-# INLINE liftO #-}

instance IsOperand Path where
    liftO = Path
    {-# INLINE liftO #-}

instance IsOperand Value where
    liftO = Value
    {-# INLINE liftO #-}

-- | Denotes a valid condition for hash types such as partition keys.
data Hash

-- | Denotes a valid condition for range types such as sort keys.
data Range

-- | A condition is a singular sub-expression that can be used or combined
-- to form an 'Expression'.
--
-- Any function signature that has an 'IsExpression' constraint, accepts
-- a 'Condition' as a parameter.
data Condition a where
    -- Comparators
    Equal          :: Operand -> Operand            -> Condition Hash
    NotEqual       :: Operand -> Operand            -> Condition Operand
    Less           :: Operand -> Operand            -> Condition Range
    LessOrEqual    :: Operand -> Operand            -> Condition Range
    Greater        :: Operand -> Operand            -> Condition Range
    GreaterOrEqual :: Operand -> Operand            -> Condition Range

    -- Functions
    Exists         :: Path                          -> Condition Operand
    NotExists      :: Path                          -> Condition Operand
    IsType         :: Path -> DynamoType            -> Condition Operand
    Contains       :: Path -> Operand               -> Condition Operand
    Size           :: Path                          -> Condition Operand
    BeginsWith     :: Path -> Text                  -> Condition Range

    -- Ranges
    Between        :: Operand -> (Operand, Operand) -> Condition Range
    In             :: Operand -> NonEmpty Operand   -> Condition Operand

-- | A compound logical expression consisting of sub-expressions and conditions
-- that can be used as part of a filter expression for 'Query' and 'Scan'
-- operations.
--
-- Any function signature that has an 'IsExpression' constraint, accepts
-- an 'Expression' as a parameter.
data Expression where
    CondE  :: Condition a                    -> Expression
    AndE   :: Expression -> Expression       -> Expression
    OrE    :: Expression -> Expression       -> Expression
    NotE   :: Expression                     -> Expression
    ParenE :: Expression                     -> Expression
    EmptyE ::                                   Expression

-- | The associative operation corresponds to conjunction using 'and'.
instance Semigroup Expression

-- | The identity of an expression will 'eval'uate to 'Nothing',
instance Monoid Expression where
    mempty = EmptyE

    mappend EmptyE b      = b
    mappend a      EmptyE = a
    mappend a      b      = AndE a b

data Expression
    = Proj Proj
    | Cond Cond
    | Upd  Upd

class IsExpression a where
    -- | Lift a condition or sub-expression to a top-level expression.
    liftE :: a -> Expression

instance IsExpression Expression where
    liftE = id
    {-# INLINE liftE #-}

instance IsExpression (Condition a) where
    liftE = CondE
    {-# INLINE liftE #-}

-- | A restricted expression that can be used as a @KeyConditionExpression@
-- for 'Query' requests, to provide a specific value the partition key must
-- match and an optional sort key condition.
--
-- /Note:/ Any function signature that has an 'IsExpression' constraint,
-- accepts a 'KeyExpression' as a parameter.
data KeyExpression
    = Partition (Condition Hash)
    | Sort      (Condition Hash) (Condition Range)

instance IsExpression KeyExpression where
    liftE = \case
        Partition h   -> liftE h
        Sort      h r -> liftE h <> liftE r
