{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.AWS.DynamoDB.Expression.Internal where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid        (Monoid (..))
import Data.Semigroup     (Semigroup (..))
import Data.Text          (Text)

import Numeric.Natural (Natural)

import Network.AWS.DynamoDB.Value (DynamoType, DynamoValue (..), Value)

data Path
    = Name   Text
    | Nested Path Path
    | Index  Path !Natural

instance Semigroup Path where
    (<>) = Nested

name :: Text -> Path
name = Name

index :: Path -> Natural -> Path
index = Index

-- A top-level attribute name, such as Id, Title, Description or ProductCategory
-- A document path that references a nested attribute
data Operand
    = Verbatim Text
    | Path     Path
    | Value    Value

class IsOperand a where
    liftO :: a -> Operand

instance IsOperand Operand where liftO = id
instance IsOperand Path    where liftO = Path

instance {-# OVERLAPPABLE #-} DynamoValue a => IsOperand a where
    liftO = Value . toValue

verbatim :: Text -> Operand
verbatim = Verbatim

-- | Denotes a valid condition for hash types such as partition keys.
data Hash

-- | Denotes a valid condition for range types such as sort keys.
data Range

data Relation a where
    Equal          :: Relation Hash
    NotEqual       :: Relation Operand
    Less           :: Relation Range
    LessOrEqual    :: Relation Range
    Greater        :: Relation Range
    GreaterOrEqual :: Relation Range

data Function a where
    Exists     :: Path               -> Function Operand
    NotExists  :: Path               -> Function Operand
    IsType     :: Path -> DynamoType -> Function Operand
    Contains   :: Path -> Operand    -> Function Operand
    Size       :: Path               -> Function Operand
    BeginsWith :: Path -> Text       -> Function Range

-- | A condition is a singular sub-expression that can be used or combined
-- to form an 'Expression'.
--
-- Any function signature that has an 'IsExpression' constraint, accepts
-- a 'Condition' as a parameter.
data Condition a where
    Compare  :: Relation a -> Operand -> Operand -> Condition a
    Function :: Function a                       -> Condition a
    Between  :: Operand -> (Operand, Operand)    -> Condition Range
    In       :: Operand -> NonEmpty Operand      -> Condition Operand

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

-- data Expr = Expr (HashMap Text) Expression

class IsExpression a where
    -- | Lift a condition or sub-expression to a top-level expression.
    liftE :: a -> Expression

instance IsExpression Expression    where liftE = id
instance IsExpression (Condition a) where liftE = CondE

-- | A restricted expression that can be used as a @KeyConditionExpression@
-- for 'Query' requests, to provide a specific value the partition key must
-- match and an optional sort key condition.
--
-- /Note:/ Any function signature that has an 'IsExpression' constraint,
-- accepts a 'KeyExpression' as a parameter.
data KeyExpression a where
   Partition :: Condition Hash                    -> KeyExpression Hash
   Sort      :: Condition Hash -> Condition Range -> KeyExpression Range

instance IsExpression (KeyExpression a) where
    liftE = \case
        Partition h   -> liftE h
        Sort      h r -> liftE h <> liftE r
