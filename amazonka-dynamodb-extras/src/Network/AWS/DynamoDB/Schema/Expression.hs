{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.AWS.DynamoDB.Schema.Expression where
    -- (

    -- ) where

import Data.List              (intersperse)
import Data.Monoid            (Monoid (..))
import Data.Semigroup         (Semigroup (..))
import Data.String
import Data.Text              (Text)
import Data.Text.Lazy.Builder (Builder)

import Network.AWS.Data.Text
import Network.AWS.DynamoDB       ()
import Network.AWS.DynamoDB.Value (DynamoType)

import qualified Data.Text.Lazy.Builder as Build

import Prelude hiding (and, not, or)

-- FilterExpression

-- KeyConditionExpression

newtype Path = Path Text
    deriving (Eq, Ord, Show, IsString, ToText)

-- A top-level attribute name, such as Id, Title, Description or ProductCategory
-- A document path that references a nested attribute
data Operand
    = AttributeName Text
    | DocumentPath  Path
      deriving (Eq, Ord, Show)

instance IsString Operand where
    fromString = DocumentPath . fromString

instance ToText Operand where
    toText = \case
        AttributeName t -> t
        DocumentPath  p -> toText p

data Comparator
    = Equal            -- ^ a =  b — true if a is equal to b
    | NotEqual         -- ^ a <> b — true if a is not equal to b
    | LessThan          -- ^ a <  b — true if a is less than b
    | LessThanEqual    -- ^ a <= b — true if a is less than or equal to b
    | GreaterThan       -- ^ a >  b — true if a is greater than b
    | GreaterThanEqual -- ^ a >= b — true if a is greater than or equal to b
      deriving (Eq, Ord, Show)

instance ToText Comparator where
    toText = \case
        Equal            -> "="
        NotEqual         -> "<>"
        LessThan         -> "<"
        LessThanEqual    -> "<="
        GreaterThan      -> ">"
        GreaterThanEqual -> ">="

data Function
    = Exists     Path
    | NotExists  Path
    | TypeOf     Path !DynamoType
    | BeginsWith Path Text
    | Contains   Path Operand
    | Size       Path
      deriving (Eq, Show)

data ConditionF a
    = CompareF  !Comparator Operand Operand
    | BetweenF  Operand Operand Operand
    | InF       Operand [Operand]
    | FunctionF Function
    | AndF      a a
    | OrF       a a
    | NotF      a
    | EmptyF
      deriving (Functor, Foldable, Traversable)

newtype Fix f = Fix { outF :: f (Fix f) }

inF :: f (Fix f) -> Fix f
inF = Fix

type ConditionExpression = Fix ConditionF

instance Semigroup ConditionExpression where
    (<>) = and

instance Monoid ConditionExpression where
    mempty  = inF EmptyF
    mappend = (<>)

equals :: Operand -> Operand -> ConditionExpression
equals a b = inF $ CompareF Equal a b

notEquals :: Operand -> Operand -> ConditionExpression
notEquals a b = inF $ CompareF NotEqual a b

lessThan :: Operand -> Operand -> ConditionExpression
lessThan a b = inF $ CompareF LessThan a b

lessThanEqual :: Operand -> Operand -> ConditionExpression
lessThanEqual a b = inF $ CompareF LessThanEqual a b

greaterThan :: Operand -> Operand -> ConditionExpression
greaterThan a b = inF $ CompareF GreaterThan a b

greaterThanEqual :: Operand -> Operand -> ConditionExpression
greaterThanEqual a b = inF $ CompareF GreaterThanEqual a b

exists :: Path -> ConditionExpression
exists p = inF $ FunctionF (Exists p)

notExists :: Path -> ConditionExpression
notExists p = inF $ FunctionF (NotExists p)

typeOf :: Path -> DynamoType -> ConditionExpression
typeOf p t = inF $ FunctionF (TypeOf p t)

-- attribute substrings etc.
-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html#Expressions.SpecifyingConditions.ConditionExpressions
beginsWith :: Path -> Text -> ConditionExpression
beginsWith p x = inF $ FunctionF (BeginsWith p x)

-- The path and the operand must be distinct; that is, contains (a, a) will return an error.
contains :: Path -> Operand -> ConditionExpression
contains p o = inF $ FunctionF (Contains p o)

size :: Path -> ConditionExpression
size p = inF $ FunctionF (Size p)

and :: ConditionExpression -> ConditionExpression -> ConditionExpression
and a b = inF $ AndF a b

or :: ConditionExpression -> ConditionExpression -> ConditionExpression
or a b = inF $ OrF a b

not :: ConditionExpression -> ConditionExpression
not = inF . NotF

eval :: ConditionExpression -> Builder
eval = go . outF
  where
    go = \case
        CompareF op a b ->
            build a >>> build op >>> build b

        BetweenF a b and' ->
            build a >>> "BETWEEN" >>> build b >>> "AND" >>> build and'

        InF a xs ->
            build a >>> "IN" >>>
                parens (mconcat (intersperse ", " (map build xs)))

        FunctionF fun ->
            case fun of
                Exists     p   ->
                   "attribute_exists"     >>> parens (build p)
                NotExists  p   ->
                   "attribute_not_exists" >>> parens (build p)
                TypeOf     p t ->
                   "attribute_type"       >>> parens (build p <> "," >>> build t)
                BeginsWith p x ->
                   "begins_with"          >>> parens (build p <> "," >>> build x)
                Contains   p o ->
                   "contains"             >>> parens (build p <> "," >>> build o)
                Size       p   ->
                   "size"                 >>> parens (build p)

        AndF a b -> parens (eval a) >>> "AND" >>> parens (eval b)
        OrF  a b -> parens (eval a) >>> "OR"  >>> parens (eval b)
        NotF a   -> "NOT" >>> parens (eval a)

        EmptyF   -> mempty

-- Type class to get a lens to set/get the field of supported requests?
-- expression :: ConditionExpression a -> Text

(>>>) :: Builder -> Builder -> Builder
(>>>) a b = a <> " " <> b
{-# INLINE (>>>) #-}

parens :: Builder -> Builder
parens x = "(" <> x <> ")"
{-# INLINE parens #-}

build :: ToText a => a -> Builder
build = Build.fromText . toText
{-# INLINE build #-}
