{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Network.AWS.DynamoDB.Expression
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A shallow DSL embedding of DynamoDB's expression language that can be used
-- to specify @KeyConditionExpression@ or @FilterExpression@ parameters for
-- 'Scan' and 'Query' operations.
module Network.AWS.DynamoDB.Expression
    (
    -- * Usage
    -- $usage

    -- * Expressions
    -- $expressions

      IsExpression (..)
    , Expression
    , Condition
    , Hash
    , Range

    -- * Key Expressions
    -- $key_expressions

    , KeyExpression
    , partition
    , partitionFilter

    -- * Making Comparisons
    -- $comparators

    , (#=)
    , (#<>)
    , (#<)
    , (#<=)
    , (#>)
    , (#>=)

    , (=:)
    , (<>:)
    , (<:)
    , (<=:)
    , (>:)
    , (>=:)

    , equal
    , notEqual
    , lessThan
    , lessThanOrEqual
    , greaterThan
    , greaterThanOrEqual

    -- * Functions
    , exists
    , notExists
    , isType
    , size
    , contains
    , beginsWith

    -- * Ranges
    -- $ranges

    , between
    , in_

    -- * Logical Evaluations
    , and_
    , or_
    , not_

    -- * Precedence
    -- $precedence

    -- * Operands
    -- $operands

    , IsOperand    (..)
    , Operand      (..)

    -- * Paths
    , Path         (..)

    -- * Evaluation
    , compile
    , evaluate
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text          (Text)

import Network.AWS.DynamoDB.Expression.Compile  (compile, evaluate)
import Network.AWS.DynamoDB.Expression.Internal
import Network.AWS.DynamoDB.Value               (DynamoType, DynamoValue (..))

import Prelude hiding (compare)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let eval = maybe mempty fst . evaluate

-- Note about how by default attribute names are substituted for placeholders,
-- as well as values. Just for the doctests the names are shown.

-- just roll partition/sort into a single additional function.
-- KeyExpression no longer needs to be a GADT, sort doesn't need to be disambiguated with Data.List
-- and partition should just take 'text' in reference to a single key, likewise the sort variant.

-- | Specify an exact partition key.
--
-- >>> eval $ partition "partition-key" (=: "bar")
-- "partition-key = :v1"
--
partition :: Text                     -- ^ The partition key name.
          -> (Path -> Condition Hash) -- ^ A partially applied hash condition, such as @(=: "bar")@.
          -> KeyExpression
partition h f = Partition (f (name h))
{-# INLINE partition #-}

-- | Specify an exact partition key, and narrow the scope
-- by specifying a sort key condition as follows:
--
-- >>> eval $ partitionFilter "partition-key" (=: "foo") "sort-key" (>: 123)
-- "(partition-key = :v1 AND sort-key < :v2)"
--
partitionFilter :: Text                      -- ^ The partition key name.
                -> (Path -> Condition Hash)  -- ^ A partially applied hash condition, such as @(=: "foo")@.
                -> Text                      -- ^ The sort key name.
                -> (Path -> Condition Range) -- ^ A partially applied range condition, such as @(>: 123)@.
                -> KeyExpression
partitionFilter h f r g = Sort (f (name h)) (g (name r))
{-# INLINE partitionFilter #-}

-- FIXME: Note about mnemonics for #/: and substituted values.

(#=) :: IsOperand b => Path -> b -> Condition Hash
(#=) = equal
{-# INLINE (#=) #-}

(#<>) :: IsOperand b => Path -> b -> Condition Operand
(#<>) = notEqual

(#<) :: IsOperand b => Path -> b -> Condition Range
(#<) = lessThan

(#<=) :: IsOperand b => Path -> b -> Condition Range
(#<=) = lessThanOrEqual

(#>) :: IsOperand b => Path -> b -> Condition Range
(#>) = greaterThan

(#>=) :: IsOperand b => Path -> b -> Condition Range
(#>=) = greaterThanOrEqual

(=:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Hash
(=:) a b = equal a (toValue b)
{-# INLINE (=:) #-}

(<>:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Operand
(<>:) a b = notEqual a (toValue b)

(<:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Range
(<:) a b = lessThan a (toValue b)

(<=:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Range
(<=:) a b = lessThanOrEqual a (toValue b)

(>:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Range
(>:) a b = greaterThan a (toValue b)

(>=:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Range
(>=:) a b = greaterThanOrEqual a (toValue b)

-- | True if a is equal to b.
--
-- >>> equal a b
-- a = b
--
equal :: (IsOperand a, IsOperand b) => a -> b -> Condition Hash
equal a b = Equal (liftO a) (liftO b)
{-# INLINE equal #-}

-- | True if a is not equal to b.
--
-- >>> notEqual a b
-- a <> b
--
notEqual :: (IsOperand a, IsOperand b) => a -> b -> Condition Operand
notEqual a b = NotEqual (liftO a) (liftO b)
{-# INLINE notEqual #-}

-- | True if a is lessThan than b.
--
-- >>> lessThan a b
-- a < b
--
lessThan :: (IsOperand a, IsOperand b) => a -> b -> Condition Range
lessThan a b = Less (liftO a) (liftO b)
{-# INLINE lessThan #-}

-- | True if a is lessThan than or equal to b.
--
-- >>> lessThanOrEqual a b
-- a <= b
--
lessThanOrEqual :: (IsOperand a, IsOperand b) => a -> b -> Condition Range
lessThanOrEqual a b = LessOrEqual (liftO a) (liftO b)
{-# INLINE lessThanOrEqual #-}

-- | True if a is greater than b.
--
-- >>> greaterThan a b
-- a > b
--
greaterThan :: (IsOperand a, IsOperand b) => a -> b -> Condition Range
greaterThan a b = Greater (liftO a) (liftO b)
{-# INLINE greaterThan #-}

-- | True if a is greater than or equal to b.
--
-- >>> greaterThanOrEqual a b
-- a >= b
--
greaterThanOrEqual :: (IsOperand a, IsOperand b) => a -> b -> Condition Range
greaterThanOrEqual a b = GreaterOrEqual (liftO a) (liftO b)
{-# INLINE greaterThanOrEqual #-}

-- | Test the existence of an attribute.
--
-- Evaluates to true if the item contains the attribute specified by 'Path'.
-- For example, to check whether an item in the table has
-- a side view picture:
--
-- >>> exists "Pictures.SideView"
-- attribute_exists ("Pictures.SideView")
--
exists :: Path -> Condition Operand
exists = Exists
{-# INLINE exists #-}

-- | Test the non-existence of an attribute.
--
-- Evaluates to true if the attribute specified by 'Path'
-- does not exist in the item.
-- For example, to check whether an item has a @Manufacturer@ attribute:
--
-- >>> notExists "Manufacturer"
-- attribute_not_exists ("Manufacturer")
--
notExists :: Path -> Condition Operand
notExists = NotExists
{-# INLINE notExists #-}

-- | Test if the attribute is of the specified 'DynamoType'.
--
-- Evaluates to true if the attribute at the specified path is of a particular
-- data type. For example, to check whether the @FiveStar@ attribute is
-- of type @L@ (list):
--
-- >>> isType "ProductReviews.FiveStar" L
-- attribute_type ("ProductReviews.FiveStar", :sub)
--
isType :: Path -> DynamoType -> Condition Operand
isType = IsType
{-# INLINE isType #-}

-- | Return a number representing an attribute's size.
--
-- The following are valid data types for use with size:
--
-- * If the attribute is of type 'S' (string), size returns the length of the string.
--
-- * If the attribute is of type 'B' (binary), size returns the number of bytes in the attribute value.
--
-- * If the attribute is a Set data type, size returns the number of elements in the set.
--
-- * If the attribute is of type 'L' (list) or 'M' (map), size returns the number of child elements.
--
size :: Path -> Condition Operand
size = Size
{-# INLINE size #-}

-- | Test if the attribute contains a particular substring or set element.
--
-- Evalutes to true if the attribute specified by path is:
--
-- * A string that contains a particular substring.
--
-- * A set that contains a particular element within the set.
--
-- The path and the operand must be distinct; that is, @contains (a, a)@
-- will result in an error.
--
-- For example, to check whether the Brand string attribute contains
-- the substring Company:
--
-- >>> contains ("Brand", "Company")
-- contains ("Brand", :sub)
--
contains :: Path -> Operand -> Condition Operand
contains = Contains
{-# INLINE contains #-}

-- | Test if the attribute begins with a particular substring.
--
-- For example, to check whether the first few characters of the front view
-- picture attribute is URL:
--
-- >>> beginsWith ("Pictures.FrontView", "http://")
-- begins_with ("Pictures.FrontView", :sub)
--
beginsWith :: Path -> Text -> Condition Range
beginsWith = BeginsWith
{-# INLINE beginsWith #-}

-- | Test the if an attribute is within the specified range.
--
-- For example:
--
-- >>> between a (b, c)
-- a BETWEEN b AND c
--
-- Which results in true if @a@ is greater than or equal to @b@, and less than
-- or equal to @c@.
--
between :: Operand -> (Operand, Operand) -> Condition Range
between = Between
{-# INLINE between #-}

-- | Test that operand is a member of the specified set, @x ∈ xs@.
--
-- Evalutes to true if the operand is equal to any value in the set. For example:
--
-- >>> in_ a (pure b <> pure a)
-- a IN (b, a)
--
-- Will result in true.
--
in_ :: Operand -> NonEmpty Operand -> Condition Operand
in_ = In
{-# INLINE in_ #-}

-- | Logical conjunction, where the resulting expression is true if both
-- sub-expressions are true.
--
-- >>> equals a b `and` greater c d
-- a = b AND c > d
--
-- /See:/ '<>', 'mappend'.
and_ :: (IsExpression a, IsExpression b) => a -> b -> Expression
and_ a b = AndE (liftE a) (liftE b)
{-# INLINE and_ #-}

-- | Logical disjunction, where the resulting expression is true if either
-- sub-expression is true.
--
-- >>> equals a b `or` equals c d
-- a = b OR c = d
--
or_ :: (IsExpression a, IsExpression b) => a -> b -> Expression
or_ a b = OrE (liftE a) (liftE b)
{-# INLINE or_ #-}

-- | Logical negation, where the resulting expression is true if
-- the sub-expression is false, and false if the sub-expression is true.
--
-- >>> not (equals a b)
-- NOT a = b
--
not_ :: IsExpression a => a -> Expression
not_ = NotE . liftE
{-# INLINE not_ #-}

-- Precedence

--infixl 9 :=:, :<>:, :<:, :<=:, :>:, :>=:
infixl 9 `equal`, `notEqual`, `lessThan`, `lessThanOrEqual`, `greaterThan`, `greaterThanOrEqual`
infixl 8 `in_`
infixl 7 `between`
infixl 6 `exists`, `notExists`, `isType`, `contains`, `size`, `beginsWith`
infixl 4 `not_`
infixl 3 `and_`
infixl 2 `or_`

{- $usage
TODO:

* Find a way to ensure sharing when passing in values that will be
  substituted with placeholders.
* Determine if placeholders are used for paths or only top-level attributes.
* Replace attribute names/paths with placeholders
  each component of a path gets a placeholder.
* Replace values with placeholders
* Supplement 'eval' with 'compile' which will return the

It seems operands are not just names/paths, but also a placeholder in
attribute values.

-}

{- $expressions
An expression represents restrictions to put in place when you read
and write items in a table. DynamoDB expressions are free-form strings that
can contain attribute names, document paths, logical operators, and
functions. For a complete list of elements allowed in an expression, see
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html#ConditionExpressionReference Condition Expression Reference>.
-}

{- $key_expressions
-}

{- $comparators
Use these comparators to compare an operand against a range of values, or an
enumerated list of values.
-}

{- $ranges
Use the 'between' and 'in_' to compare an operand against a range of values,
or an enumerated list of values.
-}

{- $precedence

The infix function precedence follows the order:

> :=:, :<>:, :<:, :<=:, :>:, :>=:
> equals, notEquals, less, lessOrEqual, greater, greaterThanOrEqual
> in_
> between
> exists, notExists, isType, contains, size, beginsWith
> not_
> and_
> or_

To demonstrate these fixities, suppose that conditions a and b are true, and that
condition c is false. The following expression will evaluate to true:

>>> a `or` b `and` c
a OR b AND c

However, if you enclose a condition in parentheses, it will be evaluated first.
For example, the following evaluates to false:

>>> (a `or` b) `and` c
(a OR b) AND c

-}

{- $operands
Note about overloading of operands, 'IsOperand', and 'DynamoValue'.

-}
