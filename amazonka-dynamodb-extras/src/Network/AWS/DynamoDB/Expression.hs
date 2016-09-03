{-# LANGUAGE GADTs #-}

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

      Expression
    , Condition
    , Hash
    , Range

    -- * Key Expressions
    -- $key_expressions

    , KeyExpression
    , partition
    , sort

    -- * Making Comparisons
    -- $comparators

    , equals
    , notEquals
    , less
    , lessOrEqual
    , greater
    , greaterOrEqual

    -- ** Ranges
    -- $ranges

    , between
    , in_

    -- * Functions
    , exists
    , notExists
    , isType
    , size
    , contains
    , beginsWith

    -- * Logical Evaluations
    , and_
    , or_
    , not_

    -- * Precedence
    -- $precedence

    -- * Operands
    , Operand      (..)

    -- * Paths
    , Path         (..)

    -- * Evaluation
    , IsExpression (..)
    , compile
    , evaluate
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text          (Text)

import Network.AWS.DynamoDB.Expression.Compile  (compile, evaluate)
import Network.AWS.DynamoDB.Expression.Internal
import Network.AWS.DynamoDB.Value               (DynamoType)

import Prelude hiding (compare)

-- | Specify the exact partition key.
--
-- >>> partition (equals "my-key-name" "bar")
-- "my-key-name" = :sub
--
partition :: Condition Hash -> KeyExpression Hash
partition = Partition
{-# INLINE partition #-}

-- | You can narrow the scope of a 'KeyExpression' by specifying a sort key
-- condition as follows:
--
-- >>> partition (equals "partition-key" "foo") `sort` lessThan "sort-key" "123"
-- "partition-key" = :sub1 AND "sort-key" < :sub2
--
sort :: KeyExpression Hash -> Condition Range -> KeyExpression Range
sort (Partition h) = Sort h
{-# INLINE sort #-}

-- | True if a is equal to b.
--
-- >>> equals a b
-- a = b
--
equals :: (IsOperand a, IsOperand b) => a -> b -> Condition Hash
equals = compare Equal
{-# INLINE equals #-}

-- | True if a is not equal to b.
--
-- >>> notEquals a b
-- a <> b
--
notEquals :: (IsOperand a, IsOperand b) => a -> b -> Condition Operand
notEquals = compare NotEqual
{-# INLINE notEquals #-}

-- | True if a is less than b.
--
-- >>> less a b
-- a < b
--
less :: (IsOperand a, IsOperand b) => a -> b -> Condition Range
less = compare Less
{-# INLINE less #-}

-- | True if a is less than or equal to b.
--
-- >>> lessOrEqual a b
-- a <= b
--
lessOrEqual :: (IsOperand a, IsOperand b) => a -> b -> Condition Range
lessOrEqual = compare LessOrEqual
{-# INLINE lessOrEqual #-}

-- | True if a is greater than b.
--
-- >>> greater a b
-- a > b
--
greater :: (IsOperand a, IsOperand b) => a -> b -> Condition Range
greater = compare Greater
{-# INLINE greater #-}

-- | True if a is greater than or equal to b.
--
-- >>> greaterOrEqual a b
-- a >= b
--
greaterOrEqual :: (IsOperand a, IsOperand b) => a -> b -> Condition Range
greaterOrEqual = compare GreaterOrEqual

-- | Lift a binary comparison and its two operands to a condition.
compare :: (IsOperand a, IsOperand b) => Relation c -> a -> b -> Condition c
compare cmp a b = Compare cmp (liftO a) (liftO b)
{-# INLINE compare #-}

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
exists p = Function (Exists p)
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
notExists p = Function (NotExists p)
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
isType p t = Function (IsType p t)
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
size p = Function (Size p)
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
contains p o = Function (Contains p o)
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
beginsWith p x = Function (BeginsWith p x)
{-# INLINE beginsWith #-}

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

infixl 9 `equals`, `notEquals`, `less`, `lessOrEqual`, `greater`, `greaterOrEqual`
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

> equals, notEquals, less, lessOrEqual, greater, greaterOrEqual
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
