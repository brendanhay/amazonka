{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Amazonka.DynamoDB.Expression.Condition
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Condition Expressions.
--
-- /See:/
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Performing Conditional Writes with Condition Expressions>
-- in the AWS documentation, and "Amazonka.DynamoDB.Expression" to get started.
module Amazonka.DynamoDB.Expression.Condition
    (
    -- * Key Condition Expressions
      KeyConditionExpression

    , partition
    , partitionFilter

    -- * Condition Expressions
    , ConditionExpression

    -- ** Logical Evaluations
    , and_
    , or_
    , not_

    -- * Sub-conditions
    , Hash
    , Range
    , Term
    , Condition

    -- ** Making Comparisons
    , equal
    , notEqual
    , lessThan
    , lessThanOrEqual
    , greaterThan
    , greaterThanOrEqual

    -- *** Path Comparators
    , (#=)
    , (#<>)
    , (#<)
    , (#<=)
    , (#>)
    , (#>=)

    -- *** Value Comparators
    , (=:)
    , (<>:)
    , (<:)
    , (<=:)
    , (>:)
    , (>=:)

    -- ** Functions
    , exists
    , notExists
    , isType
    , size
    , contains
    , beginsWith

    -- ** Ranges
    , between
    , in_
    ) where

import Amazonka.DynamoDB.Expression.Internal
import Amazonka.DynamoDB.Item.Value

import Data.Bifunctor     (bimap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text          (Text)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Amazonka.DynamoDB.Expression.Compile
-- >>> import Data.Maybe (fromMaybe)
-- >>> import Data.Semigroup ((<>))
-- >>> import Data.Text.Lazy.Builder (toLazyText)
-- >>> import qualified Data.Text.Lazy.IO as Text
-- >>> let eval = Text.putStrLn . toLazyText . maybe mempty fst . compile conditionExpression . liftC

-- | Specify an exact partition key.
--
-- >>> eval $ partition (name "partition-key" =: "bar")
-- partition-key = :v1
--
partition :: Condition Hash p v
             -- ^ A partition key hash condition, such as @name "partition-key" =: "bar"@.
          -> KeyConditionExpression p v
partition = Partition
{-# INLINE partition #-}

-- | Specify an exact partition key, and narrow the scope
-- by specifying a sort key condition as follows:
--
-- >>> eval $ partitionFilter (name "partition-key" =: "foo") (name "sort-key" >: 123)
-- (partition-key = :v1 AND sort-key > :v2)
--
partitionFilter :: (Condition Hash p v)
                   -- ^ A partition key hash condition, such as @name "partition-key" =: "foo"@.
                -> (Condition Range p v)
                   -- ^ A sort key range condition, such as @name "sort-key" >: 123@.
                -> KeyConditionExpression p v
partitionFilter = Sort
{-# INLINE partitionFilter #-}

-- Precedence

infixl 9 #=, #<>, #<, #<=, #>, #>=
infixl 9 =:, <>:, <:, <=:, >:, >=:
infixl 9 `equal`, `notEqual`, `lessThan`, `lessThanOrEqual`, `greaterThan`, `greaterThanOrEqual`
infixl 8 `in_`
infixl 7 `between`
infixl 6 `exists`, `notExists`, `isType`, `contains`, `size`, `beginsWith`
infixl 4 `not_`
infixl 3 `and_`
infixl 2 `or_`

-- | True if a is equal to b.
--
-- >>> eval $ equal (name "foo") (name "bar")
-- foo = bar
--
-- /See:/ '#=', '=:'
equal :: (IsOperand a, IsOperand b) => a -> b -> Condition Hash Name Value
equal a b = Equal (liftO a) (liftO b)
{-# INLINE equal #-}

-- | True if a is not equal to b.
--
-- >>> eval $ notEqual (name "foo") (name "bar")
-- foo <> bar
--
-- /See:/ '#<>', '<>:'
notEqual :: (IsOperand a, IsOperand b) => a -> b -> Condition Term Name Value
notEqual a b = NotEqual (liftO a) (liftO b)
{-# INLINE notEqual #-}

-- | True if a is lessThan than b.
--
-- >>> eval $ lessThan (name "foo") (name "bar")
-- foo < bar
--
-- /See:/ '#<', '<:'
lessThan :: (IsOperand a, IsOperand b) => a -> b -> Condition Range Name Value
lessThan a b = Less (liftO a) (liftO b)
{-# INLINE lessThan #-}

-- | True if a is lessThan than or equal to b.
--
-- >>> eval $ lessThanOrEqual (name "foo") (name "bar")
-- foo <= bar
--
-- /See:/ '#<=', '<=:'
lessThanOrEqual :: (IsOperand a, IsOperand b)
                => a
                -> b
                -> Condition Range Name Value
lessThanOrEqual a b = LessOrEqual (liftO a) (liftO b)
{-# INLINE lessThanOrEqual #-}

-- | True if a is greater than b.
--
-- >>> eval $ greaterThan (name "foo") (name "bar")
-- foo > bar
--
-- /See:/ '#>', '>:'
greaterThan :: (IsOperand a, IsOperand b)
            => a
            -> b
            -> Condition Range Name Value
greaterThan a b = Greater (liftO a) (liftO b)
{-# INLINE greaterThan #-}

-- | True if a is greater than or equal to b.
--
-- >>> eval $ greaterThanOrEqual (name "foo") (name "bar")
-- foo >= bar
--
-- /See:/ '#>=', '>=:'
greaterThanOrEqual :: (IsOperand a, IsOperand b)
                    => a
                   -> b
                   -> Condition Range Name Value
greaterThanOrEqual a b = GreaterOrEqual (liftO a) (liftO b)
{-# INLINE greaterThanOrEqual #-}

-- | Synonym for 'equal' where the LHS is specialized to 'Path' conditions.
(#=) :: IsOperand b => Path Name -> b -> Condition Hash Name Value
(#=) = equal
{-# INLINE (#=) #-}

-- | Synonym for 'notEqual' where the LHS is specialized to 'Path' conditions.
(#<>) :: IsOperand b => Path Name -> b -> Condition Term Name Value
(#<>) = notEqual
{-# INLINE (#<>) #-}

-- | Synonym for 'lessThan' where the LHS is specialized to 'Path' conditions.
(#<) :: IsOperand b => Path Name -> b -> Condition Range Name Value
(#<) = lessThan
{-# INLINE (#<) #-}

-- | Synonym for 'lessThanOrEqual' where the LHS is specialized to 'Path' conditions.
(#<=) :: IsOperand b => Path Name -> b -> Condition Range Name Value
(#<=) = lessThanOrEqual
{-# INLINE (#<=) #-}

-- | Synonym for 'greaterThan' where the LHS is specialized to 'Path' conditions.
(#>) :: IsOperand b => Path Name -> b -> Condition Range Name Value
(#>) = greaterThan
{-# INLINE (#>) #-}

-- | Synonym for 'greaterThanOrEqual' where the LHS is specialized to 'Path' conditions.
(#>=) :: IsOperand b => Path Name -> b -> Condition Range Name Value
(#>=) = greaterThanOrEqual
{-# INLINE (#>=) #-}

-- | Synonym for 'equal' that serializes the RHS to an 'AttributeValue'.
(=:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Hash Name Value
(=:) a b = equal a (toValue b)
{-# INLINE (=:) #-}

-- | Synonym for 'notEqual' that serializes the RHS to an 'AttributeValue'.
(<>:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Term Name Value
(<>:) a b = notEqual a (toValue b)
{-# INLINE (<>:) #-}

-- | Synonym for 'lessThan' that serializes the RHS to an 'AttributeValue'.
(<:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Range Name Value
(<:) a b = lessThan a (toValue b)
{-# INLINE (<:) #-}

-- | Synonym for 'lessThanOrEqual' that serializes the RHS to an 'AttributeValue'.
(<=:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Range Name Value
(<=:) a b = lessThanOrEqual a (toValue b)
{-# INLINE (<=:) #-}

-- | Synonym for 'greaterThan' that serializes the RHS to an 'AttributeValue'.
(>:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Range Name Value
(>:) a b = greaterThan a (toValue b)
{-# INLINE (>:) #-}

-- | Synonym for 'greaterThanOrEqual' that serializes the RHS to an 'AttributeValue'.
(>=:) :: (IsOperand a, DynamoValue b) => a -> b -> Condition Range Name Value
(>=:) a b = greaterThanOrEqual a (toValue b)
{-# INLINE (>=:) #-}

-- | Test the existence of an attribute.
--
-- Evaluates to true if the item contains the attribute specified by 'Path'.
-- For example, to check whether an item in the table has
-- a side view picture:
--
-- >>> eval $ exists (attr "Pictures" <> attr "SideView")
-- attribute_exists (Pictures.SideView)
--
exists :: Path p -> Condition Term p Value
exists = Exists
{-# INLINE exists #-}

-- | Test the non-existence of an attribute.
--
-- Evaluates to true if the attribute specified by 'Path'
-- does not exist in the item.
-- For example, to check whether an item has a @Manufacturer@ attribute:
--
-- >>> eval $ notExists (name "Manufacturer")
-- attribute_not_exists (Manufacturer)
--
notExists :: Path p -> Condition Term p Value
notExists = NotExists
{-# INLINE notExists #-}

-- | Test if the attribute is of the specified 'NativeType'.
--
-- Evaluates to true if the attribute at the specified path is of a particular
-- data type. For example, to check whether the @FiveStar@ attribute is
-- of type @L@ (list):
--
-- >>> eval $ isType (name "ProductReviews" <> name "FiveStar") L
-- attribute_type (ProductReviews.FiveStar, :v1)
--
isType :: Path p -> NativeType -> Condition Term p Value
isType p = IsType p . toValue
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
size :: Path p -> Condition Term p Value
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
-- >>> eval $ contains (name "Brand") (toValue "Company")
-- contains (Brand, :v1)
--
contains :: IsOperand a => Path Name -> a -> Condition Term Name Value
contains p = Contains p . liftO
{-# INLINE contains #-}

-- | Test if the attribute begins with a particular substring.
--
-- For example, to check whether the first few characters of the front view
-- picture attribute is URL:
--
-- >>> eval $ beginsWith (name "Pictures" <> name "FrontView") "http://"
-- begins_with (Pictures.FrontView, :v1)
--
beginsWith :: Path p -> Text -> Condition Range p Value
beginsWith p = BeginsWith p . toValue
{-# INLINE beginsWith #-}

-- | Test the if an attribute is within the specified range.
--
-- For example:
--
-- >>> eval $ between (name "Price") (toValue 1, toValue 10)
-- Price BETWEEN :v1 AND :v2
--
-- Which results in true if @Price@ is greater than or equal to @1@, and less than
-- or equal to @10@.
--
between :: (IsOperand a, IsOperand b, IsOperand c)
        => a
        -> (b, c)
        -> Condition Range Name Value
between a = Between (liftO a) . bimap liftO liftO
{-# INLINE between #-}

-- | Test that operand is a member of the specified set, @x ∈ xs@.
--
-- Evalutes to true if the operand is equal to any value in the set. For example:
--
-- >>> eval $ in_ (name "Price") (pure (toValue 10.99) <> pure (toValue 12.99) <> pure (toValue 14.99))
-- Price IN (:v1, :v2, :v3)
--
in_ :: (IsOperand a, IsOperand b) => a -> NonEmpty b -> Condition Term Name Value
in_ a = In (liftO a) . fmap liftO
{-# INLINE in_ #-}

-- | Logical conjunction, where the resulting expression is true if both
-- sub-expressions are true.
--
-- >>> eval $ equal (name "Foo") (toValue "bar") `and_` greaterThan (name "Bar") (name "Foo")
-- (Foo = :v1 AND Bar > Foo)
--
-- /See:/ '<>', 'mappend'.
and_ :: (IsCondition a, IsCondition b)
     => a p v
     -> b p v
     -> ConditionExpression p v
and_ a b = AndE (liftC a) (liftC b)
{-# INLINE and_ #-}

-- | Logical disjunction, where the resulting expression is true if either
-- sub-expression is true.
--
-- >>> eval $ equal (name "Foo") (toValue "baz") `or_` equal (name "Bar") (toValue "qux")
-- (Foo = :v1 OR Bar = :v2)
--
or_ :: (IsCondition a, IsCondition b)
    => a p v
    -> b p v
    -> ConditionExpression p v
or_ a b = OrE (liftC a) (liftC b)
{-# INLINE or_ #-}

-- | Logical negation, where the resulting expression is true if
-- the sub-expression is false, and false if the sub-expression is true.
--
-- >>> eval $ not_ (equal (name "Foo") (name "Bar"))
-- NOT Foo = Bar
--
not_ :: IsCondition a => a p v -> ConditionExpression p v
not_ = NotE . liftC
{-# INLINE not_ #-}
