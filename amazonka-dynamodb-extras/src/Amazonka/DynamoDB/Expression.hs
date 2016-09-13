{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Amazonka.DynamoDB.Expression
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- A shallow DSL embedding of DynamoDB's expression language that can be used
-- to specify projection, key condition, and filter expressions parameters for
-- 'Scan' and 'Query' operations.
module Amazonka.DynamoDB.Expression
    (
    -- * Usage
    -- $usage

    -- * Projection Expressions
    -- $projection_expressions

      ProjectionExpression
    , project

    -- * Key Condition Expressions
    -- $key_expressions

    , KeyConditionExpression
    , partition
    , partitionFilter

    -- * Filter and Condition Expressions
    -- $condition_expressions

    , ConditionExpression

    -- ** Logical Evaluations
    , and_
    , or_
    , not_

    -- ** Sub-conditions
    , Hash
    , Range
    , Term
    , Condition

    -- *** Making Comparisons
    -- $condition_comparators

    , equal
    , notEqual
    , lessThan
    , lessThanOrEqual
    , greaterThan
    , greaterThanOrEqual

    -- **** Path Comparators
    -- $condition_path_comparators

    , (#=)
    , (#<>)
    , (#<)
    , (#<=)
    , (#>)
    , (#>=)

    -- **** Value Comparators
    -- $condition_value_comparators

    , (=:)
    , (<>:)
    , (<:)
    , (<=:)
    , (>:)
    , (>=:)

    -- *** Functions
    -- $condition_functions

    , exists
    , notExists
    , isType
    , size
    , contains
    , beginsWith

    -- *** Ranges
    -- $condition_ranges

    , between
    , in_

    -- *** Precedence
    -- $condition_precedence

    -- * Update Expressions
    -- $update_expressions

    , UpdateExpression

    -- ** Set, Remove, Add, and Delete Actions
    , set
    , remove
    , add
    , delete

    , increment
    , decrement
    , prepend
    , append

    -- ** Sub-expressions
    , Update

    , plus
    , minus
    , (#+)
    , (#-)
    , ifNotExists
    , listAppend

    -- * Operands
    -- $operands

    , Operand      (..)
    , IsOperand    (..)

    -- * Document Paths
    -- $document_paths
    , Path         (..)

    -- * Compilation
    , compileNames
    , compileValues
    , compile

    -- ** Placeholders
    , finalizeNames
    , finalizeValues

    -- ** Grammars
    , projectionExpression
    , keyConditionExpression
    , conditionExpression
    , updateExpression

    -- * Values
    , Value
    , toValue
    ) where

import Amazonka.DynamoDB.Expression.Compile
import Amazonka.DynamoDB.Expression.Condition
import Amazonka.DynamoDB.Expression.Internal
import Amazonka.DynamoDB.Expression.Placeholder
import Amazonka.DynamoDB.Expression.Projection
import Amazonka.DynamoDB.Expression.Update
import Amazonka.DynamoDB.Item.Value             (Value, toValue)

-- FIXME:
-- Update Expressions vs Condition Expressions vs Projection Expressions
-- Note about how by default attribute names are substituted for placeholders,
-- as well as values. Just for the doctests the names are shown.

{- $usage
TODO
-}

{- $projection_expressions
Something about projection expressions.
-}

{- $key_expressions
Something about key expressions.
-}

{- $update_expressions
Something about key expressions.
-}

{- $condition_expressions
An expression represents restrictions to put in place when you read
and write items in a table. DynamoDB expressions are free-form strings that
can contain attribute names, document paths, logical operators, and
functions. For a complete list of elements allowed in an expression, see
<http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html#ConditionExpressionReference Condition Expression Reference>.
-}

{- $condition_comparators
Use these comparators to compare an operand against a range of values, or an
enumerated list of values.
-}

{- $condition_name_comparators
The @#@ prefix is a mnenomic for attribute name substitution. The infix
operators prefixed in this way are synonyms for their function variants.

All operator names correspond directly to the DynamoDB comparator
rather than an equivalent Haskell operator.

For example, instead of writing:

>>> eval $ equals (name "Foo") (name "Bar")
"Foo = Bar"

You can use the corresponding @#@ prefixed binary operator:

>>> eval $ name "Foo" #= name "Bar"
"Foo = Bar"
-}

{- $condition_value_comparators
The @:@ prefix is a mnenomic for attribute value substitution. The infix
operators prefixed in this way take a 'DynamoValue' on the right-hand side,
omitting the need to wrap the RHS in 'toValue'.

For example, instead of writing:

>>> eval $ name "AttributeName" #= toValue 123
"AttributeName = :v1"

You can use the corresponding @:@ prefixed binary operator:

>>> eval $ name "AttributeName" =: 123
"AttributeName = :v1"
-}

{- $condition_functions
Use the following functions to determine whether an attribute exists within an
item, or to evaluate the value of an attribute.
-}

{- $condition_ranges
Use the 'between' and 'in_' to compare an operand against a range of values,
or an enumerated list of values.
-}

{- $condition_precedence

The infix function precedence follows the order:

> #=, #<>, #<, #<=, #>, #>=
> =:, <>:, <:, <=:, >:, >=:
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

{- $document_paths
-- To do this, you must construct a path to the element's location,
-- or document path, within the item. The document path tells DynamoDB where to
-- find the attribute, even if it is deeply nested within multiple lists and
-- maps.
--
-- For a top-level attribute, the document path is simply the attribute name.
--
-- For a nested attribute, you construct the document path using dereference
-- operators which are handled by the 'Semigroup' instance for attributes,
-- and 'index' for list elements.
--
-- Accessing a Top-level Attribute:
--
-- >>> eval $ key "AttributeName"
-- AttributeName
--
-- Accessing Map Elements:
--
-- >>> eval $ key "AttributeName" <> key "Foo" <> key "Bar"
-- AttributeName.Foo.Bar
--
-- It's important to observe the property that:
--
-- > key "AttributeName" <> key "Foo" <> key "Bar" /= key "AttributeName.Foo.Bar"
--
-- This is because the @\'.\'@ dereference operator is a valid path character,
-- and the expression compiler will substitute each individual component of a
-- path with a placeholder.
--
-- So with subsitution, the above example(s) would become:
--
-- @
-- >>> key "AttributeName" <> key "Foo" <> key "Bar"
-- #n1.#n2.#n3
-- @
--
-- @
-- >>> key "AttributeName.Foo.Bar"
-- #n1
-- @
--
-- Accessing List Elements:
--
-- >>> eval $ key "MyList"      `index` 0
-- MyList[0]
--
-- >>> eval $ key "AnotherList" `index` 12
-- AnotherList[12]
--
-- >>> eval $ key "ThisList"    `index` 5  `index` 11
-- ThisList[5][11]
--
-- List dereferencing starts from zero.
--
-- /Warning:/ The maximum depth for a document path is 32. Therefore, the
-- number of dereferences in any path cannot exceed this limit. This limit
-- is not enforced by the expression language.
--
-- /Note:/ Usually in a document path, the first character of each attribute
-- must be @[a-zA-Z]@ and the second character (if present) also must be
-- [a-zA-Z0-9].  But since the expression compiler actually substitutes all
-- attribute names by default, this requirement doesn't need to be met.
-}
