{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Amazonka.DynamoDB.Expression.Update
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Update Expressions.
--
-- /See:/
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Modifying.html Modifying Items and Attributes with Update Expressions>
-- in the AWS documentation, and "Amazonka.DynamoDB.Expression" to get started.
module Amazonka.DynamoDB.Expression.Update
    (
    -- * Update Expressions
      UpdateExpression

    -- * Set, Remove, Add, and Delete Actions
    , set
    , remove
    , add
    , delete

    , increment
    , decrement
    , prepend
    , append

    -- * Sub-expressions
    , Update

    , plus
    , minus
    , (#+)
    , (#-)
    , ifNotExists
    , listAppend
    ) where

import Amazonka.DynamoDB.Expression.Internal
import Amazonka.DynamoDB.Item.Value

import Data.Monoid (mempty)

import qualified Data.Sequence as Seq

-- $setup
-- >>> import Data.Bifunctor (first)
-- >>> import Data.Text.Lazy.Builder (toLazyText)
-- >>> import qualified Amazonka.DynamoDB.Expression.Compile as Compile
-- >>> import qualified Data.Text.Lazy.IO as Text
-- >>> import qualified Data.Text.Lazy.Builder as Build
-- >>> :{
-- let eval f = Text.putStrLn . Build.toLazyText . fst . Compile.compileValues f . first Compile.name
--     uexpr  = eval Compile.updateExpression
-- :}

{-| Use the SET action in an update expression to add one or more attributes
and values to an item. If any of these attribute already exist, they are
replaced by the new values. However, note that you can also use SET to add or
subtract from an attribute that is of type Number.
-}
set :: IsUpdate a => Path Name -> a -> UpdateExpression Name Value
set p u = UnsafeUpdateExpression
    { _unsafeSet    = Seq.singleton (p, liftU u)
    , _unsafeRemove = mempty
    , _unsafeAdd    = mempty
    , _unsafeDelete = mempty
    }

-- | SET Price = Price + :p
increment :: DynamoNumber a => Path Name -> a -> UpdateExpression Name Value
increment p = set p . plus p . toValue

-- | SET Price = Price - :p
decrement :: DynamoNumber a => Path Name -> a -> UpdateExpression Name Value
decrement p = set p . minus p . toValue

-- | SET #pr.FiveStar = list_append(#pr.FiveStar, :r)
prepend :: DynamoList a => Path Name -> a -> UpdateExpression Name Value
prepend p = set p . listAppend p . toValue

-- | SET #pr.FiveStar = list_append(:r, #pr.FiveStar)
append :: DynamoList a => Path Name -> a -> UpdateExpression Name Value
append p = set p . listAppend p . toValue

plus :: IsUpdate a => Path Name -> a -> Update Name Value
plus p = Plus p . liftU

minus :: IsUpdate a => Path Name -> a -> Update Name Value
minus p = Minus p . liftU

(#+) :: DynamoNumber a => Path Name -> a -> Update Name Value
(#+) p = plus p . toValue

(#-) :: DynamoNumber a => Path Name -> a -> Update Name Value
(#-) p = minus p . toValue

ifNotExists :: IsUpdate a => Path Name -> a -> Update Name Value
ifNotExists p = IfNotExists p . liftU

listAppend :: (IsUpdate a, IsUpdate b) => a -> b -> Update Name Value
listAppend a b = ListAppend (liftU a) (liftU b)

{-| Use the REMOVE action in an update expression to remove one or more
attributes from an item.
-}
remove :: Path p -> UpdateExpression p Value
remove p = UnsafeUpdateExpression
    { _unsafeSet    = mempty
    , _unsafeRemove = Seq.singleton p
    , _unsafeAdd    = mempty
    , _unsafeDelete = mempty
    }

{-|

/Important:/ The ADD action only supports Number and set data types. In
general, we recommend using SET rather than ADD.  Use the ADD action in an
update expression to do either of the following:

* If the attribute does not already exist, add the new attribute and its value(s) to the item.
* If the attribute already exists, then the behavior of ADD depends on the attribute's data type:
    * If the attribute is a number, and the value you are adding is also a number, then the value is mathematically added to the existing attribute. (If the value is a negative number, then it is subtracted from the existing attribute.)
    * If the attribute is a set, and the value you are adding is also a set, then the value is appended to the existing set.
-}
add :: DynamoNumberOrSet a => Path p -> a -> UpdateExpression p Value
add p v = UnsafeUpdateExpression
    { _unsafeSet    = mempty
    , _unsafeRemove = mempty
    , _unsafeAdd    = Seq.singleton (p, toValue v)
    , _unsafeDelete = mempty
    }

{-| Use the DELETE action in an update expression to delete an element from a set.

/Important:/ The DELETE action only supports set data types.

The path element is the document path to an attribute. The attribute must be a
set data type.

The value element is the element(s) in the set that you want to delete.

-}
delete :: DynamoSet a => Path p -> a -> UpdateExpression p Value
delete p v = UnsafeUpdateExpression
    { _unsafeSet    = mempty
    , _unsafeRemove = mempty
    , _unsafeAdd    = mempty
    , _unsafeDelete = Seq.singleton (p, toValue v)
    }
