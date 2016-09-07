-- |
-- Module      : Amazonka.DynamoDB.Expression.Projection
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Projection Expressions.
--
-- /See:/
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes with Projection Expressions>
-- in the AWS documentation, and "Amazonka.DynamoDB.Expression" to get started.
module Amazonka.DynamoDB.Expression.Projection
    ( ProjectionExpression
    , project
    ) where

import Amazonka.DynamoDB.Expression.Compile  ()
import Amazonka.DynamoDB.Expression.Internal

import Data.Foldable (toList)

import qualified Data.Sequence as Seq

-- $setup
-- >>> import Amazonka.DynamoDB.Expression.Compile
-- >>> import Data.Semigroup ((<>))
-- >>> import Data.Text.Lazy.Builder (toLazyText)
-- >>> import qualified Data.Text.Lazy.IO as Text
-- >>> let eval = Text.putStrLn . toLazyText . projectionExpression . fmap attribute

-- | Project a document path.
--
-- The 'Semigroup' or 'Monoid' instances can be used to project mutliple paths.
--
-- >>> eval $ project (name "Title") <> project (index "RelatedItems" 2) <> project (name "Product" <> index "Reviews" 0)
-- Title, RelatedItems[2], Product.Reviews[0]
--
project :: Path p -> ProjectionExpression p
project = ProjectionExpression . Seq.singleton
{-# INLINE project #-}
