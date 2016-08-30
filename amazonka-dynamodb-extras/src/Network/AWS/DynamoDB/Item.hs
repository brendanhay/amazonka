{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.DynamoDB.Itema
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Item where
    -- (

    -- -- * Items
    --   ItemError (..)
    -- , Item      (..)

    -- -- * Attributes
    -- , Attribute (..)
    -- ) where

import Control.Exception (Exception)
import Control.Monad     ((>=>))


import Data.Bifunctor      (bimap, first)
import Data.Coerce         (coerce)
import Data.HashMap.Strict (HashMap)
import Data.Map            (Map)
import Data.Text           (Text)
import Data.Typeable       (Typeable)

import Network.AWS.DynamoDB hiding (ScalarAttributeType (..))

import Network.AWS.DynamoDB.Value
import Network.AWS.DynamoDB.Value.Unsafe

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict     as Map

item :: [(Text, Value)] -> HashMap Text Value
item = HashMap.fromList
{-# INLINE item #-}

attr :: DynamoValue a => Text -> a -> (Text, Value)
attr k v = (k, toValue v)
{-# INLINE attr #-}

parse :: DynamoValue a => Text -> HashMap Text Value -> Either ItemError a
parse k m =
    case HashMap.lookup k m of
        Nothing -> Left (MissingAttribute k)
        Just v  -> first (ValueError k) (fromValue v)
{-# INLINE parse #-}

parseMaybe :: DynamoValue a
           => Text
           -> HashMap Text Value
           -> Either ItemError (Maybe a)
parseMaybe k m =
    case HashMap.lookup k m of
        Nothing -> Right Nothing
        Just v  -> bimap (ValueError k) Just (fromValue v)
{-# INLINE parseMaybe #-}

encode :: DynamoItem a => a -> HashMap Text AttributeValue
encode = coerce . toItem
{-# INLINE encode #-}

decode :: DynamoItem a => HashMap Text AttributeValue -> Either ItemError a
decode = HashMap.traverseWithKey go >=> fromItem
  where
    go k = first (ValueError k) . newValue
{-# INLINE decode #-}

-- | You can use this if you know the AttributeValue's are safe, as in they
-- have been returned unmodified from DynamoDB.
unsafeDecode :: DynamoItem a => HashMap Text AttributeValue -> Either ItemError a
unsafeDecode = fromItem . coerce

data ItemError
    = ValueError       Text ValueError
    | MissingAttribute Text
      deriving (Eq, Show, Typeable)

instance Exception ItemError

-- | Serialise a value to a complex DynamoDB item.
--
-- Note about complex types
--
-- The maximum item size in DynamoDB is 400 KB, which includes both attribute name
-- binary length (UTF-8 length) and attribute value lengths (again binary
-- length). The attribute name counts towards the size limit.
--
-- For example, consider an item with two attributes: one attribute named
-- "shirt-color" with value "R" and another attribute named "shirt-size" with
-- value "M". The total size of that item is 23 bytes.
--
-- An 'Item' is subject to the following law:
--
-- @
-- fromItem (toItem x) ≡ Right x
-- @
--
-- That is, you get back what you put in.
class DynamoItem a where
    toItem   :: a -> HashMap Text Value
    fromItem :: HashMap Text Value -> Either ItemError a

instance DynamoValue a => DynamoItem (Map Text a) where
    toItem   = toItem . HashMap.fromList . Map.toList
    fromItem = fmap (Map.fromList . HashMap.toList) . fromItem

instance DynamoValue a => DynamoItem (HashMap Text a) where
    toItem   = HashMap.map toValue
    fromItem = HashMap.traverseWithKey (\k -> first (ValueError k) . fromValue)
