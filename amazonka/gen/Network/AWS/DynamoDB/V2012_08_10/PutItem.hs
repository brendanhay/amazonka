{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.PutItem
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new item, or replaces an old item with a new item. If an item
-- already exists in the specified table with the same primary key, the new
-- item completely replaces the existing item. You can perform a conditional
-- put (insert a new item if one with the specified primary key doesn't
-- exist), or replace an existing item if it has certain attribute values. In
-- addition to putting an item, you can also return the item's attribute
-- values in the same operation, using the ReturnValues parameter. When you
-- add an item, the primary key attribute(s) are the only required attributes.
-- Attribute values cannot be null. String and binary type attributes must
-- have lengths greater than zero. Set type attributes cannot be empty.
-- Requests with empty values will be rejected with a ValidationException. You
-- can request that PutItem return either a copy of the old item (before the
-- update) or a copy of the new item (after the update). For more information,
-- see the ReturnValues description. To prevent a new item from replacing an
-- existing item, use a conditional put operation with Exists set to false for
-- the primary key attribute, or attributes. For more information about using
-- this API, see Working with Items in the Amazon DynamoDB Developer Guide.
-- Put an Item This example puts a new item into the Thread table. To prevent
-- this new item from overwriting an existing item, "Exists" is set to false
-- for the primary key attributes. { }.
module Network.AWS.DynamoDB.V2012_08_10.PutItem where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.V2012_08_10.Types
import Network.AWS.Prelude
import qualified Data.HashMap.Strict as Map

-- | Minimum specification for a 'PutItem' request.
putItem :: HashMap Text AttributeValue -- ^ '_piiItem'
        -> Text -- ^ '_piiTableName'
        -> PutItem
putItem p1 p2 = PutItem
    { _piiItem = p1
    , _piiTableName = p2
    , _piiConditionalOperator = Nothing
    , _piiExpected = mempty
    , _piiReturnConsumedCapacity = Nothing
    , _piiReturnItemCollectionMetrics = Nothing
    , _piiReturnValues = Nothing
    }

data PutItem = PutItem
    { _piiItem :: HashMap Text AttributeValue
      -- ^ A map of attribute name/value pairs, one for each attribute. Only
      -- the primary key attributes are required; you can optionally
      -- provide other attribute name-value pairs for the item. If you
      -- specify any attributes that are part of an index key, then the
      -- data types for those attributes must match those of the schema in
      -- the table's attribute definition. For more information about
      -- primary keys, see Primary Key in the Amazon DynamoDB Developer
      -- Guide. Each element in the Item map is an AttributeValue object.
    , _piiTableName :: Text
      -- ^ The name of the table to contain the item.
    , _piiConditionalOperator :: Maybe ConditionalOperator
    , _piiExpected :: HashMap Text ExpectedAttributeValue
      -- ^ A map of attribute/condition pairs. This is the conditional block
      -- for the PutItem operation. All the conditions must be met for the
      -- operation to succeed. Expected allows you to provide an attribute
      -- name, and whether or not DynamoDB should check to see if the
      -- attribute value already exists; or if the attribute value exists
      -- and has a particular value before changing it. Each item in
      -- Expected represents an attribute name for DynamoDB to check,
      -- along with the following: Value - A value for DynamoDB to compare
      -- with an attribute. When performing the comparison, strongly
      -- consistent reads are used. Exists - Causes DynamoDB to evaluate
      -- the value before attempting a conditional operation: If Exists is
      -- true, DynamoDB will check to see if that attribute value already
      -- exists in the table. If it is found, then the operation succeeds.
      -- If it is not found, the operation fails with a
      -- ConditionalCheckFailedException. If Exists is false, DynamoDB
      -- assumes that the attribute value does not exist in the table. If
      -- in fact the value does not exist, then the assumption is valid
      -- and the operation succeeds. If the value is found, despite the
      -- assumption that it does not exist, the operation fails with a
      -- ConditionalCheckFailedException. The default setting for Exists
      -- is true. If you supply a Value all by itself, DynamoDB assumes
      -- the attribute exists: You don't have to set Exists to true,
      -- because it is implied. DynamoDB returns a ValidationException if:
      -- Exists is true but there is no Value to check. (You expect a
      -- value to exist, but don't specify what that value is.) Exists is
      -- false but you also specify a Value. (You cannot expect an
      -- attribute to have a value, while also expecting it not to exist.)
      -- If you specify more than one condition for Exists, then all of
      -- the conditions must evaluate to true. (In other words, the
      -- conditions are ANDed together.) Otherwise, the conditional
      -- operation will fail.
    , _piiReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
      -- ^ If set to TOTAL, the response includes ConsumedCapacity data for
      -- tables and indexes. If set to INDEXES, the repsonse includes
      -- ConsumedCapacity for indexes. If set to NONE (the default),
      -- ConsumedCapacity is not included in the response.
    , _piiReturnItemCollectionMetrics :: Maybe ReturnItemCollectionMetrics
      -- ^ If set to SIZE, statistics about item collections, if any, that
      -- were modified during the operation are returned in the response.
      -- If set to NONE (the default), no statistics are returned.
    , _piiReturnValues :: Maybe ReturnValue
      -- ^ Use ReturnValues if you want to get the item attributes as they
      -- appeared before they were updated with the PutItem request. For
      -- PutItem, the valid values are: NONE - If ReturnValues is not
      -- specified, or if its value is NONE, then nothing is returned.
      -- (This is the default for ReturnValues.) ALL_OLD - If PutItem
      -- overwrote an attribute name-value pair, then the content of the
      -- old item is returned.
    } deriving (Show, Generic)

makeLenses ''PutItem

instance ToPath PutItem

instance ToQuery PutItem

instance ToHeaders PutItem

instance ToJSON PutItem

data PutItemResponse = PutItemResponse
    { _pioAttributes :: HashMap Text AttributeValue
      -- ^ The attribute values as they appeared before the PutItem
      -- operation, but only if ReturnValues is specified as ALL_OLD in
      -- the request. Each element consists of an attribute name and an
      -- attribute value.
    , _pioConsumedCapacity :: Maybe ConsumedCapacity
      -- ^ Represents the capacity units consumed by an operation. The data
      -- returned includes the total provisioned throughput consumed,
      -- along with statistics for the table and any indexes involved in
      -- the operation. ConsumedCapacity is only returned if it was asked
      -- for in the request. For more information, see Provisioned
      -- Throughput in the Amazon DynamoDB Developer Guide.
    , _pioItemCollectionMetrics :: Maybe ItemCollectionMetrics
      -- ^ Information about item collections, if any, that were affected by
      -- the operation. ItemCollectionMetrics is only returned if it was
      -- asked for in the request. If the table does not have any local
      -- secondary indexes, this information is not returned in the
      -- response. Each ItemCollectionMetrics element consists of:
      -- ItemCollectionKey - The hash key value of the item collection.
      -- This is the same as the hash key of the item. SizeEstimateRange -
      -- An estimate of item collection size, measured in gigabytes. This
      -- is a two-element array containing a lower bound and an upper
      -- bound for the estimate. The estimate includes the size of all the
      -- items in the table, plus the size of all attributes projected
      -- into all of the local secondary indexes on that table. Use this
      -- estimate to measure whether a local secondary index is
      -- approaching its size limit. The estimate is subject to change
      -- over time; therefore, do not rely on the precision or accuracy of
      -- the estimate.
    } deriving (Show, Generic)

makeLenses ''PutItemResponse

instance FromJSON PutItemResponse

instance AWSRequest PutItem where
    type Sv PutItem = DynamoDB
    type Rs PutItem = PutItemResponse

    request = get
    response _ = jsonResponse
