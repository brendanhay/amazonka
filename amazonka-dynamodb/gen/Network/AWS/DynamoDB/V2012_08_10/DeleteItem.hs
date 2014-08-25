{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.DeleteItem
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a single item in a table by primary key. You can perform a
-- conditional delete operation that deletes the item if it exists, or if it
-- has an expected attribute value. In addition to deleting an item, you can
-- also return the item's attribute values in the same operation, using the
-- ReturnValues parameter. Unless you specify conditions, the DeleteItem is an
-- idempotent operation; running it multiple times on the same item or
-- attribute does not result in an error response. Conditional deletes are
-- useful for only deleting items if specific conditions are met. If those
-- conditions are met, DynamoDB performs the delete. Otherwise, the item is
-- not deleted. Delete an Item This example deletes an item from the Thread
-- table, but only if that item does not have an attribute named Replies.
-- Because ReturnValues is set to ALL_OLD, the response contains the item as
-- it appeared before the delete. { "Attributes": { "LastPostedBy": { "S":
-- "fred@example.com" }, "ForumName": { "S": "Amazon DynamoDB" },
-- "LastPostDateTime": { "S": "201303201023" }, "Tags": { "SS":
-- ["Update","Multiple Items","HelpMe"] }, "Subject": { "S": "How do I update
-- multiple items?" }, "Message": { "S": "I want to update multiple items in a
-- single API call. What's the best way to do that?" } } }.
module Network.AWS.DynamoDB.V2012_08_10.DeleteItem where

import           Network.AWS.DynamoDB.V2012_08_10.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DeleteItem' request.
deleteItem :: Map Text AttributeValue -- ^ '_diiKey'
           -> Text -- ^ '_diiTableName'
           -> DeleteItem
deleteItem p1 p2 = DeleteItem
    { _diiKey = p1
    , _diiTableName = p2
    , _diiConditionalOperator = Nothing
    , _diiExpected = mempty
    , _diiReturnConsumedCapacity = Nothing
    , _diiReturnItemCollectionMetrics = Nothing
    , _diiReturnValues = Nothing
    }

data DeleteItem = DeleteItem
    { _diiKey :: Map Text AttributeValue
      -- ^ A map of attribute names to AttributeValue objects, representing
      -- the primary key of the item to delete.
    , _diiTableName :: Text
      -- ^ The name of the table from which to delete the item.
    , _diiConditionalOperator :: Maybe ConditionalOperator
    , _diiExpected :: Map Text ExpectedAttributeValue
      -- ^ A map of attribute/condition pairs. This is the conditional block
      -- for the DeleteItem operation. All the conditions must be met for
      -- the operation to succeed. Expected allows you to provide an
      -- attribute name, and whether or not DynamoDB should check to see
      -- if the attribute value already exists; or if the attribute value
      -- exists and has a particular value before changing it. Each item
      -- in Expected represents an attribute name for DynamoDB to check,
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
    , _diiReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
      -- ^ If set to TOTAL, the response includes ConsumedCapacity data for
      -- tables and indexes. If set to INDEXES, the repsonse includes
      -- ConsumedCapacity for indexes. If set to NONE (the default),
      -- ConsumedCapacity is not included in the response.
    , _diiReturnItemCollectionMetrics :: Maybe ReturnItemCollectionMetrics
      -- ^ If set to SIZE, statistics about item collections, if any, that
      -- were modified during the operation are returned in the response.
      -- If set to NONE (the default), no statistics are returned.
    , _diiReturnValues :: Maybe ReturnValue
      -- ^ Use ReturnValues if you want to get the item attributes as they
      -- appeared before they were deleted. For DeleteItem, the valid
      -- values are: NONE - If ReturnValues is not specified, or if its
      -- value is NONE, then nothing is returned. (This is the default for
      -- ReturnValues.) ALL_OLD - The content of the old item is returned.
    } deriving (Show, Generic)

makeLenses ''DeleteItem

instance ToPath DeleteItem

instance ToQuery DeleteItem

instance ToHeaders DeleteItem

instance ToJSON DeleteItem

data DeleteItemResponse = DeleteItemResponse
    { _dioAttributes :: Map Text AttributeValue
      -- ^ A map of attribute names to AttributeValue objects, representing
      -- the item as it appeared before the DeleteItem operation. This map
      -- appears in the response only if ReturnValues was specified as
      -- ALL_OLD in the request.
    , _dioConsumedCapacity :: Maybe ConsumedCapacity
      -- ^ Represents the capacity units consumed by an operation. The data
      -- returned includes the total provisioned throughput consumed,
      -- along with statistics for the table and any indexes involved in
      -- the operation. ConsumedCapacity is only returned if it was asked
      -- for in the request. For more information, see Provisioned
      -- Throughput in the Amazon DynamoDB Developer Guide.
    , _dioItemCollectionMetrics :: Maybe ItemCollectionMetrics
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

makeLenses ''DeleteItemResponse

instance FromJSON DeleteItemResponse

instance AWSRequest DeleteItem where
    type Sv DeleteItem = DynamoDB
    type Rs DeleteItem = DeleteItemResponse

    request = get
    response _ = jsonResponse
