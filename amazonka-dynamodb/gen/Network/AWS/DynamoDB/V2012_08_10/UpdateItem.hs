{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.UpdateItem
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Edits an existing item's attributes, or inserts a new item if it does not
-- already exist. You can put, delete, or add attribute values. You can also
-- perform a conditional update (insert a new attribute name-value pair if it
-- doesn't exist, or replace an existing name-value pair if it has certain
-- expected attribute values). In addition to updating an item, you can also
-- return the item's attribute values in the same operation, using the
-- ReturnValues parameter. Conditional Update This example updates the Thread
-- table, changing the LastPostedBy attribute-but only if LastPostedBy is
-- currently "fred@example.com". All of the item's attributes, as they appear
-- after the update, are returned in the response. { }.
module Network.AWS.DynamoDB.V2012_08_10.UpdateItem
    (
    -- * Request
      UpdateItem
    -- ** Request constructor
    , mkUpdateItem
    -- ** Request lenses
    , uiTableName
    , uiKey
    , uiAttributeUpdates
    , uiExpected
    , uiConditionalOperator
    , uiReturnValues
    , uiReturnConsumedCapacity
    , uiReturnItemCollectionMetrics

    -- * Response
    , UpdateItemResponse
    -- ** Response lenses
    , uirsAttributes
    , uirsConsumedCapacity
    , uirsItemCollectionMetrics
    ) where

import           Network.AWS.DynamoDB.V2012_08_10.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Represents the input of an UpdateItem operation.
data UpdateItem = UpdateItem
    { _uiTableName :: Text
    , _uiKey :: Map Text AttributeValue
    , _uiAttributeUpdates :: Map Text AttributeValueUpdate
    , _uiExpected :: Map Text ExpectedAttributeValue
    , _uiConditionalOperator :: Maybe ConditionalOperator
    , _uiReturnValues :: Maybe ReturnValue
    , _uiReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
    , _uiReturnItemCollectionMetrics :: Maybe ReturnItemCollectionMetrics
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateItem' request.
mkUpdateItem :: Text -- ^ 'uiTableName'
             -> Map Text AttributeValue -- ^ 'uiKey'
             -> UpdateItem
mkUpdateItem p1 p2 = UpdateItem
    { _uiTableName = p1
    , _uiKey = p2
    , _uiAttributeUpdates = mempty
    , _uiExpected = mempty
    , _uiConditionalOperator = Nothing
    , _uiReturnValues = Nothing
    , _uiReturnConsumedCapacity = Nothing
    , _uiReturnItemCollectionMetrics = Nothing
    }
{-# INLINE mkUpdateItem #-}

-- | The name of the table containing the item to update.
uiTableName :: Lens' UpdateItem Text
uiTableName = lens _uiTableName (\s a -> s { _uiTableName = a })
{-# INLINE uiTableName #-}

-- | The primary key that defines the item. Each element consists of an
-- attribute name and a value for that attribute.
uiKey :: Lens' UpdateItem (Map Text AttributeValue)
uiKey = lens _uiKey (\s a -> s { _uiKey = a })
{-# INLINE uiKey #-}

-- | The names of attributes to be modified, the action to perform on each, and
-- the new value for each. If you are updating an attribute that is an index
-- key attribute for any indexes on that table, the attribute type must match
-- the index key type defined in the AttributesDefinition of the table
-- description. You can use UpdateItem to update any non-key attributes.
-- Attribute values cannot be null. String and binary type attributes must
-- have lengths greater than zero. Set type attributes must not be empty.
-- Requests with empty values will be rejected with a ValidationException.
-- Each AttributeUpdates element consists of an attribute name to modify,
-- along with the following: Value - The new value, if applicable, for this
-- attribute. Action - Specifies how to perform the update. Valid values for
-- Action are PUT, DELETE, and ADD. The behavior depends on whether the
-- specified primary key already exists in the table. If an item with the
-- specified Key is found in the table: PUT - Adds the specified attribute to
-- the item. If the attribute already exists, it is replaced by the new value.
-- DELETE - If no value is specified, the attribute and its value are removed
-- from the item. The data type of the specified value must match the existing
-- value's data type. If a set of values is specified, then those values are
-- subtracted from the old set. For example, if the attribute value was the
-- set [a,b,c] and the DELETE action specified [a,c], then the final attribute
-- value would be [b]. Specifying an empty set is an error. ADD - If the
-- attribute does not already exist, then the attribute and its values are
-- added to the item. If the attribute does exist, then the behavior of ADD
-- depends on the data type of the attribute: If the existing attribute is a
-- number, and if Value is also a number, then the Value is mathematically
-- added to the existing attribute. If Value is a negative number, then it is
-- subtracted from the existing attribute. If you use ADD to increment or
-- decrement a number value for an item that doesn't exist before the update,
-- DynamoDB uses 0 as the initial value. In addition, if you use ADD to update
-- an existing item, and intend to increment or decrement an attribute value
-- which does not yet exist, DynamoDB uses 0 as the initial value. For
-- example, suppose that the item you want to update does not yet have an
-- attribute named itemcount, but you decide to ADD the number 3 to this
-- attribute anyway, even though it currently does not exist. DynamoDB will
-- create the itemcount attribute, set its initial value to 0, and finally add
-- 3 to it. The result will be a new itemcount attribute in the item, with a
-- value of 3. If the existing data type is a set, and if the Value is also a
-- set, then the Value is added to the existing set. (This is a set operation,
-- not mathematical addition.) For example, if the attribute value was the set
-- [1,2], and the ADD action specified [3], then the final attribute value
-- would be [1,2,3]. An error occurs if an Add action is specified for a set
-- attribute and the attribute type specified does not match the existing set
-- type. Both sets must have the same primitive data type. For example, if the
-- existing data type is a set of strings, the Value must also be a set of
-- strings. The same holds true for number sets and binary sets. This action
-- is only valid for an existing attribute whose data type is number or is a
-- set. Do not use ADD for any other data types. If no item with the specified
-- Key is found: PUT - DynamoDB creates a new item with the specified primary
-- key, and then adds the attribute. DELETE - Nothing happens; there is no
-- attribute to delete. ADD - DynamoDB creates an item with the supplied
-- primary key and number (or set of numbers) for the attribute value. The
-- only data types allowed are number and number set; no other data types can
-- be specified. If you specify any attributes that are part of an index key,
-- then the data types for those attributes must match those of the schema in
-- the table's attribute definition.
uiAttributeUpdates :: Lens' UpdateItem (Map Text AttributeValueUpdate)
uiAttributeUpdates =
    lens _uiAttributeUpdates (\s a -> s { _uiAttributeUpdates = a })
{-# INLINE uiAttributeUpdates #-}

-- | A map of attribute/condition pairs. This is the conditional block for the
-- UpdateItem operation. All the conditions must be met for the operation to
-- succeed. Expected allows you to provide an attribute name, and whether or
-- not DynamoDB should check to see if the attribute value already exists; or
-- if the attribute value exists and has a particular value before changing
-- it. Each item in Expected represents an attribute name for DynamoDB to
-- check, along with the following: Value - A value for DynamoDB to compare
-- with an attribute. When performing the comparison, strongly consistent
-- reads are used. Exists - Causes DynamoDB to evaluate the value before
-- attempting a conditional operation: If Exists is true, DynamoDB will check
-- to see if that attribute value already exists in the table. If it is found,
-- then the operation succeeds. If it is not found, the operation fails with a
-- ConditionalCheckFailedException. If Exists is false, DynamoDB assumes that
-- the attribute value does not exist in the table. If in fact the value does
-- not exist, then the assumption is valid and the operation succeeds. If the
-- value is found, despite the assumption that it does not exist, the
-- operation fails with a ConditionalCheckFailedException. The default setting
-- for Exists is true. If you supply a Value all by itself, DynamoDB assumes
-- the attribute exists: You don't have to set Exists to true, because it is
-- implied. DynamoDB returns a ValidationException if: Exists is true but
-- there is no Value to check. (You expect a value to exist, but don't specify
-- what that value is.) Exists is false but you also specify a Value. (You
-- cannot expect an attribute to have a value, while also expecting it not to
-- exist.) If you specify more than one condition for Exists, then all of the
-- conditions must evaluate to true. (In other words, the conditions are ANDed
-- together.) Otherwise, the conditional operation will fail.
uiExpected :: Lens' UpdateItem (Map Text ExpectedAttributeValue)
uiExpected = lens _uiExpected (\s a -> s { _uiExpected = a })
{-# INLINE uiExpected #-}

uiConditionalOperator :: Lens' UpdateItem (Maybe ConditionalOperator)
uiConditionalOperator =
    lens _uiConditionalOperator (\s a -> s { _uiConditionalOperator = a })
{-# INLINE uiConditionalOperator #-}

-- | Use ReturnValues if you want to get the item attributes as they appeared
-- either before or after they were updated. For UpdateItem, the valid values
-- are: NONE - If ReturnValues is not specified, or if its value is NONE, then
-- nothing is returned. (This is the default for ReturnValues.) ALL_OLD - If
-- UpdateItem overwrote an attribute name-value pair, then the content of the
-- old item is returned. UPDATED_OLD - The old versions of only the updated
-- attributes are returned. ALL_NEW - All of the attributes of the new version
-- of the item are returned. UPDATED_NEW - The new versions of only the
-- updated attributes are returned.
uiReturnValues :: Lens' UpdateItem (Maybe ReturnValue)
uiReturnValues = lens _uiReturnValues (\s a -> s { _uiReturnValues = a })
{-# INLINE uiReturnValues #-}

-- | If set to TOTAL, the response includes ConsumedCapacity data for tables and
-- indexes. If set to INDEXES, the repsonse includes ConsumedCapacity for
-- indexes. If set to NONE (the default), ConsumedCapacity is not included in
-- the response.
uiReturnConsumedCapacity :: Lens' UpdateItem (Maybe ReturnConsumedCapacity)
uiReturnConsumedCapacity =
    lens _uiReturnConsumedCapacity
         (\s a -> s { _uiReturnConsumedCapacity = a })
{-# INLINE uiReturnConsumedCapacity #-}

-- | If set to SIZE, statistics about item collections, if any, that were
-- modified during the operation are returned in the response. If set to NONE
-- (the default), no statistics are returned.
uiReturnItemCollectionMetrics :: Lens' UpdateItem (Maybe ReturnItemCollectionMetrics)
uiReturnItemCollectionMetrics =
    lens _uiReturnItemCollectionMetrics
         (\s a -> s { _uiReturnItemCollectionMetrics = a })
{-# INLINE uiReturnItemCollectionMetrics #-}

instance ToPath UpdateItem

instance ToQuery UpdateItem

instance ToHeaders UpdateItem

instance ToJSON UpdateItem

-- | Represents the output of an UpdateItem operation.
data UpdateItemResponse = UpdateItemResponse
    { _uirsAttributes :: Map Text AttributeValue
    , _uirsConsumedCapacity :: Maybe ConsumedCapacity
    , _uirsItemCollectionMetrics :: Maybe ItemCollectionMetrics
    } deriving (Show, Generic)

-- | A map of attribute values as they appeared before the UpdateItem operation,
-- but only if ReturnValues was specified as something other than NONE in the
-- request. Each element represents one attribute.
uirsAttributes :: Lens' UpdateItemResponse (Map Text AttributeValue)
uirsAttributes = lens _uirsAttributes (\s a -> s { _uirsAttributes = a })
{-# INLINE uirsAttributes #-}

-- | Represents the capacity units consumed by an operation. The data returned
-- includes the total provisioned throughput consumed, along with statistics
-- for the table and any indexes involved in the operation. ConsumedCapacity
-- is only returned if it was asked for in the request. For more information,
-- see Provisioned Throughput in the Amazon DynamoDB Developer Guide.
uirsConsumedCapacity :: Lens' UpdateItemResponse (Maybe ConsumedCapacity)
uirsConsumedCapacity =
    lens _uirsConsumedCapacity (\s a -> s { _uirsConsumedCapacity = a })
{-# INLINE uirsConsumedCapacity #-}

-- | Information about item collections, if any, that were affected by the
-- operation. ItemCollectionMetrics is only returned if it was asked for in
-- the request. If the table does not have any local secondary indexes, this
-- information is not returned in the response.
uirsItemCollectionMetrics :: Lens' UpdateItemResponse (Maybe ItemCollectionMetrics)
uirsItemCollectionMetrics =
    lens _uirsItemCollectionMetrics
         (\s a -> s { _uirsItemCollectionMetrics = a })
{-# INLINE uirsItemCollectionMetrics #-}

instance FromJSON UpdateItemResponse

instance AWSRequest UpdateItem where
    type Sv UpdateItem = DynamoDB
    type Rs UpdateItem = UpdateItemResponse

    request = get
    response _ = jsonResponse
