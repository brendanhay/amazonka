{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.BatchGetItem
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /BatchGetItem/ operation returns the attributes of one or more items
-- from one or more tables. You identify requested items by primary key.
--
-- A single operation can retrieve up to 16 MB of data, which can contain
-- as many as 100 items. /BatchGetItem/ will return a partial result if the
-- response size limit is exceeded, the table\'s provisioned throughput is
-- exceeded, or an internal processing failure occurs. If a partial result
-- is returned, the operation returns a value for /UnprocessedKeys/. You
-- can use this value to retry the operation starting with the next item to
-- get.
--
-- If you request more than 100 items /BatchGetItem/ will return a
-- /ValidationException/ with the message \"Too many items requested for
-- the BatchGetItem call\".
--
-- For example, if you ask to retrieve 100 items, but each individual item
-- is 300 KB in size, the system returns 52 items (so as not to exceed the
-- 16 MB limit). It also returns an appropriate /UnprocessedKeys/ value so
-- you can get the next page of results. If desired, your application can
-- include its own logic to assemble the pages of results into one data
-- set.
--
-- If /none/ of the items can be processed due to insufficient provisioned
-- throughput on all of the tables in the request, then /BatchGetItem/ will
-- return a /ProvisionedThroughputExceededException/. If /at least one/ of
-- the items is successfully processed, then /BatchGetItem/ completes
-- successfully, while returning the keys of the unread items in
-- /UnprocessedKeys/.
--
-- If DynamoDB returns any unprocessed items, you should retry the batch
-- operation on those items. However, /we strongly recommend that you use
-- an exponential backoff algorithm/. If you retry the batch operation
-- immediately, the underlying read or write requests can still fail due to
-- throttling on the individual tables. If you delay the batch operation
-- using exponential backoff, the individual requests in the batch are much
-- more likely to succeed.
--
-- For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#BatchOperations Batch Operations and Error Handling>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- By default, /BatchGetItem/ performs eventually consistent reads on every
-- table in the request. If you want strongly consistent reads instead, you
-- can set /ConsistentRead/ to @true@ for any or all tables.
--
-- In order to minimize response latency, /BatchGetItem/ retrieves items in
-- parallel.
--
-- When designing your application, keep in mind that DynamoDB does not
-- return attributes in any particular order. To help parse the response by
-- item, include the primary key values for the items in your request in
-- the /AttributesToGet/ parameter.
--
-- If a requested item does not exist, it is not returned in the result.
-- Requests for nonexistent items consume the minimum read capacity units
-- according to the type of read. For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#CapacityUnitCalculations Capacity Units Calculations>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_BatchGetItem.html>
module Network.AWS.DynamoDB.BatchGetItem
    (
    -- * Request
      BatchGetItem
    -- ** Request constructor
    , batchGetItem
    -- ** Request lenses
    , bgiReturnConsumedCapacity
    , bgiRequestItems

    -- * Response
    , BatchGetItemResponse
    -- ** Response constructor
    , batchGetItemResponse
    -- ** Response lenses
    , bgirUnprocessedKeys
    , bgirResponses
    , bgirConsumedCapacity
    , bgirStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /BatchGetItem/ operation.
--
-- /See:/ 'batchGetItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgiReturnConsumedCapacity'
--
-- * 'bgiRequestItems'
data BatchGetItem = BatchGetItem'
    { _bgiReturnConsumedCapacity :: !(Maybe ReturnConsumedCapacity)
    , _bgiRequestItems           :: !(Map Text KeysAndAttributes)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'BatchGetItem' smart constructor.
batchGetItem :: BatchGetItem
batchGetItem =
    BatchGetItem'
    { _bgiReturnConsumedCapacity = Nothing
    , _bgiRequestItems = mempty
    }

-- | FIXME: Undocumented member.
bgiReturnConsumedCapacity :: Lens' BatchGetItem (Maybe ReturnConsumedCapacity)
bgiReturnConsumedCapacity = lens _bgiReturnConsumedCapacity (\ s a -> s{_bgiReturnConsumedCapacity = a});

-- | A map of one or more table names and, for each table, a map that
-- describes one or more items to retrieve from that table. Each table name
-- can be used only once per /BatchGetItem/ request.
--
-- Each element in the map of items to retrieve consists of the following:
--
-- -   /ConsistentRead/ - If @true@, a strongly consistent read is used; if
--     @false@ (the default), an eventually consistent read is used.
--
-- -   /ExpressionAttributeNames/ - One or more substitution tokens for
--     attribute names in the /ProjectionExpression/ parameter. The
--     following are some use cases for using /ExpressionAttributeNames/:
--
--     -   To access an attribute whose name conflicts with a DynamoDB
--         reserved word.
--
--     -   To create a placeholder for repeating occurrences of an
--         attribute name in an expression.
--
--     -   To prevent special characters in an attribute name from being
--         misinterpreted in an expression.
--
--     Use the __#__ character in an expression to dereference an attribute
--     name. For example, consider the following attribute name:
--
--     -   @Percentile@
--
--     The name of this attribute conflicts with a reserved word, so it
--     cannot be used directly in an expression. (For the complete list of
--     reserved words, see
--     <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
--     in the /Amazon DynamoDB Developer Guide/). To work around this, you
--     could specify the following for /ExpressionAttributeNames/:
--
--     -   @{\"#P\":\"Percentile\"}@
--
--     You could then use this substitution in an expression, as in this
--     example:
--
--     -   @#P = :val@
--
--     Tokens that begin with the __:__ character are /expression attribute
--     values/, which are placeholders for the actual value at runtime.
--
--     For more information on expression attribute names, see
--     <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
--     in the /Amazon DynamoDB Developer Guide/.
--
-- -   /Keys/ - An array of primary key attribute values that define
--     specific items in the table. For each primary key, you must provide
--     /all/ of the key attributes. For example, with a hash type primary
--     key, you only need to provide the hash attribute. For a
--     hash-and-range type primary key, you must provide /both/ the hash
--     attribute and the range attribute.
--
-- -   /ProjectionExpression/ - A string that identifies one or more
--     attributes to retrieve from the table. These attributes can include
--     scalars, sets, or elements of a JSON document. The attributes in the
--     expression must be separated by commas.
--
--     If no attribute names are specified, then all attributes will be
--     returned. If any of the requested attributes are not found, they
--     will not appear in the result.
--
--     For more information, see
--     <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
--     in the /Amazon DynamoDB Developer Guide/.
--
-- -   /AttributesToGet/ -
--
--     This is a legacy parameter, for backward compatibility. New
--     applications should use /ProjectionExpression/ instead. Do not
--     combine legacy parameters and expression parameters in a single API
--     call; otherwise, DynamoDB will return a /ValidationException/
--     exception.
--
--     This parameter allows you to retrieve attributes of type List or
--     Map; however, it cannot retrieve individual elements within a List
--     or a Map.
--
--     The names of one or more attributes to retrieve. If no attribute
--     names are provided, then all attributes will be returned. If any of
--     the requested attributes are not found, they will not appear in the
--     result.
--
--     Note that /AttributesToGet/ has no effect on provisioned throughput
--     consumption. DynamoDB determines capacity units consumed based on
--     item size, not on the amount of data that is returned to an
--     application.
--
bgiRequestItems :: Lens' BatchGetItem (HashMap Text KeysAndAttributes)
bgiRequestItems = lens _bgiRequestItems (\ s a -> s{_bgiRequestItems = a}) . _Map;

instance AWSRequest BatchGetItem where
        type Sv BatchGetItem = DynamoDB
        type Rs BatchGetItem = BatchGetItemResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetItemResponse' <$>
                   (x .?> "UnprocessedKeys" .!@ mempty) <*>
                     (x .?> "Responses" .!@ mempty)
                     <*> (x .?> "ConsumedCapacity" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders BatchGetItem where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.BatchGetItem" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON BatchGetItem where
        toJSON BatchGetItem'{..}
          = object
              ["ReturnConsumedCapacity" .=
                 _bgiReturnConsumedCapacity,
               "RequestItems" .= _bgiRequestItems]

instance ToPath BatchGetItem where
        toPath = const "/"

instance ToQuery BatchGetItem where
        toQuery = const mempty

-- | Represents the output of a /BatchGetItem/ operation.
--
-- /See:/ 'batchGetItemResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgirUnprocessedKeys'
--
-- * 'bgirResponses'
--
-- * 'bgirConsumedCapacity'
--
-- * 'bgirStatus'
data BatchGetItemResponse = BatchGetItemResponse'
    { _bgirUnprocessedKeys  :: !(Maybe (Map Text KeysAndAttributes))
    , _bgirResponses        :: !(Maybe (Map Text [Map Text AttributeValue]))
    , _bgirConsumedCapacity :: !(Maybe [ConsumedCapacity])
    , _bgirStatus           :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'BatchGetItemResponse' smart constructor.
batchGetItemResponse :: Int -> BatchGetItemResponse
batchGetItemResponse pStatus =
    BatchGetItemResponse'
    { _bgirUnprocessedKeys = Nothing
    , _bgirResponses = Nothing
    , _bgirConsumedCapacity = Nothing
    , _bgirStatus = pStatus
    }

-- | A map of tables and their respective keys that were not processed with
-- the current response. The /UnprocessedKeys/ value is in the same form as
-- /RequestItems/, so the value can be provided directly to a subsequent
-- /BatchGetItem/ operation. For more information, see /RequestItems/ in
-- the Request Parameters section.
--
-- Each element consists of:
--
-- -   /Keys/ - An array of primary key attribute values that define
--     specific items in the table.
--
-- -   /AttributesToGet/ - One or more attributes to be retrieved from the
--     table or index. By default, all attributes are returned. If a
--     requested attribute is not found, it does not appear in the result.
--
-- -   /ConsistentRead/ - The consistency of a read operation. If set to
--     @true@, then a strongly consistent read is used; otherwise, an
--     eventually consistent read is used.
--
-- If there are no unprocessed keys remaining, the response contains an
-- empty /UnprocessedKeys/ map.
bgirUnprocessedKeys :: Lens' BatchGetItemResponse (HashMap Text KeysAndAttributes)
bgirUnprocessedKeys = lens _bgirUnprocessedKeys (\ s a -> s{_bgirUnprocessedKeys = a}) . _Default . _Map;

-- | A map of table name to a list of items. Each object in /Responses/
-- consists of a table name, along with a map of attribute data consisting
-- of the data type and attribute value.
bgirResponses :: Lens' BatchGetItemResponse (HashMap Text [HashMap Text AttributeValue])
bgirResponses = lens _bgirResponses (\ s a -> s{_bgirResponses = a}) . _Default . _Map;

-- | The read capacity units consumed by the operation.
--
-- Each element consists of:
--
-- -   /TableName/ - The table that consumed the provisioned throughput.
--
-- -   /CapacityUnits/ - The total number of capacity units consumed.
--
bgirConsumedCapacity :: Lens' BatchGetItemResponse [ConsumedCapacity]
bgirConsumedCapacity = lens _bgirConsumedCapacity (\ s a -> s{_bgirConsumedCapacity = a}) . _Default;

-- | FIXME: Undocumented member.
bgirStatus :: Lens' BatchGetItemResponse Int
bgirStatus = lens _bgirStatus (\ s a -> s{_bgirStatus = a});
