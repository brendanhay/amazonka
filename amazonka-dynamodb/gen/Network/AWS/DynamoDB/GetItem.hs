{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DynamoDB.GetItem
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /GetItem/ operation returns a set of attributes for the item with
-- the given primary key. If there is no matching item, /GetItem/ does not
-- return any data.
--
-- /GetItem/ provides an eventually consistent read by default. If your
-- application requires a strongly consistent read, set /ConsistentRead/ to
-- @true@. Although a strongly consistent read might take more time than an
-- eventually consistent read, it always returns the last updated value.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_GetItem.html>
module Network.AWS.DynamoDB.GetItem
    (
    -- * Request
      GetItem
    -- ** Request constructor
    , getItem
    -- ** Request lenses
    , giProjectionExpression
    , giConsistentRead
    , giExpressionAttributeNames
    , giAttributesToGet
    , giReturnConsumedCapacity
    , giTableName
    , giKey

    -- * Response
    , GetItemResponse
    -- ** Response constructor
    , getItemResponse
    -- ** Response lenses
    , girConsumedCapacity
    , girItem
    , girStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /GetItem/ operation.
--
-- /See:/ 'getItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giProjectionExpression'
--
-- * 'giConsistentRead'
--
-- * 'giExpressionAttributeNames'
--
-- * 'giAttributesToGet'
--
-- * 'giReturnConsumedCapacity'
--
-- * 'giTableName'
--
-- * 'giKey'
data GetItem = GetItem'
    { _giProjectionExpression     :: !(Maybe Text)
    , _giConsistentRead           :: !(Maybe Bool)
    , _giExpressionAttributeNames :: !(Maybe (Map Text Text))
    , _giAttributesToGet          :: !(Maybe (List1 Text))
    , _giReturnConsumedCapacity   :: !(Maybe ReturnConsumedCapacity)
    , _giTableName                :: !Text
    , _giKey                      :: !(Map Text AttributeValue)
    } deriving (Eq,Show)

-- | 'GetItem' smart constructor.
getItem :: Text -> GetItem
getItem pTableName =
    GetItem'
    { _giProjectionExpression = Nothing
    , _giConsistentRead = Nothing
    , _giExpressionAttributeNames = Nothing
    , _giAttributesToGet = Nothing
    , _giReturnConsumedCapacity = Nothing
    , _giTableName = pTableName
    , _giKey = mempty
    }

-- | A string that identifies one or more attributes to retrieve from the
-- table. These attributes can include scalars, sets, or elements of a JSON
-- document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be
-- returned. If any of the requested attributes are not found, they will
-- not appear in the result.
--
-- For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /ProjectionExpression/ replaces the legacy /AttributesToGet/ parameter.
giProjectionExpression :: Lens' GetItem (Maybe Text)
giProjectionExpression = lens _giProjectionExpression (\ s a -> s{_giProjectionExpression = a});

-- | A value that if set to @true@, then the operation uses strongly
-- consistent reads; otherwise, eventually consistent reads are used.
giConsistentRead :: Lens' GetItem (Maybe Bool)
giConsistentRead = lens _giConsistentRead (\ s a -> s{_giConsistentRead = a});

-- | One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using /ExpressionAttributeNames/:
--
-- -   To access an attribute whose name conflicts with a DynamoDB reserved
--     word.
--
-- -   To create a placeholder for repeating occurrences of an attribute
--     name in an expression.
--
-- -   To prevent special characters in an attribute name from being
--     misinterpreted in an expression.
--
-- Use the __#__ character in an expression to dereference an attribute
-- name. For example, consider the following attribute name:
--
-- -   @Percentile@
--
-- The name of this attribute conflicts with a reserved word, so it cannot
-- be used directly in an expression. (For the complete list of reserved
-- words, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
-- could specify the following for /ExpressionAttributeNames/:
--
-- -   @{\"#P\":\"Percentile\"}@
--
-- You could then use this substitution in an expression, as in this
-- example:
--
-- -   @#P = :val@
--
-- Tokens that begin with the __:__ character are /expression attribute
-- values/, which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
-- in the /Amazon DynamoDB Developer Guide/.
giExpressionAttributeNames :: Lens' GetItem (HashMap Text Text)
giExpressionAttributeNames = lens _giExpressionAttributeNames (\ s a -> s{_giExpressionAttributeNames = a}) . _Default . _Map;

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /ProjectionExpression/ instead. Do not combine legacy
-- parameters and expression parameters in a single API call; otherwise,
-- DynamoDB will return a /ValidationException/ exception.
--
-- This parameter allows you to retrieve attributes of type List or Map;
-- however, it cannot retrieve individual elements within a List or a Map.
--
-- The names of one or more attributes to retrieve. If no attribute names
-- are provided, then all attributes will be returned. If any of the
-- requested attributes are not found, they will not appear in the result.
--
-- Note that /AttributesToGet/ has no effect on provisioned throughput
-- consumption. DynamoDB determines capacity units consumed based on item
-- size, not on the amount of data that is returned to an application.
giAttributesToGet :: Lens' GetItem (Maybe (NonEmpty Text))
giAttributesToGet = lens _giAttributesToGet (\ s a -> s{_giAttributesToGet = a}) . mapping _List1;

-- | FIXME: Undocumented member.
giReturnConsumedCapacity :: Lens' GetItem (Maybe ReturnConsumedCapacity)
giReturnConsumedCapacity = lens _giReturnConsumedCapacity (\ s a -> s{_giReturnConsumedCapacity = a});

-- | The name of the table containing the requested item.
giTableName :: Lens' GetItem Text
giTableName = lens _giTableName (\ s a -> s{_giTableName = a});

-- | A map of attribute names to /AttributeValue/ objects, representing the
-- primary key of the item to retrieve.
--
-- For the primary key, you must provide all of the attributes. For
-- example, with a hash type primary key, you only need to provide the hash
-- attribute. For a hash-and-range type primary key, you must provide both
-- the hash attribute and the range attribute.
giKey :: Lens' GetItem (HashMap Text AttributeValue)
giKey = lens _giKey (\ s a -> s{_giKey = a}) . _Map;

instance AWSRequest GetItem where
        type Sv GetItem = DynamoDB
        type Rs GetItem = GetItemResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetItemResponse' <$>
                   (x .?> "ConsumedCapacity") <*>
                     (x .?> "Item" .!@ mempty)
                     <*> (pure s))

instance ToHeaders GetItem where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.GetItem" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON GetItem where
        toJSON GetItem'{..}
          = object
              ["ProjectionExpression" .= _giProjectionExpression,
               "ConsistentRead" .= _giConsistentRead,
               "ExpressionAttributeNames" .=
                 _giExpressionAttributeNames,
               "AttributesToGet" .= _giAttributesToGet,
               "ReturnConsumedCapacity" .=
                 _giReturnConsumedCapacity,
               "TableName" .= _giTableName, "Key" .= _giKey]

instance ToPath GetItem where
        toPath = const "/"

instance ToQuery GetItem where
        toQuery = const mempty

-- | Represents the output of a /GetItem/ operation.
--
-- /See:/ 'getItemResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girConsumedCapacity'
--
-- * 'girItem'
--
-- * 'girStatus'
data GetItemResponse = GetItemResponse'
    { _girConsumedCapacity :: !(Maybe ConsumedCapacity)
    , _girItem             :: !(Maybe (Map Text AttributeValue))
    , _girStatus           :: !Status
    } deriving (Eq,Show)

-- | 'GetItemResponse' smart constructor.
getItemResponse :: Status -> GetItemResponse
getItemResponse pStatus =
    GetItemResponse'
    { _girConsumedCapacity = Nothing
    , _girItem = Nothing
    , _girStatus = pStatus
    }

-- | FIXME: Undocumented member.
girConsumedCapacity :: Lens' GetItemResponse (Maybe ConsumedCapacity)
girConsumedCapacity = lens _girConsumedCapacity (\ s a -> s{_girConsumedCapacity = a});

-- | A map of attribute names to /AttributeValue/ objects, as specified by
-- /AttributesToGet/.
girItem :: Lens' GetItemResponse (HashMap Text AttributeValue)
girItem = lens _girItem (\ s a -> s{_girItem = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
girStatus :: Lens' GetItemResponse Status
girStatus = lens _girStatus (\ s a -> s{_girStatus = a});
