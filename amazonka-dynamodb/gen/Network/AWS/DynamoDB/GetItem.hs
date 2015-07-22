{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.GetItem
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /GetItem/ operation returns a set of attributes for the item with
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
    , girqProjectionExpression
    , girqConsistentRead
    , girqExpressionAttributeNames
    , girqAttributesToGet
    , girqReturnConsumedCapacity
    , girqTableName
    , girqKey

    -- * Response
    , GetItemResponse
    -- ** Response constructor
    , getItemResponse
    -- ** Response lenses
    , girsConsumedCapacity
    , girsItem
    , girsStatus
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
-- * 'girqProjectionExpression'
--
-- * 'girqConsistentRead'
--
-- * 'girqExpressionAttributeNames'
--
-- * 'girqAttributesToGet'
--
-- * 'girqReturnConsumedCapacity'
--
-- * 'girqTableName'
--
-- * 'girqKey'
data GetItem = GetItem'
    { _girqProjectionExpression     :: !(Maybe Text)
    , _girqConsistentRead           :: !(Maybe Bool)
    , _girqExpressionAttributeNames :: !(Maybe (Map Text Text))
    , _girqAttributesToGet          :: !(Maybe (List1 Text))
    , _girqReturnConsumedCapacity   :: !(Maybe ReturnConsumedCapacity)
    , _girqTableName                :: !Text
    , _girqKey                      :: !(Map Text AttributeValue)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetItem' smart constructor.
getItem :: Text -> GetItem
getItem pTableName_ =
    GetItem'
    { _girqProjectionExpression = Nothing
    , _girqConsistentRead = Nothing
    , _girqExpressionAttributeNames = Nothing
    , _girqAttributesToGet = Nothing
    , _girqReturnConsumedCapacity = Nothing
    , _girqTableName = pTableName_
    , _girqKey = mempty
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
girqProjectionExpression :: Lens' GetItem (Maybe Text)
girqProjectionExpression = lens _girqProjectionExpression (\ s a -> s{_girqProjectionExpression = a});

-- | Determines the read consistency model: If set to @true@, then the
-- operation uses strongly consistent reads; otherwise, the operation uses
-- eventually consistent reads.
girqConsistentRead :: Lens' GetItem (Maybe Bool)
girqConsistentRead = lens _girqConsistentRead (\ s a -> s{_girqConsistentRead = a});

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
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
girqExpressionAttributeNames :: Lens' GetItem (HashMap Text Text)
girqExpressionAttributeNames = lens _girqExpressionAttributeNames (\ s a -> s{_girqExpressionAttributeNames = a}) . _Default . _Map;

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
girqAttributesToGet :: Lens' GetItem (Maybe (NonEmpty Text))
girqAttributesToGet = lens _girqAttributesToGet (\ s a -> s{_girqAttributesToGet = a}) . mapping _List1;

-- | FIXME: Undocumented member.
girqReturnConsumedCapacity :: Lens' GetItem (Maybe ReturnConsumedCapacity)
girqReturnConsumedCapacity = lens _girqReturnConsumedCapacity (\ s a -> s{_girqReturnConsumedCapacity = a});

-- | The name of the table containing the requested item.
girqTableName :: Lens' GetItem Text
girqTableName = lens _girqTableName (\ s a -> s{_girqTableName = a});

-- | A map of attribute names to /AttributeValue/ objects, representing the
-- primary key of the item to retrieve.
--
-- For the primary key, you must provide all of the attributes. For
-- example, with a hash type primary key, you only need to provide the hash
-- attribute. For a hash-and-range type primary key, you must provide both
-- the hash attribute and the range attribute.
girqKey :: Lens' GetItem (HashMap Text AttributeValue)
girqKey = lens _girqKey (\ s a -> s{_girqKey = a}) . _Map;

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
                     <*> (pure (fromEnum s)))

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
              ["ProjectionExpression" .= _girqProjectionExpression,
               "ConsistentRead" .= _girqConsistentRead,
               "ExpressionAttributeNames" .=
                 _girqExpressionAttributeNames,
               "AttributesToGet" .= _girqAttributesToGet,
               "ReturnConsumedCapacity" .=
                 _girqReturnConsumedCapacity,
               "TableName" .= _girqTableName, "Key" .= _girqKey]

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
-- * 'girsConsumedCapacity'
--
-- * 'girsItem'
--
-- * 'girsStatus'
data GetItemResponse = GetItemResponse'
    { _girsConsumedCapacity :: !(Maybe ConsumedCapacity)
    , _girsItem             :: !(Maybe (Map Text AttributeValue))
    , _girsStatus           :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetItemResponse' smart constructor.
getItemResponse :: Int -> GetItemResponse
getItemResponse pStatus_ =
    GetItemResponse'
    { _girsConsumedCapacity = Nothing
    , _girsItem = Nothing
    , _girsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
girsConsumedCapacity :: Lens' GetItemResponse (Maybe ConsumedCapacity)
girsConsumedCapacity = lens _girsConsumedCapacity (\ s a -> s{_girsConsumedCapacity = a});

-- | A map of attribute names to /AttributeValue/ objects, as specified by
-- /AttributesToGet/.
girsItem :: Lens' GetItemResponse (HashMap Text AttributeValue)
girsItem = lens _girsItem (\ s a -> s{_girsItem = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
girsStatus :: Lens' GetItemResponse Int
girsStatus = lens _girsStatus (\ s a -> s{_girsStatus = a});
