{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.GetItem
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetItem@ operation returns a set of attributes for the item with the given primary key. If there is no matching item, @GetItem@ does not return any data and there will be no @Item@ element in the response.
--
--
-- @GetItem@ provides an eventually consistent read by default. If your application requires a strongly consistent read, set @ConsistentRead@ to @true@ . Although a strongly consistent read might take more time than an eventually consistent read, it always returns the last updated value.
--
module Network.AWS.DynamoDB.GetItem
    (
    -- * Creating a Request
      getItem
    , GetItem
    -- * Request Lenses
    , giProjectionExpression
    , giAttributesToGet
    , giExpressionAttributeNames
    , giConsistentRead
    , giReturnConsumedCapacity
    , giTableName
    , giKey

    -- * Destructuring the Response
    , getItemResponse
    , GetItemResponse
    -- * Response Lenses
    , girsConsumedCapacity
    , girsItem
    , girsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @GetItem@ operation.
--
--
--
-- /See:/ 'getItem' smart constructor.
data GetItem = GetItem'
  { _giProjectionExpression     :: !(Maybe Text)
  , _giAttributesToGet          :: !(Maybe (List1 Text))
  , _giExpressionAttributeNames :: !(Maybe (Map Text Text))
  , _giConsistentRead           :: !(Maybe Bool)
  , _giReturnConsumedCapacity   :: !(Maybe ReturnConsumedCapacity)
  , _giTableName                :: !Text
  , _giKey                      :: !(Map Text AttributeValue)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giProjectionExpression' - A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas. If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'giAttributesToGet' - This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'giExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :     * To access an attribute whose name conflicts with a DynamoDB reserved word.     * To create a placeholder for repeating occurrences of an attribute name in an expression.     * To prevent special characters in an attribute name from being misinterpreted in an expression. Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:     * @Percentile@  The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :     * @{"#P":"Percentile"}@  You could then use this substitution in an expression, as in this example:     * @#P = :val@  For more information on expression attribute names, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'giConsistentRead' - Determines the read consistency model: If set to @true@ , then the operation uses strongly consistent reads; otherwise, the operation uses eventually consistent reads.
--
-- * 'giReturnConsumedCapacity' - Undocumented member.
--
-- * 'giTableName' - The name of the table containing the requested item.
--
-- * 'giKey' - A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to retrieve. For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
getItem
    :: Text -- ^ 'giTableName'
    -> GetItem
getItem pTableName_ =
  GetItem'
    { _giProjectionExpression = Nothing
    , _giAttributesToGet = Nothing
    , _giExpressionAttributeNames = Nothing
    , _giConsistentRead = Nothing
    , _giReturnConsumedCapacity = Nothing
    , _giTableName = pTableName_
    , _giKey = mempty
    }


-- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas. If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
giProjectionExpression :: Lens' GetItem (Maybe Text)
giProjectionExpression = lens _giProjectionExpression (\ s a -> s{_giProjectionExpression = a})

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
giAttributesToGet :: Lens' GetItem (Maybe (NonEmpty Text))
giAttributesToGet = lens _giAttributesToGet (\ s a -> s{_giAttributesToGet = a}) . mapping _List1

-- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :     * To access an attribute whose name conflicts with a DynamoDB reserved word.     * To create a placeholder for repeating occurrences of an attribute name in an expression.     * To prevent special characters in an attribute name from being misinterpreted in an expression. Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:     * @Percentile@  The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :     * @{"#P":"Percentile"}@  You could then use this substitution in an expression, as in this example:     * @#P = :val@  For more information on expression attribute names, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
giExpressionAttributeNames :: Lens' GetItem (HashMap Text Text)
giExpressionAttributeNames = lens _giExpressionAttributeNames (\ s a -> s{_giExpressionAttributeNames = a}) . _Default . _Map

-- | Determines the read consistency model: If set to @true@ , then the operation uses strongly consistent reads; otherwise, the operation uses eventually consistent reads.
giConsistentRead :: Lens' GetItem (Maybe Bool)
giConsistentRead = lens _giConsistentRead (\ s a -> s{_giConsistentRead = a})

-- | Undocumented member.
giReturnConsumedCapacity :: Lens' GetItem (Maybe ReturnConsumedCapacity)
giReturnConsumedCapacity = lens _giReturnConsumedCapacity (\ s a -> s{_giReturnConsumedCapacity = a})

-- | The name of the table containing the requested item.
giTableName :: Lens' GetItem Text
giTableName = lens _giTableName (\ s a -> s{_giTableName = a})

-- | A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to retrieve. For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
giKey :: Lens' GetItem (HashMap Text AttributeValue)
giKey = lens _giKey (\ s a -> s{_giKey = a}) . _Map

instance AWSRequest GetItem where
        type Rs GetItem = GetItemResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 GetItemResponse' <$>
                   (x .?> "ConsumedCapacity") <*>
                     (x .?> "Item" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetItem where

instance NFData GetItem where

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
              (catMaybes
                 [("ProjectionExpression" .=) <$>
                    _giProjectionExpression,
                  ("AttributesToGet" .=) <$> _giAttributesToGet,
                  ("ExpressionAttributeNames" .=) <$>
                    _giExpressionAttributeNames,
                  ("ConsistentRead" .=) <$> _giConsistentRead,
                  ("ReturnConsumedCapacity" .=) <$>
                    _giReturnConsumedCapacity,
                  Just ("TableName" .= _giTableName),
                  Just ("Key" .= _giKey)])

instance ToPath GetItem where
        toPath = const "/"

instance ToQuery GetItem where
        toQuery = const mempty

-- | Represents the output of a @GetItem@ operation.
--
--
--
-- /See:/ 'getItemResponse' smart constructor.
data GetItemResponse = GetItemResponse'
  { _girsConsumedCapacity :: !(Maybe ConsumedCapacity)
  , _girsItem             :: !(Maybe (Map Text AttributeValue))
  , _girsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girsConsumedCapacity' - The capacity units consumed by the @GetItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'girsItem' - A map of attribute names to @AttributeValue@ objects, as specified by @ProjectionExpression@ .
--
-- * 'girsResponseStatus' - -- | The response status code.
getItemResponse
    :: Int -- ^ 'girsResponseStatus'
    -> GetItemResponse
getItemResponse pResponseStatus_ =
  GetItemResponse'
    { _girsConsumedCapacity = Nothing
    , _girsItem = Nothing
    , _girsResponseStatus = pResponseStatus_
    }


-- | The capacity units consumed by the @GetItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
girsConsumedCapacity :: Lens' GetItemResponse (Maybe ConsumedCapacity)
girsConsumedCapacity = lens _girsConsumedCapacity (\ s a -> s{_girsConsumedCapacity = a})

-- | A map of attribute names to @AttributeValue@ objects, as specified by @ProjectionExpression@ .
girsItem :: Lens' GetItemResponse (HashMap Text AttributeValue)
girsItem = lens _girsItem (\ s a -> s{_girsItem = a}) . _Default . _Map

-- | -- | The response status code.
girsResponseStatus :: Lens' GetItemResponse Int
girsResponseStatus = lens _girsResponseStatus (\ s a -> s{_girsResponseStatus = a})

instance NFData GetItemResponse where
