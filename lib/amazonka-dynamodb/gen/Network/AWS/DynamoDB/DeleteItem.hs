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
-- Module      : Network.AWS.DynamoDB.DeleteItem
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a single item in a table by primary key. You can perform a conditional delete operation that deletes the item if it exists, or if it has an expected attribute value.
--
--
-- In addition to deleting an item, you can also return the item's attribute values in the same operation, using the @ReturnValues@ parameter.
--
-- Unless you specify conditions, the @DeleteItem@ is an idempotent operation; running it multiple times on the same item or attribute does /not/ result in an error response.
--
-- Conditional deletes are useful for deleting items only if specific conditions are met. If those conditions are met, DynamoDB performs the delete. Otherwise, the item is not deleted.
--
module Network.AWS.DynamoDB.DeleteItem
    (
    -- * Creating a Request
      deleteItem
    , DeleteItem
    -- * Request Lenses
    , diExpressionAttributeNames
    , diReturnValues
    , diExpressionAttributeValues
    , diReturnConsumedCapacity
    , diReturnItemCollectionMetrics
    , diConditionExpression
    , diConditionalOperator
    , diExpected
    , diTableName
    , diKey

    -- * Destructuring the Response
    , deleteItemResponse
    , DeleteItemResponse
    -- * Response Lenses
    , dirsItemCollectionMetrics
    , dirsConsumedCapacity
    , dirsAttributes
    , dirsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DeleteItem@ operation.
--
--
--
-- /See:/ 'deleteItem' smart constructor.
data DeleteItem = DeleteItem'
  { _diExpressionAttributeNames    :: !(Maybe (Map Text Text))
  , _diReturnValues                :: !(Maybe ReturnValue)
  , _diExpressionAttributeValues   :: !(Maybe (Map Text AttributeValue))
  , _diReturnConsumedCapacity      :: !(Maybe ReturnConsumedCapacity)
  , _diReturnItemCollectionMetrics :: !(Maybe ReturnItemCollectionMetrics)
  , _diConditionExpression         :: !(Maybe Text)
  , _diConditionalOperator         :: !(Maybe ConditionalOperator)
  , _diExpected                    :: !(Maybe (Map Text ExpectedAttributeValue))
  , _diTableName                   :: !Text
  , _diKey                         :: !(Map Text AttributeValue)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :     * To access an attribute whose name conflicts with a DynamoDB reserved word.     * To create a placeholder for repeating occurrences of an attribute name in an expression.     * To prevent special characters in an attribute name from being misinterpreted in an expression. Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:     * @Percentile@  The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :     * @{"#P":"Percentile"}@  You could then use this substitution in an expression, as in this example:     * @#P = :val@  For more information on expression attribute names, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'diReturnValues' - Use @ReturnValues@ if you want to get the item attributes as they appeared before they were deleted. For @DeleteItem@ , the valid values are:     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)     * @ALL_OLD@ - The content of the old item is returned.
--
-- * 'diExpressionAttributeValues' - One or more values that can be substituted in an expression. Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:  @Available | Backordered | Discontinued@  You would first need to specify @ExpressionAttributeValues@ as follows: @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@  You could then use these values in an expression, such as this: @ProductStatus IN (:avail, :back, :disc)@  For more information on expression attribute values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'diReturnConsumedCapacity' - Undocumented member.
--
-- * 'diReturnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
--
-- * 'diConditionExpression' - A condition that must be satisfied in order for a conditional @DeleteItem@ to succeed. An expression can contain any of the following:     * Functions: @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@  These function names are case-sensitive.     * Comparison operators: @= | <> | < | > | <= | >= | BETWEEN | IN @      * Logical operators: @AND | OR | NOT@  For more information on condition expressions, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'diConditionalOperator' - This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'diExpected' - This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'diTableName' - The name of the table from which to delete the item.
--
-- * 'diKey' - A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to delete. For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
deleteItem
    :: Text -- ^ 'diTableName'
    -> DeleteItem
deleteItem pTableName_ =
  DeleteItem'
    { _diExpressionAttributeNames = Nothing
    , _diReturnValues = Nothing
    , _diExpressionAttributeValues = Nothing
    , _diReturnConsumedCapacity = Nothing
    , _diReturnItemCollectionMetrics = Nothing
    , _diConditionExpression = Nothing
    , _diConditionalOperator = Nothing
    , _diExpected = Nothing
    , _diTableName = pTableName_
    , _diKey = mempty
    }


-- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :     * To access an attribute whose name conflicts with a DynamoDB reserved word.     * To create a placeholder for repeating occurrences of an attribute name in an expression.     * To prevent special characters in an attribute name from being misinterpreted in an expression. Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:     * @Percentile@  The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :     * @{"#P":"Percentile"}@  You could then use this substitution in an expression, as in this example:     * @#P = :val@  For more information on expression attribute names, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
diExpressionAttributeNames :: Lens' DeleteItem (HashMap Text Text)
diExpressionAttributeNames = lens _diExpressionAttributeNames (\ s a -> s{_diExpressionAttributeNames = a}) . _Default . _Map

-- | Use @ReturnValues@ if you want to get the item attributes as they appeared before they were deleted. For @DeleteItem@ , the valid values are:     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)     * @ALL_OLD@ - The content of the old item is returned.
diReturnValues :: Lens' DeleteItem (Maybe ReturnValue)
diReturnValues = lens _diReturnValues (\ s a -> s{_diReturnValues = a})

-- | One or more values that can be substituted in an expression. Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:  @Available | Backordered | Discontinued@  You would first need to specify @ExpressionAttributeValues@ as follows: @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@  You could then use these values in an expression, such as this: @ProductStatus IN (:avail, :back, :disc)@  For more information on expression attribute values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
diExpressionAttributeValues :: Lens' DeleteItem (HashMap Text AttributeValue)
diExpressionAttributeValues = lens _diExpressionAttributeValues (\ s a -> s{_diExpressionAttributeValues = a}) . _Default . _Map

-- | Undocumented member.
diReturnConsumedCapacity :: Lens' DeleteItem (Maybe ReturnConsumedCapacity)
diReturnConsumedCapacity = lens _diReturnConsumedCapacity (\ s a -> s{_diReturnConsumedCapacity = a})

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
diReturnItemCollectionMetrics :: Lens' DeleteItem (Maybe ReturnItemCollectionMetrics)
diReturnItemCollectionMetrics = lens _diReturnItemCollectionMetrics (\ s a -> s{_diReturnItemCollectionMetrics = a})

-- | A condition that must be satisfied in order for a conditional @DeleteItem@ to succeed. An expression can contain any of the following:     * Functions: @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@  These function names are case-sensitive.     * Comparison operators: @= | <> | < | > | <= | >= | BETWEEN | IN @      * Logical operators: @AND | OR | NOT@  For more information on condition expressions, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
diConditionExpression :: Lens' DeleteItem (Maybe Text)
diConditionExpression = lens _diConditionExpression (\ s a -> s{_diConditionExpression = a})

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
diConditionalOperator :: Lens' DeleteItem (Maybe ConditionalOperator)
diConditionalOperator = lens _diConditionalOperator (\ s a -> s{_diConditionalOperator = a})

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
diExpected :: Lens' DeleteItem (HashMap Text ExpectedAttributeValue)
diExpected = lens _diExpected (\ s a -> s{_diExpected = a}) . _Default . _Map

-- | The name of the table from which to delete the item.
diTableName :: Lens' DeleteItem Text
diTableName = lens _diTableName (\ s a -> s{_diTableName = a})

-- | A map of attribute names to @AttributeValue@ objects, representing the primary key of the item to delete. For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
diKey :: Lens' DeleteItem (HashMap Text AttributeValue)
diKey = lens _diKey (\ s a -> s{_diKey = a}) . _Map

instance AWSRequest DeleteItem where
        type Rs DeleteItem = DeleteItemResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 DeleteItemResponse' <$>
                   (x .?> "ItemCollectionMetrics") <*>
                     (x .?> "ConsumedCapacity")
                     <*> (x .?> "Attributes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DeleteItem where

instance NFData DeleteItem where

instance ToHeaders DeleteItem where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.DeleteItem" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DeleteItem where
        toJSON DeleteItem'{..}
          = object
              (catMaybes
                 [("ExpressionAttributeNames" .=) <$>
                    _diExpressionAttributeNames,
                  ("ReturnValues" .=) <$> _diReturnValues,
                  ("ExpressionAttributeValues" .=) <$>
                    _diExpressionAttributeValues,
                  ("ReturnConsumedCapacity" .=) <$>
                    _diReturnConsumedCapacity,
                  ("ReturnItemCollectionMetrics" .=) <$>
                    _diReturnItemCollectionMetrics,
                  ("ConditionExpression" .=) <$>
                    _diConditionExpression,
                  ("ConditionalOperator" .=) <$>
                    _diConditionalOperator,
                  ("Expected" .=) <$> _diExpected,
                  Just ("TableName" .= _diTableName),
                  Just ("Key" .= _diKey)])

instance ToPath DeleteItem where
        toPath = const "/"

instance ToQuery DeleteItem where
        toQuery = const mempty

-- | Represents the output of a @DeleteItem@ operation.
--
--
--
-- /See:/ 'deleteItemResponse' smart constructor.
data DeleteItemResponse = DeleteItemResponse'
  { _dirsItemCollectionMetrics :: !(Maybe ItemCollectionMetrics)
  , _dirsConsumedCapacity      :: !(Maybe ConsumedCapacity)
  , _dirsAttributes            :: !(Maybe (Map Text AttributeValue))
  , _dirsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsItemCollectionMetrics' - Information about item collections, if any, that were affected by the @DeleteItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response. Each @ItemCollectionMetrics@ element consists of:     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit. The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
-- * 'dirsConsumedCapacity' - The capacity units consumed by the @DeleteItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'dirsAttributes' - A map of attribute names to @AttributeValue@ objects, representing the item as it appeared before the @DeleteItem@ operation. This map appears in the response only if @ReturnValues@ was specified as @ALL_OLD@ in the request.
--
-- * 'dirsResponseStatus' - -- | The response status code.
deleteItemResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DeleteItemResponse
deleteItemResponse pResponseStatus_ =
  DeleteItemResponse'
    { _dirsItemCollectionMetrics = Nothing
    , _dirsConsumedCapacity = Nothing
    , _dirsAttributes = Nothing
    , _dirsResponseStatus = pResponseStatus_
    }


-- | Information about item collections, if any, that were affected by the @DeleteItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response. Each @ItemCollectionMetrics@ element consists of:     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit. The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
dirsItemCollectionMetrics :: Lens' DeleteItemResponse (Maybe ItemCollectionMetrics)
dirsItemCollectionMetrics = lens _dirsItemCollectionMetrics (\ s a -> s{_dirsItemCollectionMetrics = a})

-- | The capacity units consumed by the @DeleteItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
dirsConsumedCapacity :: Lens' DeleteItemResponse (Maybe ConsumedCapacity)
dirsConsumedCapacity = lens _dirsConsumedCapacity (\ s a -> s{_dirsConsumedCapacity = a})

-- | A map of attribute names to @AttributeValue@ objects, representing the item as it appeared before the @DeleteItem@ operation. This map appears in the response only if @ReturnValues@ was specified as @ALL_OLD@ in the request.
dirsAttributes :: Lens' DeleteItemResponse (HashMap Text AttributeValue)
dirsAttributes = lens _dirsAttributes (\ s a -> s{_dirsAttributes = a}) . _Default . _Map

-- | -- | The response status code.
dirsResponseStatus :: Lens' DeleteItemResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DeleteItemResponse where
