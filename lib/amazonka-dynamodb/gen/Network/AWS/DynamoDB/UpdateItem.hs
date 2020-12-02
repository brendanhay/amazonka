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
-- Module      : Network.AWS.DynamoDB.UpdateItem
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Edits an existing item's attributes, or adds a new item to the table if it does not already exist. You can put, delete, or add attribute values. You can also perform a conditional update on an existing item (insert a new attribute name-value pair if it doesn't exist, or replace an existing name-value pair if it has certain expected attribute values).
--
--
-- You can also return the item's attribute values in the same @UpdateItem@ operation using the @ReturnValues@ parameter.
--
module Network.AWS.DynamoDB.UpdateItem
    (
    -- * Creating a Request
      updateItem
    , UpdateItem
    -- * Request Lenses
    , uiExpressionAttributeNames
    , uiReturnValues
    , uiUpdateExpression
    , uiExpressionAttributeValues
    , uiAttributeUpdates
    , uiReturnConsumedCapacity
    , uiReturnItemCollectionMetrics
    , uiConditionExpression
    , uiConditionalOperator
    , uiExpected
    , uiTableName
    , uiKey

    -- * Destructuring the Response
    , updateItemResponse
    , UpdateItemResponse
    -- * Response Lenses
    , uirsItemCollectionMetrics
    , uirsConsumedCapacity
    , uirsAttributes
    , uirsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of an @UpdateItem@ operation.
--
--
--
-- /See:/ 'updateItem' smart constructor.
data UpdateItem = UpdateItem'
  { _uiExpressionAttributeNames    :: !(Maybe (Map Text Text))
  , _uiReturnValues                :: !(Maybe ReturnValue)
  , _uiUpdateExpression            :: !(Maybe Text)
  , _uiExpressionAttributeValues   :: !(Maybe (Map Text AttributeValue))
  , _uiAttributeUpdates            :: !(Maybe (Map Text AttributeValueUpdate))
  , _uiReturnConsumedCapacity      :: !(Maybe ReturnConsumedCapacity)
  , _uiReturnItemCollectionMetrics :: !(Maybe ReturnItemCollectionMetrics)
  , _uiConditionExpression         :: !(Maybe Text)
  , _uiConditionalOperator         :: !(Maybe ConditionalOperator)
  , _uiExpected                    :: !(Maybe (Map Text ExpectedAttributeValue))
  , _uiTableName                   :: !Text
  , _uiKey                         :: !(Map Text AttributeValue)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiExpressionAttributeNames' - One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :     * To access an attribute whose name conflicts with a DynamoDB reserved word.     * To create a placeholder for repeating occurrences of an attribute name in an expression.     * To prevent special characters in an attribute name from being misinterpreted in an expression. Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:     * @Percentile@  The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :     * @{"#P":"Percentile"}@  You could then use this substitution in an expression, as in this example:     * @#P = :val@  For more information on expression attribute names, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'uiReturnValues' - Use @ReturnValues@ if you want to get the item attributes as they appear before or after they are updated. For @UpdateItem@ , the valid values are:     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)     * @ALL_OLD@ - Returns all of the attributes of the item, as they appeared before the UpdateItem operation.     * @UPDATED_OLD@ - Returns only the updated attributes, as they appeared before the UpdateItem operation.     * @ALL_NEW@ - Returns all of the attributes of the item, as they appear after the UpdateItem operation.     * @UPDATED_NEW@ - Returns only the updated attributes, as they appear after the UpdateItem operation. There is no additional cost associated with requesting a return value aside from the small network and processing overhead of receiving a larger response. No read capacity units are consumed. The values returned are strongly consistent.
--
-- * 'uiUpdateExpression' - An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them. The following action values are available for @UpdateExpression@ .     * @SET@ - Adds one or more attributes and values to an item. If any of these attribute already exist, they are replaced by the new values. You can also use @SET@ to add or subtract from an attribute that is of type Number. For example: @SET myNum = myNum + :val@  @SET@ supports the following functions:     * @if_not_exists (path, operand)@ - if the item does not contain an attribute at the specified path, then @if_not_exists@ evaluates to operand; otherwise, it evaluates to path. You can use this function to avoid overwriting an attribute that may already be present in the item.     * @list_append (operand, operand)@ - evaluates to a list with a new element added to it. You can append the new element to the start or the end of the list by reversing the order of the operands. These function names are case-sensitive.     * @REMOVE@ - Removes one or more attributes from an item.     * @ADD@ - Adds the specified value to the item, if the attribute does not already exist. If the attribute does exist, then the behavior of @ADD@ depends on the data type of the attribute:     * If the existing attribute is a number, and if @Value@ is also a number, then @Value@ is mathematically added to the existing attribute. If @Value@ is a negative number, then it is subtracted from the existing attribute.     * If the existing data type is a set and if @Value@ is also a set, then @Value@ is added to the existing set. For example, if the attribute value is the set @[1,2]@ , and the @ADD@ action specified @[3]@ , then the final attribute value is @[1,2,3]@ . An error occurs if an @ADD@ action is specified for a set attribute and the attribute type specified does not match the existing set type.  Both sets must have the same primitive data type. For example, if the existing data type is a set of strings, the @Value@ must also be a set of strings. /Important:/ The @ADD@ action only supports Number and set data types. In addition, @ADD@ can only be used on top-level attributes, not nested attributes.     * @DELETE@ - Deletes an element from a set. If a set of values is specified, then those values are subtracted from the old set. For example, if the attribute value was the set @[a,b,c]@ and the @DELETE@ action specifies @[a,c]@ , then the final attribute value is @[b]@ . Specifying an empty set is an error. /Important:/ The @DELETE@ action only supports set data types. In addition, @DELETE@ can only be used on top-level attributes, not nested attributes. You can have many actions in a single expression, such as the following: @SET a=:value1, b=:value2 DELETE :value3, :value4, :value5@  For more information on update expressions, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Modifying.html Modifying Items and Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'uiExpressionAttributeValues' - One or more values that can be substituted in an expression. Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:  @Available | Backordered | Discontinued@  You would first need to specify @ExpressionAttributeValues@ as follows: @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@  You could then use these values in an expression, such as this: @ProductStatus IN (:avail, :back, :disc)@  For more information on expression attribute values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'uiAttributeUpdates' - This is a legacy parameter. Use @UpdateExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributeUpdates.html AttributeUpdates> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'uiReturnConsumedCapacity' - Undocumented member.
--
-- * 'uiReturnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
--
-- * 'uiConditionExpression' - A condition that must be satisfied in order for a conditional update to succeed. An expression can contain any of the following:     * Functions: @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@  These function names are case-sensitive.     * Comparison operators: @= | <> | < | > | <= | >= | BETWEEN | IN @      * Logical operators: @AND | OR | NOT@  For more information on condition expressions, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'uiConditionalOperator' - This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'uiExpected' - This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'uiTableName' - The name of the table containing the item to update.
--
-- * 'uiKey' - The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute. For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
updateItem
    :: Text -- ^ 'uiTableName'
    -> UpdateItem
updateItem pTableName_ =
  UpdateItem'
    { _uiExpressionAttributeNames = Nothing
    , _uiReturnValues = Nothing
    , _uiUpdateExpression = Nothing
    , _uiExpressionAttributeValues = Nothing
    , _uiAttributeUpdates = Nothing
    , _uiReturnConsumedCapacity = Nothing
    , _uiReturnItemCollectionMetrics = Nothing
    , _uiConditionExpression = Nothing
    , _uiConditionalOperator = Nothing
    , _uiExpected = Nothing
    , _uiTableName = pTableName_
    , _uiKey = mempty
    }


-- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :     * To access an attribute whose name conflicts with a DynamoDB reserved word.     * To create a placeholder for repeating occurrences of an attribute name in an expression.     * To prevent special characters in an attribute name from being misinterpreted in an expression. Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:     * @Percentile@  The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :     * @{"#P":"Percentile"}@  You could then use this substitution in an expression, as in this example:     * @#P = :val@  For more information on expression attribute names, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
uiExpressionAttributeNames :: Lens' UpdateItem (HashMap Text Text)
uiExpressionAttributeNames = lens _uiExpressionAttributeNames (\ s a -> s{_uiExpressionAttributeNames = a}) . _Default . _Map

-- | Use @ReturnValues@ if you want to get the item attributes as they appear before or after they are updated. For @UpdateItem@ , the valid values are:     * @NONE@ - If @ReturnValues@ is not specified, or if its value is @NONE@ , then nothing is returned. (This setting is the default for @ReturnValues@ .)     * @ALL_OLD@ - Returns all of the attributes of the item, as they appeared before the UpdateItem operation.     * @UPDATED_OLD@ - Returns only the updated attributes, as they appeared before the UpdateItem operation.     * @ALL_NEW@ - Returns all of the attributes of the item, as they appear after the UpdateItem operation.     * @UPDATED_NEW@ - Returns only the updated attributes, as they appear after the UpdateItem operation. There is no additional cost associated with requesting a return value aside from the small network and processing overhead of receiving a larger response. No read capacity units are consumed. The values returned are strongly consistent.
uiReturnValues :: Lens' UpdateItem (Maybe ReturnValue)
uiReturnValues = lens _uiReturnValues (\ s a -> s{_uiReturnValues = a})

-- | An expression that defines one or more attributes to be updated, the action to be performed on them, and new value(s) for them. The following action values are available for @UpdateExpression@ .     * @SET@ - Adds one or more attributes and values to an item. If any of these attribute already exist, they are replaced by the new values. You can also use @SET@ to add or subtract from an attribute that is of type Number. For example: @SET myNum = myNum + :val@  @SET@ supports the following functions:     * @if_not_exists (path, operand)@ - if the item does not contain an attribute at the specified path, then @if_not_exists@ evaluates to operand; otherwise, it evaluates to path. You can use this function to avoid overwriting an attribute that may already be present in the item.     * @list_append (operand, operand)@ - evaluates to a list with a new element added to it. You can append the new element to the start or the end of the list by reversing the order of the operands. These function names are case-sensitive.     * @REMOVE@ - Removes one or more attributes from an item.     * @ADD@ - Adds the specified value to the item, if the attribute does not already exist. If the attribute does exist, then the behavior of @ADD@ depends on the data type of the attribute:     * If the existing attribute is a number, and if @Value@ is also a number, then @Value@ is mathematically added to the existing attribute. If @Value@ is a negative number, then it is subtracted from the existing attribute.     * If the existing data type is a set and if @Value@ is also a set, then @Value@ is added to the existing set. For example, if the attribute value is the set @[1,2]@ , and the @ADD@ action specified @[3]@ , then the final attribute value is @[1,2,3]@ . An error occurs if an @ADD@ action is specified for a set attribute and the attribute type specified does not match the existing set type.  Both sets must have the same primitive data type. For example, if the existing data type is a set of strings, the @Value@ must also be a set of strings. /Important:/ The @ADD@ action only supports Number and set data types. In addition, @ADD@ can only be used on top-level attributes, not nested attributes.     * @DELETE@ - Deletes an element from a set. If a set of values is specified, then those values are subtracted from the old set. For example, if the attribute value was the set @[a,b,c]@ and the @DELETE@ action specifies @[a,c]@ , then the final attribute value is @[b]@ . Specifying an empty set is an error. /Important:/ The @DELETE@ action only supports set data types. In addition, @DELETE@ can only be used on top-level attributes, not nested attributes. You can have many actions in a single expression, such as the following: @SET a=:value1, b=:value2 DELETE :value3, :value4, :value5@  For more information on update expressions, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.Modifying.html Modifying Items and Attributes> in the /Amazon DynamoDB Developer Guide/ .
uiUpdateExpression :: Lens' UpdateItem (Maybe Text)
uiUpdateExpression = lens _uiUpdateExpression (\ s a -> s{_uiUpdateExpression = a})

-- | One or more values that can be substituted in an expression. Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:  @Available | Backordered | Discontinued@  You would first need to specify @ExpressionAttributeValues@ as follows: @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@  You could then use these values in an expression, such as this: @ProductStatus IN (:avail, :back, :disc)@  For more information on expression attribute values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
uiExpressionAttributeValues :: Lens' UpdateItem (HashMap Text AttributeValue)
uiExpressionAttributeValues = lens _uiExpressionAttributeValues (\ s a -> s{_uiExpressionAttributeValues = a}) . _Default . _Map

-- | This is a legacy parameter. Use @UpdateExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributeUpdates.html AttributeUpdates> in the /Amazon DynamoDB Developer Guide/ .
uiAttributeUpdates :: Lens' UpdateItem (HashMap Text AttributeValueUpdate)
uiAttributeUpdates = lens _uiAttributeUpdates (\ s a -> s{_uiAttributeUpdates = a}) . _Default . _Map

-- | Undocumented member.
uiReturnConsumedCapacity :: Lens' UpdateItem (Maybe ReturnConsumedCapacity)
uiReturnConsumedCapacity = lens _uiReturnConsumedCapacity (\ s a -> s{_uiReturnConsumedCapacity = a})

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to @NONE@ (the default), no statistics are returned.
uiReturnItemCollectionMetrics :: Lens' UpdateItem (Maybe ReturnItemCollectionMetrics)
uiReturnItemCollectionMetrics = lens _uiReturnItemCollectionMetrics (\ s a -> s{_uiReturnItemCollectionMetrics = a})

-- | A condition that must be satisfied in order for a conditional update to succeed. An expression can contain any of the following:     * Functions: @attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size@  These function names are case-sensitive.     * Comparison operators: @= | <> | < | > | <= | >= | BETWEEN | IN @      * Logical operators: @AND | OR | NOT@  For more information on condition expressions, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
uiConditionExpression :: Lens' UpdateItem (Maybe Text)
uiConditionExpression = lens _uiConditionExpression (\ s a -> s{_uiConditionExpression = a})

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
uiConditionalOperator :: Lens' UpdateItem (Maybe ConditionalOperator)
uiConditionalOperator = lens _uiConditionalOperator (\ s a -> s{_uiConditionalOperator = a})

-- | This is a legacy parameter. Use @ConditionExpression@ instead. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.Expected.html Expected> in the /Amazon DynamoDB Developer Guide/ .
uiExpected :: Lens' UpdateItem (HashMap Text ExpectedAttributeValue)
uiExpected = lens _uiExpected (\ s a -> s{_uiExpected = a}) . _Default . _Map

-- | The name of the table containing the item to update.
uiTableName :: Lens' UpdateItem Text
uiTableName = lens _uiTableName (\ s a -> s{_uiTableName = a})

-- | The primary key of the item to be updated. Each element consists of an attribute name and a value for that attribute. For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
uiKey :: Lens' UpdateItem (HashMap Text AttributeValue)
uiKey = lens _uiKey (\ s a -> s{_uiKey = a}) . _Map

instance AWSRequest UpdateItem where
        type Rs UpdateItem = UpdateItemResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 UpdateItemResponse' <$>
                   (x .?> "ItemCollectionMetrics") <*>
                     (x .?> "ConsumedCapacity")
                     <*> (x .?> "Attributes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable UpdateItem where

instance NFData UpdateItem where

instance ToHeaders UpdateItem where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.UpdateItem" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON UpdateItem where
        toJSON UpdateItem'{..}
          = object
              (catMaybes
                 [("ExpressionAttributeNames" .=) <$>
                    _uiExpressionAttributeNames,
                  ("ReturnValues" .=) <$> _uiReturnValues,
                  ("UpdateExpression" .=) <$> _uiUpdateExpression,
                  ("ExpressionAttributeValues" .=) <$>
                    _uiExpressionAttributeValues,
                  ("AttributeUpdates" .=) <$> _uiAttributeUpdates,
                  ("ReturnConsumedCapacity" .=) <$>
                    _uiReturnConsumedCapacity,
                  ("ReturnItemCollectionMetrics" .=) <$>
                    _uiReturnItemCollectionMetrics,
                  ("ConditionExpression" .=) <$>
                    _uiConditionExpression,
                  ("ConditionalOperator" .=) <$>
                    _uiConditionalOperator,
                  ("Expected" .=) <$> _uiExpected,
                  Just ("TableName" .= _uiTableName),
                  Just ("Key" .= _uiKey)])

instance ToPath UpdateItem where
        toPath = const "/"

instance ToQuery UpdateItem where
        toQuery = const mempty

-- | Represents the output of an @UpdateItem@ operation.
--
--
--
-- /See:/ 'updateItemResponse' smart constructor.
data UpdateItemResponse = UpdateItemResponse'
  { _uirsItemCollectionMetrics :: !(Maybe ItemCollectionMetrics)
  , _uirsConsumedCapacity      :: !(Maybe ConsumedCapacity)
  , _uirsAttributes            :: !(Maybe (Map Text AttributeValue))
  , _uirsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uirsItemCollectionMetrics' - Information about item collections, if any, that were affected by the @UpdateItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response. Each @ItemCollectionMetrics@ element consists of:     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit. The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
-- * 'uirsConsumedCapacity' - The capacity units consumed by the @UpdateItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'uirsAttributes' - A map of attribute values as they appear before or after the @UpdateItem@ operation, as determined by the @ReturnValues@ parameter. The @Attributes@ map is only present if @ReturnValues@ was specified as something other than @NONE@ in the request. Each element represents one attribute.
--
-- * 'uirsResponseStatus' - -- | The response status code.
updateItemResponse
    :: Int -- ^ 'uirsResponseStatus'
    -> UpdateItemResponse
updateItemResponse pResponseStatus_ =
  UpdateItemResponse'
    { _uirsItemCollectionMetrics = Nothing
    , _uirsConsumedCapacity = Nothing
    , _uirsAttributes = Nothing
    , _uirsResponseStatus = pResponseStatus_
    }


-- | Information about item collections, if any, that were affected by the @UpdateItem@ operation. @ItemCollectionMetrics@ is only returned if the @ReturnItemCollectionMetrics@ parameter was specified. If the table does not have any local secondary indexes, this information is not returned in the response. Each @ItemCollectionMetrics@ element consists of:     * @ItemCollectionKey@ - The partition key value of the item collection. This is the same as the partition key value of the item itself.     * @SizeEstimateRangeGB@ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit. The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
uirsItemCollectionMetrics :: Lens' UpdateItemResponse (Maybe ItemCollectionMetrics)
uirsItemCollectionMetrics = lens _uirsItemCollectionMetrics (\ s a -> s{_uirsItemCollectionMetrics = a})

-- | The capacity units consumed by the @UpdateItem@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
uirsConsumedCapacity :: Lens' UpdateItemResponse (Maybe ConsumedCapacity)
uirsConsumedCapacity = lens _uirsConsumedCapacity (\ s a -> s{_uirsConsumedCapacity = a})

-- | A map of attribute values as they appear before or after the @UpdateItem@ operation, as determined by the @ReturnValues@ parameter. The @Attributes@ map is only present if @ReturnValues@ was specified as something other than @NONE@ in the request. Each element represents one attribute.
uirsAttributes :: Lens' UpdateItemResponse (HashMap Text AttributeValue)
uirsAttributes = lens _uirsAttributes (\ s a -> s{_uirsAttributes = a}) . _Default . _Map

-- | -- | The response status code.
uirsResponseStatus :: Lens' UpdateItemResponse Int
uirsResponseStatus = lens _uirsResponseStatus (\ s a -> s{_uirsResponseStatus = a})

instance NFData UpdateItemResponse where
