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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a single item in a table by primary key. You can perform a conditional delete operation that deletes the item if it exists, or if it has an expected attribute value.
--
-- In addition to deleting an item, you can also return the item\'s attribute values in the same operation, using the /ReturnValues/ parameter.
--
-- Unless you specify conditions, the /DeleteItem/ is an idempotent operation; running it multiple times on the same item or attribute does /not/ result in an error response.
--
-- Conditional deletes are useful for deleting items only if specific conditions are met. If those conditions are met, DynamoDB performs the delete. Otherwise, the item is not deleted.
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

import           Network.AWS.DynamoDB.Types
import           Network.AWS.DynamoDB.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteItem/ operation.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diExpressionAttributeNames'
--
-- * 'diReturnValues'
--
-- * 'diExpressionAttributeValues'
--
-- * 'diReturnConsumedCapacity'
--
-- * 'diReturnItemCollectionMetrics'
--
-- * 'diConditionExpression'
--
-- * 'diConditionalOperator'
--
-- * 'diExpected'
--
-- * 'diTableName'
--
-- * 'diKey'
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

-- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using /ExpressionAttributeNames/:
--
-- -   To access an attribute whose name conflicts with a DynamoDB reserved word.
--
-- -   To create a placeholder for repeating occurrences of an attribute name in an expression.
--
-- -   To prevent special characters in an attribute name from being misinterpreted in an expression.
--
-- Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:
--
-- -   'Percentile'
--
-- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/). To work around this, you could specify the following for /ExpressionAttributeNames/:
--
-- -   '{\"#P\":\"Percentile\"}'
--
-- You could then use this substitution in an expression, as in this example:
--
-- -   '#P = :val'
--
-- Tokens that begin with the __:__ character are /expression attribute values/, which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/.
diExpressionAttributeNames :: Lens' DeleteItem (HashMap Text Text)
diExpressionAttributeNames = lens _diExpressionAttributeNames (\ s a -> s{_diExpressionAttributeNames = a}) . _Default . _Map;

-- | Use /ReturnValues/ if you want to get the item attributes as they appeared before they were deleted. For /DeleteItem/, the valid values are:
--
-- -   'NONE' - If /ReturnValues/ is not specified, or if its value is 'NONE', then nothing is returned. (This setting is the default for /ReturnValues/.)
--
-- -   'ALL_OLD' - The content of the old item is returned.
--
diReturnValues :: Lens' DeleteItem (Maybe ReturnValue)
diReturnValues = lens _diReturnValues (\ s a -> s{_diReturnValues = a});

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:
--
-- 'Available | Backordered | Discontinued'
--
-- You would first need to specify /ExpressionAttributeValues/ as follows:
--
-- '{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }'
--
-- You could then use these values in an expression, such as this:
--
-- 'ProductStatus IN (:avail, :back, :disc)'
--
-- For more information on expression attribute values, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/.
diExpressionAttributeValues :: Lens' DeleteItem (HashMap Text AttributeValue)
diExpressionAttributeValues = lens _diExpressionAttributeValues (\ s a -> s{_diExpressionAttributeValues = a}) . _Default . _Map;

-- | Undocumented member.
diReturnConsumedCapacity :: Lens' DeleteItem (Maybe ReturnConsumedCapacity)
diReturnConsumedCapacity = lens _diReturnConsumedCapacity (\ s a -> s{_diReturnConsumedCapacity = a});

-- | Determines whether item collection metrics are returned. If set to 'SIZE', the response includes statistics about item collections, if any, that were modified during the operation are returned in the response. If set to 'NONE' (the default), no statistics are returned.
diReturnItemCollectionMetrics :: Lens' DeleteItem (Maybe ReturnItemCollectionMetrics)
diReturnItemCollectionMetrics = lens _diReturnItemCollectionMetrics (\ s a -> s{_diReturnItemCollectionMetrics = a});

-- | A condition that must be satisfied in order for a conditional /DeleteItem/ to succeed.
--
-- An expression can contain any of the following:
--
-- -   Functions: 'attribute_exists | attribute_not_exists | attribute_type | contains | begins_with | size'
--
--     These function names are case-sensitive.
--
-- -   Comparison operators: ' = | &#x3C;&#x3E; | &#x3C; | &#x3E; | &#x3C;= | &#x3E;= | BETWEEN | IN'
--
-- -   Logical operators: 'AND | OR | NOT'
--
-- For more information on condition expressions, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/.
--
-- /ConditionExpression/ replaces the legacy /ConditionalOperator/ and /Expected/ parameters.
diConditionExpression :: Lens' DeleteItem (Maybe Text)
diConditionExpression = lens _diConditionExpression (\ s a -> s{_diConditionExpression = a});

-- | This is a legacy parameter, for backward compatibility. New applications should use /ConditionExpression/ instead. Do not combine legacy parameters and expression parameters in a single API call; otherwise, DynamoDB will return a /ValidationException/ exception.
--
-- A logical operator to apply to the conditions in the /Expected/ map:
--
-- -   'AND' - If all of the conditions evaluate to true, then the entire map evaluates to true.
--
-- -   'OR' - If at least one of the conditions evaluate to true, then the entire map evaluates to true.
--
-- If you omit /ConditionalOperator/, then 'AND' is the default.
--
-- The operation will succeed only if the entire map evaluates to true.
--
-- This parameter does not support attributes of type List or Map.
diConditionalOperator :: Lens' DeleteItem (Maybe ConditionalOperator)
diConditionalOperator = lens _diConditionalOperator (\ s a -> s{_diConditionalOperator = a});

-- | This is a legacy parameter, for backward compatibility. New applications should use /ConditionExpression/ instead. Do not combine legacy parameters and expression parameters in a single API call; otherwise, DynamoDB will return a /ValidationException/ exception.
--
-- A map of attribute\/condition pairs. /Expected/ provides a conditional block for the /DeleteItem/ operation.
--
-- Each element of /Expected/ consists of an attribute name, a comparison operator, and one or more values. DynamoDB compares the attribute with the value(s) you supplied, using the comparison operator. For each /Expected/ element, the result of the evaluation is either true or false.
--
-- If you specify more than one element in the /Expected/ map, then by default all of the conditions must evaluate to true. In other words, the conditions are ANDed together. (You can use the /ConditionalOperator/ parameter to OR the conditions instead. If you do this, then at least one of the conditions must evaluate to true, rather than all of them.)
--
-- If the /Expected/ map evaluates to true, then the conditional operation succeeds; otherwise, it fails.
--
-- /Expected/ contains the following:
--
-- -   /AttributeValueList/ - One or more values to evaluate against the supplied attribute. The number of values in the list depends on the /ComparisonOperator/ being used.
--
--     For type Number, value comparisons are numeric.
--
--     String value comparisons for greater than, equals, or less than are based on ASCII character code values. For example, 'a' is greater than 'A', and 'a' is greater than 'B'. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
--     For type Binary, DynamoDB treats each byte of the binary data as unsigned when it compares binary values.
--
-- -   /ComparisonOperator/ - A comparator for evaluating attributes in the /AttributeValueList/. When performing the comparison, DynamoDB uses strongly consistent reads.
--
--     The following comparison operators are available:
--
--     'EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN'
--
--     The following are descriptions of each comparison operator.
--
--     -   'EQ' : Equal. 'EQ' is supported for all datatypes, including lists and maps.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ element of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value does not match. For example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'. Also, '{\"N\":\"6\"}' does not equal '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'NE' : Not equal. 'NE' is supported for all datatypes, including lists and maps.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an /AttributeValue/ of a different type than the one provided in the request, the value does not match. For example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'. Also, '{\"N\":\"6\"}' does not equal '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'LE' : Less than or equal.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ element of type String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value does not match. For example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'. Also, '{\"N\":\"6\"}' does not compare to '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'LT' : Less than.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of type String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value does not match. For example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'. Also, '{\"N\":\"6\"}' does not compare to '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'GE' : Greater than or equal.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ element of type String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value does not match. For example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'. Also, '{\"N\":\"6\"}' does not compare to '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'GT' : Greater than.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ element of type String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value does not match. For example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'. Also, '{\"N\":\"6\"}' does not compare to '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'NOT_NULL' : The attribute exists. 'NOT_NULL' is supported for all datatypes, including lists and maps.
--
--         This operator tests for the existence of an attribute, not its data type. If the data type of attribute \"'a'\" is null, and you evaluate it using 'NOT_NULL', the result is a Boolean /true/. This result is because the attribute \"'a'\" exists; its data type is not relevant to the 'NOT_NULL' comparison operator.
--
--     -   'NULL' : The attribute does not exist. 'NULL' is supported for all datatypes, including lists and maps.
--
--         This operator tests for the nonexistence of an attribute, not its data type. If the data type of attribute \"'a'\" is null, and you evaluate it using 'NULL', the result is a Boolean /false/. This is because the attribute \"'a'\" exists; its data type is not relevant to the 'NULL' comparison operator.
--
--     -   'CONTAINS' : Checks for a subsequence, or value in a set.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is of type String, then the operator checks for a substring match. If the target attribute of the comparison is of type Binary, then the operator looks for a subsequence of the target that matches the input. If the target attribute of the comparison is a set (\"'SS'\", \"'NS'\", or \"'BS'\"), then the operator evaluates to true if it finds an exact match with any member of the set.
--
--         CONTAINS is supported for lists: When evaluating \"'a CONTAINS b'\", \"'a'\" can be a list; however, \"'b'\" cannot be a set, a map, or a list.
--
--     -   'NOT_CONTAINS' : Checks for absence of a subsequence, or absence of a value in a set.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is a String, then the operator checks for the absence of a substring match. If the target attribute of the comparison is Binary, then the operator checks for the absence of a subsequence of the target that matches the input. If the target attribute of the comparison is a set (\"'SS'\", \"'NS'\", or \"'BS'\"), then the operator evaluates to true if it /does not/ find an exact match with any member of the set.
--
--         NOT_CONTAINS is supported for lists: When evaluating \"'a NOT CONTAINS b'\", \"'a'\" can be a list; however, \"'b'\" cannot be a set, a map, or a list.
--
--     -   'BEGINS_WITH' : Checks for a prefix.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of type String or Binary (not a Number or a set type). The target attribute of the comparison must be of type String or Binary (not a Number or a set type).
--
--     -   'IN' : Checks for matching elements within two sets.
--
--         /AttributeValueList/ can contain one or more /AttributeValue/ elements of type String, Number, or Binary (not a set type). These attributes are compared against an existing set type attribute of an item. If any elements of the input set are present in the item attribute, the expression evaluates to true.
--
--     -   'BETWEEN' : Greater than or equal to the first value, and less than or equal to the second value.
--
--         /AttributeValueList/ must contain two /AttributeValue/ elements of the same type, either String, Number, or Binary (not a set type). A target attribute matches if the target value is greater than, or equal to, the first element and less than, or equal to, the second element. If an item contains an /AttributeValue/ element of a different type than the one provided in the request, the value does not match. For example, '{\"S\":\"6\"}' does not compare to '{\"N\":\"6\"}'. Also, '{\"N\":\"6\"}' does not compare to '{\"NS\":[\"6\", \"2\", \"1\"]}'
--
-- For usage examples of /AttributeValueList/ and /ComparisonOperator/, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/.
--
-- For backward compatibility with previous DynamoDB releases, the following parameters can be used instead of /AttributeValueList/ and /ComparisonOperator/:
--
-- -   /Value/ - A value for DynamoDB to compare with an attribute.
--
-- -   /Exists/ - A Boolean value that causes DynamoDB to evaluate the value before attempting the conditional operation:
--
--     -   If /Exists/ is 'true', DynamoDB will check to see if that attribute value already exists in the table. If it is found, then the condition evaluates to true; otherwise the condition evaluate to false.
--
--     -   If /Exists/ is 'false', DynamoDB assumes that the attribute value does /not/ exist in the table. If in fact the value does not exist, then the assumption is valid and the condition evaluates to true. If the value is found, despite the assumption that it does not exist, the condition evaluates to false.
--
--     Note that the default value for /Exists/ is 'true'.
--
-- The /Value/ and /Exists/ parameters are incompatible with /AttributeValueList/ and /ComparisonOperator/. Note that if you use both sets of parameters at once, DynamoDB will return a /ValidationException/ exception.
--
-- This parameter does not support attributes of type List or Map.
diExpected :: Lens' DeleteItem (HashMap Text ExpectedAttributeValue)
diExpected = lens _diExpected (\ s a -> s{_diExpected = a}) . _Default . _Map;

-- | The name of the table from which to delete the item.
diTableName :: Lens' DeleteItem Text
diTableName = lens _diTableName (\ s a -> s{_diTableName = a});

-- | A map of attribute names to /AttributeValue/ objects, representing the primary key of the item to delete.
--
-- For the primary key, you must provide all of the attributes. For example, with a simple primary key, you only need to provide a value for the partition key. For a composite primary key, you must provide values for both the partition key and the sort key.
diKey :: Lens' DeleteItem (HashMap Text AttributeValue)
diKey = lens _diKey (\ s a -> s{_diKey = a}) . _Map;

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

instance Hashable DeleteItem

instance NFData DeleteItem

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

-- | Represents the output of a /DeleteItem/ operation.
--
-- /See:/ 'deleteItemResponse' smart constructor.
data DeleteItemResponse = DeleteItemResponse'
    { _dirsItemCollectionMetrics :: !(Maybe ItemCollectionMetrics)
    , _dirsConsumedCapacity      :: !(Maybe ConsumedCapacity)
    , _dirsAttributes            :: !(Maybe (Map Text AttributeValue))
    , _dirsResponseStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsItemCollectionMetrics'
--
-- * 'dirsConsumedCapacity'
--
-- * 'dirsAttributes'
--
-- * 'dirsResponseStatus'
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

-- | Information about item collections, if any, that were affected by the operation. /ItemCollectionMetrics/ is only returned if the request asked for it. If the table does not have any local secondary indexes, this information is not returned in the response.
--
-- Each /ItemCollectionMetrics/ element consists of:
--
-- -   /ItemCollectionKey/ - The partition key value of the item collection. This is the same as the partition key value of the item itself.
--
-- -   /SizeEstimateRange/ - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit.
--
--     The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
--
dirsItemCollectionMetrics :: Lens' DeleteItemResponse (Maybe ItemCollectionMetrics)
dirsItemCollectionMetrics = lens _dirsItemCollectionMetrics (\ s a -> s{_dirsItemCollectionMetrics = a});

-- | Undocumented member.
dirsConsumedCapacity :: Lens' DeleteItemResponse (Maybe ConsumedCapacity)
dirsConsumedCapacity = lens _dirsConsumedCapacity (\ s a -> s{_dirsConsumedCapacity = a});

-- | A map of attribute names to /AttributeValue/ objects, representing the item as it appeared before the /DeleteItem/ operation. This map appears in the response only if /ReturnValues/ was specified as 'ALL_OLD' in the request.
dirsAttributes :: Lens' DeleteItemResponse (HashMap Text AttributeValue)
dirsAttributes = lens _dirsAttributes (\ s a -> s{_dirsAttributes = a}) . _Default . _Map;

-- | The response status code.
dirsResponseStatus :: Lens' DeleteItemResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a});

instance NFData DeleteItemResponse
