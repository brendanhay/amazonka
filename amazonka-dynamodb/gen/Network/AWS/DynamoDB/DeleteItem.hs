{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.DeleteItem
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a single item in a table by primary key. You can perform a
-- conditional delete operation that deletes the item if it exists, or if it has
-- an expected attribute value.
--
-- In addition to deleting an item, you can also return the item's attribute
-- values in the same operation, using the /ReturnValues/ parameter.
--
-- Unless you specify conditions, the /DeleteItem/ is an idempotent operation;
-- running it multiple times on the same item or attribute does /not/ result in an
-- error response.
--
-- Conditional deletes are useful for deleting items only if specific
-- conditions are met. If those conditions are met, DynamoDB performs the
-- delete. Otherwise, the item is not deleted.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_DeleteItem.html>
module Network.AWS.DynamoDB.DeleteItem
    (
    -- * Request
      DeleteItem
    -- ** Request constructor
    , deleteItem
    -- ** Request lenses
    , diConditionExpression
    , diConditionalOperator
    , diExpected
    , diExpressionAttributeNames
    , diExpressionAttributeValues
    , diKey
    , diReturnConsumedCapacity
    , diReturnItemCollectionMetrics
    , diReturnValues
    , diTableName

    -- * Response
    , DeleteItemResponse
    -- ** Response constructor
    , deleteItemResponse
    -- ** Response lenses
    , dirAttributes
    , dirConsumedCapacity
    , dirItemCollectionMetrics
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.Types
import qualified GHC.Exts

data DeleteItem = DeleteItem
    { _diConditionExpression         :: Maybe Text
    , _diConditionalOperator         :: Maybe ConditionalOperator
    , _diExpected                    :: Map Text ExpectedAttributeValue
    , _diExpressionAttributeNames    :: Map Text Text
    , _diExpressionAttributeValues   :: Map Text AttributeValue
    , _diKey                         :: Map Text AttributeValue
    , _diReturnConsumedCapacity      :: Maybe ReturnConsumedCapacity
    , _diReturnItemCollectionMetrics :: Maybe ReturnItemCollectionMetrics
    , _diReturnValues                :: Maybe ReturnValue
    , _diTableName                   :: Text
    } deriving (Eq, Show)

-- | 'DeleteItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diConditionExpression' @::@ 'Maybe' 'Text'
--
-- * 'diConditionalOperator' @::@ 'Maybe' 'ConditionalOperator'
--
-- * 'diExpected' @::@ 'HashMap' 'Text' 'ExpectedAttributeValue'
--
-- * 'diExpressionAttributeNames' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'diExpressionAttributeValues' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'diKey' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'diReturnConsumedCapacity' @::@ 'Maybe' 'ReturnConsumedCapacity'
--
-- * 'diReturnItemCollectionMetrics' @::@ 'Maybe' 'ReturnItemCollectionMetrics'
--
-- * 'diReturnValues' @::@ 'Maybe' 'ReturnValue'
--
-- * 'diTableName' @::@ 'Text'
--
deleteItem :: Text -- ^ 'diTableName'
           -> DeleteItem
deleteItem p1 = DeleteItem
    { _diTableName                   = p1
    , _diKey                         = mempty
    , _diExpected                    = mempty
    , _diConditionalOperator         = Nothing
    , _diReturnValues                = Nothing
    , _diReturnConsumedCapacity      = Nothing
    , _diReturnItemCollectionMetrics = Nothing
    , _diConditionExpression         = Nothing
    , _diExpressionAttributeNames    = mempty
    , _diExpressionAttributeValues   = mempty
    }

-- | A condition that must be satisfied in order for a conditional /DeleteItem/ to
-- succeed.
--
-- An expression can contain any of the following:
--
-- Boolean functions: 'ATTRIBUTE_EXIST | CONTAINS | BEGINS_WITH'
--
-- Comparison operators: ' = | <> | < | > | <= | >= | BETWEEN | IN'
--
-- Logical operators: 'NOT | AND | OR'
--
--
diConditionExpression :: Lens' DeleteItem (Maybe Text)
diConditionExpression =
    lens _diConditionExpression (\s a -> s { _diConditionExpression = a })

-- | There is a newer parameter available. Use /ConditionExpression/ instead. Note
-- that if you use /ConditionalOperator/ and / ConditionExpression / at the same
-- time, DynamoDB will return a /ValidationException/ exception.
--
-- This parameter does not support lists or maps.
--
-- A logical operator to apply to the conditions in the /Expected/ map:
--
-- 'AND' - If all of the conditions evaluate to true, then the entire map
-- evaluates to true.
--
-- 'OR' - If at least one of the conditions evaluate to true, then the entire map
-- evaluates to true.
--
-- If you omit /ConditionalOperator/, then 'AND' is the default.
--
-- The operation will succeed only if the entire map evaluates to true.
--
diConditionalOperator :: Lens' DeleteItem (Maybe ConditionalOperator)
diConditionalOperator =
    lens _diConditionalOperator (\s a -> s { _diConditionalOperator = a })

-- | There is a newer parameter available. Use /ConditionExpression/ instead. Note
-- that if you use /Expected/ and / ConditionExpression / at the same time, DynamoDB
-- will return a /ValidationException/ exception.
--
-- This parameter does not support lists or maps.
--
-- A map of attribute/condition pairs. /Expected/ provides a conditional block
-- for the /DeleteItem/ operation.
--
-- Each element of /Expected/ consists of an attribute name, a comparison
-- operator, and one or more values. DynamoDB compares the attribute with the
-- value(s) you supplied, using the comparison operator. For each /Expected/
-- element, the result of the evaluation is either true or false.
--
-- If you specify more than one element in the /Expected/ map, then by default
-- all of the conditions must evaluate to true. In other words, the conditions
-- are ANDed together. (You can use the /ConditionalOperator/ parameter to OR the
-- conditions instead. If you do this, then at least one of the conditions must
-- evaluate to true, rather than all of them.)
--
-- If the /Expected/ map evaluates to true, then the conditional operation
-- succeeds; otherwise, it fails.
--
-- /Expected/ contains the following:
--
-- /AttributeValueList/ - One or more values to evaluate against the supplied
-- attribute. The number of values in the list depends on the /ComparisonOperator/
-- being used.
--
-- For type Number, value comparisons are numeric.
--
-- String value comparisons for greater than, equals, or less than are based on
-- ASCII character code values. For example, 'a' is greater than 'A', and 'aa' is
-- greater than 'B'. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
-- For type Binary, DynamoDB treats each byte of the binary data as unsigned
-- when it compares binary values, for example when evaluating query expressions.
--
-- /ComparisonOperator/ - A comparator for evaluating attributes in the /AttributeValueList/. When performing the comparison, DynamoDB uses strongly consistent reads.
--
-- The following comparison operators are available:
--
-- 'EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS |BEGINS_WITH | IN | BETWEEN'
--
-- The following are descriptions of each comparison operator.
--
-- 'EQ' : Equal. 'EQ' is supported for all datatypes, including lists and maps.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, Binary, String Set, Number Set, or Binary Set. If an item
-- contains an /AttributeValue/ element of a different type than the one specified
-- in the request, the value does not match. For example, '{"S":"6"}' does not
-- equal '{"N":"6"}'. Also, '{"N":"6"}' does not equal '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'NE' : Not equal. 'NE' is supported for all datatypes, including lists and
-- maps.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String,
-- Number, Binary, String Set, Number Set, or Binary Set. If an item contains an /AttributeValue/ of a different type than the one specified in the request, the
-- value does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not equal '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'LE' : Less than or equal.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one specified in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'LT' : Less than.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String,
-- Number, or Binary (not a set type). If an item contains an /AttributeValue/
-- element of a different type than the one specified in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'GE' : Greater than or equal.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one specified in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'GT' : Greater than.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If an item contains an /AttributeValue/ element of a different type than the one specified in the request, the value
-- does not match. For example, '{"S":"6"}' does not equal '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'.
--
--
--
-- 'NOT_NULL' : The attribute exists. 'NOT_NULL' is supported for all datatypes,
-- including lists and maps.
--
-- 'NULL' : The attribute does not exist. 'NULL' is supported for all datatypes,
-- including lists and maps.
--
-- 'CONTAINS' : Checks for a subsequence, or value in a set.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If the target attribute of the
-- comparison is of type String, then the operator checks for a substring match.
-- If the target attribute of the comparison is of type Binary, then the
-- operator looks for a subsequence of the target that matches the input. If the
-- target attribute of the comparison is a set ("'SS'", "'NS'", or "'BS'"), then the
-- operator evaluates to true if it finds an exact match with any member of the
-- set.
--
-- CONTAINS is supported for lists: When evaluating "'a CONTAINS b'", "'a'" can be
-- a list; however, "'b'" cannot be a set, a map, or a list.
--
-- 'NOT_CONTAINS' : Checks for absence of a subsequence, or absence of a value
-- in a set.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ element of type
-- String, Number, or Binary (not a set type). If the target attribute of the
-- comparison is a String, then the operator checks for the absence of a
-- substring match. If the target attribute of the comparison is Binary, then
-- the operator checks for the absence of a subsequence of the target that
-- matches the input. If the target attribute of the comparison is a set ("'SS'", "'NS'", or "'BS'"), then the operator evaluates to true if it /does not/ find an
-- exact match with any member of the set.
--
-- NOT_CONTAINS is supported for lists: When evaluating "'a NOT CONTAINS b'", "'a'"
-- can be a list; however, "'b'" cannot be a set, a map, or a list.
--
-- 'BEGINS_WITH' : Checks for a prefix.
--
-- /AttributeValueList/ can contain only one /AttributeValue/ of type String or
-- Binary (not a Number or a set type). The target attribute of the comparison
-- must be of type String or Binary (not a Number or a set type).
--
--
--
-- 'IN' : Checks for matching elements within two sets.
--
-- /AttributeValueList/ can contain one or more /AttributeValue/ elements of type
-- String, Number, or Binary (not a set type). These attributes are compared
-- against an existing set type attribute of an item. If any elements of the
-- input set are present in the item attribute, the expression evaluates to true.
--
-- 'BETWEEN' : Greater than or equal to the first value, and less than or equal
-- to the second value.
--
-- /AttributeValueList/ must contain two /AttributeValue/ elements of the same
-- type, either String, Number, or Binary (not a set type). A target attribute
-- matches if the target value is greater than, or equal to, the first element
-- and less than, or equal to, the second element. If an item contains an /AttributeValue/ element of a different type than the one specified in the request, the value
-- does not match. For example, '{"S":"6"}' does not compare to '{"N":"6"}'. Also, '{"N":"6"}' does not compare to '{"NS":["6", "2", "1"]}'
--
-- For usage examples of /AttributeValueList/ and /ComparisonOperator/, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/.
--
-- For backward compatibility with previous DynamoDB releases, the following
-- parameters can be used instead of /AttributeValueList/ and /ComparisonOperator/:
--
-- /Value/ - A value for DynamoDB to compare with an attribute.
--
-- /Exists/ - A Boolean value that causes DynamoDB to evaluate the value before
-- attempting the conditional operation:
--
-- If /Exists/ is 'true', DynamoDB will check to see if that attribute value
-- already exists in the table. If it is found, then the condition evaluates to
-- true; otherwise the condition evaluate to false.
--
-- If /Exists/ is 'false', DynamoDB assumes that the attribute value does /not/
-- exist in the table. If in fact the value does not exist, then the assumption
-- is valid and the condition evaluates to true. If the value is found, despite
-- the assumption that it does not exist, the condition evaluates to false.
--
-- The /Value/ and /Exists/ parameters are incompatible with /AttributeValueList/
-- and /ComparisonOperator/. Note that if you use both sets of parameters at once,
-- DynamoDB will return a /ValidationException/ exception.
--
diExpected :: Lens' DeleteItem (HashMap Text ExpectedAttributeValue)
diExpected = lens _diExpected (\s a -> s { _diExpected = a }) . _Map

-- | One or more substitution tokens for simplifying complex expressions. The
-- following are some use cases for an /ExpressionAttributeNames/ value:
--
-- To shorten an attribute name that is very long or unwieldy in an
-- expression.
--
-- To create a placeholder for repeating occurrences of an attribute name in
-- an expression.
--
-- To prevent special characters in an attribute name from being
-- misinterpreted in an expression.
--
-- Use the # character in an expression to dereference an attribute name. For
-- example, consider the following expression:
--
-- 'order.customerInfo.LastName = "Smith" OR order.customerInfo.LastName ="Jones"'
--
-- Now suppose that you specified the following for /ExpressionAttributeNames/:
--
-- '{"n":"order.customerInfo.LastName"}'
--
-- The expression can now be simplified as follows:
--
-- '#n = "Smith" OR #n = "Jones"'
--
diExpressionAttributeNames :: Lens' DeleteItem (HashMap Text Text)
diExpressionAttributeNames =
    lens _diExpressionAttributeNames
        (\s a -> s { _diExpressionAttributeNames = a })
            . _Map

-- | One or more values that can be substituted in an expression.
--
-- Use the : character in an expression to dereference an attribute value. For
-- example, consider the following expression:
--
-- 'ProductStatus IN ("Available","Backordered","Discontinued")'
--
-- Now suppose that you specified the following for /ExpressionAttributeValues/:
--
-- '{ "a":{"S":"Available"}, "b":{"S":"Backordered"}, "d":{"S":"Discontinued"} }'
--
-- The expression can now be simplified as follows:
--
-- 'ProductStatus IN (:a,:b,:c)'
--
diExpressionAttributeValues :: Lens' DeleteItem (HashMap Text AttributeValue)
diExpressionAttributeValues =
    lens _diExpressionAttributeValues
        (\s a -> s { _diExpressionAttributeValues = a })
            . _Map

-- | A map of attribute names to /AttributeValue/ objects, representing the primary
-- key of the item to delete.
--
-- For the primary key, you must provide all of the attributes. For example,
-- with a hash type primary key, you only need to specify the hash attribute.
-- For a hash-and-range type primary key, you must specify both the hash
-- attribute and the range attribute.
--
diKey :: Lens' DeleteItem (HashMap Text AttributeValue)
diKey = lens _diKey (\s a -> s { _diKey = a }) . _Map

diReturnConsumedCapacity :: Lens' DeleteItem (Maybe ReturnConsumedCapacity)
diReturnConsumedCapacity =
    lens _diReturnConsumedCapacity
        (\s a -> s { _diReturnConsumedCapacity = a })

-- | A value that if set to 'SIZE', the response includes statistics about item
-- collections, if any, that were modified during the operation are returned in
-- the response. If set to 'NONE' (the default), no statistics are returned.
--
diReturnItemCollectionMetrics :: Lens' DeleteItem (Maybe ReturnItemCollectionMetrics)
diReturnItemCollectionMetrics =
    lens _diReturnItemCollectionMetrics
        (\s a -> s { _diReturnItemCollectionMetrics = a })

-- | Use /ReturnValues/ if you want to get the item attributes as they appeared
-- before they were deleted. For /DeleteItem/, the valid values are:
--
-- 'NONE' - If /ReturnValues/ is not specified, or if its value is 'NONE', then
-- nothing is returned. (This setting is the default for /ReturnValues/.)
--
-- 'ALL_OLD' - The content of the old item is returned.
--
--
diReturnValues :: Lens' DeleteItem (Maybe ReturnValue)
diReturnValues = lens _diReturnValues (\s a -> s { _diReturnValues = a })

-- | The name of the table from which to delete the item.
--
diTableName :: Lens' DeleteItem Text
diTableName = lens _diTableName (\s a -> s { _diTableName = a })

data DeleteItemResponse = DeleteItemResponse
    { _dirAttributes            :: Map Text AttributeValue
    , _dirConsumedCapacity      :: Maybe ConsumedCapacity
    , _dirItemCollectionMetrics :: Maybe ItemCollectionMetrics
    } deriving (Eq, Show)

-- | 'DeleteItemResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirAttributes' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'dirConsumedCapacity' @::@ 'Maybe' 'ConsumedCapacity'
--
-- * 'dirItemCollectionMetrics' @::@ 'Maybe' 'ItemCollectionMetrics'
--
deleteItemResponse :: DeleteItemResponse
deleteItemResponse = DeleteItemResponse
    { _dirAttributes            = mempty
    , _dirConsumedCapacity      = Nothing
    , _dirItemCollectionMetrics = Nothing
    }

-- | A map of attribute names to /AttributeValue/ objects, representing the item as
-- it appeared before the /DeleteItem/ operation. This map appears in the response
-- only if /ReturnValues/ was specified as 'ALL_OLD' in the request.
--
dirAttributes :: Lens' DeleteItemResponse (HashMap Text AttributeValue)
dirAttributes = lens _dirAttributes (\s a -> s { _dirAttributes = a }) . _Map

dirConsumedCapacity :: Lens' DeleteItemResponse (Maybe ConsumedCapacity)
dirConsumedCapacity =
    lens _dirConsumedCapacity (\s a -> s { _dirConsumedCapacity = a })

-- | Information about item collections, if any, that were affected by the
-- operation. /ItemCollectionMetrics/ is only returned if the request asked for
-- it. If the table does not have any local secondary indexes, this information
-- is not returned in the response.
--
-- Each /ItemCollectionMetrics/ element consists of:
--
-- /ItemCollectionKey/ - The hash key value of the item collection. This is the
-- same as the hash key of the item.
--
-- /SizeEstimateRange/ - An estimate of item collection size, in gigabytes. This
-- value is a two-element array containing a lower bound and an upper bound for
-- the estimate. The estimate includes the size of all the items in the table,
-- plus the size of all attributes projected into all of the local secondary
-- indexes on that table. Use this estimate to measure whether a local secondary
-- index is approaching its size limit.
--
-- The estimate is subject to change over time; therefore, do not rely on the
-- precision or accuracy of the estimate.
--
--
dirItemCollectionMetrics :: Lens' DeleteItemResponse (Maybe ItemCollectionMetrics)
dirItemCollectionMetrics =
    lens _dirItemCollectionMetrics
        (\s a -> s { _dirItemCollectionMetrics = a })

instance ToPath DeleteItem where
    toPath = const "/"

instance ToQuery DeleteItem where
    toQuery = const mempty

instance ToHeaders DeleteItem

instance ToJSON DeleteItem where
    toJSON DeleteItem{..} = object
        [ "TableName"                   .= _diTableName
        , "Key"                         .= _diKey
        , "Expected"                    .= _diExpected
        , "ConditionalOperator"         .= _diConditionalOperator
        , "ReturnValues"                .= _diReturnValues
        , "ReturnConsumedCapacity"      .= _diReturnConsumedCapacity
        , "ReturnItemCollectionMetrics" .= _diReturnItemCollectionMetrics
        , "ConditionExpression"         .= _diConditionExpression
        , "ExpressionAttributeNames"    .= _diExpressionAttributeNames
        , "ExpressionAttributeValues"   .= _diExpressionAttributeValues
        ]

instance AWSRequest DeleteItem where
    type Sv DeleteItem = DynamoDB
    type Rs DeleteItem = DeleteItemResponse

    request  = post "DeleteItem"
    response = jsonResponse

instance FromJSON DeleteItemResponse where
    parseJSON = withObject "DeleteItemResponse" $ \o -> DeleteItemResponse
        <$> o .:  "Attributes"
        <*> o .:? "ConsumedCapacity"
        <*> o .:? "ItemCollectionMetrics"
