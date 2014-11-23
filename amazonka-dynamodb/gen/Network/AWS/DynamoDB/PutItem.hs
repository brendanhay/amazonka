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

-- Module      : Network.AWS.DynamoDB.PutItem
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new item, or replaces an old item with a new item. If an item
-- that has the same primary key as the new item already exists in the
-- specified table, the new item completely replaces the existing item. You
-- can perform a conditional put operation (add a new item if one with the
-- specified primary key doesn't exist), or replace an existing item if it has
-- certain attribute values. In addition to putting an item, you can also
-- return the item's attribute values in the same operation, using the
-- ReturnValues parameter. When you add an item, the primary key attribute(s)
-- are the only required attributes. Attribute values cannot be null. String
-- and Binary type attributes must have lengths greater than zero. Set type
-- attributes cannot be empty. Requests with empty values will be rejected
-- with a ValidationException exception. You can request that PutItem return
-- either a copy of the original item (before the update) or a copy of the
-- updated item (after the update). For more information, see the ReturnValues
-- description below. For more information about using this API, see Working
-- with Items in the Amazon DynamoDB Developer Guide.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_PutItem.html>
module Network.AWS.DynamoDB.PutItem
    (
    -- * Request
      PutItem
    -- ** Request constructor
    , putItem
    -- ** Request lenses
    , piConditionExpression
    , piConditionalOperator
    , piExpected
    , piExpressionAttributeNames
    , piExpressionAttributeValues
    , piItem
    , piReturnConsumedCapacity
    , piReturnItemCollectionMetrics
    , piReturnValues
    , piTableName

    -- * Response
    , PutItemResponse
    -- ** Response constructor
    , putItemResponse
    -- ** Response lenses
    , pirAttributes
    , pirConsumedCapacity
    , pirItemCollectionMetrics
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.Types
import qualified GHC.Exts

data PutItem = PutItem
    { _piConditionExpression         :: Maybe Text
    , _piConditionalOperator         :: Maybe Text
    , _piExpected                    :: Map Text ExpectedAttributeValue
    , _piExpressionAttributeNames    :: Map Text Text
    , _piExpressionAttributeValues   :: Map Text AttributeValue
    , _piItem                        :: Map Text AttributeValue
    , _piReturnConsumedCapacity      :: Maybe Text
    , _piReturnItemCollectionMetrics :: Maybe Text
    , _piReturnValues                :: Maybe Text
    , _piTableName                   :: Text
    } deriving (Eq, Show)

-- | 'PutItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'piConditionExpression' @::@ 'Maybe' 'Text'
--
-- * 'piConditionalOperator' @::@ 'Maybe' 'Text'
--
-- * 'piExpected' @::@ 'HashMap' 'Text' 'ExpectedAttributeValue'
--
-- * 'piExpressionAttributeNames' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'piExpressionAttributeValues' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'piItem' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'piReturnConsumedCapacity' @::@ 'Maybe' 'Text'
--
-- * 'piReturnItemCollectionMetrics' @::@ 'Maybe' 'Text'
--
-- * 'piReturnValues' @::@ 'Maybe' 'Text'
--
-- * 'piTableName' @::@ 'Text'
--
putItem :: Text -- ^ 'piTableName'
        -> PutItem
putItem p1 = PutItem
    { _piTableName                   = p1
    , _piItem                        = mempty
    , _piExpected                    = mempty
    , _piReturnValues                = Nothing
    , _piReturnConsumedCapacity      = Nothing
    , _piReturnItemCollectionMetrics = Nothing
    , _piConditionalOperator         = Nothing
    , _piConditionExpression         = Nothing
    , _piExpressionAttributeNames    = mempty
    , _piExpressionAttributeValues   = mempty
    }

-- | A condition that must be satisfied in order for a conditional PutItem
-- operation to succeed. An expression can contain any of the following:
-- Boolean functions: ATTRIBUTE_EXIST | CONTAINS | BEGINS_WITH Comparison
-- operators: = | &#x3C;&#x3E; | &#x3C; | &#x3E; | &#x3C;= | &#x3E;= |
-- BETWEEN | IN Logical operators: NOT | AND | OR.
piConditionExpression :: Lens' PutItem (Maybe Text)
piConditionExpression =
    lens _piConditionExpression (\s a -> s { _piConditionExpression = a })

-- | There is a newer parameter available. Use ConditionExpression instead.
-- Note that if you use ConditionalOperator and ConditionExpression at the
-- same time, DynamoDB will return a ValidationException exception. This
-- parameter does not support lists or maps. A logical operator to apply to
-- the conditions in the Expected map: AND - If all of the conditions
-- evaluate to true, then the entire map evaluates to true. OR - If at least
-- one of the conditions evaluate to true, then the entire map evaluates to
-- true. If you omit ConditionalOperator, then AND is the default. The
-- operation will succeed only if the entire map evaluates to true.
piConditionalOperator :: Lens' PutItem (Maybe Text)
piConditionalOperator =
    lens _piConditionalOperator (\s a -> s { _piConditionalOperator = a })

-- | There is a newer parameter available. Use ConditionExpression instead.
-- Note that if you use Expected and ConditionExpression at the same time,
-- DynamoDB will return a ValidationException exception. This parameter does
-- not support lists or maps. A map of attribute/condition pairs. Expected
-- provides a conditional block for the PutItem operation. Each element of
-- Expected consists of an attribute name, a comparison operator, and one or
-- more values. DynamoDB compares the attribute with the value(s) you
-- supplied, using the comparison operator. For each Expected element, the
-- result of the evaluation is either true or false. If you specify more
-- than one element in the Expected map, then by default all of the
-- conditions must evaluate to true. In other words, the conditions are
-- ANDed together. (You can use the ConditionalOperator parameter to OR the
-- conditions instead. If you do this, then at least one of the conditions
-- must evaluate to true, rather than all of them.) If the Expected map
-- evaluates to true, then the conditional operation succeeds; otherwise, it
-- fails. Expected contains the following: AttributeValueList - One or more
-- values to evaluate against the supplied attribute. The number of values
-- in the list depends on the ComparisonOperator being used. For type
-- Number, value comparisons are numeric. String value comparisons for
-- greater than, equals, or less than are based on ASCII character code
-- values. For example, a is greater than A, and aa is greater than B. For a
-- list of code values, see
-- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters. For type
-- Binary, DynamoDB treats each byte of the binary data as unsigned when it
-- compares binary values, for example when evaluating query expressions.
-- ComparisonOperator - A comparator for evaluating attributes in the
-- AttributeValueList. When performing the comparison, DynamoDB uses
-- strongly consistent reads. The following comparison operators are
-- available: EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS |
-- NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN The following are descriptions
-- of each comparison operator. EQ : Equal. EQ is supported for all
-- datatypes, including lists and maps. AttributeValueList can contain only
-- one AttributeValue element of type String, Number, Binary, String Set,
-- Number Set, or Binary Set. If an item contains an AttributeValue element
-- of a different type than the one specified in the request, the value does
-- not match. For example, {"S":"6"} does not equal {"N":"6"}. Also,
-- {"N":"6"} does not equal {"NS":["6", "2", "1"]}. NE : Not equal. NE is
-- supported for all datatypes, including lists and maps. AttributeValueList
-- can contain only one AttributeValue of type String, Number, Binary,
-- String Set, Number Set, or Binary Set. If an item contains an
-- AttributeValue of a different type than the one specified in the request,
-- the value does not match. For example, {"S":"6"} does not equal
-- {"N":"6"}. Also, {"N":"6"} does not equal {"NS":["6", "2", "1"]}. LE :
-- Less than or equal. AttributeValueList can contain only one
-- AttributeValue element of type String, Number, or Binary (not a set
-- type). If an item contains an AttributeValue element of a different type
-- than the one specified in the request, the value does not match. For
-- example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not
-- compare to {"NS":["6", "2", "1"]}. LT : Less than. AttributeValueList can
-- contain only one AttributeValue of type String, Number, or Binary (not a
-- set type). If an item contains an AttributeValue element of a different
-- type than the one specified in the request, the value does not match. For
-- example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not
-- compare to {"NS":["6", "2", "1"]}. GE : Greater than or equal.
-- AttributeValueList can contain only one AttributeValue element of type
-- String, Number, or Binary (not a set type). If an item contains an
-- AttributeValue element of a different type than the one specified in the
-- request, the value does not match. For example, {"S":"6"} does not equal
-- {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2", "1"]}. GT
-- : Greater than. AttributeValueList can contain only one AttributeValue
-- element of type String, Number, or Binary (not a set type). If an item
-- contains an AttributeValue element of a different type than the one
-- specified in the request, the value does not match. For example,
-- {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not compare to
-- {"NS":["6", "2", "1"]}. NOT_NULL : The attribute exists. NOT_NULL is
-- supported for all datatypes, including lists and maps. NULL : The
-- attribute does not exist. NULL is supported for all datatypes, including
-- lists and maps. CONTAINS : Checks for a subsequence, or value in a set.
-- AttributeValueList can contain only one AttributeValue element of type
-- String, Number, or Binary (not a set type). If the target attribute of
-- the comparison is of type String, then the operator checks for a
-- substring match. If the target attribute of the comparison is of type
-- Binary, then the operator looks for a subsequence of the target that
-- matches the input. If the target attribute of the comparison is a set
-- ("SS", "NS", or "BS"), then the operator evaluates to true if it finds an
-- exact match with any member of the set. CONTAINS is supported for lists:
-- When evaluating "a CONTAINS b", "a" can be a list; however, "b" cannot be
-- a set, a map, or a list. NOT_CONTAINS : Checks for absence of a
-- subsequence, or absence of a value in a set. AttributeValueList can
-- contain only one AttributeValue element of type String, Number, or Binary
-- (not a set type). If the target attribute of the comparison is a String,
-- then the operator checks for the absence of a substring match. If the
-- target attribute of the comparison is Binary, then the operator checks
-- for the absence of a subsequence of the target that matches the input. If
-- the target attribute of the comparison is a set ("SS", "NS", or "BS"),
-- then the operator evaluates to true if it does not find an exact match
-- with any member of the set. NOT_CONTAINS is supported for lists: When
-- evaluating "a NOT CONTAINS b", "a" can be a list; however, "b" cannot be
-- a set, a map, or a list. BEGINS_WITH : Checks for a prefix.
-- AttributeValueList can contain only one AttributeValue of type String or
-- Binary (not a Number or a set type). The target attribute of the
-- comparison must be of type String or Binary (not a Number or a set type).
-- IN : Checks for matching elements within two sets. AttributeValueList can
-- contain one or more AttributeValue elements of type String, Number, or
-- Binary (not a set type). These attributes are compared against an
-- existing set type attribute of an item. If any elements of the input set
-- are present in the item attribute, the expression evaluates to true.
-- BETWEEN : Greater than or equal to the first value, and less than or
-- equal to the second value. AttributeValueList must contain two
-- AttributeValue elements of the same type, either String, Number, or
-- Binary (not a set type). A target attribute matches if the target value
-- is greater than, or equal to, the first element and less than, or equal
-- to, the second element. If an item contains an AttributeValue element of
-- a different type than the one specified in the request, the value does
-- not match. For example, {"S":"6"} does not compare to {"N":"6"}. Also,
-- {"N":"6"} does not compare to {"NS":["6", "2", "1"]} For usage examples
-- of AttributeValueList and ComparisonOperator, see Legacy Conditional
-- Parameters in the Amazon DynamoDB Developer Guide. For backward
-- compatibility with previous DynamoDB releases, the following parameters
-- can be used instead of AttributeValueList and ComparisonOperator: Value -
-- A value for DynamoDB to compare with an attribute. Exists - A Boolean
-- value that causes DynamoDB to evaluate the value before attempting the
-- conditional operation: If Exists is true, DynamoDB will check to see if
-- that attribute value already exists in the table. If it is found, then
-- the condition evaluates to true; otherwise the condition evaluate to
-- false. If Exists is false, DynamoDB assumes that the attribute value does
-- not exist in the table. If in fact the value does not exist, then the
-- assumption is valid and the condition evaluates to true. If the value is
-- found, despite the assumption that it does not exist, the condition
-- evaluates to false. The Value and Exists parameters are incompatible with
-- AttributeValueList and ComparisonOperator. Note that if you use both sets
-- of parameters at once, DynamoDB will return a ValidationException
-- exception.
piExpected :: Lens' PutItem (HashMap Text ExpectedAttributeValue)
piExpected = lens _piExpected (\s a -> s { _piExpected = a }) . _Map

-- | One or more substitution tokens for simplifying complex expressions. The
-- following are some use cases for an ExpressionAttributeNames value: To
-- shorten an attribute name that is very long or unwieldy in an expression.
-- To create a placeholder for repeating occurrences of an attribute name in
-- an expression. To prevent special characters in an attribute name from
-- being misinterpreted in an expression. Use the # character in an
-- expression to dereference an attribute name. For example, consider the
-- following expression: order.customerInfo.LastName = "Smith" OR
-- order.customerInfo.LastName = "Jones" Now suppose that you specified the
-- following for ExpressionAttributeNames:
-- {"n":"order.customerInfo.LastName"} The expression can now be simplified
-- as follows: #n = "Smith" OR #n = "Jones".
piExpressionAttributeNames :: Lens' PutItem (HashMap Text Text)
piExpressionAttributeNames =
    lens _piExpressionAttributeNames
        (\s a -> s { _piExpressionAttributeNames = a })
            . _Map

-- | One or more values that can be substituted in an expression. Use the :
-- character in an expression to dereference an attribute value. For
-- example, consider the following expression: ProductStatus IN
-- ("Available","Backordered","Discontinued") Now suppose that you specified
-- the following for ExpressionAttributeValues: { "a":{"S":"Available"},
-- "b":{"S":"Backordered"}, "d":{"S":"Discontinued"} } The expression can
-- now be simplified as follows: ProductStatus IN (:a,:b,:c).
piExpressionAttributeValues :: Lens' PutItem (HashMap Text AttributeValue)
piExpressionAttributeValues =
    lens _piExpressionAttributeValues
        (\s a -> s { _piExpressionAttributeValues = a })
            . _Map

-- | A map of attribute name/value pairs, one for each attribute. Only the
-- primary key attributes are required; you can optionally provide other
-- attribute name-value pairs for the item. You must provide all of the
-- attributes for the primary key. For example, with a hash type primary
-- key, you only need to specify the hash attribute. For a hash-and-range
-- type primary key, you must specify both the hash attribute and the range
-- attribute. If you specify any attributes that are part of an index key,
-- then the data types for those attributes must match those of the schema
-- in the table's attribute definition. For more information about primary
-- keys, see Primary Key in the Amazon DynamoDB Developer Guide. Each
-- element in the Item map is an AttributeValue object.
piItem :: Lens' PutItem (HashMap Text AttributeValue)
piItem = lens _piItem (\s a -> s { _piItem = a }) . _Map

piReturnConsumedCapacity :: Lens' PutItem (Maybe Text)
piReturnConsumedCapacity =
    lens _piReturnConsumedCapacity
        (\s a -> s { _piReturnConsumedCapacity = a })

-- | A value that if set to SIZE, the response includes statistics about item
-- collections, if any, that were modified during the operation are returned
-- in the response. If set to NONE (the default), no statistics are
-- returned.
piReturnItemCollectionMetrics :: Lens' PutItem (Maybe Text)
piReturnItemCollectionMetrics =
    lens _piReturnItemCollectionMetrics
        (\s a -> s { _piReturnItemCollectionMetrics = a })

-- | Use ReturnValues if you want to get the item attributes as they appeared
-- before they were updated with the PutItem request. For PutItem, the valid
-- values are: NONE - If ReturnValues is not specified, or if its value is
-- NONE, then nothing is returned. (This setting is the default for
-- ReturnValues.) ALL_OLD - If PutItem overwrote an attribute name-value
-- pair, then the content of the old item is returned.
piReturnValues :: Lens' PutItem (Maybe Text)
piReturnValues = lens _piReturnValues (\s a -> s { _piReturnValues = a })

-- | The name of the table to contain the item.
piTableName :: Lens' PutItem Text
piTableName = lens _piTableName (\s a -> s { _piTableName = a })

data PutItemResponse = PutItemResponse
    { _pirAttributes            :: Map Text AttributeValue
    , _pirConsumedCapacity      :: Maybe ConsumedCapacity
    , _pirItemCollectionMetrics :: Maybe ItemCollectionMetrics
    } deriving (Eq, Show)

-- | 'PutItemResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pirAttributes' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'pirConsumedCapacity' @::@ 'Maybe' 'ConsumedCapacity'
--
-- * 'pirItemCollectionMetrics' @::@ 'Maybe' 'ItemCollectionMetrics'
--
putItemResponse :: PutItemResponse
putItemResponse = PutItemResponse
    { _pirAttributes            = mempty
    , _pirConsumedCapacity      = Nothing
    , _pirItemCollectionMetrics = Nothing
    }

-- | The attribute values as they appeared before the PutItem operation, but
-- only if ReturnValues is specified as ALL_OLD in the request. Each element
-- consists of an attribute name and an attribute value.
pirAttributes :: Lens' PutItemResponse (HashMap Text AttributeValue)
pirAttributes = lens _pirAttributes (\s a -> s { _pirAttributes = a }) . _Map

pirConsumedCapacity :: Lens' PutItemResponse (Maybe ConsumedCapacity)
pirConsumedCapacity =
    lens _pirConsumedCapacity (\s a -> s { _pirConsumedCapacity = a })

-- | Information about item collections, if any, that were affected by the
-- operation. ItemCollectionMetrics is only returned if the request asked
-- for it. If the table does not have any local secondary indexes, this
-- information is not returned in the response. Each ItemCollectionMetrics
-- element consists of: ItemCollectionKey - The hash key value of the item
-- collection. This is the same as the hash key of the item.
-- SizeEstimateRange - An estimate of item collection size, in gigabytes.
-- This value is a two-element array containing a lower bound and an upper
-- bound for the estimate. The estimate includes the size of all the items
-- in the table, plus the size of all attributes projected into all of the
-- local secondary indexes on that table. Use this estimate to measure
-- whether a local secondary index is approaching its size limit. The
-- estimate is subject to change over time; therefore, do not rely on the
-- precision or accuracy of the estimate.
pirItemCollectionMetrics :: Lens' PutItemResponse (Maybe ItemCollectionMetrics)
pirItemCollectionMetrics =
    lens _pirItemCollectionMetrics
        (\s a -> s { _pirItemCollectionMetrics = a })

instance ToPath PutItem where
    toPath = const "/"

instance ToQuery PutItem where
    toQuery = const mempty

instance ToHeaders PutItem

instance ToJSON PutItem where
    toJSON PutItem{..} = object
        [ "TableName"                   .= _piTableName
        , "Item"                        .= _piItem
        , "Expected"                    .= _piExpected
        , "ReturnValues"                .= _piReturnValues
        , "ReturnConsumedCapacity"      .= _piReturnConsumedCapacity
        , "ReturnItemCollectionMetrics" .= _piReturnItemCollectionMetrics
        , "ConditionalOperator"         .= _piConditionalOperator
        , "ConditionExpression"         .= _piConditionExpression
        , "ExpressionAttributeNames"    .= _piExpressionAttributeNames
        , "ExpressionAttributeValues"   .= _piExpressionAttributeValues
        ]

instance AWSRequest PutItem where
    type Sv PutItem = DynamoDB
    type Rs PutItem = PutItemResponse

    request  = post "PutItem"
    response = jsonResponse

instance FromJSON PutItemResponse where
    parseJSON = withObject "PutItemResponse" $ \o -> PutItemResponse
        <$> o .:? "Attributes"
        <*> o .:? "ConsumedCapacity"
        <*> o .:? "ItemCollectionMetrics"
