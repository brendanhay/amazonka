{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.Query
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | A Query operation directly accesses items from a table using the table
-- primary key, or from an index using the index key. You must provide a
-- specific hash key value. You can narrow the scope of the query by using
-- comparison operators on the range key value, or on the index key. You can
-- use the ScanIndexForward parameter to get results in forward or reverse
-- order, by range key or by index key. Queries that do not return results
-- consume the minimum number of read capacity units for that type of read
-- operation. If the total number of items meeting the query criteria exceeds
-- the result set size limit of 1 MB, the query stops and results are returned
-- to the user with LastEvaluatedKey to continue the query in a subsequent
-- operation. Unlike a Scan operation, a Query operation never returns both an
-- empty result set and a LastEvaluatedKey. The LastEvaluatedKey is only
-- provided if the results exceed 1 MB, or if you have used Limit. You can
-- query a table, a local secondary index, or a global secondary index. For a
-- query on a table or on a local secondary index, you can set ConsistentRead
-- to true and obtain a strongly consistent result. Global secondary indexes
-- support eventually consistent reads only, so do not specify ConsistentRead
-- when querying a global secondary index.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Query.html>
module Network.AWS.DynamoDB.Query
    (
    -- * Request
      Query
    -- ** Request constructor
    , query
    -- ** Request lenses
    , qAttributesToGet
    , qConditionalOperator
    , qConsistentRead
    , qExclusiveStartKey
    , qExpressionAttributeNames
    , qExpressionAttributeValues
    , qFilterExpression
    , qIndexName
    , qKeyConditions
    , qLimit
    , qProjectionExpression
    , qQueryFilter
    , qReturnConsumedCapacity
    , qScanIndexForward
    , qSelect
    , qTableName

    -- * Response
    , QueryResponse
    -- ** Response constructor
    , queryResponse
    -- ** Response lenses
    , qrConsumedCapacity
    , qrCount
    , qrItems
    , qrLastEvaluatedKey
    , qrScannedCount
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.Types
import qualified GHC.Exts

data Query = Query
    { _qAttributesToGet           :: List1 Text
    , _qConditionalOperator       :: Maybe Text
    , _qConsistentRead            :: Maybe Bool
    , _qExclusiveStartKey         :: Map Text AttributeValue
    , _qExpressionAttributeNames  :: Map Text Text
    , _qExpressionAttributeValues :: Map Text AttributeValue
    , _qFilterExpression          :: Maybe Text
    , _qIndexName                 :: Maybe Text
    , _qKeyConditions             :: Map Text Condition
    , _qLimit                     :: Maybe Nat
    , _qProjectionExpression      :: Maybe Text
    , _qQueryFilter               :: Map Text Condition
    , _qReturnConsumedCapacity    :: Maybe Text
    , _qScanIndexForward          :: Maybe Bool
    , _qSelect                    :: Maybe Text
    , _qTableName                 :: Text
    } deriving (Eq, Show, Generic)

-- | 'Query' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qAttributesToGet' @::@ 'NonEmpty' 'Text'
--
-- * 'qConditionalOperator' @::@ 'Maybe' 'Text'
--
-- * 'qConsistentRead' @::@ 'Maybe' 'Bool'
--
-- * 'qExclusiveStartKey' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'qExpressionAttributeNames' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'qExpressionAttributeValues' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'qFilterExpression' @::@ 'Maybe' 'Text'
--
-- * 'qIndexName' @::@ 'Maybe' 'Text'
--
-- * 'qKeyConditions' @::@ 'HashMap' 'Text' 'Condition'
--
-- * 'qLimit' @::@ 'Maybe' 'Natural'
--
-- * 'qProjectionExpression' @::@ 'Maybe' 'Text'
--
-- * 'qQueryFilter' @::@ 'HashMap' 'Text' 'Condition'
--
-- * 'qReturnConsumedCapacity' @::@ 'Maybe' 'Text'
--
-- * 'qScanIndexForward' @::@ 'Maybe' 'Bool'
--
-- * 'qSelect' @::@ 'Maybe' 'Text'
--
-- * 'qTableName' @::@ 'Text'
--
query :: Text -- ^ 'qTableName'
      -> NonEmpty Text -- ^ 'qAttributesToGet'
      -> Query
query p1 p2 = Query
    { _qTableName                 = p1
    , _qAttributesToGet           = withIso _List1 (const id) p2
    , _qIndexName                 = Nothing
    , _qSelect                    = Nothing
    , _qLimit                     = Nothing
    , _qConsistentRead            = Nothing
    , _qKeyConditions             = mempty
    , _qQueryFilter               = mempty
    , _qConditionalOperator       = Nothing
    , _qScanIndexForward          = Nothing
    , _qExclusiveStartKey         = mempty
    , _qReturnConsumedCapacity    = Nothing
    , _qProjectionExpression      = Nothing
    , _qFilterExpression          = Nothing
    , _qExpressionAttributeNames  = mempty
    , _qExpressionAttributeValues = mempty
    }

-- | There is a newer parameter available. Use ProjectionExpression instead.
-- Note that if you use AttributesToGet and ProjectionExpression at the same
-- time, DynamoDB will return a ValidationException exception. This
-- parameter allows you to retrieve lists or maps; however, it cannot
-- retrieve individual list or map elements. The names of one or more
-- attributes to retrieve. If no attribute names are specified, then all
-- attributes will be returned. If any of the requested attributes are not
-- found, they will not appear in the result. Note that AttributesToGet has
-- no effect on provisioned throughput consumption. DynamoDB determines
-- capacity units consumed based on item size, not on the amount of data
-- that is returned to an application. You cannot use both AttributesToGet
-- and Select together in a Query request, unless the value for Select is
-- SPECIFIC_ATTRIBUTES. (This usage is equivalent to specifying
-- AttributesToGet without any value for Select.) If you query a local
-- secondary index and request only attributes that are projected into that
-- index, the operation will read only the index and not the table. If any
-- of the requested attributes are not projected into the local secondary
-- index, DynamoDB will fetch each of these attributes from the parent
-- table. This extra fetching incurs additional throughput cost and latency.
-- If you query a global secondary index, you can only request attributes
-- that are projected into the index. Global secondary index queries cannot
-- fetch attributes from the parent table.
qAttributesToGet :: Lens' Query (NonEmpty Text)
qAttributesToGet = lens _qAttributesToGet (\s a -> s { _qAttributesToGet = a })
    . _List1

-- | There is a newer parameter available. Use ConditionExpression instead.
-- Note that if you use ConditionalOperator and ConditionExpression at the
-- same time, DynamoDB will return a ValidationException exception. This
-- parameter does not support lists or maps. A logical operator to apply to
-- the conditions in the QueryFilter map: AND - If all of the conditions
-- evaluate to true, then the entire map evaluates to true. OR - If at least
-- one of the conditions evaluate to true, then the entire map evaluates to
-- true. If you omit ConditionalOperator, then AND is the default. The
-- operation will succeed only if the entire map evaluates to true.
qConditionalOperator :: Lens' Query (Maybe Text)
qConditionalOperator =
    lens _qConditionalOperator (\s a -> s { _qConditionalOperator = a })

-- | A value that if set to true, then the operation uses strongly consistent
-- reads; otherwise, eventually consistent reads are used. Strongly
-- consistent reads are not supported on global secondary indexes. If you
-- query a global secondary index with ConsistentRead set to true, you will
-- receive an error message.
qConsistentRead :: Lens' Query (Maybe Bool)
qConsistentRead = lens _qConsistentRead (\s a -> s { _qConsistentRead = a })

-- | The primary key of the first item that this operation will evaluate. Use
-- the value that was returned for LastEvaluatedKey in the previous
-- operation. The data type for ExclusiveStartKey must be String, Number or
-- Binary. No set data types are allowed.
qExclusiveStartKey :: Lens' Query (HashMap Text AttributeValue)
qExclusiveStartKey =
    lens _qExclusiveStartKey (\s a -> s { _qExclusiveStartKey = a })
        . _Map

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
qExpressionAttributeNames :: Lens' Query (HashMap Text Text)
qExpressionAttributeNames =
    lens _qExpressionAttributeNames
        (\s a -> s { _qExpressionAttributeNames = a })
            . _Map

-- | One or more values that can be substituted in an expression. Use the :
-- character in an expression to dereference an attribute value. For
-- example, consider the following expression: ProductStatus IN
-- ("Available","Backordered","Discontinued") Now suppose that you specified
-- the following for ExpressionAttributeValues: { "a":{"S":"Available"},
-- "b":{"S":"Backordered"}, "d":{"S":"Discontinued"} } The expression can
-- now be simplified as follows: ProductStatus IN (:a,:b,:c).
qExpressionAttributeValues :: Lens' Query (HashMap Text AttributeValue)
qExpressionAttributeValues =
    lens _qExpressionAttributeValues
        (\s a -> s { _qExpressionAttributeValues = a })
            . _Map

-- | A condition that evaluates the query results and returns only the desired
-- values. The condition you specify is applied to the items queried; any
-- items that do not match the expression are not returned.
qFilterExpression :: Lens' Query (Maybe Text)
qFilterExpression =
    lens _qFilterExpression (\s a -> s { _qFilterExpression = a })

-- | The name of an index to query. This index can be any local secondary
-- index or global secondary index on the table.
qIndexName :: Lens' Query (Maybe Text)
qIndexName = lens _qIndexName (\s a -> s { _qIndexName = a })

-- | The selection criteria for the query. For a query on a table, you can
-- have conditions only on the table primary key attributes. You must
-- specify the hash key attribute name and value as an EQ condition. You can
-- optionally specify a second condition, referring to the range key
-- attribute. For a query on an index, you can have conditions only on the
-- index key attributes. You must specify the index hash attribute name and
-- value as an EQ condition. You can optionally specify a second condition,
-- referring to the index key range attribute. Each KeyConditions element
-- consists of an attribute name to compare, along with the following:
-- AttributeValueList - One or more values to evaluate against the supplied
-- attribute. The number of values in the list depends on the
-- ComparisonOperator being used. For type Number, value comparisons are
-- numeric. String value comparisons for greater than, equals, or less than
-- are based on ASCII character code values. For example, a is greater than
-- A, and aa is greater than B. For a list of code values, see
-- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters. For
-- Binary, DynamoDB treats each byte of the binary data as unsigned when it
-- compares binary values, for example when evaluating query expressions.
-- ComparisonOperator - A comparator for evaluating attributes, for example,
-- equals, greater than, less than, and so on. For KeyConditions, only the
-- following comparison operators are supported: EQ | LE | LT | GE | GT |
-- BEGINS_WITH | BETWEEN The following are descriptions of these comparison
-- operators. EQ : Equal. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set type). If an
-- item contains an AttributeValue element of a different type than the one
-- specified in the request, the value does not match. For example,
-- {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not equal
-- {"NS":["6", "2", "1"]}. LE : Less than or equal. AttributeValueList can
-- contain only one AttributeValue element of type String, Number, or Binary
-- (not a set type). If an item contains an AttributeValue element of a
-- different type than the one specified in the request, the value does not
-- match. For example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"}
-- does not compare to {"NS":["6", "2", "1"]}. LT : Less than.
-- AttributeValueList can contain only one AttributeValue of type String,
-- Number, or Binary (not a set type). If an item contains an AttributeValue
-- element of a different type than the one specified in the request, the
-- value does not match. For example, {"S":"6"} does not equal {"N":"6"}.
-- Also, {"N":"6"} does not compare to {"NS":["6", "2", "1"]}. GE : Greater
-- than or equal. AttributeValueList can contain only one AttributeValue
-- element of type String, Number, or Binary (not a set type). If an item
-- contains an AttributeValue element of a different type than the one
-- specified in the request, the value does not match. For example,
-- {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not compare to
-- {"NS":["6", "2", "1"]}. GT : Greater than. AttributeValueList can contain
-- only one AttributeValue element of type String, Number, or Binary (not a
-- set type). If an item contains an AttributeValue element of a different
-- type than the one specified in the request, the value does not match. For
-- example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does not
-- compare to {"NS":["6", "2", "1"]}. BEGINS_WITH : Checks for a prefix.
-- AttributeValueList can contain only one AttributeValue of type String or
-- Binary (not a Number or a set type). The target attribute of the
-- comparison must be of type String or Binary (not a Number or a set type).
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
-- Parameters in the Amazon DynamoDB Developer Guide.
qKeyConditions :: Lens' Query (HashMap Text Condition)
qKeyConditions = lens _qKeyConditions (\s a -> s { _qKeyConditions = a })
    . _Map

-- | The maximum number of items to evaluate (not necessarily the number of
-- matching items). If DynamoDB processes the number of items up to the
-- limit while processing the results, it stops the operation and returns
-- the matching values up to that point, and a key in LastEvaluatedKey to
-- apply in a subsequent operation, so that you can pick up where you left
-- off. Also, if the processed data set size exceeds 1 MB before DynamoDB
-- reaches this limit, it stops the operation and returns the matching
-- values up to the limit, and a key in LastEvaluatedKey to apply in a
-- subsequent operation to continue the operation. For more information, see
-- Query and Scan in the Amazon DynamoDB Developer Guide.
qLimit :: Lens' Query (Maybe Natural)
qLimit = lens _qLimit (\s a -> s { _qLimit = a })
    . mapping _Nat

-- | One or more attributes to retrieve from the table. These attributes can
-- include scalars, sets, or elements of a JSON document. The attributes in
-- the expression must be separated by commas. If no attribute names are
-- specified, then all attributes will be returned. If any of the requested
-- attributes are not found, they will not appear in the result.
qProjectionExpression :: Lens' Query (Maybe Text)
qProjectionExpression =
    lens _qProjectionExpression (\s a -> s { _qProjectionExpression = a })

-- | There is a newer parameter available. Use FilterExpression instead. Note
-- that if you use QueryFilter and FilterExpression at the same time,
-- DynamoDB will return a ValidationException exception. This parameter does
-- not support lists or maps. A condition that evaluates the query results
-- and returns only the desired values. If you specify more than one
-- condition in the QueryFilter map, then by default all of the conditions
-- must evaluate to true. In other words, the conditions are ANDed together.
-- (You can use the ConditionalOperator parameter to OR the conditions
-- instead. If you do this, then at least one of the conditions must
-- evaluate to true, rather than all of them.) Each QueryFilter element
-- consists of an attribute name to compare, along with the following:
-- AttributeValueList - One or more values to evaluate against the supplied
-- attribute. The number of values in the list depends on the operator
-- specified in ComparisonOperator. For type Number, value comparisons are
-- numeric. String value comparisons for greater than, equals, or less than
-- are based on ASCII character code values. For example, a is greater than
-- A, and aa is greater than B. For a list of code values, see
-- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters. For type
-- Binary, DynamoDB treats each byte of the binary data as unsigned when it
-- compares binary values, for example when evaluating query expressions.
-- For information on specifying data types in JSON, see JSON Data Format in
-- the Amazon DynamoDB Developer Guide. ComparisonOperator - A comparator
-- for evaluating attributes. For example, equals, greater than, less than,
-- etc. The following comparison operators are available: EQ | NE | LE | LT
-- | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN
-- | BETWEEN For complete descriptions of all comparison operators, see
-- API_Condition.html.
qQueryFilter :: Lens' Query (HashMap Text Condition)
qQueryFilter = lens _qQueryFilter (\s a -> s { _qQueryFilter = a })
    . _Map

qReturnConsumedCapacity :: Lens' Query (Maybe Text)
qReturnConsumedCapacity =
    lens _qReturnConsumedCapacity (\s a -> s { _qReturnConsumedCapacity = a })

-- | A value that specifies ascending (true) or descending (false) traversal
-- of the index. DynamoDB returns results reflecting the requested order
-- determined by the range key. If the data type is Number, the results are
-- returned in numeric order. For type String, the results are returned in
-- order of ASCII character code values. For type Binary, DynamoDB treats
-- each byte of the binary data as unsigned when it compares binary values.
-- If ScanIndexForward is not specified, the results are returned in
-- ascending order.
qScanIndexForward :: Lens' Query (Maybe Bool)
qScanIndexForward =
    lens _qScanIndexForward (\s a -> s { _qScanIndexForward = a })

-- | The attributes to be returned in the result. You can retrieve all item
-- attributes, specific item attributes, the count of matching items, or in
-- the case of an index, some or all of the attributes projected into the
-- index. ALL_ATTRIBUTES - Returns all of the item attributes from the
-- specified table or index. If you query a local secondary index, then for
-- each matching item in the index DynamoDB will fetch the entire item from
-- the parent table. If the index is configured to project all item
-- attributes, then all of the data can be obtained from the local secondary
-- index, and no fetching is required. ALL_PROJECTED_ATTRIBUTES - Allowed
-- only when querying an index. Retrieves all attributes that have been
-- projected into the index. If the index is configured to project all
-- attributes, this return value is equivalent to specifying ALL_ATTRIBUTES.
-- COUNT - Returns the number of matching items, rather than the matching
-- items themselves. SPECIFIC_ATTRIBUTES - Returns only the attributes
-- listed in AttributesToGet. This return value is equivalent to specifying
-- AttributesToGet without specifying any value for Select. If you query a
-- local secondary index and request only attributes that are projected into
-- that index, the operation will read only the index and not the table. If
-- any of the requested attributes are not projected into the local
-- secondary index, DynamoDB will fetch each of these attributes from the
-- parent table. This extra fetching incurs additional throughput cost and
-- latency. If you query a global secondary index, you can only request
-- attributes that are projected into the index. Global secondary index
-- queries cannot fetch attributes from the parent table. If neither Select
-- nor AttributesToGet are specified, DynamoDB defaults to ALL_ATTRIBUTES
-- when accessing a table, and ALL_PROJECTED_ATTRIBUTES when accessing an
-- index. You cannot use both Select and AttributesToGet together in a
-- single request, unless the value for Select is SPECIFIC_ATTRIBUTES. (This
-- usage is equivalent to specifying AttributesToGet without any value for
-- Select.).
qSelect :: Lens' Query (Maybe Text)
qSelect = lens _qSelect (\s a -> s { _qSelect = a })

-- | The name of the table containing the requested items.
qTableName :: Lens' Query Text
qTableName = lens _qTableName (\s a -> s { _qTableName = a })

data QueryResponse = QueryResponse
    { _qrConsumedCapacity :: Maybe ConsumedCapacity
    , _qrCount            :: Maybe Int
    , _qrItems            :: [(Map Text AttributeValue)]
    , _qrLastEvaluatedKey :: Map Text AttributeValue
    , _qrScannedCount     :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'QueryResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qrConsumedCapacity' @::@ 'Maybe' 'ConsumedCapacity'
--
-- * 'qrCount' @::@ 'Maybe' 'Int'
--
-- * 'qrItems' @::@ ['(HashMap' 'Text' 'AttributeValue)']
--
-- * 'qrLastEvaluatedKey' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'qrScannedCount' @::@ 'Maybe' 'Int'
--
queryResponse :: QueryResponse
queryResponse = QueryResponse
    { _qrItems            = mempty
    , _qrCount            = Nothing
    , _qrScannedCount     = Nothing
    , _qrLastEvaluatedKey = mempty
    , _qrConsumedCapacity = Nothing
    }

qrConsumedCapacity :: Lens' QueryResponse (Maybe ConsumedCapacity)
qrConsumedCapacity =
    lens _qrConsumedCapacity (\s a -> s { _qrConsumedCapacity = a })

-- | The number of items in the response. If you used a QueryFilter in the
-- request, then Count is the number of items returned after the filter was
-- applied, and ScannedCount is the number of matching items before&gt; the
-- filter was applied. If you did not use a filter in the request, then
-- Count and ScannedCount are the same.
qrCount :: Lens' QueryResponse (Maybe Int)
qrCount = lens _qrCount (\s a -> s { _qrCount = a })

-- | An array of item attributes that match the query criteria. Each element
-- in this array consists of an attribute name and the value for that
-- attribute.
qrItems :: Lens' QueryResponse ([(HashMap Text AttributeValue)])
qrItems = lens _qrItems (\s a -> s { _qrItems = a })
    . mapping _Map

-- | The primary key of the item where the operation stopped, inclusive of the
-- previous result set. Use this value to start a new operation, excluding
-- this value in the new request. If LastEvaluatedKey is empty, then the
-- "last page" of results has been processed and there is no more data to be
-- retrieved. If LastEvaluatedKey is not empty, it does not necessarily mean
-- that there is more data in the result set. The only way to know when you
-- have reached the end of the result set is when LastEvaluatedKey is empty.
qrLastEvaluatedKey :: Lens' QueryResponse (HashMap Text AttributeValue)
qrLastEvaluatedKey =
    lens _qrLastEvaluatedKey (\s a -> s { _qrLastEvaluatedKey = a })
        . _Map

-- | The number of items evaluated, before any QueryFilter is applied. A high
-- ScannedCount value with few, or no, Count results indicates an
-- inefficient Query operation. For more information, see Count and
-- ScannedCount in the Amazon DynamoDB Developer Guide. If you did not use a
-- filter in the request, then ScannedCount is the same as Count.
qrScannedCount :: Lens' QueryResponse (Maybe Int)
qrScannedCount = lens _qrScannedCount (\s a -> s { _qrScannedCount = a })

instance ToPath Query where
    toPath = const "/"

instance ToQuery Query where
    toQuery = const mempty

instance ToHeaders Query

instance ToJSON Query where
    toJSON Query{..} = object
        [ "TableName"                 .= _qTableName
        , "IndexName"                 .= _qIndexName
        , "Select"                    .= _qSelect
        , "AttributesToGet"           .= _qAttributesToGet
        , "Limit"                     .= _qLimit
        , "ConsistentRead"            .= _qConsistentRead
        , "KeyConditions"             .= _qKeyConditions
        , "QueryFilter"               .= _qQueryFilter
        , "ConditionalOperator"       .= _qConditionalOperator
        , "ScanIndexForward"          .= _qScanIndexForward
        , "ExclusiveStartKey"         .= _qExclusiveStartKey
        , "ReturnConsumedCapacity"    .= _qReturnConsumedCapacity
        , "ProjectionExpression"      .= _qProjectionExpression
        , "FilterExpression"          .= _qFilterExpression
        , "ExpressionAttributeNames"  .= _qExpressionAttributeNames
        , "ExpressionAttributeValues" .= _qExpressionAttributeValues
        ]

instance AWSRequest Query where
    type Sv Query = DynamoDB
    type Rs Query = QueryResponse

    request  = post "Query"
    response = jsonResponse

instance FromJSON QueryResponse where
    parseJSON = withObject "QueryResponse" $ \o -> QueryResponse
        <$> o .:? "ConsumedCapacity"
        <*> o .:? "Count"
        <*> o .: "Items"
        <*> o .: "LastEvaluatedKey"
        <*> o .:? "ScannedCount"

instance AWSPager Query where
    next rq rs = (\x -> rq & qExclusiveStartKey ?~ x)
        <$> (rs ^. qrLastEvaluatedKey)
