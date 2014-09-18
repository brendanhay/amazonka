{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- consume the minimum read capacity units according to the type of read. If
-- the total number of items meeting the query criteria exceeds the result set
-- size limit of 1 MB, the query stops and results are returned to the user
-- with a LastEvaluatedKey to continue the query in a subsequent operation.
-- Unlike a Scan operation, a Query operation never returns an empty result
-- set and a LastEvaluatedKey. The LastEvaluatedKey is only provided if the
-- results exceed 1 MB, or if you have used Limit. You can query a table, a
-- local secondary index (LSI), or a global secondary index (GSI). For a query
-- on a table or on an LSI, you can set ConsistentRead to true and obtain a
-- strongly consistent result. GSIs support eventually consistent reads only,
-- so do not specify ConsistentRead when querying a GSI. Retrieving a Range of
-- Items This example queries the Thread table for postings between two dates.
-- There is an index on LastPostDateTime to facilitate fast lookups on this
-- attribute. All of the attributes will be returned. Any attributes that are
-- not projected into the index will cause DynamoDB to fetch those attributes
-- from the Thread table; this fetching occurs automatically. { "Count":`17 }.
module Network.AWS.DynamoDB.Query
    (
    -- * Request
      Query
    -- ** Request constructor
    , query
    -- ** Request lenses
    , qTableName
    , qIndexName
    , qSelect
    , qAttributesToGet
    , qLimit
    , qConsistentRead
    , qKeyConditions
    , qQueryFilter
    , qConditionalOperator
    , qScanIndexForward
    , qExclusiveStartKey
    , qReturnConsumedCapacity

    -- * Response
    , QueryResponse
    -- ** Response constructor
    , queryResponse
    -- ** Response lenses
    , qrItems
    , qrCount
    , qrScannedCount
    , qrLastEvaluatedKey
    , qrConsumedCapacity
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a Query operation.
data Query = Query
    { _qTableName :: Text
    , _qIndexName :: Maybe Text
    , _qSelect :: Maybe Select
    , _qAttributesToGet :: Maybe (List1 Text)
    , _qLimit :: Maybe Integer
    , _qConsistentRead :: Maybe Bool
    , _qKeyConditions :: Map Text Condition
    , _qQueryFilter :: Map Text Condition
    , _qConditionalOperator :: Maybe ConditionalOperator
    , _qScanIndexForward :: Maybe Bool
    , _qExclusiveStartKey :: Map Text AttributeValue
    , _qReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'Query' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TableName ::@ @Text@
--
-- * @IndexName ::@ @Maybe Text@
--
-- * @Select ::@ @Maybe Select@
--
-- * @AttributesToGet ::@ @Maybe (List1 Text)@
--
-- * @Limit ::@ @Maybe Integer@
--
-- * @ConsistentRead ::@ @Maybe Bool@
--
-- * @KeyConditions ::@ @Map Text Condition@
--
-- * @QueryFilter ::@ @Map Text Condition@
--
-- * @ConditionalOperator ::@ @Maybe ConditionalOperator@
--
-- * @ScanIndexForward ::@ @Maybe Bool@
--
-- * @ExclusiveStartKey ::@ @Map Text AttributeValue@
--
-- * @ReturnConsumedCapacity ::@ @Maybe ReturnConsumedCapacity@
--
query :: Text -- ^ 'qTableName'
        -> Query
query p1 = Query
    { _qTableName = p1
    , _qIndexName = Nothing
    , _qSelect = Nothing
    , _qAttributesToGet = Nothing
    , _qLimit = Nothing
    , _qConsistentRead = Nothing
    , _qKeyConditions = mempty
    , _qQueryFilter = mempty
    , _qConditionalOperator = Nothing
    , _qScanIndexForward = Nothing
    , _qExclusiveStartKey = mempty
    , _qReturnConsumedCapacity = Nothing
    }

-- | The name of the table containing the requested items.
qTableName :: Lens' Query Text
qTableName = lens _qTableName (\s a -> s { _qTableName = a })

-- | The name of an index to query. This can be any local secondary index or
-- global secondary index on the table.
qIndexName :: Lens' Query (Maybe Text)
qIndexName = lens _qIndexName (\s a -> s { _qIndexName = a })

-- | The attributes to be returned in the result. You can retrieve all item
-- attributes, specific item attributes, the count of matching items, or in
-- the case of an index, some or all of the attributes projected into the
-- index. ALL_ATTRIBUTES: Returns all of the item attributes from the
-- specified table or index. If you are querying a local secondary index, then
-- for each matching item in the index DynamoDB will fetch the entire item
-- from the parent table. If the index is configured to project all item
-- attributes, then all of the data can be obtained from the local secondary
-- index, and no fetching is required.. ALL_PROJECTED_ATTRIBUTES: Allowed only
-- when querying an index. Retrieves all attributes which have been projected
-- into the index. If the index is configured to project all attributes, this
-- is equivalent to specifying ALL_ATTRIBUTES. COUNT: Returns the number of
-- matching items, rather than the matching items themselves.
-- SPECIFIC_ATTRIBUTES : Returns only the attributes listed in
-- AttributesToGet. This is equivalent to specifying AttributesToGet without
-- specifying any value for Select. If you are querying a local secondary
-- index and request only attributes that are projected into that index, the
-- operation will read only the index and not the table. If any of the
-- requested attributes are not projected into the local secondary index,
-- DynamoDB will fetch each of these attributes from the parent table. This
-- extra fetching incurs additional throughput cost and latency. If you are
-- querying a global secondary index, you can only request attributes that are
-- projected into the index. Global secondary index queries cannot fetch
-- attributes from the parent table. If neither Select nor AttributesToGet are
-- specified, DynamoDB defaults to ALL_ATTRIBUTES when accessing a table, and
-- ALL_PROJECTED_ATTRIBUTES when accessing an index. You cannot use both
-- Select and AttributesToGet together in a single request, unless the value
-- for Select is SPECIFIC_ATTRIBUTES. (This usage is equivalent to specifying
-- AttributesToGet without any value for Select.).
qSelect :: Lens' Query (Maybe Select)
qSelect = lens _qSelect (\s a -> s { _qSelect = a })

-- | The names of one or more attributes to retrieve. If no attribute names are
-- specified, then all attributes will be returned. If any of the requested
-- attributes are not found, they will not appear in the result. You cannot
-- use both AttributesToGet and Select together in a Query request, unless the
-- value for Select is SPECIFIC_ATTRIBUTES. (This usage is equivalent to
-- specifying AttributesToGet without any value for Select.) If you are
-- querying a local secondary index and request only attributes that are
-- projected into that index, the operation will read only the index and not
-- the table. If any of the requested attributes are not projected into the
-- local secondary index, DynamoDB will fetch each of these attributes from
-- the parent table. This extra fetching incurs additional throughput cost and
-- latency. If you are querying a global secondary index, you can only request
-- attributes that are projected into the index. Global secondary index
-- queries cannot fetch attributes from the parent table.
qAttributesToGet :: Lens' Query (Maybe (List1 Text))
qAttributesToGet =
    lens _qAttributesToGet (\s a -> s { _qAttributesToGet = a })

-- | The maximum number of items to evaluate (not necessarily the number of
-- matching items). If DynamoDB processes the number of items up to the limit
-- while processing the results, it stops the operation and returns the
-- matching values up to that point, and a LastEvaluatedKey to apply in a
-- subsequent operation, so that you can pick up where you left off. Also, if
-- the processed data set size exceeds 1 MB before DynamoDB reaches this
-- limit, it stops the operation and returns the matching values up to the
-- limit, and a LastEvaluatedKey to apply in a subsequent operation to
-- continue the operation. For more information see
-- href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html"
-- >Query and Scan in the Amazon DynamoDB Developer Guide.
qLimit :: Lens' Query (Maybe Integer)
qLimit = lens _qLimit (\s a -> s { _qLimit = a })

-- | If set to true, then the operation uses strongly consistent reads;
-- otherwise, eventually consistent reads are used. Strongly consistent reads
-- are not supported on global secondary indexes. If you query a global
-- secondary index with ConsistentRead set to true, you will receive an error
-- message.
qConsistentRead :: Lens' Query (Maybe Bool)
qConsistentRead = lens _qConsistentRead (\s a -> s { _qConsistentRead = a })

-- | The selection criteria for the query. For a query on a table, you can only
-- have conditions on the table primary key attributes. You must specify the
-- hash key attribute name and value as an EQ condition. You can optionally
-- specify a second condition, referring to the range key attribute. For a
-- query on an index, you can only have conditions on the index key
-- attributes. You must specify the index hash attribute name and value as an
-- EQ condition. You can optionally specify a second condition, referring to
-- the index key range attribute. Multiple conditions are evaluated using
-- "AND"; in other words, all of the conditions must be met in order for an
-- item to appear in the results results. Each KeyConditions element consists
-- of an attribute name to compare, along with the following:
-- AttributeValueList - One or more values to evaluate against the supplied
-- attribute. This list contains exactly one value, except for a BETWEEN
-- comparison, in which case the list contains two values. For type Number,
-- value comparisons are numeric. String value comparisons for greater than,
-- equals, or less than are based on ASCII character code values. For example,
-- a is greater than A, and aa is greater than B. For a list of code values,
-- see http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters. For
-- Binary, DynamoDB treats each byte of the binary data as unsigned when it
-- compares binary values, for example when evaluating query expressions.
-- ComparisonOperator - A comparator for evaluating attributes. For example,
-- equals, greater than, less than, etc. Valid comparison operators for Query:
-- EQ | LE | LT | GE | GT | BEGINS_WITH | BETWEEN For information on
-- specifying data types in JSON, see JSON Data Format in the Amazon DynamoDB
-- Developer Guide. The following are descriptions of each comparison
-- operator. EQ : Equal. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not equal {"NS":["6", "2", "1"]}. LE
-- : Less than or equal. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. LT : Less than. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. GE : Greater than or equal. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. GT : Greater than. AttributeValueList can contain only one
-- AttributeValue of type String, Number, or Binary (not a set). If an item
-- contains an AttributeValue of a different type than the one specified in
-- the request, the value does not match. For example, {"S":"6"} does not
-- equal {"N":"6"}. Also, {"N":"6"} does not compare to {"NS":["6", "2",
-- "1"]}. BEGINS_WITH : checks for a prefix. AttributeValueList can contain
-- only one AttributeValue of type String or Binary (not a Number or a set).
-- The target attribute of the comparison must be a String or Binary (not a
-- Number or a set). BETWEEN : Greater than or equal to the first value, and
-- less than or equal to the second value. AttributeValueList must contain two
-- AttributeValue elements of the same type, either String, Number, or Binary
-- (not a set). A target attribute matches if the target value is greater
-- than, or equal to, the first element and less than, or equal to, the second
-- element. If an item contains an AttributeValue of a different type than the
-- one specified in the request, the value does not match. For example,
-- {"S":"6"} does not compare to {"N":"6"}. Also, {"N":"6"} does not compare
-- to {"NS":["6", "2", "1"]}.
qKeyConditions :: Lens' Query (Map Text Condition)
qKeyConditions = lens _qKeyConditions (\s a -> s { _qKeyConditions = a })

qQueryFilter :: Lens' Query (Map Text Condition)
qQueryFilter = lens _qQueryFilter (\s a -> s { _qQueryFilter = a })

qConditionalOperator :: Lens' Query (Maybe ConditionalOperator)
qConditionalOperator =
    lens _qConditionalOperator (\s a -> s { _qConditionalOperator = a })

-- | Specifies ascending (true) or descending (false) traversal of the index.
-- DynamoDB returns results reflecting the requested order determined by the
-- range key. If the data type is Number, the results are returned in numeric
-- order. For String, the results are returned in order of ASCII character
-- code values. For Binary, Amazon DynamoDB treats each byte of the binary
-- data as unsigned when it compares binary values. If ScanIndexForward is not
-- specified, the results are returned in ascending order.
qScanIndexForward :: Lens' Query (Maybe Bool)
qScanIndexForward =
    lens _qScanIndexForward (\s a -> s { _qScanIndexForward = a })

-- | The primary key of the first item that this operation will evalute. Use the
-- value that was returned for LastEvaluatedKey in the previous operation. The
-- data type for ExclusiveStartKey must be String, Number or Binary. No set
-- data types are allowed.
qExclusiveStartKey :: Lens' Query (Map Text AttributeValue)
qExclusiveStartKey =
    lens _qExclusiveStartKey (\s a -> s { _qExclusiveStartKey = a })

-- | If set to TOTAL, the response includes ConsumedCapacity data for tables and
-- indexes. If set to INDEXES, the repsonse includes ConsumedCapacity for
-- indexes. If set to NONE (the default), ConsumedCapacity is not included in
-- the response.
qReturnConsumedCapacity :: Lens' Query (Maybe ReturnConsumedCapacity)
qReturnConsumedCapacity =
    lens _qReturnConsumedCapacity
         (\s a -> s { _qReturnConsumedCapacity = a })

instance ToPath Query

instance ToQuery Query

instance ToHeaders Query

instance ToJSON Query

-- | Represents the output of a Query operation.
data QueryResponse = QueryResponse
    { _qrItems :: [Map Text AttributeValue]
    , _qrCount :: Maybe Integer
    , _qrScannedCount :: Maybe Integer
    , _qrLastEvaluatedKey :: Map Text AttributeValue
    , _qrConsumedCapacity :: Maybe ConsumedCapacity
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'QueryResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Items ::@ @[Map Text AttributeValue]@
--
-- * @Count ::@ @Maybe Integer@
--
-- * @ScannedCount ::@ @Maybe Integer@
--
-- * @LastEvaluatedKey ::@ @Map Text AttributeValue@
--
-- * @ConsumedCapacity ::@ @Maybe ConsumedCapacity@
--
queryResponse :: QueryResponse
queryResponse = QueryResponse
    { _qrItems = mempty
    , _qrCount = Nothing
    , _qrScannedCount = Nothing
    , _qrLastEvaluatedKey = mempty
    , _qrConsumedCapacity = Nothing
    }

-- | An array of item attributes that match the query criteria. Each element in
-- this array consists of an attribute name and the value for that attribute.
qrItems :: Lens' QueryResponse [Map Text AttributeValue]
qrItems = lens _qrItems (\s a -> s { _qrItems = a })

-- | The number of items in the response.
qrCount :: Lens' QueryResponse (Maybe Integer)
qrCount = lens _qrCount (\s a -> s { _qrCount = a })

qrScannedCount :: Lens' QueryResponse (Maybe Integer)
qrScannedCount = lens _qrScannedCount (\s a -> s { _qrScannedCount = a })

-- | The primary key of the item where the operation stopped, inclusive of the
-- previous result set. Use this value to start a new operation, excluding
-- this value in the new request. If LastEvaluatedKey is null, then the "last
-- page" of results has been processed and there is no more data to be
-- retrieved. If LastEvaluatedKey is anything other than null, this does not
-- necessarily mean that there is more data in the result set. The only way to
-- know when you have reached the end of the result set is when
-- LastEvaluatedKey is null.
qrLastEvaluatedKey :: Lens' QueryResponse (Map Text AttributeValue)
qrLastEvaluatedKey =
    lens _qrLastEvaluatedKey (\s a -> s { _qrLastEvaluatedKey = a })

-- | Represents the capacity units consumed by an operation. The data returned
-- includes the total provisioned throughput consumed, along with statistics
-- for the table and any indexes involved in the operation. ConsumedCapacity
-- is only returned if it was asked for in the request. For more information,
-- see Provisioned Throughput in the Amazon DynamoDB Developer Guide.
qrConsumedCapacity :: Lens' QueryResponse (Maybe ConsumedCapacity)
qrConsumedCapacity =
    lens _qrConsumedCapacity (\s a -> s { _qrConsumedCapacity = a })

instance FromJSON QueryResponse

instance AWSRequest Query where
    type Sv Query = DynamoDB
    type Rs Query = QueryResponse

    request = get
    response _ = jsonResponse

instance AWSPager Query where
    next rq rs
        | nullMap k = Nothing
        | otherwise = Just (rq & qExclusiveStartKey .~ k)
      where
        k = rs ^. qrLastEvaluatedKey
