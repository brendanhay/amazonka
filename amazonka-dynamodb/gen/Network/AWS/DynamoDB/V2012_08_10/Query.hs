{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.Query
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
module Network.AWS.DynamoDB.V2012_08_10.Query
    (
    -- * Request
      Query
    -- ** Request constructor
    , mkQueryInput
    -- ** Request lenses
    , qiTableName
    , qiIndexName
    , qiSelect
    , qiAttributesToGet
    , qiLimit
    , qiConsistentRead
    , qiKeyConditions
    , qiQueryFilter
    , qiConditionalOperator
    , qiScanIndexForward
    , qiExclusiveStartKey
    , qiReturnConsumedCapacity

    -- * Response
    , QueryResponse
    -- ** Response lenses
    , qoItems
    , qoCount
    , qoScannedCount
    , qoLastEvaluatedKey
    , qoConsumedCapacity
    ) where

import           Network.AWS.DynamoDB.V2012_08_10.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'Query' request.
mkQueryInput :: Text -- ^ 'qiTableName'
             -> Query
mkQueryInput p1 = Query
    { _qiTableName = p1
    , _qiIndexName = Nothing
    , _qiSelect = Nothing
    , _qiAttributesToGet = Nothing
    , _qiLimit = Nothing
    , _qiConsistentRead = Nothing
    , _qiKeyConditions = mempty
    , _qiQueryFilter = mempty
    , _qiConditionalOperator = Nothing
    , _qiScanIndexForward = Nothing
    , _qiExclusiveStartKey = mempty
    , _qiReturnConsumedCapacity = Nothing
    }
{-# INLINE mkQueryInput #-}

data Query = Query
    { _qiTableName :: Text
      -- ^ The name of the table containing the requested items.
    , _qiIndexName :: Maybe Text
      -- ^ The name of an index to query. This can be any local secondary
      -- index or global secondary index on the table.
    , _qiSelect :: Maybe Select
      -- ^ The attributes to be returned in the result. You can retrieve all
      -- item attributes, specific item attributes, the count of matching
      -- items, or in the case of an index, some or all of the attributes
      -- projected into the index. ALL_ATTRIBUTES: Returns all of the item
      -- attributes from the specified table or index. If you are querying
      -- a local secondary index, then for each matching item in the index
      -- DynamoDB will fetch the entire item from the parent table. If the
      -- index is configured to project all item attributes, then all of
      -- the data can be obtained from the local secondary index, and no
      -- fetching is required.. ALL_PROJECTED_ATTRIBUTES: Allowed only
      -- when querying an index. Retrieves all attributes which have been
      -- projected into the index. If the index is configured to project
      -- all attributes, this is equivalent to specifying ALL_ATTRIBUTES.
      -- COUNT: Returns the number of matching items, rather than the
      -- matching items themselves. SPECIFIC_ATTRIBUTES : Returns only the
      -- attributes listed in AttributesToGet. This is equivalent to
      -- specifying AttributesToGet without specifying any value for
      -- Select. If you are querying a local secondary index and request
      -- only attributes that are projected into that index, the operation
      -- will read only the index and not the table. If any of the
      -- requested attributes are not projected into the local secondary
      -- index, DynamoDB will fetch each of these attributes from the
      -- parent table. This extra fetching incurs additional throughput
      -- cost and latency. If you are querying a global secondary index,
      -- you can only request attributes that are projected into the
      -- index. Global secondary index queries cannot fetch attributes
      -- from the parent table. If neither Select nor AttributesToGet are
      -- specified, DynamoDB defaults to ALL_ATTRIBUTES when accessing a
      -- table, and ALL_PROJECTED_ATTRIBUTES when accessing an index. You
      -- cannot use both Select and AttributesToGet together in a single
      -- request, unless the value for Select is SPECIFIC_ATTRIBUTES.
      -- (This usage is equivalent to specifying AttributesToGet without
      -- any value for Select.).
    , _qiAttributesToGet :: Maybe [Text]
      -- ^ The names of one or more attributes to retrieve. If no attribute
      -- names are specified, then all attributes will be returned. If any
      -- of the requested attributes are not found, they will not appear
      -- in the result. You cannot use both AttributesToGet and Select
      -- together in a Query request, unless the value for Select is
      -- SPECIFIC_ATTRIBUTES. (This usage is equivalent to specifying
      -- AttributesToGet without any value for Select.) If you are
      -- querying a local secondary index and request only attributes that
      -- are projected into that index, the operation will read only the
      -- index and not the table. If any of the requested attributes are
      -- not projected into the local secondary index, DynamoDB will fetch
      -- each of these attributes from the parent table. This extra
      -- fetching incurs additional throughput cost and latency. If you
      -- are querying a global secondary index, you can only request
      -- attributes that are projected into the index. Global secondary
      -- index queries cannot fetch attributes from the parent table.
    , _qiLimit :: Maybe Integer
      -- ^ The maximum number of items to evaluate (not necessarily the
      -- number of matching items). If DynamoDB processes the number of
      -- items up to the limit while processing the results, it stops the
      -- operation and returns the matching values up to that point, and a
      -- LastEvaluatedKey to apply in a subsequent operation, so that you
      -- can pick up where you left off. Also, if the processed data set
      -- size exceeds 1 MB before DynamoDB reaches this limit, it stops
      -- the operation and returns the matching values up to the limit,
      -- and a LastEvaluatedKey to apply in a subsequent operation to
      -- continue the operation. For more information see
      -- href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html"
      -- >Query and Scan in the Amazon DynamoDB Developer Guide.
    , _qiConsistentRead :: Maybe Bool
      -- ^ If set to true, then the operation uses strongly consistent
      -- reads; otherwise, eventually consistent reads are used. Strongly
      -- consistent reads are not supported on global secondary indexes.
      -- If you query a global secondary index with ConsistentRead set to
      -- true, you will receive an error message.
    , _qiKeyConditions :: Map Text Condition
      -- ^ The selection criteria for the query. For a query on a table, you
      -- can only have conditions on the table primary key attributes. You
      -- must specify the hash key attribute name and value as an EQ
      -- condition. You can optionally specify a second condition,
      -- referring to the range key attribute. For a query on an index,
      -- you can only have conditions on the index key attributes. You
      -- must specify the index hash attribute name and value as an EQ
      -- condition. You can optionally specify a second condition,
      -- referring to the index key range attribute. Multiple conditions
      -- are evaluated using "AND"; in other words, all of the conditions
      -- must be met in order for an item to appear in the results
      -- results. Each KeyConditions element consists of an attribute name
      -- to compare, along with the following: AttributeValueList - One or
      -- more values to evaluate against the supplied attribute. This list
      -- contains exactly one value, except for a BETWEEN comparison, in
      -- which case the list contains two values. For type Number, value
      -- comparisons are numeric. String value comparisons for greater
      -- than, equals, or less than are based on ASCII character code
      -- values. For example, a is greater than A, and aa is greater than
      -- B. For a list of code values, see
      -- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters.
      -- For Binary, DynamoDB treats each byte of the binary data as
      -- unsigned when it compares binary values, for example when
      -- evaluating query expressions. ComparisonOperator - A comparator
      -- for evaluating attributes. For example, equals, greater than,
      -- less than, etc. Valid comparison operators for Query: EQ | LE |
      -- LT | GE | GT | BEGINS_WITH | BETWEEN For information on
      -- specifying data types in JSON, see JSON Data Format in the Amazon
      -- DynamoDB Developer Guide. The following are descriptions of each
      -- comparison operator. EQ : Equal. AttributeValueList can contain
      -- only one AttributeValue of type String, Number, or Binary (not a
      -- set). If an item contains an AttributeValue of a different type
      -- than the one specified in the request, the value does not match.
      -- For example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"}
      -- does not equal {"NS":["6", "2", "1"]}. LE : Less than or equal.
      -- AttributeValueList can contain only one AttributeValue of type
      -- String, Number, or Binary (not a set). If an item contains an
      -- AttributeValue of a different type than the one specified in the
      -- request, the value does not match. For example, {"S":"6"} does
      -- not equal {"N":"6"}. Also, {"N":"6"} does not compare to
      -- {"NS":["6", "2", "1"]}. LT : Less than. AttributeValueList can
      -- contain only one AttributeValue of type String, Number, or Binary
      -- (not a set). If an item contains an AttributeValue of a different
      -- type than the one specified in the request, the value does not
      -- match. For example, {"S":"6"} does not equal {"N":"6"}. Also,
      -- {"N":"6"} does not compare to {"NS":["6", "2", "1"]}. GE :
      -- Greater than or equal. AttributeValueList can contain only one
      -- AttributeValue of type String, Number, or Binary (not a set). If
      -- an item contains an AttributeValue of a different type than the
      -- one specified in the request, the value does not match. For
      -- example, {"S":"6"} does not equal {"N":"6"}. Also, {"N":"6"} does
      -- not compare to {"NS":["6", "2", "1"]}. GT : Greater than.
      -- AttributeValueList can contain only one AttributeValue of type
      -- String, Number, or Binary (not a set). If an item contains an
      -- AttributeValue of a different type than the one specified in the
      -- request, the value does not match. For example, {"S":"6"} does
      -- not equal {"N":"6"}. Also, {"N":"6"} does not compare to
      -- {"NS":["6", "2", "1"]}. BEGINS_WITH : checks for a prefix.
      -- AttributeValueList can contain only one AttributeValue of type
      -- String or Binary (not a Number or a set). The target attribute of
      -- the comparison must be a String or Binary (not a Number or a
      -- set). BETWEEN : Greater than or equal to the first value, and
      -- less than or equal to the second value. AttributeValueList must
      -- contain two AttributeValue elements of the same type, either
      -- String, Number, or Binary (not a set). A target attribute matches
      -- if the target value is greater than, or equal to, the first
      -- element and less than, or equal to, the second element. If an
      -- item contains an AttributeValue of a different type than the one
      -- specified in the request, the value does not match. For example,
      -- {"S":"6"} does not compare to {"N":"6"}. Also, {"N":"6"} does not
      -- compare to {"NS":["6", "2", "1"]}.
    , _qiQueryFilter :: Map Text Condition
    , _qiConditionalOperator :: Maybe ConditionalOperator
    , _qiScanIndexForward :: Maybe Bool
      -- ^ Specifies ascending (true) or descending (false) traversal of the
      -- index. DynamoDB returns results reflecting the requested order
      -- determined by the range key. If the data type is Number, the
      -- results are returned in numeric order. For String, the results
      -- are returned in order of ASCII character code values. For Binary,
      -- Amazon DynamoDB treats each byte of the binary data as unsigned
      -- when it compares binary values. If ScanIndexForward is not
      -- specified, the results are returned in ascending order.
    , _qiExclusiveStartKey :: Map Text AttributeValue
      -- ^ The primary key of the first item that this operation will
      -- evalute. Use the value that was returned for LastEvaluatedKey in
      -- the previous operation. The data type for ExclusiveStartKey must
      -- be String, Number or Binary. No set data types are allowed.
    , _qiReturnConsumedCapacity :: Maybe ReturnConsumedCapacity
      -- ^ If set to TOTAL, the response includes ConsumedCapacity data for
      -- tables and indexes. If set to INDEXES, the repsonse includes
      -- ConsumedCapacity for indexes. If set to NONE (the default),
      -- ConsumedCapacity is not included in the response.
    } deriving (Show, Generic)

-- | The name of the table containing the requested items.
qiTableName :: Lens' Query (Text)
qiTableName = lens _qiTableName (\s a -> s { _qiTableName = a })
{-# INLINE qiTableName #-}

-- | The name of an index to query. This can be any local secondary index or
-- global secondary index on the table.
qiIndexName :: Lens' Query (Maybe Text)
qiIndexName = lens _qiIndexName (\s a -> s { _qiIndexName = a })
{-# INLINE qiIndexName #-}

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
qiSelect :: Lens' Query (Maybe Select)
qiSelect = lens _qiSelect (\s a -> s { _qiSelect = a })
{-# INLINE qiSelect #-}

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
qiAttributesToGet :: Lens' Query (Maybe [Text])
qiAttributesToGet = lens _qiAttributesToGet (\s a -> s { _qiAttributesToGet = a })
{-# INLINE qiAttributesToGet #-}

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
qiLimit :: Lens' Query (Maybe Integer)
qiLimit = lens _qiLimit (\s a -> s { _qiLimit = a })
{-# INLINE qiLimit #-}

-- | If set to true, then the operation uses strongly consistent reads;
-- otherwise, eventually consistent reads are used. Strongly consistent reads
-- are not supported on global secondary indexes. If you query a global
-- secondary index with ConsistentRead set to true, you will receive an error
-- message.
qiConsistentRead :: Lens' Query (Maybe Bool)
qiConsistentRead = lens _qiConsistentRead (\s a -> s { _qiConsistentRead = a })
{-# INLINE qiConsistentRead #-}

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
qiKeyConditions :: Lens' Query (Map Text Condition)
qiKeyConditions = lens _qiKeyConditions (\s a -> s { _qiKeyConditions = a })
{-# INLINE qiKeyConditions #-}

qiQueryFilter :: Lens' Query (Map Text Condition)
qiQueryFilter = lens _qiQueryFilter (\s a -> s { _qiQueryFilter = a })
{-# INLINE qiQueryFilter #-}

qiConditionalOperator :: Lens' Query (Maybe ConditionalOperator)
qiConditionalOperator = lens _qiConditionalOperator (\s a -> s { _qiConditionalOperator = a })
{-# INLINE qiConditionalOperator #-}

-- | Specifies ascending (true) or descending (false) traversal of the index.
-- DynamoDB returns results reflecting the requested order determined by the
-- range key. If the data type is Number, the results are returned in numeric
-- order. For String, the results are returned in order of ASCII character
-- code values. For Binary, Amazon DynamoDB treats each byte of the binary
-- data as unsigned when it compares binary values. If ScanIndexForward is not
-- specified, the results are returned in ascending order.
qiScanIndexForward :: Lens' Query (Maybe Bool)
qiScanIndexForward = lens _qiScanIndexForward (\s a -> s { _qiScanIndexForward = a })
{-# INLINE qiScanIndexForward #-}

-- | The primary key of the first item that this operation will evalute. Use the
-- value that was returned for LastEvaluatedKey in the previous operation. The
-- data type for ExclusiveStartKey must be String, Number or Binary. No set
-- data types are allowed.
qiExclusiveStartKey :: Lens' Query (Map Text AttributeValue)
qiExclusiveStartKey = lens _qiExclusiveStartKey (\s a -> s { _qiExclusiveStartKey = a })
{-# INLINE qiExclusiveStartKey #-}

-- | If set to TOTAL, the response includes ConsumedCapacity data for tables and
-- indexes. If set to INDEXES, the repsonse includes ConsumedCapacity for
-- indexes. If set to NONE (the default), ConsumedCapacity is not included in
-- the response.
qiReturnConsumedCapacity :: Lens' Query (Maybe ReturnConsumedCapacity)
qiReturnConsumedCapacity = lens _qiReturnConsumedCapacity (\s a -> s { _qiReturnConsumedCapacity = a })
{-# INLINE qiReturnConsumedCapacity #-}

instance ToPath Query

instance ToQuery Query

instance ToHeaders Query

instance ToJSON Query

data QueryResponse = QueryResponse
    { _qoItems :: [Map Text AttributeValue]
      -- ^ An array of item attributes that match the query criteria. Each
      -- element in this array consists of an attribute name and the value
      -- for that attribute.
    , _qoCount :: Maybe Integer
      -- ^ The number of items in the response.
    , _qoScannedCount :: Maybe Integer
    , _qoLastEvaluatedKey :: Map Text AttributeValue
      -- ^ The primary key of the item where the operation stopped,
      -- inclusive of the previous result set. Use this value to start a
      -- new operation, excluding this value in the new request. If
      -- LastEvaluatedKey is null, then the "last page" of results has
      -- been processed and there is no more data to be retrieved. If
      -- LastEvaluatedKey is anything other than null, this does not
      -- necessarily mean that there is more data in the result set. The
      -- only way to know when you have reached the end of the result set
      -- is when LastEvaluatedKey is null.
    , _qoConsumedCapacity :: Maybe ConsumedCapacity
      -- ^ Represents the capacity units consumed by an operation. The data
      -- returned includes the total provisioned throughput consumed,
      -- along with statistics for the table and any indexes involved in
      -- the operation. ConsumedCapacity is only returned if it was asked
      -- for in the request. For more information, see Provisioned
      -- Throughput in the Amazon DynamoDB Developer Guide.
    } deriving (Show, Generic)

-- | An array of item attributes that match the query criteria. Each element in
-- this array consists of an attribute name and the value for that attribute.
qoItems :: Lens' QueryResponse ([Map Text AttributeValue])
qoItems = lens _qoItems (\s a -> s { _qoItems = a })
{-# INLINE qoItems #-}

-- | The number of items in the response.
qoCount :: Lens' QueryResponse (Maybe Integer)
qoCount = lens _qoCount (\s a -> s { _qoCount = a })
{-# INLINE qoCount #-}

qoScannedCount :: Lens' QueryResponse (Maybe Integer)
qoScannedCount = lens _qoScannedCount (\s a -> s { _qoScannedCount = a })
{-# INLINE qoScannedCount #-}

-- | The primary key of the item where the operation stopped, inclusive of the
-- previous result set. Use this value to start a new operation, excluding
-- this value in the new request. If LastEvaluatedKey is null, then the "last
-- page" of results has been processed and there is no more data to be
-- retrieved. If LastEvaluatedKey is anything other than null, this does not
-- necessarily mean that there is more data in the result set. The only way to
-- know when you have reached the end of the result set is when
-- LastEvaluatedKey is null.
qoLastEvaluatedKey :: Lens' QueryResponse (Map Text AttributeValue)
qoLastEvaluatedKey = lens _qoLastEvaluatedKey (\s a -> s { _qoLastEvaluatedKey = a })
{-# INLINE qoLastEvaluatedKey #-}

-- | Represents the capacity units consumed by an operation. The data returned
-- includes the total provisioned throughput consumed, along with statistics
-- for the table and any indexes involved in the operation. ConsumedCapacity
-- is only returned if it was asked for in the request. For more information,
-- see Provisioned Throughput in the Amazon DynamoDB Developer Guide.
qoConsumedCapacity :: Lens' QueryResponse (Maybe ConsumedCapacity)
qoConsumedCapacity = lens _qoConsumedCapacity (\s a -> s { _qoConsumedCapacity = a })
{-# INLINE qoConsumedCapacity #-}

instance FromJSON QueryResponse

instance AWSRequest Query where
    type Sv Query = DynamoDB
    type Rs Query = QueryResponse

    request = get
    response _ = jsonResponse

instance AWSPager Query where
    next rq rs
        | Map.null k = Nothing
        | otherwise  = Just (rq { _qiExclusiveStartKey = k })
      where
        k = _qoLastEvaluatedKey rs
