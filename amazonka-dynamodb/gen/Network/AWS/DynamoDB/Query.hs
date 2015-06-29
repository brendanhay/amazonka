{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DynamoDB.Query
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | A /Query/ operation uses the primary key of a table or a secondary index
-- to directly access items from that table or index.
--
-- Use the /KeyConditionExpression/ parameter to provide a specific hash
-- key value. The /Query/ operation will return all of the items from the
-- table or index with that hash key value. You can optionally narrow the
-- scope of the /Query/ by specifying a range key value and a comparison
-- operator in the /KeyConditionExpression/. You can use the
-- /ScanIndexForward/ parameter to get results in forward or reverse order,
-- by range key or by index key.
--
-- Queries that do not return results consume the minimum number of read
-- capacity units for that type of read operation.
--
-- If the total number of items meeting the query criteria exceeds the
-- result set size limit of 1 MB, the query stops and results are returned
-- to the user with /LastEvaluatedKey/ to continue the query in a
-- subsequent operation. Unlike a /Scan/ operation, a /Query/ operation
-- never returns both an empty result set and a /LastEvaluatedKey/. The
-- /LastEvaluatedKey/ is only provided if the results exceed 1 MB, or if
-- you have used /Limit/.
--
-- You can query a table, a local secondary index, or a global secondary
-- index. For a query on a table or on a local secondary index, you can set
-- /ConsistentRead/ to true and obtain a strongly consistent result. Global
-- secondary indexes support eventually consistent reads only, so do not
-- specify /ConsistentRead/ when querying a global secondary index.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Query.html>
module Network.AWS.DynamoDB.Query
    (
    -- * Request
      Query
    -- ** Request constructor
    , query
    -- ** Request lenses
    , queProjectionExpression
    , queKeyConditions
    , queFilterExpression
    , queQueryFilter
    , queConsistentRead
    , queExpressionAttributeNames
    , queAttributesToGet
    , queReturnConsumedCapacity
    , queExpressionAttributeValues
    , queScanIndexForward
    , queLimit
    , queSelect
    , queConditionalOperator
    , queKeyConditionExpression
    , queExclusiveStartKey
    , queIndexName
    , queTableName

    -- * Response
    , QueryResponse
    -- ** Response constructor
    , queryResponse
    -- ** Response lenses
    , qrLastEvaluatedKey
    , qrCount
    , qrScannedCount
    , qrItems
    , qrConsumedCapacity
    , qrStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /Query/ operation.
--
-- /See:/ 'query' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'queProjectionExpression'
--
-- * 'queKeyConditions'
--
-- * 'queFilterExpression'
--
-- * 'queQueryFilter'
--
-- * 'queConsistentRead'
--
-- * 'queExpressionAttributeNames'
--
-- * 'queAttributesToGet'
--
-- * 'queReturnConsumedCapacity'
--
-- * 'queExpressionAttributeValues'
--
-- * 'queScanIndexForward'
--
-- * 'queLimit'
--
-- * 'queSelect'
--
-- * 'queConditionalOperator'
--
-- * 'queKeyConditionExpression'
--
-- * 'queExclusiveStartKey'
--
-- * 'queIndexName'
--
-- * 'queTableName'
data Query = Query'
    { _queProjectionExpression      :: !(Maybe Text)
    , _queKeyConditions             :: !(Maybe (Map Text Condition))
    , _queFilterExpression          :: !(Maybe Text)
    , _queQueryFilter               :: !(Maybe (Map Text Condition))
    , _queConsistentRead            :: !(Maybe Bool)
    , _queExpressionAttributeNames  :: !(Maybe (Map Text Text))
    , _queAttributesToGet           :: !(Maybe (List1 Text))
    , _queReturnConsumedCapacity    :: !(Maybe ReturnConsumedCapacity)
    , _queExpressionAttributeValues :: !(Maybe (Map Text AttributeValue))
    , _queScanIndexForward          :: !(Maybe Bool)
    , _queLimit                     :: !(Maybe Nat)
    , _queSelect                    :: !(Maybe Select)
    , _queConditionalOperator       :: !(Maybe ConditionalOperator)
    , _queKeyConditionExpression    :: !(Maybe Text)
    , _queExclusiveStartKey         :: !(Maybe (Map Text AttributeValue))
    , _queIndexName                 :: !(Maybe Text)
    , _queTableName                 :: !Text
    } deriving (Eq,Show)

-- | 'Query' smart constructor.
query :: Text -> Query
query pTableName =
    Query'
    { _queProjectionExpression = Nothing
    , _queKeyConditions = Nothing
    , _queFilterExpression = Nothing
    , _queQueryFilter = Nothing
    , _queConsistentRead = Nothing
    , _queExpressionAttributeNames = Nothing
    , _queAttributesToGet = Nothing
    , _queReturnConsumedCapacity = Nothing
    , _queExpressionAttributeValues = Nothing
    , _queScanIndexForward = Nothing
    , _queLimit = Nothing
    , _queSelect = Nothing
    , _queConditionalOperator = Nothing
    , _queKeyConditionExpression = Nothing
    , _queExclusiveStartKey = Nothing
    , _queIndexName = Nothing
    , _queTableName = pTableName
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
queProjectionExpression :: Lens' Query (Maybe Text)
queProjectionExpression = lens _queProjectionExpression (\ s a -> s{_queProjectionExpression = a});

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /KeyConditionExpression/ instead. Do not combine legacy
-- parameters and expression parameters in a single API call; otherwise,
-- DynamoDB will return a /ValidationException/ exception.
--
-- The selection criteria for the query. For a query on a table, you can
-- have conditions only on the table primary key attributes. You must
-- provide the hash key attribute name and value as an @EQ@ condition. You
-- can optionally provide a second condition, referring to the range key
-- attribute.
--
-- If you don\'t provide a range key condition, all of the items that match
-- the hash key will be retrieved. If a /FilterExpression/ or /QueryFilter/
-- is present, it will be applied after the items are retrieved.
--
-- For a query on an index, you can have conditions only on the index key
-- attributes. You must provide the index hash attribute name and value as
-- an @EQ@ condition. You can optionally provide a second condition,
-- referring to the index key range attribute.
--
-- Each /KeyConditions/ element consists of an attribute name to compare,
-- along with the following:
--
-- -   /AttributeValueList/ - One or more values to evaluate against the
--     supplied attribute. The number of values in the list depends on the
--     /ComparisonOperator/ being used.
--
--     For type Number, value comparisons are numeric.
--
--     String value comparisons for greater than, equals, or less than are
--     based on ASCII character code values. For example, @a@ is greater
--     than @A@, and @a@ is greater than @B@. For a list of code values,
--     see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
--     For Binary, DynamoDB treats each byte of the binary data as unsigned
--     when it compares binary values.
--
-- -   /ComparisonOperator/ - A comparator for evaluating attributes, for
--     example, equals, greater than, less than, and so on.
--
--     For /KeyConditions/, only the following comparison operators are
--     supported:
--
--     @EQ | LE | LT | GE | GT | BEGINS_WITH | BETWEEN@
--
--     The following are descriptions of these comparison operators.
--
--     -   @EQ@ : Equal.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of
--         type String, Number, or Binary (not a set type). If an item
--         contains an /AttributeValue/ element of a different type than
--         the one specified in the request, the value does not match. For
--         example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--         @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @LE@ : Less than or equal.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         an item contains an /AttributeValue/ element of a different type
--         than the one provided in the request, the value does not match.
--         For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@.
--         Also, @{\"N\":\"6\"}@ does not compare to
--         @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @LT@ : Less than.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of
--         type String, Number, or Binary (not a set type). If an item
--         contains an /AttributeValue/ element of a different type than
--         the one provided in the request, the value does not match. For
--         example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--         @{\"N\":\"6\"}@ does not compare to
--         @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @GE@ : Greater than or equal.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         an item contains an /AttributeValue/ element of a different type
--         than the one provided in the request, the value does not match.
--         For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@.
--         Also, @{\"N\":\"6\"}@ does not compare to
--         @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @GT@ : Greater than.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         an item contains an /AttributeValue/ element of a different type
--         than the one provided in the request, the value does not match.
--         For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@.
--         Also, @{\"N\":\"6\"}@ does not compare to
--         @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
--     -   @BEGINS_WITH@ : Checks for a prefix.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of
--         type String or Binary (not a Number or a set type). The target
--         attribute of the comparison must be of type String or Binary
--         (not a Number or a set type).
--
--     -   @BETWEEN@ : Greater than or equal to the first value, and less
--         than or equal to the second value.
--
--         /AttributeValueList/ must contain two /AttributeValue/ elements
--         of the same type, either String, Number, or Binary (not a set
--         type). A target attribute matches if the target value is greater
--         than, or equal to, the first element and less than, or equal to,
--         the second element. If an item contains an /AttributeValue/
--         element of a different type than the one provided in the
--         request, the value does not match. For example, @{\"S\":\"6\"}@
--         does not compare to @{\"N\":\"6\"}@. Also, @{\"N\":\"6\"}@ does
--         not compare to @{\"NS\":[\"6\", \"2\", \"1\"]}@
--
-- For usage examples of /AttributeValueList/ and /ComparisonOperator/, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters>
-- in the /Amazon DynamoDB Developer Guide/.
queKeyConditions :: Lens' Query (HashMap Text Condition)
queKeyConditions = lens _queKeyConditions (\ s a -> s{_queKeyConditions = a}) . _Default . _Map;

-- | A string that contains conditions that DynamoDB applies after the
-- /Query/ operation, but before the data is returned to you. Items that do
-- not satisfy the /FilterExpression/ criteria are not returned.
--
-- A /FilterExpression/ is applied after the items have already been read;
-- the process of filtering does not consume any additional read capacity
-- units.
--
-- For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#FilteringResults Filter Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /FilterExpression/ replaces the legacy /QueryFilter/ and
-- /ConditionalOperator/ parameters.
queFilterExpression :: Lens' Query (Maybe Text)
queFilterExpression = lens _queFilterExpression (\ s a -> s{_queFilterExpression = a});

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /FilterExpression/ instead. Do not combine legacy parameters
-- and expression parameters in a single API call; otherwise, DynamoDB will
-- return a /ValidationException/ exception.
--
-- A condition that evaluates the query results after the items are read
-- and returns only the desired values.
--
-- This parameter does not support attributes of type List or Map.
--
-- A /QueryFilter/ is applied after the items have already been read; the
-- process of filtering does not consume any additional read capacity
-- units.
--
-- If you provide more than one condition in the /QueryFilter/ map, then by
-- default all of the conditions must evaluate to true. In other words, the
-- conditions are ANDed together. (You can use the /ConditionalOperator/
-- parameter to OR the conditions instead. If you do this, then at least
-- one of the conditions must evaluate to true, rather than all of them.)
--
-- Note that /QueryFilter/ does not allow key attributes. You cannot define
-- a filter condition on a hash key or range key.
--
-- Each /QueryFilter/ element consists of an attribute name to compare,
-- along with the following:
--
-- -   /AttributeValueList/ - One or more values to evaluate against the
--     supplied attribute. The number of values in the list depends on the
--     operator specified in /ComparisonOperator/.
--
--     For type Number, value comparisons are numeric.
--
--     String value comparisons for greater than, equals, or less than are
--     based on ASCII character code values. For example, @a@ is greater
--     than @A@, and @a@ is greater than @B@. For a list of code values,
--     see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
--     For type Binary, DynamoDB treats each byte of the binary data as
--     unsigned when it compares binary values.
--
--     For information on specifying data types in JSON, see
--     <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataFormat.html JSON Data Format>
--     in the /Amazon DynamoDB Developer Guide/.
--
-- -   /ComparisonOperator/ - A comparator for evaluating attributes. For
--     example, equals, greater than, less than, etc.
--
--     The following comparison operators are available:
--
--     @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@
--
--     For complete descriptions of all comparison operators, see the
--     <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Condition.html Condition>
--     data type.
--
queQueryFilter :: Lens' Query (HashMap Text Condition)
queQueryFilter = lens _queQueryFilter (\ s a -> s{_queQueryFilter = a}) . _Default . _Map;

-- | A value that if set to @true@, then the operation uses strongly
-- consistent reads; otherwise, eventually consistent reads are used.
--
-- Strongly consistent reads are not supported on global secondary indexes.
-- If you query a global secondary index with /ConsistentRead/ set to
-- @true@, you will receive an error message.
queConsistentRead :: Lens' Query (Maybe Bool)
queConsistentRead = lens _queConsistentRead (\ s a -> s{_queConsistentRead = a});

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
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
-- in the /Amazon DynamoDB Developer Guide/.
queExpressionAttributeNames :: Lens' Query (HashMap Text Text)
queExpressionAttributeNames = lens _queExpressionAttributeNames (\ s a -> s{_queExpressionAttributeNames = a}) . _Default . _Map;

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
--
-- You cannot use both /AttributesToGet/ and /Select/ together in a /Query/
-- request, /unless/ the value for /Select/ is @SPECIFIC_ATTRIBUTES@. (This
-- usage is equivalent to specifying /AttributesToGet/ without any value
-- for /Select/.)
--
-- If you query a local secondary index and request only attributes that
-- are projected into that index, the operation will read only the index
-- and not the table. If any of the requested attributes are not projected
-- into the local secondary index, DynamoDB will fetch each of these
-- attributes from the parent table. This extra fetching incurs additional
-- throughput cost and latency.
--
-- If you query a global secondary index, you can only request attributes
-- that are projected into the index. Global secondary index queries cannot
-- fetch attributes from the parent table.
queAttributesToGet :: Lens' Query (Maybe (NonEmpty Text))
queAttributesToGet = lens _queAttributesToGet (\ s a -> s{_queAttributesToGet = a}) . mapping _List1;

-- | FIXME: Undocumented member.
queReturnConsumedCapacity :: Lens' Query (Maybe ReturnConsumedCapacity)
queReturnConsumedCapacity = lens _queReturnConsumedCapacity (\ s a -> s{_queReturnConsumedCapacity = a});

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the /ProductStatus/ attribute was one of the following:
--
-- @Available | Backordered | Discontinued@
--
-- You would first need to specify /ExpressionAttributeValues/ as follows:
--
-- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
--
-- You could then use these values in an expression, such as this:
--
-- @ProductStatus IN (:avail, :back, :disc)@
--
-- For more information on expression attribute values, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
-- in the /Amazon DynamoDB Developer Guide/.
queExpressionAttributeValues :: Lens' Query (HashMap Text AttributeValue)
queExpressionAttributeValues = lens _queExpressionAttributeValues (\ s a -> s{_queExpressionAttributeValues = a}) . _Default . _Map;

-- | A value that specifies ascending (true) or descending (false) traversal
-- of the index. DynamoDB returns results reflecting the requested order
-- determined by the range key. If the data type is Number, the results are
-- returned in numeric order. For type String, the results are returned in
-- order of ASCII character code values. For type Binary, DynamoDB treats
-- each byte of the binary data as unsigned when it compares binary values.
--
-- If /ScanIndexForward/ is not specified, the results are returned in
-- ascending order.
queScanIndexForward :: Lens' Query (Maybe Bool)
queScanIndexForward = lens _queScanIndexForward (\ s a -> s{_queScanIndexForward = a});

-- | The maximum number of items to evaluate (not necessarily the number of
-- matching items). If DynamoDB processes the number of items up to the
-- limit while processing the results, it stops the operation and returns
-- the matching values up to that point, and a key in /LastEvaluatedKey/ to
-- apply in a subsequent operation, so that you can pick up where you left
-- off. Also, if the processed data set size exceeds 1 MB before DynamoDB
-- reaches this limit, it stops the operation and returns the matching
-- values up to the limit, and a key in /LastEvaluatedKey/ to apply in a
-- subsequent operation to continue the operation. For more information,
-- see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan>
-- in the /Amazon DynamoDB Developer Guide/.
queLimit :: Lens' Query (Maybe Natural)
queLimit = lens _queLimit (\ s a -> s{_queLimit = a}) . mapping _Nat;

-- | The attributes to be returned in the result. You can retrieve all item
-- attributes, specific item attributes, the count of matching items, or in
-- the case of an index, some or all of the attributes projected into the
-- index.
--
-- -   @ALL_ATTRIBUTES@ - Returns all of the item attributes from the
--     specified table or index. If you query a local secondary index, then
--     for each matching item in the index DynamoDB will fetch the entire
--     item from the parent table. If the index is configured to project
--     all item attributes, then all of the data can be obtained from the
--     local secondary index, and no fetching is required.
--
-- -   @ALL_PROJECTED_ATTRIBUTES@ - Allowed only when querying an index.
--     Retrieves all attributes that have been projected into the index. If
--     the index is configured to project all attributes, this return value
--     is equivalent to specifying @ALL_ATTRIBUTES@.
--
-- -   @COUNT@ - Returns the number of matching items, rather than the
--     matching items themselves.
--
-- -   @SPECIFIC_ATTRIBUTES@ - Returns only the attributes listed in
--     /AttributesToGet/. This return value is equivalent to specifying
--     /AttributesToGet/ without specifying any value for /Select/.
--
--     If you query a local secondary index and request only attributes
--     that are projected into that index, the operation will read only the
--     index and not the table. If any of the requested attributes are not
--     projected into the local secondary index, DynamoDB will fetch each
--     of these attributes from the parent table. This extra fetching
--     incurs additional throughput cost and latency.
--
--     If you query a global secondary index, you can only request
--     attributes that are projected into the index. Global secondary index
--     queries cannot fetch attributes from the parent table.
--
-- If neither /Select/ nor /AttributesToGet/ are specified, DynamoDB
-- defaults to @ALL_ATTRIBUTES@ when accessing a table, and
-- @ALL_PROJECTED_ATTRIBUTES@ when accessing an index. You cannot use both
-- /Select/ and /AttributesToGet/ together in a single request, unless the
-- value for /Select/ is @SPECIFIC_ATTRIBUTES@. (This usage is equivalent
-- to specifying /AttributesToGet/ without any value for /Select/.)
--
-- If you use the /ProjectionExpression/ parameter, then the value for
-- /Select/ can only be @SPECIFIC_ATTRIBUTES@. Any other value for /Select/
-- will return an error.
queSelect :: Lens' Query (Maybe Select)
queSelect = lens _queSelect (\ s a -> s{_queSelect = a});

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /FilterExpression/ instead. Do not combine legacy parameters
-- and expression parameters in a single API call; otherwise, DynamoDB will
-- return a /ValidationException/ exception.
--
-- A logical operator to apply to the conditions in a /QueryFilter/ map:
--
-- -   @AND@ - If all of the conditions evaluate to true, then the entire
--     map evaluates to true.
--
-- -   @OR@ - If at least one of the conditions evaluate to true, then the
--     entire map evaluates to true.
--
-- If you omit /ConditionalOperator/, then @AND@ is the default.
--
-- The operation will succeed only if the entire map evaluates to true.
--
-- This parameter does not support attributes of type List or Map.
queConditionalOperator :: Lens' Query (Maybe ConditionalOperator)
queConditionalOperator = lens _queConditionalOperator (\ s a -> s{_queConditionalOperator = a});

-- | The condition that specifies the key value(s) for items to be retrieved
-- by the /Query/ action.
--
-- The condition must perform an equality test on a single hash key value.
-- The condition can also test for one or more range key values. A /Query/
-- can use /KeyConditionExpression/ to retrieve a single item with a given
-- hash and range key value, or several items that have the same hash key
-- value but different range key values.
--
-- The hash key equality test is required, and must be specified in the
-- following format:
--
-- @hashAttributeName@ /=/ @:hashval@
--
-- If you also want to provide a range key condition, it must be combined
-- using /AND/ with the hash key condition. Following is an example, using
-- the __=__ comparison operator for the range key:
--
-- @hashAttributeName@ /=/ @:hashval@ /AND/ @rangeAttributeName@ /=/
-- @:rangeval@
--
-- Valid comparisons for the range key condition are as follows:
--
-- -   @rangeAttributeName@ /=/ @:rangeval@ - true if the range key is
--     equal to @:rangeval@.
--
-- -   @rangeAttributeName@ /\</ @:rangeval@ - true if the range key is
--     less than @:rangeval@.
--
-- -   @rangeAttributeName@ /\<=/ @:rangeval@ - true if the range key is
--     less than or equal to @:rangeval@.
--
-- -   @rangeAttributeName@ />/ @:rangeval@ - true if the range key is
--     greater than @:rangeval@.
--
-- -   @rangeAttributeName@ />=/ @:rangeval@ - true if the range key is
--     greater than or equal to @:rangeval@.
--
-- -   @rangeAttributeName@ /BETWEEN/ @:rangeval1@ /AND/ @:rangeval2@ -
--     true if the range key is less than or greater than @:rangeval1@, and
--     less than or equal to @:rangeval2@.
--
-- -   /begins_with (/@rangeAttributeName@, @:rangeval@/)/ - true if the
--     range key begins with a particular operand. Note that the function
--     name @begins_with@ is case-sensitive.
--
-- Use the /ExpressionAttributeValues/ parameter to replace tokens such as
-- @:hashval@ and @:rangeval@ with actual values at runtime.
--
-- You can optionally use the /ExpressionAttributeNames/ parameter to
-- replace the names of the hash and range attributes with placeholder
-- tokens. This might be necessary if an attribute name conflicts with a
-- DynamoDB reserved word. For example, the following
-- /KeyConditionExpression/ causes an error because /Size/ is a reserved
-- word:
--
-- -   @Size = :myval@
--
-- To work around this, define a placeholder (such a @#myval@) to represent
-- the attribute name /Size/. /KeyConditionExpression/ then is as follows:
--
-- -   @#S = :myval@
--
-- For a list of reserved words, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- For more information on /ExpressionAttributeNames/ and
-- /ExpressionAttributeValues/, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /KeyConditionExpression/ replaces the legacy /KeyConditions/ parameter.
queKeyConditionExpression :: Lens' Query (Maybe Text)
queKeyConditionExpression = lens _queKeyConditionExpression (\ s a -> s{_queKeyConditionExpression = a});

-- | The primary key of the first item that this operation will evaluate. Use
-- the value that was returned for /LastEvaluatedKey/ in the previous
-- operation.
--
-- The data type for /ExclusiveStartKey/ must be String, Number or Binary.
-- No set data types are allowed.
queExclusiveStartKey :: Lens' Query (HashMap Text AttributeValue)
queExclusiveStartKey = lens _queExclusiveStartKey (\ s a -> s{_queExclusiveStartKey = a}) . _Default . _Map;

-- | The name of an index to query. This index can be any local secondary
-- index or global secondary index on the table. Note that if you use the
-- /IndexName/ parameter, you must also provide /TableName./
queIndexName :: Lens' Query (Maybe Text)
queIndexName = lens _queIndexName (\ s a -> s{_queIndexName = a});

-- | The name of the table containing the requested items.
queTableName :: Lens' Query Text
queTableName = lens _queTableName (\ s a -> s{_queTableName = a});

instance AWSPager Query where
        page rq rs
          | stop (rs ^. qrLastEvaluatedKey) = Nothing
          | stop (rs ^. qrItems) = Nothing
          | otherwise =
            Just $ rq &
              queExclusiveStartKey .~ rs ^. qrLastEvaluatedKey

instance AWSRequest Query where
        type Sv Query = DynamoDB
        type Rs Query = QueryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 QueryResponse' <$>
                   (x .?> "LastEvaluatedKey" .!@ mempty) <*>
                     (x .?> "Count")
                     <*> (x .?> "ScannedCount")
                     <*> (x .?> "Items" .!@ mempty)
                     <*> (x .?> "ConsumedCapacity")
                     <*> (pure (fromEnum s)))

instance ToHeaders Query where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.Query" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON Query where
        toJSON Query'{..}
          = object
              ["ProjectionExpression" .= _queProjectionExpression,
               "KeyConditions" .= _queKeyConditions,
               "FilterExpression" .= _queFilterExpression,
               "QueryFilter" .= _queQueryFilter,
               "ConsistentRead" .= _queConsistentRead,
               "ExpressionAttributeNames" .=
                 _queExpressionAttributeNames,
               "AttributesToGet" .= _queAttributesToGet,
               "ReturnConsumedCapacity" .=
                 _queReturnConsumedCapacity,
               "ExpressionAttributeValues" .=
                 _queExpressionAttributeValues,
               "ScanIndexForward" .= _queScanIndexForward,
               "Limit" .= _queLimit, "Select" .= _queSelect,
               "ConditionalOperator" .= _queConditionalOperator,
               "KeyConditionExpression" .=
                 _queKeyConditionExpression,
               "ExclusiveStartKey" .= _queExclusiveStartKey,
               "IndexName" .= _queIndexName,
               "TableName" .= _queTableName]

instance ToPath Query where
        toPath = const "/"

instance ToQuery Query where
        toQuery = const mempty

-- | Represents the output of a /Query/ operation.
--
-- /See:/ 'queryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qrLastEvaluatedKey'
--
-- * 'qrCount'
--
-- * 'qrScannedCount'
--
-- * 'qrItems'
--
-- * 'qrConsumedCapacity'
--
-- * 'qrStatus'
data QueryResponse = QueryResponse'
    { _qrLastEvaluatedKey :: !(Maybe (Map Text AttributeValue))
    , _qrCount            :: !(Maybe Int)
    , _qrScannedCount     :: !(Maybe Int)
    , _qrItems            :: !(Maybe [Map Text AttributeValue])
    , _qrConsumedCapacity :: !(Maybe ConsumedCapacity)
    , _qrStatus           :: !Int
    } deriving (Eq,Show)

-- | 'QueryResponse' smart constructor.
queryResponse :: Int -> QueryResponse
queryResponse pStatus =
    QueryResponse'
    { _qrLastEvaluatedKey = Nothing
    , _qrCount = Nothing
    , _qrScannedCount = Nothing
    , _qrItems = Nothing
    , _qrConsumedCapacity = Nothing
    , _qrStatus = pStatus
    }

-- | The primary key of the item where the operation stopped, inclusive of
-- the previous result set. Use this value to start a new operation,
-- excluding this value in the new request.
--
-- If /LastEvaluatedKey/ is empty, then the \"last page\" of results has
-- been processed and there is no more data to be retrieved.
--
-- If /LastEvaluatedKey/ is not empty, it does not necessarily mean that
-- there is more data in the result set. The only way to know when you have
-- reached the end of the result set is when /LastEvaluatedKey/ is empty.
qrLastEvaluatedKey :: Lens' QueryResponse (HashMap Text AttributeValue)
qrLastEvaluatedKey = lens _qrLastEvaluatedKey (\ s a -> s{_qrLastEvaluatedKey = a}) . _Default . _Map;

-- | The number of items in the response.
--
-- If you used a /QueryFilter/ in the request, then /Count/ is the number
-- of items returned after the filter was applied, and /ScannedCount/ is
-- the number of matching items before> the filter was applied.
--
-- If you did not use a filter in the request, then /Count/ and
-- /ScannedCount/ are the same.
qrCount :: Lens' QueryResponse (Maybe Int)
qrCount = lens _qrCount (\ s a -> s{_qrCount = a});

-- | The number of items evaluated, before any /QueryFilter/ is applied. A
-- high /ScannedCount/ value with few, or no, /Count/ results indicates an
-- inefficient /Query/ operation. For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If you did not use a filter in the request, then /ScannedCount/ is the
-- same as /Count/.
qrScannedCount :: Lens' QueryResponse (Maybe Int)
qrScannedCount = lens _qrScannedCount (\ s a -> s{_qrScannedCount = a});

-- | An array of item attributes that match the query criteria. Each element
-- in this array consists of an attribute name and the value for that
-- attribute.
qrItems :: Lens' QueryResponse [HashMap Text AttributeValue]
qrItems = lens _qrItems (\ s a -> s{_qrItems = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
qrConsumedCapacity :: Lens' QueryResponse (Maybe ConsumedCapacity)
qrConsumedCapacity = lens _qrConsumedCapacity (\ s a -> s{_qrConsumedCapacity = a});

-- | FIXME: Undocumented member.
qrStatus :: Lens' QueryResponse Int
qrStatus = lens _qrStatus (\ s a -> s{_qrStatus = a});
