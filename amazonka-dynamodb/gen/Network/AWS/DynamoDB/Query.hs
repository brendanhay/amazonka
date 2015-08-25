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
-- Module      : Network.AWS.DynamoDB.Query
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A /Query/ operation uses the primary key of a table or a secondary index
-- to directly access items from that table or index.
--
-- Use the /KeyConditionExpression/ parameter to provide a specific hash
-- key value. The /Query/ operation will return all of the items from the
-- table or index with that hash key value. You can optionally narrow the
-- scope of the /Query/ operation by specifying a range key value and a
-- comparison operator in /KeyConditionExpression/. You can use the
-- /ScanIndexForward/ parameter to get results in forward or reverse order,
-- by range key or by index key.
--
-- Queries that do not return results consume the minimum number of read
-- capacity units for that type of read operation.
--
-- If the total number of items meeting the query criteria exceeds the
-- result set size limit of 1 MB, the query stops and results are returned
-- to the user with the /LastEvaluatedKey/ element to continue the query in
-- a subsequent operation. Unlike a /Scan/ operation, a /Query/ operation
-- never returns both an empty result set and a /LastEvaluatedKey/ value.
-- /LastEvaluatedKey/ is only provided if the results exceed 1 MB, or if
-- you have used the /Limit/ parameter.
--
-- You can query a table, a local secondary index, or a global secondary
-- index. For a query on a table or on a local secondary index, you can set
-- the /ConsistentRead/ parameter to 'true' and obtain a strongly
-- consistent result. Global secondary indexes support eventually
-- consistent reads only, so do not specify /ConsistentRead/ when querying
-- a global secondary index.
--
-- /See:/ <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Query.html AWS API Reference> for Query.
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.Query
    (
    -- * Creating a Request
      query
    , Query
    -- * Request Lenses
    , qProjectionExpression
    , qKeyConditions
    , qFilterExpression
    , qQueryFilter
    , qConsistentRead
    , qExpressionAttributeNames
    , qAttributesToGet
    , qReturnConsumedCapacity
    , qExpressionAttributeValues
    , qScanIndexForward
    , qLimit
    , qSelect
    , qConditionalOperator
    , qKeyConditionExpression
    , qExclusiveStartKey
    , qIndexName
    , qTableName

    -- * Destructuring the Response
    , queryResponse
    , QueryResponse
    -- * Response Lenses
    , qrsLastEvaluatedKey
    , qrsCount
    , qrsScannedCount
    , qrsItems
    , qrsConsumedCapacity
    , qrsStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.DynamoDB.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /Query/ operation.
--
-- /See:/ 'query' smart constructor.
data Query = Query'
    { _qProjectionExpression      :: !(Maybe Text)
    , _qKeyConditions             :: !(Maybe (Map Text Condition))
    , _qFilterExpression          :: !(Maybe Text)
    , _qQueryFilter               :: !(Maybe (Map Text Condition))
    , _qConsistentRead            :: !(Maybe Bool)
    , _qExpressionAttributeNames  :: !(Maybe (Map Text Text))
    , _qAttributesToGet           :: !(Maybe (List1 Text))
    , _qReturnConsumedCapacity    :: !(Maybe ReturnConsumedCapacity)
    , _qExpressionAttributeValues :: !(Maybe (Map Text AttributeValue))
    , _qScanIndexForward          :: !(Maybe Bool)
    , _qLimit                     :: !(Maybe Nat)
    , _qSelect                    :: !(Maybe Select)
    , _qConditionalOperator       :: !(Maybe ConditionalOperator)
    , _qKeyConditionExpression    :: !(Maybe Text)
    , _qExclusiveStartKey         :: !(Maybe (Map Text AttributeValue))
    , _qIndexName                 :: !(Maybe Text)
    , _qTableName                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Query' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qProjectionExpression'
--
-- * 'qKeyConditions'
--
-- * 'qFilterExpression'
--
-- * 'qQueryFilter'
--
-- * 'qConsistentRead'
--
-- * 'qExpressionAttributeNames'
--
-- * 'qAttributesToGet'
--
-- * 'qReturnConsumedCapacity'
--
-- * 'qExpressionAttributeValues'
--
-- * 'qScanIndexForward'
--
-- * 'qLimit'
--
-- * 'qSelect'
--
-- * 'qConditionalOperator'
--
-- * 'qKeyConditionExpression'
--
-- * 'qExclusiveStartKey'
--
-- * 'qIndexName'
--
-- * 'qTableName'
query
    :: Text -- ^ 'qTableName'
    -> Query
query pTableName_ =
    Query'
    { _qProjectionExpression = Nothing
    , _qKeyConditions = Nothing
    , _qFilterExpression = Nothing
    , _qQueryFilter = Nothing
    , _qConsistentRead = Nothing
    , _qExpressionAttributeNames = Nothing
    , _qAttributesToGet = Nothing
    , _qReturnConsumedCapacity = Nothing
    , _qExpressionAttributeValues = Nothing
    , _qScanIndexForward = Nothing
    , _qLimit = Nothing
    , _qSelect = Nothing
    , _qConditionalOperator = Nothing
    , _qKeyConditionExpression = Nothing
    , _qExclusiveStartKey = Nothing
    , _qIndexName = Nothing
    , _qTableName = pTableName_
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
qProjectionExpression :: Lens' Query (Maybe Text)
qProjectionExpression = lens _qProjectionExpression (\ s a -> s{_qProjectionExpression = a});

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /KeyConditionExpression/ instead. Do not combine legacy
-- parameters and expression parameters in a single API call; otherwise,
-- DynamoDB will return a /ValidationException/ exception.
--
-- The selection criteria for the query. For a query on a table, you can
-- have conditions only on the table primary key attributes. You must
-- provide the hash key attribute name and value as an 'EQ' condition. You
-- can optionally provide a second condition, referring to the range key
-- attribute.
--
-- If you don\'t provide a range key condition, all of the items that match
-- the hash key will be retrieved. If a /FilterExpression/ or /QueryFilter/
-- is present, it will be applied after the items are retrieved.
--
-- For a query on an index, you can have conditions only on the index key
-- attributes. You must provide the index hash attribute name and value as
-- an 'EQ' condition. You can optionally provide a second condition,
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
--     based on ASCII character code values. For example, 'a' is greater
--     than 'A', and 'a' is greater than 'B'. For a list of code values,
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
--     'EQ | LE | LT | GE | GT | BEGINS_WITH | BETWEEN'
--
--     The following are descriptions of these comparison operators.
--
--     -   'EQ' : Equal.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of
--         type String, Number, or Binary (not a set type). If an item
--         contains an /AttributeValue/ element of a different type than
--         the one specified in the request, the value does not match. For
--         example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'. Also,
--         '{\"N\":\"6\"}' does not equal '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'LE' : Less than or equal.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         an item contains an /AttributeValue/ element of a different type
--         than the one provided in the request, the value does not match.
--         For example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'.
--         Also, '{\"N\":\"6\"}' does not compare to
--         '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'LT' : Less than.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of
--         type String, Number, or Binary (not a set type). If an item
--         contains an /AttributeValue/ element of a different type than
--         the one provided in the request, the value does not match. For
--         example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'. Also,
--         '{\"N\":\"6\"}' does not compare to
--         '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'GE' : Greater than or equal.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         an item contains an /AttributeValue/ element of a different type
--         than the one provided in the request, the value does not match.
--         For example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'.
--         Also, '{\"N\":\"6\"}' does not compare to
--         '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'GT' : Greater than.
--
--         /AttributeValueList/ can contain only one /AttributeValue/
--         element of type String, Number, or Binary (not a set type). If
--         an item contains an /AttributeValue/ element of a different type
--         than the one provided in the request, the value does not match.
--         For example, '{\"S\":\"6\"}' does not equal '{\"N\":\"6\"}'.
--         Also, '{\"N\":\"6\"}' does not compare to
--         '{\"NS\":[\"6\", \"2\", \"1\"]}'.
--
--     -   'BEGINS_WITH' : Checks for a prefix.
--
--         /AttributeValueList/ can contain only one /AttributeValue/ of
--         type String or Binary (not a Number or a set type). The target
--         attribute of the comparison must be of type String or Binary
--         (not a Number or a set type).
--
--     -   'BETWEEN' : Greater than or equal to the first value, and less
--         than or equal to the second value.
--
--         /AttributeValueList/ must contain two /AttributeValue/ elements
--         of the same type, either String, Number, or Binary (not a set
--         type). A target attribute matches if the target value is greater
--         than, or equal to, the first element and less than, or equal to,
--         the second element. If an item contains an /AttributeValue/
--         element of a different type than the one provided in the
--         request, the value does not match. For example, '{\"S\":\"6\"}'
--         does not compare to '{\"N\":\"6\"}'. Also, '{\"N\":\"6\"}' does
--         not compare to '{\"NS\":[\"6\", \"2\", \"1\"]}'
--
-- For usage examples of /AttributeValueList/ and /ComparisonOperator/, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters>
-- in the /Amazon DynamoDB Developer Guide/.
qKeyConditions :: Lens' Query (HashMap Text Condition)
qKeyConditions = lens _qKeyConditions (\ s a -> s{_qKeyConditions = a}) . _Default . _Map;

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
qFilterExpression :: Lens' Query (Maybe Text)
qFilterExpression = lens _qFilterExpression (\ s a -> s{_qFilterExpression = a});

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
--     based on ASCII character code values. For example, 'a' is greater
--     than 'A', and 'a' is greater than 'B'. For a list of code values,
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
--     'EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN'
--
--     For complete descriptions of all comparison operators, see the
--     <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Condition.html Condition>
--     data type.
--
qQueryFilter :: Lens' Query (HashMap Text Condition)
qQueryFilter = lens _qQueryFilter (\ s a -> s{_qQueryFilter = a}) . _Default . _Map;

-- | Determines the read consistency model: If set to 'true', then the
-- operation uses strongly consistent reads; otherwise, the operation uses
-- eventually consistent reads.
--
-- Strongly consistent reads are not supported on global secondary indexes.
-- If you query a global secondary index with /ConsistentRead/ set to
-- 'true', you will receive a /ValidationException/.
qConsistentRead :: Lens' Query (Maybe Bool)
qConsistentRead = lens _qConsistentRead (\ s a -> s{_qConsistentRead = a});

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
-- -   'Percentile'
--
-- The name of this attribute conflicts with a reserved word, so it cannot
-- be used directly in an expression. (For the complete list of reserved
-- words, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
-- could specify the following for /ExpressionAttributeNames/:
--
-- -   '{\"#P\":\"Percentile\"}'
--
-- You could then use this substitution in an expression, as in this
-- example:
--
-- -   '#P = :val'
--
-- Tokens that begin with the __:__ character are /expression attribute
-- values/, which are placeholders for the actual value at runtime.
--
-- For more information on expression attribute names, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
qExpressionAttributeNames :: Lens' Query (HashMap Text Text)
qExpressionAttributeNames = lens _qExpressionAttributeNames (\ s a -> s{_qExpressionAttributeNames = a}) . _Default . _Map;

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
-- request, /unless/ the value for /Select/ is 'SPECIFIC_ATTRIBUTES'. (This
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
qAttributesToGet :: Lens' Query (Maybe (NonEmpty Text))
qAttributesToGet = lens _qAttributesToGet (\ s a -> s{_qAttributesToGet = a}) . mapping _List1;

-- | Undocumented member.
qReturnConsumedCapacity :: Lens' Query (Maybe ReturnConsumedCapacity)
qReturnConsumedCapacity = lens _qReturnConsumedCapacity (\ s a -> s{_qReturnConsumedCapacity = a});

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the /ProductStatus/ attribute was one of the following:
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
-- For more information on expression attribute values, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions>
-- in the /Amazon DynamoDB Developer Guide/.
qExpressionAttributeValues :: Lens' Query (HashMap Text AttributeValue)
qExpressionAttributeValues = lens _qExpressionAttributeValues (\ s a -> s{_qExpressionAttributeValues = a}) . _Default . _Map;

-- | Specifies the order in which to return the query results - either
-- ascending ('true') or descending ('false').
--
-- Items with the same hash key are stored in sorted order by range key .If
-- the range key data type is Number, the results are stored in numeric
-- order. For type String, the results are returned in order of ASCII
-- character code values. For type Binary, DynamoDB treats each byte of the
-- binary data as unsigned.
--
-- If /ScanIndexForward/ is 'true', DynamoDB returns the results in order,
-- by range key. This is the default behavior.
--
-- If /ScanIndexForward/ is 'false', DynamoDB sorts the results in
-- descending order by range key, and then returns the results to the
-- client.
qScanIndexForward :: Lens' Query (Maybe Bool)
qScanIndexForward = lens _qScanIndexForward (\ s a -> s{_qScanIndexForward = a});

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
qLimit :: Lens' Query (Maybe Natural)
qLimit = lens _qLimit (\ s a -> s{_qLimit = a}) . mapping _Nat;

-- | The attributes to be returned in the result. You can retrieve all item
-- attributes, specific item attributes, the count of matching items, or in
-- the case of an index, some or all of the attributes projected into the
-- index.
--
-- -   'ALL_ATTRIBUTES' - Returns all of the item attributes from the
--     specified table or index. If you query a local secondary index, then
--     for each matching item in the index DynamoDB will fetch the entire
--     item from the parent table. If the index is configured to project
--     all item attributes, then all of the data can be obtained from the
--     local secondary index, and no fetching is required.
--
-- -   'ALL_PROJECTED_ATTRIBUTES' - Allowed only when querying an index.
--     Retrieves all attributes that have been projected into the index. If
--     the index is configured to project all attributes, this return value
--     is equivalent to specifying 'ALL_ATTRIBUTES'.
--
-- -   'COUNT' - Returns the number of matching items, rather than the
--     matching items themselves.
--
-- -   'SPECIFIC_ATTRIBUTES' - Returns only the attributes listed in
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
-- defaults to 'ALL_ATTRIBUTES' when accessing a table, and
-- 'ALL_PROJECTED_ATTRIBUTES' when accessing an index. You cannot use both
-- /Select/ and /AttributesToGet/ together in a single request, unless the
-- value for /Select/ is 'SPECIFIC_ATTRIBUTES'. (This usage is equivalent
-- to specifying /AttributesToGet/ without any value for /Select/.)
--
-- If you use the /ProjectionExpression/ parameter, then the value for
-- /Select/ can only be 'SPECIFIC_ATTRIBUTES'. Any other value for /Select/
-- will return an error.
qSelect :: Lens' Query (Maybe Select)
qSelect = lens _qSelect (\ s a -> s{_qSelect = a});

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /FilterExpression/ instead. Do not combine legacy parameters
-- and expression parameters in a single API call; otherwise, DynamoDB will
-- return a /ValidationException/ exception.
--
-- A logical operator to apply to the conditions in a /QueryFilter/ map:
--
-- -   'AND' - If all of the conditions evaluate to true, then the entire
--     map evaluates to true.
--
-- -   'OR' - If at least one of the conditions evaluate to true, then the
--     entire map evaluates to true.
--
-- If you omit /ConditionalOperator/, then 'AND' is the default.
--
-- The operation will succeed only if the entire map evaluates to true.
--
-- This parameter does not support attributes of type List or Map.
qConditionalOperator :: Lens' Query (Maybe ConditionalOperator)
qConditionalOperator = lens _qConditionalOperator (\ s a -> s{_qConditionalOperator = a});

-- | The condition that specifies the key value(s) for items to be retrieved
-- by the /Query/ action.
--
-- The condition must perform an equality test on a single hash key value.
-- The condition can also perform one of several comparison tests on a
-- single range key value. /Query/ can use /KeyConditionExpression/ to
-- retrieve one item with a given hash and range key value, or several
-- items that have the same hash key value but different range key values.
--
-- The hash key equality test is required, and must be specified in the
-- following format:
--
-- 'hashAttributeName' /=/ ':hashval'
--
-- If you also want to provide a range key condition, it must be combined
-- using /AND/ with the hash key condition. Following is an example, using
-- the __=__ comparison operator for the range key:
--
-- 'hashAttributeName' /=/ ':hashval' /AND/ 'rangeAttributeName' /=/
-- ':rangeval'
--
-- Valid comparisons for the range key condition are as follows:
--
-- -   'rangeAttributeName' /=/ ':rangeval' - true if the range key is
--     equal to ':rangeval'.
--
-- -   'rangeAttributeName' /\</ ':rangeval' - true if the range key is
--     less than ':rangeval'.
--
-- -   'rangeAttributeName' /\<=/ ':rangeval' - true if the range key is
--     less than or equal to ':rangeval'.
--
-- -   'rangeAttributeName' />/ ':rangeval' - true if the range key is
--     greater than ':rangeval'.
--
-- -   'rangeAttributeName' />=/ ':rangeval' - true if the range key is
--     greater than or equal to ':rangeval'.
--
-- -   'rangeAttributeName' /BETWEEN/ ':rangeval1' /AND/ ':rangeval2' -
--     true if the range key is greater than or equal to ':rangeval1', and
--     less than or equal to ':rangeval2'.
--
-- -   /begins_with (/'rangeAttributeName', ':rangeval'/)/ - true if the
--     range key begins with a particular operand. (You cannot use this
--     function with a range key that is of type Number.) Note that the
--     function name 'begins_with' is case-sensitive.
--
-- Use the /ExpressionAttributeValues/ parameter to replace tokens such as
-- ':hashval' and ':rangeval' with actual values at runtime.
--
-- You can optionally use the /ExpressionAttributeNames/ parameter to
-- replace the names of the hash and range attributes with placeholder
-- tokens. This option might be necessary if an attribute name conflicts
-- with a DynamoDB reserved word. For example, the following
-- /KeyConditionExpression/ parameter causes an error because /Size/ is a
-- reserved word:
--
-- -   'Size = :myval'
--
-- To work around this, define a placeholder (such a '#S') to represent the
-- attribute name /Size/. /KeyConditionExpression/ then is as follows:
--
-- -   '#S = :myval'
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
qKeyConditionExpression :: Lens' Query (Maybe Text)
qKeyConditionExpression = lens _qKeyConditionExpression (\ s a -> s{_qKeyConditionExpression = a});

-- | The primary key of the first item that this operation will evaluate. Use
-- the value that was returned for /LastEvaluatedKey/ in the previous
-- operation.
--
-- The data type for /ExclusiveStartKey/ must be String, Number or Binary.
-- No set data types are allowed.
qExclusiveStartKey :: Lens' Query (HashMap Text AttributeValue)
qExclusiveStartKey = lens _qExclusiveStartKey (\ s a -> s{_qExclusiveStartKey = a}) . _Default . _Map;

-- | The name of an index to query. This index can be any local secondary
-- index or global secondary index on the table. Note that if you use the
-- /IndexName/ parameter, you must also provide /TableName./
qIndexName :: Lens' Query (Maybe Text)
qIndexName = lens _qIndexName (\ s a -> s{_qIndexName = a});

-- | The name of the table containing the requested items.
qTableName :: Lens' Query Text
qTableName = lens _qTableName (\ s a -> s{_qTableName = a});

instance AWSPager Query where
        page rq rs
          | stop (rs ^. qrsLastEvaluatedKey) = Nothing
          | stop (rs ^. qrsItems) = Nothing
          | otherwise =
            Just $ rq &
              qExclusiveStartKey .~ rs ^. qrsLastEvaluatedKey

instance AWSRequest Query where
        type Rs Query = QueryResponse
        request = postJSON dynamoDB
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
              (catMaybes
                 [("ProjectionExpression" .=) <$>
                    _qProjectionExpression,
                  ("KeyConditions" .=) <$> _qKeyConditions,
                  ("FilterExpression" .=) <$> _qFilterExpression,
                  ("QueryFilter" .=) <$> _qQueryFilter,
                  ("ConsistentRead" .=) <$> _qConsistentRead,
                  ("ExpressionAttributeNames" .=) <$>
                    _qExpressionAttributeNames,
                  ("AttributesToGet" .=) <$> _qAttributesToGet,
                  ("ReturnConsumedCapacity" .=) <$>
                    _qReturnConsumedCapacity,
                  ("ExpressionAttributeValues" .=) <$>
                    _qExpressionAttributeValues,
                  ("ScanIndexForward" .=) <$> _qScanIndexForward,
                  ("Limit" .=) <$> _qLimit, ("Select" .=) <$> _qSelect,
                  ("ConditionalOperator" .=) <$> _qConditionalOperator,
                  ("KeyConditionExpression" .=) <$>
                    _qKeyConditionExpression,
                  ("ExclusiveStartKey" .=) <$> _qExclusiveStartKey,
                  ("IndexName" .=) <$> _qIndexName,
                  Just ("TableName" .= _qTableName)])

instance ToPath Query where
        toPath = const "/"

instance ToQuery Query where
        toQuery = const mempty

-- | Represents the output of a /Query/ operation.
--
-- /See:/ 'queryResponse' smart constructor.
data QueryResponse = QueryResponse'
    { _qrsLastEvaluatedKey :: !(Maybe (Map Text AttributeValue))
    , _qrsCount            :: !(Maybe Int)
    , _qrsScannedCount     :: !(Maybe Int)
    , _qrsItems            :: !(Maybe [Map Text AttributeValue])
    , _qrsConsumedCapacity :: !(Maybe ConsumedCapacity)
    , _qrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'QueryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qrsLastEvaluatedKey'
--
-- * 'qrsCount'
--
-- * 'qrsScannedCount'
--
-- * 'qrsItems'
--
-- * 'qrsConsumedCapacity'
--
-- * 'qrsStatus'
queryResponse
    :: Int -- ^ 'qrsStatus'
    -> QueryResponse
queryResponse pStatus_ =
    QueryResponse'
    { _qrsLastEvaluatedKey = Nothing
    , _qrsCount = Nothing
    , _qrsScannedCount = Nothing
    , _qrsItems = Nothing
    , _qrsConsumedCapacity = Nothing
    , _qrsStatus = pStatus_
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
qrsLastEvaluatedKey :: Lens' QueryResponse (HashMap Text AttributeValue)
qrsLastEvaluatedKey = lens _qrsLastEvaluatedKey (\ s a -> s{_qrsLastEvaluatedKey = a}) . _Default . _Map;

-- | The number of items in the response.
--
-- If you used a /QueryFilter/ in the request, then /Count/ is the number
-- of items returned after the filter was applied, and /ScannedCount/ is
-- the number of matching items before> the filter was applied.
--
-- If you did not use a filter in the request, then /Count/ and
-- /ScannedCount/ are the same.
qrsCount :: Lens' QueryResponse (Maybe Int)
qrsCount = lens _qrsCount (\ s a -> s{_qrsCount = a});

-- | The number of items evaluated, before any /QueryFilter/ is applied. A
-- high /ScannedCount/ value with few, or no, /Count/ results indicates an
-- inefficient /Query/ operation. For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If you did not use a filter in the request, then /ScannedCount/ is the
-- same as /Count/.
qrsScannedCount :: Lens' QueryResponse (Maybe Int)
qrsScannedCount = lens _qrsScannedCount (\ s a -> s{_qrsScannedCount = a});

-- | An array of item attributes that match the query criteria. Each element
-- in this array consists of an attribute name and the value for that
-- attribute.
qrsItems :: Lens' QueryResponse [HashMap Text AttributeValue]
qrsItems = lens _qrsItems (\ s a -> s{_qrsItems = a}) . _Default . _Coerce;

-- | Undocumented member.
qrsConsumedCapacity :: Lens' QueryResponse (Maybe ConsumedCapacity)
qrsConsumedCapacity = lens _qrsConsumedCapacity (\ s a -> s{_qrsConsumedCapacity = a});

-- | The response status code.
qrsStatus :: Lens' QueryResponse Int
qrsStatus = lens _qrsStatus (\ s a -> s{_qrsStatus = a});
