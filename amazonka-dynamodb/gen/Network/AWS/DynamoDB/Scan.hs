{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DynamoDB.Scan
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

-- | The /Scan/ operation returns one or more items and item attributes by
-- accessing every item in a table or a secondary index. To have DynamoDB
-- return fewer items, you can provide a /ScanFilter/ operation.
--
-- If the total number of scanned items exceeds the maximum data set size
-- limit of 1 MB, the scan stops and results are returned to the user as a
-- /LastEvaluatedKey/ value to continue the scan in a subsequent operation.
-- The results also include the number of items exceeding the limit. A scan
-- can result in no table data meeting the filter criteria.
--
-- The result set is eventually consistent.
--
-- By default, /Scan/ operations proceed sequentially; however, for faster
-- performance on a large table or secondary index, applications can
-- request a parallel /Scan/ operation by providing the /Segment/ and
-- /TotalSegments/ parameters. For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#QueryAndScanParallelScan Parallel Scan>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Scan.html>
module Network.AWS.DynamoDB.Scan
    (
    -- * Request
      Scan
    -- ** Request constructor
    , scan
    -- ** Request lenses
    , scaProjectionExpression
    , scaScanFilter
    , scaTotalSegments
    , scaFilterExpression
    , scaExpressionAttributeNames
    , scaAttributesToGet
    , scaReturnConsumedCapacity
    , scaExpressionAttributeValues
    , scaLimit
    , scaSelect
    , scaSegment
    , scaConditionalOperator
    , scaExclusiveStartKey
    , scaIndexName
    , scaTableName

    -- * Response
    , ScanResponse
    -- ** Response constructor
    , scanResponse
    -- ** Response lenses
    , srLastEvaluatedKey
    , srCount
    , srScannedCount
    , srItems
    , srConsumedCapacity
    , srStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /Scan/ operation.
--
-- /See:/ 'scan' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scaProjectionExpression'
--
-- * 'scaScanFilter'
--
-- * 'scaTotalSegments'
--
-- * 'scaFilterExpression'
--
-- * 'scaExpressionAttributeNames'
--
-- * 'scaAttributesToGet'
--
-- * 'scaReturnConsumedCapacity'
--
-- * 'scaExpressionAttributeValues'
--
-- * 'scaLimit'
--
-- * 'scaSelect'
--
-- * 'scaSegment'
--
-- * 'scaConditionalOperator'
--
-- * 'scaExclusiveStartKey'
--
-- * 'scaIndexName'
--
-- * 'scaTableName'
data Scan = Scan'
    { _scaProjectionExpression      :: !(Maybe Text)
    , _scaScanFilter                :: !(Maybe (Map Text Condition))
    , _scaTotalSegments             :: !(Maybe Nat)
    , _scaFilterExpression          :: !(Maybe Text)
    , _scaExpressionAttributeNames  :: !(Maybe (Map Text Text))
    , _scaAttributesToGet           :: !(Maybe (List1 Text))
    , _scaReturnConsumedCapacity    :: !(Maybe ReturnConsumedCapacity)
    , _scaExpressionAttributeValues :: !(Maybe (Map Text AttributeValue))
    , _scaLimit                     :: !(Maybe Nat)
    , _scaSelect                    :: !(Maybe Select)
    , _scaSegment                   :: !(Maybe Nat)
    , _scaConditionalOperator       :: !(Maybe ConditionalOperator)
    , _scaExclusiveStartKey         :: !(Maybe (Map Text AttributeValue))
    , _scaIndexName                 :: !(Maybe Text)
    , _scaTableName                 :: !Text
    } deriving (Eq,Read,Show)

-- | 'Scan' smart constructor.
scan :: Text -> Scan
scan pTableName =
    Scan'
    { _scaProjectionExpression = Nothing
    , _scaScanFilter = Nothing
    , _scaTotalSegments = Nothing
    , _scaFilterExpression = Nothing
    , _scaExpressionAttributeNames = Nothing
    , _scaAttributesToGet = Nothing
    , _scaReturnConsumedCapacity = Nothing
    , _scaExpressionAttributeValues = Nothing
    , _scaLimit = Nothing
    , _scaSelect = Nothing
    , _scaSegment = Nothing
    , _scaConditionalOperator = Nothing
    , _scaExclusiveStartKey = Nothing
    , _scaIndexName = Nothing
    , _scaTableName = pTableName
    }

-- | A string that identifies one or more attributes to retrieve from the
-- specified table or index. These attributes can include scalars, sets, or
-- elements of a JSON document. The attributes in the expression must be
-- separated by commas.
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
scaProjectionExpression :: Lens' Scan (Maybe Text)
scaProjectionExpression = lens _scaProjectionExpression (\ s a -> s{_scaProjectionExpression = a});

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /FilterExpression/ instead. Do not combine legacy parameters
-- and expression parameters in a single API call; otherwise, DynamoDB will
-- return a /ValidationException/ exception.
--
-- A condition that evaluates the scan results and returns only the desired
-- values.
--
-- This parameter does not support attributes of type List or Map.
--
-- If you specify more than one condition in the /ScanFilter/ map, then by
-- default all of the conditions must evaluate to true. In other words, the
-- conditions are ANDed together. (You can use the /ConditionalOperator/
-- parameter to OR the conditions instead. If you do this, then at least
-- one of the conditions must evaluate to true, rather than all of them.)
--
-- Each /ScanFilter/ element consists of an attribute name to compare,
-- along with the following:
--
-- -   /AttributeValueList/ - One or more values to evaluate against the
--     supplied attribute. The number of values in the list depends on the
--     operator specified in /ComparisonOperator/ .
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
--     For complete descriptions of all comparison operators, see
--     <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Condition.html Condition>.
--
scaScanFilter :: Lens' Scan (HashMap Text Condition)
scaScanFilter = lens _scaScanFilter (\ s a -> s{_scaScanFilter = a}) . _Default . _Map;

-- | For a parallel /Scan/ request, /TotalSegments/ represents the total
-- number of segments into which the /Scan/ operation will be divided. The
-- value of /TotalSegments/ corresponds to the number of application
-- workers that will perform the parallel scan. For example, if you want to
-- use four application threads to scan a table or an index, specify a
-- /TotalSegments/ value of 4.
--
-- The value for /TotalSegments/ must be greater than or equal to 1, and
-- less than or equal to 1000000. If you specify a /TotalSegments/ value of
-- 1, the /Scan/ operation will be sequential rather than parallel.
--
-- If you specify /TotalSegments/, you must also specify /Segment/.
scaTotalSegments :: Lens' Scan (Maybe Natural)
scaTotalSegments = lens _scaTotalSegments (\ s a -> s{_scaTotalSegments = a}) . mapping _Nat;

-- | A string that contains conditions that DynamoDB applies after the /Scan/
-- operation, but before the data is returned to you. Items that do not
-- satisfy the /FilterExpression/ criteria are not returned.
--
-- A /FilterExpression/ is applied after the items have already been read;
-- the process of filtering does not consume any additional read capacity
-- units.
--
-- For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#FilteringResults Filter Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /FilterExpression/ replaces the legacy /ScanFilter/ and
-- /ConditionalOperator/ parameters.
scaFilterExpression :: Lens' Scan (Maybe Text)
scaFilterExpression = lens _scaFilterExpression (\ s a -> s{_scaFilterExpression = a});

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
scaExpressionAttributeNames :: Lens' Scan (HashMap Text Text)
scaExpressionAttributeNames = lens _scaExpressionAttributeNames (\ s a -> s{_scaExpressionAttributeNames = a}) . _Default . _Map;

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
scaAttributesToGet :: Lens' Scan (Maybe (NonEmpty Text))
scaAttributesToGet = lens _scaAttributesToGet (\ s a -> s{_scaAttributesToGet = a}) . mapping _List1;

-- | FIXME: Undocumented member.
scaReturnConsumedCapacity :: Lens' Scan (Maybe ReturnConsumedCapacity)
scaReturnConsumedCapacity = lens _scaReturnConsumedCapacity (\ s a -> s{_scaReturnConsumedCapacity = a});

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
scaExpressionAttributeValues :: Lens' Scan (HashMap Text AttributeValue)
scaExpressionAttributeValues = lens _scaExpressionAttributeValues (\ s a -> s{_scaExpressionAttributeValues = a}) . _Default . _Map;

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
scaLimit :: Lens' Scan (Maybe Natural)
scaLimit = lens _scaLimit (\ s a -> s{_scaLimit = a}) . mapping _Nat;

-- | The attributes to be returned in the result. You can retrieve all item
-- attributes, specific item attributes, or the count of matching items.
--
-- -   @ALL_ATTRIBUTES@ - Returns all of the item attributes.
--
-- -   @COUNT@ - Returns the number of matching items, rather than the
--     matching items themselves.
--
-- -   @SPECIFIC_ATTRIBUTES@ - Returns only the attributes listed in
--     /AttributesToGet/. This return value is equivalent to specifying
--     /AttributesToGet/ without specifying any value for /Select/.
--
-- If neither /Select/ nor /AttributesToGet/ are specified, DynamoDB
-- defaults to @ALL_ATTRIBUTES@. You cannot use both /AttributesToGet/ and
-- /Select/ together in a single request, unless the value for /Select/ is
-- @SPECIFIC_ATTRIBUTES@. (This usage is equivalent to specifying
-- /AttributesToGet/ without any value for /Select/.)
scaSelect :: Lens' Scan (Maybe Select)
scaSelect = lens _scaSelect (\ s a -> s{_scaSelect = a});

-- | For a parallel /Scan/ request, /Segment/ identifies an individual
-- segment to be scanned by an application worker.
--
-- Segment IDs are zero-based, so the first segment is always 0. For
-- example, if you want to use four application threads to scan a table or
-- an index, then the first thread specifies a /Segment/ value of 0, the
-- second thread specifies 1, and so on.
--
-- The value of /LastEvaluatedKey/ returned from a parallel /Scan/ request
-- must be used as /ExclusiveStartKey/ with the same segment ID in a
-- subsequent /Scan/ operation.
--
-- The value for /Segment/ must be greater than or equal to 0, and less
-- than the value provided for /TotalSegments/.
--
-- If you provide /Segment/, you must also provide /TotalSegments/.
scaSegment :: Lens' Scan (Maybe Natural)
scaSegment = lens _scaSegment (\ s a -> s{_scaSegment = a}) . mapping _Nat;

-- | This is a legacy parameter, for backward compatibility. New applications
-- should use /FilterExpression/ instead. Do not combine legacy parameters
-- and expression parameters in a single API call; otherwise, DynamoDB will
-- return a /ValidationException/ exception.
--
-- A logical operator to apply to the conditions in a /ScanFilter/ map:
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
scaConditionalOperator :: Lens' Scan (Maybe ConditionalOperator)
scaConditionalOperator = lens _scaConditionalOperator (\ s a -> s{_scaConditionalOperator = a});

-- | The primary key of the first item that this operation will evaluate. Use
-- the value that was returned for /LastEvaluatedKey/ in the previous
-- operation.
--
-- The data type for /ExclusiveStartKey/ must be String, Number or Binary.
-- No set data types are allowed.
--
-- In a parallel scan, a /Scan/ request that includes /ExclusiveStartKey/
-- must specify the same segment whose previous /Scan/ returned the
-- corresponding value of /LastEvaluatedKey/.
scaExclusiveStartKey :: Lens' Scan (HashMap Text AttributeValue)
scaExclusiveStartKey = lens _scaExclusiveStartKey (\ s a -> s{_scaExclusiveStartKey = a}) . _Default . _Map;

-- | The name of a secondary index to scan. This index can be any local
-- secondary index or global secondary index. Note that if you use the
-- @IndexName@ parameter, you must also provide @TableName@.
scaIndexName :: Lens' Scan (Maybe Text)
scaIndexName = lens _scaIndexName (\ s a -> s{_scaIndexName = a});

-- | The name of the table containing the requested items; or, if you provide
-- @IndexName@, the name of the table to which that index belongs.
scaTableName :: Lens' Scan Text
scaTableName = lens _scaTableName (\ s a -> s{_scaTableName = a});

instance AWSPager Scan where
        page rq rs
          | stop (rs ^. srLastEvaluatedKey) = Nothing
          | stop (rs ^. srItems) = Nothing
          | otherwise =
            Just $ rq &
              scaExclusiveStartKey .~ rs ^. srLastEvaluatedKey

instance AWSRequest Scan where
        type Sv Scan = DynamoDB
        type Rs Scan = ScanResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ScanResponse' <$>
                   (x .?> "LastEvaluatedKey" .!@ mempty) <*>
                     (x .?> "Count")
                     <*> (x .?> "ScannedCount")
                     <*> (x .?> "Items" .!@ mempty)
                     <*> (x .?> "ConsumedCapacity")
                     <*> (pure s))

instance ToHeaders Scan where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.Scan" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON Scan where
        toJSON Scan'{..}
          = object
              ["ProjectionExpression" .= _scaProjectionExpression,
               "ScanFilter" .= _scaScanFilter,
               "TotalSegments" .= _scaTotalSegments,
               "FilterExpression" .= _scaFilterExpression,
               "ExpressionAttributeNames" .=
                 _scaExpressionAttributeNames,
               "AttributesToGet" .= _scaAttributesToGet,
               "ReturnConsumedCapacity" .=
                 _scaReturnConsumedCapacity,
               "ExpressionAttributeValues" .=
                 _scaExpressionAttributeValues,
               "Limit" .= _scaLimit, "Select" .= _scaSelect,
               "Segment" .= _scaSegment,
               "ConditionalOperator" .= _scaConditionalOperator,
               "ExclusiveStartKey" .= _scaExclusiveStartKey,
               "IndexName" .= _scaIndexName,
               "TableName" .= _scaTableName]

instance ToPath Scan where
        toPath = const "/"

instance ToQuery Scan where
        toQuery = const mempty

-- | Represents the output of a /Scan/ operation.
--
-- /See:/ 'scanResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srLastEvaluatedKey'
--
-- * 'srCount'
--
-- * 'srScannedCount'
--
-- * 'srItems'
--
-- * 'srConsumedCapacity'
--
-- * 'srStatus'
data ScanResponse = ScanResponse'
    { _srLastEvaluatedKey :: !(Maybe (Map Text AttributeValue))
    , _srCount            :: !(Maybe Int)
    , _srScannedCount     :: !(Maybe Int)
    , _srItems            :: !(Maybe [Map Text AttributeValue])
    , _srConsumedCapacity :: !(Maybe ConsumedCapacity)
    , _srStatus           :: !Status
    } deriving (Eq,Read,Show)

-- | 'ScanResponse' smart constructor.
scanResponse :: Status -> ScanResponse
scanResponse pStatus =
    ScanResponse'
    { _srLastEvaluatedKey = Nothing
    , _srCount = Nothing
    , _srScannedCount = Nothing
    , _srItems = Nothing
    , _srConsumedCapacity = Nothing
    , _srStatus = pStatus
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
srLastEvaluatedKey :: Lens' ScanResponse (HashMap Text AttributeValue)
srLastEvaluatedKey = lens _srLastEvaluatedKey (\ s a -> s{_srLastEvaluatedKey = a}) . _Default . _Map;

-- | The number of items in the response.
--
-- If you set /ScanFilter/ in the request, then /Count/ is the number of
-- items returned after the filter was applied, and /ScannedCount/ is the
-- number of matching items before the filter was applied.
--
-- If you did not use a filter in the request, then /Count/ is the same as
-- /ScannedCount/.
srCount :: Lens' ScanResponse (Maybe Int)
srCount = lens _srCount (\ s a -> s{_srCount = a});

-- | The number of items evaluated, before any /ScanFilter/ is applied. A
-- high /ScannedCount/ value with few, or no, /Count/ results indicates an
-- inefficient /Scan/ operation. For more information, see
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If you did not use a filter in the request, then /ScannedCount/ is the
-- same as /Count/.
srScannedCount :: Lens' ScanResponse (Maybe Int)
srScannedCount = lens _srScannedCount (\ s a -> s{_srScannedCount = a});

-- | An array of item attributes that match the scan criteria. Each element
-- in this array consists of an attribute name and the value for that
-- attribute.
srItems :: Lens' ScanResponse [HashMap Text AttributeValue]
srItems = lens _srItems (\ s a -> s{_srItems = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
srConsumedCapacity :: Lens' ScanResponse (Maybe ConsumedCapacity)
srConsumedCapacity = lens _srConsumedCapacity (\ s a -> s{_srConsumedCapacity = a});

-- | FIXME: Undocumented member.
srStatus :: Lens' ScanResponse Status
srStatus = lens _srStatus (\ s a -> s{_srStatus = a});
