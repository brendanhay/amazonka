{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Scan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @Scan@ operation returns one or more items and item attributes by accessing every item in a table or a secondary index. To have DynamoDB return fewer items, you can provide a @FilterExpression@ operation.
--
-- If the total number of scanned items exceeds the maximum dataset size limit of 1 MB, the scan stops and results are returned to the user as a @LastEvaluatedKey@ value to continue the scan in a subsequent operation. The results also include the number of items exceeding the limit. A scan can result in no table data meeting the filter criteria.
-- A single @Scan@ operation reads up to the maximum number of items set (if using the @Limit@ parameter) or a maximum of 1 MB of data and then apply any filtering to the results using @FilterExpression@ . If @LastEvaluatedKey@ is present in the response, you need to paginate the result set. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Scan.html#Scan.Pagination Paginating the Results> in the /Amazon DynamoDB Developer Guide/ .
-- @Scan@ operations proceed sequentially; however, for faster performance on a large table or secondary index, applications can request a parallel @Scan@ operation by providing the @Segment@ and @TotalSegments@ parameters. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Scan.html#Scan.ParallelScan Parallel Scan> in the /Amazon DynamoDB Developer Guide/ .
-- @Scan@ uses eventually consistent reads when accessing the data in a table; therefore, the result set might not include the changes to data in the table immediately before the operation began. If you need a consistent copy of the data, as of the time that the @Scan@ begins, you can set the @ConsistentRead@ parameter to @true@ .
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.Scan
  ( -- * Creating a request
    Scan (..),
    mkScan,

    -- ** Request lenses
    sProjectionExpression,
    sScanFilter,
    sAttributesToGet,
    sTotalSegments,
    sExpressionAttributeNames,
    sFilterExpression,
    sConsistentRead,
    sExpressionAttributeValues,
    sReturnConsumedCapacity,
    sLimit,
    sSelect,
    sSegment,
    sConditionalOperator,
    sExclusiveStartKey,
    sIndexName,
    sTableName,

    -- * Destructuring the response
    ScanResponse (..),
    mkScanResponse,

    -- ** Response lenses
    srsLastEvaluatedKey,
    srsCount,
    srsScannedCount,
    srsItems,
    srsConsumedCapacity,
    srsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @Scan@ operation.
--
-- /See:/ 'mkScan' smart constructor.
data Scan = Scan'
  { projectionExpression :: Lude.Maybe Lude.Text,
    scanFilter :: Lude.Maybe (Lude.HashMap Lude.Text (Condition)),
    attributesToGet :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    totalSegments :: Lude.Maybe Lude.Natural,
    expressionAttributeNames ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    filterExpression :: Lude.Maybe Lude.Text,
    consistentRead :: Lude.Maybe Lude.Bool,
    expressionAttributeValues ::
      Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    returnConsumedCapacity :: Lude.Maybe ReturnConsumedCapacity,
    limit :: Lude.Maybe Lude.Natural,
    select :: Lude.Maybe Select,
    segment :: Lude.Maybe Lude.Natural,
    conditionalOperator :: Lude.Maybe ConditionalOperator,
    exclusiveStartKey ::
      Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    indexName :: Lude.Maybe Lude.Text,
    tableName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scan' with the minimum fields required to make a request.
--
-- * 'attributesToGet' - This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
-- * 'conditionalOperator' - This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
-- * 'consistentRead' - A Boolean value that determines the read consistency model during the scan:
--
--
--     * If @ConsistentRead@ is @false@ , then the data returned from @Scan@ might not contain the results from other recently completed write operations (@PutItem@ , @UpdateItem@ , or @DeleteItem@ ).
--
--
--     * If @ConsistentRead@ is @true@ , then all of the write operations that completed before the @Scan@ began are guaranteed to be contained in the @Scan@ response.
--
--
-- The default setting for @ConsistentRead@ is @false@ .
-- The @ConsistentRead@ parameter is not supported on global secondary indexes. If you scan a global secondary index with @ConsistentRead@ set to true, you will receive a @ValidationException@ .
-- * 'exclusiveStartKey' - The primary key of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedKey@ in the previous operation.
--
-- The data type for @ExclusiveStartKey@ must be String, Number or Binary. No set data types are allowed.
-- In a parallel scan, a @Scan@ request that includes @ExclusiveStartKey@ must specify the same segment whose previous @Scan@ returned the corresponding value of @LastEvaluatedKey@ .
-- * 'expressionAttributeNames' - One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :
--
--
--     * To access an attribute whose name conflicts with a DynamoDB reserved word.
--
--
--     * To create a placeholder for repeating occurrences of an attribute name in an expression.
--
--
--     * To prevent special characters in an attribute name from being misinterpreted in an expression.
--
--
-- Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:
--
--     * @Percentile@
--
--
-- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :
--
--     * @{"#P":"Percentile"}@
--
--
-- You could then use this substitution in an expression, as in this example:
--
--     * @#P = :val@
--
--
-- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
-- * 'expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the @ProductStatus@ attribute was one of the following:
-- @Available | Backordered | Discontinued@
-- You would first need to specify @ExpressionAttributeValues@ as follows:
-- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
-- You could then use these values in an expression, such as this:
-- @ProductStatus IN (:avail, :back, :disc)@
-- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
-- * 'filterExpression' - A string that contains conditions that DynamoDB applies after the @Scan@ operation, but before the data is returned to you. Items that do not satisfy the @FilterExpression@ criteria are not returned.
--
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#FilteringResults Filter Expressions> in the /Amazon DynamoDB Developer Guide/ .
-- * 'indexName' - The name of a secondary index to scan. This index can be any local secondary index or global secondary index. Note that if you use the @IndexName@ parameter, you must also provide @TableName@ .
-- * 'limit' - The maximum number of items to evaluate (not necessarily the number of matching items). If DynamoDB processes the number of items up to the limit while processing the results, it stops the operation and returns the matching values up to that point, and a key in @LastEvaluatedKey@ to apply in a subsequent operation, so that you can pick up where you left off. Also, if the processed dataset size exceeds 1 MB before DynamoDB reaches this limit, it stops the operation and returns the matching values up to the limit, and a key in @LastEvaluatedKey@ to apply in a subsequent operation to continue the operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Working with Queries> in the /Amazon DynamoDB Developer Guide/ .
-- * 'projectionExpression' - A string that identifies one or more attributes to retrieve from the specified table or index. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
-- * 'returnConsumedCapacity' - Undocumented field.
-- * 'scanFilter' - This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ScanFilter.html ScanFilter> in the /Amazon DynamoDB Developer Guide/ .
-- * 'segment' - For a parallel @Scan@ request, @Segment@ identifies an individual segment to be scanned by an application worker.
--
-- Segment IDs are zero-based, so the first segment is always 0. For example, if you want to use four application threads to scan a table or an index, then the first thread specifies a @Segment@ value of 0, the second thread specifies 1, and so on.
-- The value of @LastEvaluatedKey@ returned from a parallel @Scan@ request must be used as @ExclusiveStartKey@ with the same segment ID in a subsequent @Scan@ operation.
-- The value for @Segment@ must be greater than or equal to 0, and less than the value provided for @TotalSegments@ .
-- If you provide @Segment@ , you must also provide @TotalSegments@ .
-- * 'select' - The attributes to be returned in the result. You can retrieve all item attributes, specific item attributes, the count of matching items, or in the case of an index, some or all of the attributes projected into the index.
--
--
--     * @ALL_ATTRIBUTES@ - Returns all of the item attributes from the specified table or index. If you query a local secondary index, then for each matching item in the index, DynamoDB fetches the entire item from the parent table. If the index is configured to project all item attributes, then all of the data can be obtained from the local secondary index, and no fetching is required.
--
--
--     * @ALL_PROJECTED_ATTRIBUTES@ - Allowed only when querying an index. Retrieves all attributes that have been projected into the index. If the index is configured to project all attributes, this return value is equivalent to specifying @ALL_ATTRIBUTES@ .
--
--
--     * @COUNT@ - Returns the number of matching items, rather than the matching items themselves.
--
--
--     * @SPECIFIC_ATTRIBUTES@ - Returns only the attributes listed in @AttributesToGet@ . This return value is equivalent to specifying @AttributesToGet@ without specifying any value for @Select@ .
-- If you query or scan a local secondary index and request only attributes that are projected into that index, the operation reads only the index and not the table. If any of the requested attributes are not projected into the local secondary index, DynamoDB fetches each of these attributes from the parent table. This extra fetching incurs additional throughput cost and latency.
-- If you query or scan a global secondary index, you can only request attributes that are projected into the index. Global secondary index queries cannot fetch attributes from the parent table.
--
--
-- If neither @Select@ nor @AttributesToGet@ are specified, DynamoDB defaults to @ALL_ATTRIBUTES@ when accessing a table, and @ALL_PROJECTED_ATTRIBUTES@ when accessing an index. You cannot use both @Select@ and @AttributesToGet@ together in a single request, unless the value for @Select@ is @SPECIFIC_ATTRIBUTES@ . (This usage is equivalent to specifying @AttributesToGet@ without any value for @Select@ .)
-- * 'tableName' - The name of the table containing the requested items; or, if you provide @IndexName@ , the name of the table to which that index belongs.
-- * 'totalSegments' - For a parallel @Scan@ request, @TotalSegments@ represents the total number of segments into which the @Scan@ operation will be divided. The value of @TotalSegments@ corresponds to the number of application workers that will perform the parallel scan. For example, if you want to use four application threads to scan a table or an index, specify a @TotalSegments@ value of 4.
--
-- The value for @TotalSegments@ must be greater than or equal to 1, and less than or equal to 1000000. If you specify a @TotalSegments@ value of 1, the @Scan@ operation will be sequential rather than parallel.
-- If you specify @TotalSegments@ , you must also specify @Segment@ .
mkScan ::
  -- | 'tableName'
  Lude.Text ->
  Scan
mkScan pTableName_ =
  Scan'
    { projectionExpression = Lude.Nothing,
      scanFilter = Lude.Nothing,
      attributesToGet = Lude.Nothing,
      totalSegments = Lude.Nothing,
      expressionAttributeNames = Lude.Nothing,
      filterExpression = Lude.Nothing,
      consistentRead = Lude.Nothing,
      expressionAttributeValues = Lude.Nothing,
      returnConsumedCapacity = Lude.Nothing,
      limit = Lude.Nothing,
      select = Lude.Nothing,
      segment = Lude.Nothing,
      conditionalOperator = Lude.Nothing,
      exclusiveStartKey = Lude.Nothing,
      indexName = Lude.Nothing,
      tableName = pTableName_
    }

-- | A string that identifies one or more attributes to retrieve from the specified table or index. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'projectionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProjectionExpression :: Lens.Lens' Scan (Lude.Maybe Lude.Text)
sProjectionExpression = Lens.lens (projectionExpression :: Scan -> Lude.Maybe Lude.Text) (\s a -> s {projectionExpression = a} :: Scan)
{-# DEPRECATED sProjectionExpression "Use generic-lens or generic-optics with 'projectionExpression' instead." #-}

-- | This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ScanFilter.html ScanFilter> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'scanFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScanFilter :: Lens.Lens' Scan (Lude.Maybe (Lude.HashMap Lude.Text (Condition)))
sScanFilter = Lens.lens (scanFilter :: Scan -> Lude.Maybe (Lude.HashMap Lude.Text (Condition))) (\s a -> s {scanFilter = a} :: Scan)
{-# DEPRECATED sScanFilter "Use generic-lens or generic-optics with 'scanFilter' instead." #-}

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'attributesToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAttributesToGet :: Lens.Lens' Scan (Lude.Maybe (Lude.NonEmpty Lude.Text))
sAttributesToGet = Lens.lens (attributesToGet :: Scan -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {attributesToGet = a} :: Scan)
{-# DEPRECATED sAttributesToGet "Use generic-lens or generic-optics with 'attributesToGet' instead." #-}

-- | For a parallel @Scan@ request, @TotalSegments@ represents the total number of segments into which the @Scan@ operation will be divided. The value of @TotalSegments@ corresponds to the number of application workers that will perform the parallel scan. For example, if you want to use four application threads to scan a table or an index, specify a @TotalSegments@ value of 4.
--
-- The value for @TotalSegments@ must be greater than or equal to 1, and less than or equal to 1000000. If you specify a @TotalSegments@ value of 1, the @Scan@ operation will be sequential rather than parallel.
-- If you specify @TotalSegments@ , you must also specify @Segment@ .
--
-- /Note:/ Consider using 'totalSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTotalSegments :: Lens.Lens' Scan (Lude.Maybe Lude.Natural)
sTotalSegments = Lens.lens (totalSegments :: Scan -> Lude.Maybe Lude.Natural) (\s a -> s {totalSegments = a} :: Scan)
{-# DEPRECATED sTotalSegments "Use generic-lens or generic-optics with 'totalSegments' instead." #-}

-- | One or more substitution tokens for attribute names in an expression. The following are some use cases for using @ExpressionAttributeNames@ :
--
--
--     * To access an attribute whose name conflicts with a DynamoDB reserved word.
--
--
--     * To create a placeholder for repeating occurrences of an attribute name in an expression.
--
--
--     * To prevent special characters in an attribute name from being misinterpreted in an expression.
--
--
-- Use the __#__ character in an expression to dereference an attribute name. For example, consider the following attribute name:
--
--     * @Percentile@
--
--
-- The name of this attribute conflicts with a reserved word, so it cannot be used directly in an expression. (For the complete list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ ). To work around this, you could specify the following for @ExpressionAttributeNames@ :
--
--     * @{"#P":"Percentile"}@
--
--
-- You could then use this substitution in an expression, as in this example:
--
--     * @#P = :val@
--
--
-- For more information on expression attribute names, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expressionAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sExpressionAttributeNames :: Lens.Lens' Scan (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sExpressionAttributeNames = Lens.lens (expressionAttributeNames :: Scan -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {expressionAttributeNames = a} :: Scan)
{-# DEPRECATED sExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | A string that contains conditions that DynamoDB applies after the @Scan@ operation, but before the data is returned to you. Items that do not satisfy the @FilterExpression@ criteria are not returned.
--
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#FilteringResults Filter Expressions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFilterExpression :: Lens.Lens' Scan (Lude.Maybe Lude.Text)
sFilterExpression = Lens.lens (filterExpression :: Scan -> Lude.Maybe Lude.Text) (\s a -> s {filterExpression = a} :: Scan)
{-# DEPRECATED sFilterExpression "Use generic-lens or generic-optics with 'filterExpression' instead." #-}

-- | A Boolean value that determines the read consistency model during the scan:
--
--
--     * If @ConsistentRead@ is @false@ , then the data returned from @Scan@ might not contain the results from other recently completed write operations (@PutItem@ , @UpdateItem@ , or @DeleteItem@ ).
--
--
--     * If @ConsistentRead@ is @true@ , then all of the write operations that completed before the @Scan@ began are guaranteed to be contained in the @Scan@ response.
--
--
-- The default setting for @ConsistentRead@ is @false@ .
-- The @ConsistentRead@ parameter is not supported on global secondary indexes. If you scan a global secondary index with @ConsistentRead@ set to true, you will receive a @ValidationException@ .
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConsistentRead :: Lens.Lens' Scan (Lude.Maybe Lude.Bool)
sConsistentRead = Lens.lens (consistentRead :: Scan -> Lude.Maybe Lude.Bool) (\s a -> s {consistentRead = a} :: Scan)
{-# DEPRECATED sConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the @ProductStatus@ attribute was one of the following:
-- @Available | Backordered | Discontinued@
-- You would first need to specify @ExpressionAttributeValues@ as follows:
-- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
-- You could then use these values in an expression, such as this:
-- @ProductStatus IN (:avail, :back, :disc)@
-- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sExpressionAttributeValues :: Lens.Lens' Scan (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
sExpressionAttributeValues = Lens.lens (expressionAttributeValues :: Scan -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {expressionAttributeValues = a} :: Scan)
{-# DEPRECATED sExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReturnConsumedCapacity :: Lens.Lens' Scan (Lude.Maybe ReturnConsumedCapacity)
sReturnConsumedCapacity = Lens.lens (returnConsumedCapacity :: Scan -> Lude.Maybe ReturnConsumedCapacity) (\s a -> s {returnConsumedCapacity = a} :: Scan)
{-# DEPRECATED sReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

-- | The maximum number of items to evaluate (not necessarily the number of matching items). If DynamoDB processes the number of items up to the limit while processing the results, it stops the operation and returns the matching values up to that point, and a key in @LastEvaluatedKey@ to apply in a subsequent operation, so that you can pick up where you left off. Also, if the processed dataset size exceeds 1 MB before DynamoDB reaches this limit, it stops the operation and returns the matching values up to the limit, and a key in @LastEvaluatedKey@ to apply in a subsequent operation to continue the operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Working with Queries> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLimit :: Lens.Lens' Scan (Lude.Maybe Lude.Natural)
sLimit = Lens.lens (limit :: Scan -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: Scan)
{-# DEPRECATED sLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The attributes to be returned in the result. You can retrieve all item attributes, specific item attributes, the count of matching items, or in the case of an index, some or all of the attributes projected into the index.
--
--
--     * @ALL_ATTRIBUTES@ - Returns all of the item attributes from the specified table or index. If you query a local secondary index, then for each matching item in the index, DynamoDB fetches the entire item from the parent table. If the index is configured to project all item attributes, then all of the data can be obtained from the local secondary index, and no fetching is required.
--
--
--     * @ALL_PROJECTED_ATTRIBUTES@ - Allowed only when querying an index. Retrieves all attributes that have been projected into the index. If the index is configured to project all attributes, this return value is equivalent to specifying @ALL_ATTRIBUTES@ .
--
--
--     * @COUNT@ - Returns the number of matching items, rather than the matching items themselves.
--
--
--     * @SPECIFIC_ATTRIBUTES@ - Returns only the attributes listed in @AttributesToGet@ . This return value is equivalent to specifying @AttributesToGet@ without specifying any value for @Select@ .
-- If you query or scan a local secondary index and request only attributes that are projected into that index, the operation reads only the index and not the table. If any of the requested attributes are not projected into the local secondary index, DynamoDB fetches each of these attributes from the parent table. This extra fetching incurs additional throughput cost and latency.
-- If you query or scan a global secondary index, you can only request attributes that are projected into the index. Global secondary index queries cannot fetch attributes from the parent table.
--
--
-- If neither @Select@ nor @AttributesToGet@ are specified, DynamoDB defaults to @ALL_ATTRIBUTES@ when accessing a table, and @ALL_PROJECTED_ATTRIBUTES@ when accessing an index. You cannot use both @Select@ and @AttributesToGet@ together in a single request, unless the value for @Select@ is @SPECIFIC_ATTRIBUTES@ . (This usage is equivalent to specifying @AttributesToGet@ without any value for @Select@ .)
--
-- /Note:/ Consider using 'select' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSelect :: Lens.Lens' Scan (Lude.Maybe Select)
sSelect = Lens.lens (select :: Scan -> Lude.Maybe Select) (\s a -> s {select = a} :: Scan)
{-# DEPRECATED sSelect "Use generic-lens or generic-optics with 'select' instead." #-}

-- | For a parallel @Scan@ request, @Segment@ identifies an individual segment to be scanned by an application worker.
--
-- Segment IDs are zero-based, so the first segment is always 0. For example, if you want to use four application threads to scan a table or an index, then the first thread specifies a @Segment@ value of 0, the second thread specifies 1, and so on.
-- The value of @LastEvaluatedKey@ returned from a parallel @Scan@ request must be used as @ExclusiveStartKey@ with the same segment ID in a subsequent @Scan@ operation.
-- The value for @Segment@ must be greater than or equal to 0, and less than the value provided for @TotalSegments@ .
-- If you provide @Segment@ , you must also provide @TotalSegments@ .
--
-- /Note:/ Consider using 'segment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSegment :: Lens.Lens' Scan (Lude.Maybe Lude.Natural)
sSegment = Lens.lens (segment :: Scan -> Lude.Maybe Lude.Natural) (\s a -> s {segment = a} :: Scan)
{-# DEPRECATED sSegment "Use generic-lens or generic-optics with 'segment' instead." #-}

-- | This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionalOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConditionalOperator :: Lens.Lens' Scan (Lude.Maybe ConditionalOperator)
sConditionalOperator = Lens.lens (conditionalOperator :: Scan -> Lude.Maybe ConditionalOperator) (\s a -> s {conditionalOperator = a} :: Scan)
{-# DEPRECATED sConditionalOperator "Use generic-lens or generic-optics with 'conditionalOperator' instead." #-}

-- | The primary key of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedKey@ in the previous operation.
--
-- The data type for @ExclusiveStartKey@ must be String, Number or Binary. No set data types are allowed.
-- In a parallel scan, a @Scan@ request that includes @ExclusiveStartKey@ must specify the same segment whose previous @Scan@ returned the corresponding value of @LastEvaluatedKey@ .
--
-- /Note:/ Consider using 'exclusiveStartKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sExclusiveStartKey :: Lens.Lens' Scan (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
sExclusiveStartKey = Lens.lens (exclusiveStartKey :: Scan -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {exclusiveStartKey = a} :: Scan)
{-# DEPRECATED sExclusiveStartKey "Use generic-lens or generic-optics with 'exclusiveStartKey' instead." #-}

-- | The name of a secondary index to scan. This index can be any local secondary index or global secondary index. Note that if you use the @IndexName@ parameter, you must also provide @TableName@ .
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sIndexName :: Lens.Lens' Scan (Lude.Maybe Lude.Text)
sIndexName = Lens.lens (indexName :: Scan -> Lude.Maybe Lude.Text) (\s a -> s {indexName = a} :: Scan)
{-# DEPRECATED sIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The name of the table containing the requested items; or, if you provide @IndexName@ , the name of the table to which that index belongs.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTableName :: Lens.Lens' Scan Lude.Text
sTableName = Lens.lens (tableName :: Scan -> Lude.Text) (\s a -> s {tableName = a} :: Scan)
{-# DEPRECATED sTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Page.AWSPager Scan where
  page rq rs
    | Page.stop (rs Lens.^. srsLastEvaluatedKey) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& sExclusiveStartKey Lens..~ rs Lens.^. srsLastEvaluatedKey

instance Lude.AWSRequest Scan where
  type Rs Scan = ScanResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          ScanResponse'
            Lude.<$> (x Lude..?> "LastEvaluatedKey" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Count")
            Lude.<*> (x Lude..?> "ScannedCount")
            Lude.<*> (x Lude..?> "Items" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ConsumedCapacity")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Scan where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.Scan" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON Scan where
  toJSON Scan' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProjectionExpression" Lude..=) Lude.<$> projectionExpression,
            ("ScanFilter" Lude..=) Lude.<$> scanFilter,
            ("AttributesToGet" Lude..=) Lude.<$> attributesToGet,
            ("TotalSegments" Lude..=) Lude.<$> totalSegments,
            ("ExpressionAttributeNames" Lude..=)
              Lude.<$> expressionAttributeNames,
            ("FilterExpression" Lude..=) Lude.<$> filterExpression,
            ("ConsistentRead" Lude..=) Lude.<$> consistentRead,
            ("ExpressionAttributeValues" Lude..=)
              Lude.<$> expressionAttributeValues,
            ("ReturnConsumedCapacity" Lude..=) Lude.<$> returnConsumedCapacity,
            ("Limit" Lude..=) Lude.<$> limit,
            ("Select" Lude..=) Lude.<$> select,
            ("Segment" Lude..=) Lude.<$> segment,
            ("ConditionalOperator" Lude..=) Lude.<$> conditionalOperator,
            ("ExclusiveStartKey" Lude..=) Lude.<$> exclusiveStartKey,
            ("IndexName" Lude..=) Lude.<$> indexName,
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath Scan where
  toPath = Lude.const "/"

instance Lude.ToQuery Scan where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @Scan@ operation.
--
-- /See:/ 'mkScanResponse' smart constructor.
data ScanResponse = ScanResponse'
  { lastEvaluatedKey ::
      Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    count :: Lude.Maybe Lude.Int,
    scannedCount :: Lude.Maybe Lude.Int,
    items :: Lude.Maybe [Lude.HashMap Lude.Text (AttributeValue)],
    consumedCapacity :: Lude.Maybe ConsumedCapacity,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScanResponse' with the minimum fields required to make a request.
--
-- * 'consumedCapacity' - The capacity units consumed by the @Scan@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
-- * 'count' - The number of items in the response.
--
-- If you set @ScanFilter@ in the request, then @Count@ is the number of items returned after the filter was applied, and @ScannedCount@ is the number of matching items before the filter was applied.
-- If you did not use a filter in the request, then @Count@ is the same as @ScannedCount@ .
-- * 'items' - An array of item attributes that match the scan criteria. Each element in this array consists of an attribute name and the value for that attribute.
-- * 'lastEvaluatedKey' - The primary key of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedKey@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved.
-- If @LastEvaluatedKey@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedKey@ is empty.
-- * 'responseStatus' - The response status code.
-- * 'scannedCount' - The number of items evaluated, before any @ScanFilter@ is applied. A high @ScannedCount@ value with few, or no, @Count@ results indicates an inefficient @Scan@ operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount> in the /Amazon DynamoDB Developer Guide/ .
--
-- If you did not use a filter in the request, then @ScannedCount@ is the same as @Count@ .
mkScanResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ScanResponse
mkScanResponse pResponseStatus_ =
  ScanResponse'
    { lastEvaluatedKey = Lude.Nothing,
      count = Lude.Nothing,
      scannedCount = Lude.Nothing,
      items = Lude.Nothing,
      consumedCapacity = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The primary key of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedKey@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved.
-- If @LastEvaluatedKey@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedKey@ is empty.
--
-- /Note:/ Consider using 'lastEvaluatedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsLastEvaluatedKey :: Lens.Lens' ScanResponse (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
srsLastEvaluatedKey = Lens.lens (lastEvaluatedKey :: ScanResponse -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {lastEvaluatedKey = a} :: ScanResponse)
{-# DEPRECATED srsLastEvaluatedKey "Use generic-lens or generic-optics with 'lastEvaluatedKey' instead." #-}

-- | The number of items in the response.
--
-- If you set @ScanFilter@ in the request, then @Count@ is the number of items returned after the filter was applied, and @ScannedCount@ is the number of matching items before the filter was applied.
-- If you did not use a filter in the request, then @Count@ is the same as @ScannedCount@ .
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsCount :: Lens.Lens' ScanResponse (Lude.Maybe Lude.Int)
srsCount = Lens.lens (count :: ScanResponse -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: ScanResponse)
{-# DEPRECATED srsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The number of items evaluated, before any @ScanFilter@ is applied. A high @ScannedCount@ value with few, or no, @Count@ results indicates an inefficient @Scan@ operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount> in the /Amazon DynamoDB Developer Guide/ .
--
-- If you did not use a filter in the request, then @ScannedCount@ is the same as @Count@ .
--
-- /Note:/ Consider using 'scannedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsScannedCount :: Lens.Lens' ScanResponse (Lude.Maybe Lude.Int)
srsScannedCount = Lens.lens (scannedCount :: ScanResponse -> Lude.Maybe Lude.Int) (\s a -> s {scannedCount = a} :: ScanResponse)
{-# DEPRECATED srsScannedCount "Use generic-lens or generic-optics with 'scannedCount' instead." #-}

-- | An array of item attributes that match the scan criteria. Each element in this array consists of an attribute name and the value for that attribute.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsItems :: Lens.Lens' ScanResponse (Lude.Maybe [Lude.HashMap Lude.Text (AttributeValue)])
srsItems = Lens.lens (items :: ScanResponse -> Lude.Maybe [Lude.HashMap Lude.Text (AttributeValue)]) (\s a -> s {items = a} :: ScanResponse)
{-# DEPRECATED srsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The capacity units consumed by the @Scan@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsConsumedCapacity :: Lens.Lens' ScanResponse (Lude.Maybe ConsumedCapacity)
srsConsumedCapacity = Lens.lens (consumedCapacity :: ScanResponse -> Lude.Maybe ConsumedCapacity) (\s a -> s {consumedCapacity = a} :: ScanResponse)
{-# DEPRECATED srsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' ScanResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: ScanResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ScanResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
