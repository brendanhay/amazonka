{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    sTableName,
    sAttributesToGet,
    sConditionalOperator,
    sConsistentRead,
    sExclusiveStartKey,
    sExpressionAttributeNames,
    sExpressionAttributeValues,
    sFilterExpression,
    sIndexName,
    sLimit,
    sProjectionExpression,
    sReturnConsumedCapacity,
    sScanFilter,
    sSegment,
    sSelect,
    sTotalSegments,

    -- * Destructuring the response
    ScanResponse (..),
    mkScanResponse,

    -- ** Response lenses
    srrsConsumedCapacity,
    srrsCount,
    srrsItems,
    srrsLastEvaluatedKey,
    srrsScannedCount,
    srrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @Scan@ operation.
--
-- /See:/ 'mkScan' smart constructor.
data Scan = Scan'
  { -- | The name of the table containing the requested items; or, if you provide @IndexName@ , the name of the table to which that index belongs.
    tableName :: Types.TableName,
    -- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
    attributesToGet :: Core.Maybe (Core.NonEmpty Types.AttributeName),
    -- | This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
    conditionalOperator :: Core.Maybe Types.ConditionalOperator,
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
    consistentRead :: Core.Maybe Core.Bool,
    -- | The primary key of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedKey@ in the previous operation.
    --
    -- The data type for @ExclusiveStartKey@ must be String, Number or Binary. No set data types are allowed.
    -- In a parallel scan, a @Scan@ request that includes @ExclusiveStartKey@ must specify the same segment whose previous @Scan@ returned the corresponding value of @LastEvaluatedKey@ .
    exclusiveStartKey :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
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
    expressionAttributeNames :: Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName),
    -- | One or more values that can be substituted in an expression.
    --
    -- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the @ProductStatus@ attribute was one of the following:
    -- @Available | Backordered | Discontinued@
    -- You would first need to specify @ExpressionAttributeValues@ as follows:
    -- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
    -- You could then use these values in an expression, such as this:
    -- @ProductStatus IN (:avail, :back, :disc)@
    -- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions> in the /Amazon DynamoDB Developer Guide/ .
    expressionAttributeValues :: Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue),
    -- | A string that contains conditions that DynamoDB applies after the @Scan@ operation, but before the data is returned to you. Items that do not satisfy the @FilterExpression@ criteria are not returned.
    --
    -- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#FilteringResults Filter Expressions> in the /Amazon DynamoDB Developer Guide/ .
    filterExpression :: Core.Maybe Types.ConditionExpression,
    -- | The name of a secondary index to scan. This index can be any local secondary index or global secondary index. Note that if you use the @IndexName@ parameter, you must also provide @TableName@ .
    indexName :: Core.Maybe Types.IndexName,
    -- | The maximum number of items to evaluate (not necessarily the number of matching items). If DynamoDB processes the number of items up to the limit while processing the results, it stops the operation and returns the matching values up to that point, and a key in @LastEvaluatedKey@ to apply in a subsequent operation, so that you can pick up where you left off. Also, if the processed dataset size exceeds 1 MB before DynamoDB reaches this limit, it stops the operation and returns the matching values up to the limit, and a key in @LastEvaluatedKey@ to apply in a subsequent operation to continue the operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Working with Queries> in the /Amazon DynamoDB Developer Guide/ .
    limit :: Core.Maybe Core.Natural,
    -- | A string that identifies one or more attributes to retrieve from the specified table or index. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
    --
    -- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
    -- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    projectionExpression :: Core.Maybe Types.ProjectionExpression,
    returnConsumedCapacity :: Core.Maybe Types.ReturnConsumedCapacity,
    -- | This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ScanFilter.html ScanFilter> in the /Amazon DynamoDB Developer Guide/ .
    scanFilter :: Core.Maybe (Core.HashMap Types.AttributeName Types.Condition),
    -- | For a parallel @Scan@ request, @Segment@ identifies an individual segment to be scanned by an application worker.
    --
    -- Segment IDs are zero-based, so the first segment is always 0. For example, if you want to use four application threads to scan a table or an index, then the first thread specifies a @Segment@ value of 0, the second thread specifies 1, and so on.
    -- The value of @LastEvaluatedKey@ returned from a parallel @Scan@ request must be used as @ExclusiveStartKey@ with the same segment ID in a subsequent @Scan@ operation.
    -- The value for @Segment@ must be greater than or equal to 0, and less than the value provided for @TotalSegments@ .
    -- If you provide @Segment@ , you must also provide @TotalSegments@ .
    segment :: Core.Maybe Core.Natural,
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
    select :: Core.Maybe Types.Select,
    -- | For a parallel @Scan@ request, @TotalSegments@ represents the total number of segments into which the @Scan@ operation will be divided. The value of @TotalSegments@ corresponds to the number of application workers that will perform the parallel scan. For example, if you want to use four application threads to scan a table or an index, specify a @TotalSegments@ value of 4.
    --
    -- The value for @TotalSegments@ must be greater than or equal to 1, and less than or equal to 1000000. If you specify a @TotalSegments@ value of 1, the @Scan@ operation will be sequential rather than parallel.
    -- If you specify @TotalSegments@ , you must also specify @Segment@ .
    totalSegments :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scan' value with any optional fields omitted.
mkScan ::
  -- | 'tableName'
  Types.TableName ->
  Scan
mkScan tableName =
  Scan'
    { tableName,
      attributesToGet = Core.Nothing,
      conditionalOperator = Core.Nothing,
      consistentRead = Core.Nothing,
      exclusiveStartKey = Core.Nothing,
      expressionAttributeNames = Core.Nothing,
      expressionAttributeValues = Core.Nothing,
      filterExpression = Core.Nothing,
      indexName = Core.Nothing,
      limit = Core.Nothing,
      projectionExpression = Core.Nothing,
      returnConsumedCapacity = Core.Nothing,
      scanFilter = Core.Nothing,
      segment = Core.Nothing,
      select = Core.Nothing,
      totalSegments = Core.Nothing
    }

-- | The name of the table containing the requested items; or, if you provide @IndexName@ , the name of the table to which that index belongs.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTableName :: Lens.Lens' Scan Types.TableName
sTableName = Lens.field @"tableName"
{-# DEPRECATED sTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'attributesToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAttributesToGet :: Lens.Lens' Scan (Core.Maybe (Core.NonEmpty Types.AttributeName))
sAttributesToGet = Lens.field @"attributesToGet"
{-# DEPRECATED sAttributesToGet "Use generic-lens or generic-optics with 'attributesToGet' instead." #-}

-- | This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionalOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConditionalOperator :: Lens.Lens' Scan (Core.Maybe Types.ConditionalOperator)
sConditionalOperator = Lens.field @"conditionalOperator"
{-# DEPRECATED sConditionalOperator "Use generic-lens or generic-optics with 'conditionalOperator' instead." #-}

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
sConsistentRead :: Lens.Lens' Scan (Core.Maybe Core.Bool)
sConsistentRead = Lens.field @"consistentRead"
{-# DEPRECATED sConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | The primary key of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedKey@ in the previous operation.
--
-- The data type for @ExclusiveStartKey@ must be String, Number or Binary. No set data types are allowed.
-- In a parallel scan, a @Scan@ request that includes @ExclusiveStartKey@ must specify the same segment whose previous @Scan@ returned the corresponding value of @LastEvaluatedKey@ .
--
-- /Note:/ Consider using 'exclusiveStartKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sExclusiveStartKey :: Lens.Lens' Scan (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
sExclusiveStartKey = Lens.field @"exclusiveStartKey"
{-# DEPRECATED sExclusiveStartKey "Use generic-lens or generic-optics with 'exclusiveStartKey' instead." #-}

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
sExpressionAttributeNames :: Lens.Lens' Scan (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
sExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# DEPRECATED sExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

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
sExpressionAttributeValues :: Lens.Lens' Scan (Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue))
sExpressionAttributeValues = Lens.field @"expressionAttributeValues"
{-# DEPRECATED sExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | A string that contains conditions that DynamoDB applies after the @Scan@ operation, but before the data is returned to you. Items that do not satisfy the @FilterExpression@ criteria are not returned.
--
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#FilteringResults Filter Expressions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFilterExpression :: Lens.Lens' Scan (Core.Maybe Types.ConditionExpression)
sFilterExpression = Lens.field @"filterExpression"
{-# DEPRECATED sFilterExpression "Use generic-lens or generic-optics with 'filterExpression' instead." #-}

-- | The name of a secondary index to scan. This index can be any local secondary index or global secondary index. Note that if you use the @IndexName@ parameter, you must also provide @TableName@ .
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sIndexName :: Lens.Lens' Scan (Core.Maybe Types.IndexName)
sIndexName = Lens.field @"indexName"
{-# DEPRECATED sIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The maximum number of items to evaluate (not necessarily the number of matching items). If DynamoDB processes the number of items up to the limit while processing the results, it stops the operation and returns the matching values up to that point, and a key in @LastEvaluatedKey@ to apply in a subsequent operation, so that you can pick up where you left off. Also, if the processed dataset size exceeds 1 MB before DynamoDB reaches this limit, it stops the operation and returns the matching values up to the limit, and a key in @LastEvaluatedKey@ to apply in a subsequent operation to continue the operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Working with Queries> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLimit :: Lens.Lens' Scan (Core.Maybe Core.Natural)
sLimit = Lens.field @"limit"
{-# DEPRECATED sLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A string that identifies one or more attributes to retrieve from the specified table or index. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'projectionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProjectionExpression :: Lens.Lens' Scan (Core.Maybe Types.ProjectionExpression)
sProjectionExpression = Lens.field @"projectionExpression"
{-# DEPRECATED sProjectionExpression "Use generic-lens or generic-optics with 'projectionExpression' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReturnConsumedCapacity :: Lens.Lens' Scan (Core.Maybe Types.ReturnConsumedCapacity)
sReturnConsumedCapacity = Lens.field @"returnConsumedCapacity"
{-# DEPRECATED sReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

-- | This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ScanFilter.html ScanFilter> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'scanFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sScanFilter :: Lens.Lens' Scan (Core.Maybe (Core.HashMap Types.AttributeName Types.Condition))
sScanFilter = Lens.field @"scanFilter"
{-# DEPRECATED sScanFilter "Use generic-lens or generic-optics with 'scanFilter' instead." #-}

-- | For a parallel @Scan@ request, @Segment@ identifies an individual segment to be scanned by an application worker.
--
-- Segment IDs are zero-based, so the first segment is always 0. For example, if you want to use four application threads to scan a table or an index, then the first thread specifies a @Segment@ value of 0, the second thread specifies 1, and so on.
-- The value of @LastEvaluatedKey@ returned from a parallel @Scan@ request must be used as @ExclusiveStartKey@ with the same segment ID in a subsequent @Scan@ operation.
-- The value for @Segment@ must be greater than or equal to 0, and less than the value provided for @TotalSegments@ .
-- If you provide @Segment@ , you must also provide @TotalSegments@ .
--
-- /Note:/ Consider using 'segment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSegment :: Lens.Lens' Scan (Core.Maybe Core.Natural)
sSegment = Lens.field @"segment"
{-# DEPRECATED sSegment "Use generic-lens or generic-optics with 'segment' instead." #-}

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
sSelect :: Lens.Lens' Scan (Core.Maybe Types.Select)
sSelect = Lens.field @"select"
{-# DEPRECATED sSelect "Use generic-lens or generic-optics with 'select' instead." #-}

-- | For a parallel @Scan@ request, @TotalSegments@ represents the total number of segments into which the @Scan@ operation will be divided. The value of @TotalSegments@ corresponds to the number of application workers that will perform the parallel scan. For example, if you want to use four application threads to scan a table or an index, specify a @TotalSegments@ value of 4.
--
-- The value for @TotalSegments@ must be greater than or equal to 1, and less than or equal to 1000000. If you specify a @TotalSegments@ value of 1, the @Scan@ operation will be sequential rather than parallel.
-- If you specify @TotalSegments@ , you must also specify @Segment@ .
--
-- /Note:/ Consider using 'totalSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTotalSegments :: Lens.Lens' Scan (Core.Maybe Core.Natural)
sTotalSegments = Lens.field @"totalSegments"
{-# DEPRECATED sTotalSegments "Use generic-lens or generic-optics with 'totalSegments' instead." #-}

instance Core.FromJSON Scan where
  toJSON Scan {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TableName" Core..= tableName),
            ("AttributesToGet" Core..=) Core.<$> attributesToGet,
            ("ConditionalOperator" Core..=) Core.<$> conditionalOperator,
            ("ConsistentRead" Core..=) Core.<$> consistentRead,
            ("ExclusiveStartKey" Core..=) Core.<$> exclusiveStartKey,
            ("ExpressionAttributeNames" Core..=)
              Core.<$> expressionAttributeNames,
            ("ExpressionAttributeValues" Core..=)
              Core.<$> expressionAttributeValues,
            ("FilterExpression" Core..=) Core.<$> filterExpression,
            ("IndexName" Core..=) Core.<$> indexName,
            ("Limit" Core..=) Core.<$> limit,
            ("ProjectionExpression" Core..=) Core.<$> projectionExpression,
            ("ReturnConsumedCapacity" Core..=) Core.<$> returnConsumedCapacity,
            ("ScanFilter" Core..=) Core.<$> scanFilter,
            ("Segment" Core..=) Core.<$> segment,
            ("Select" Core..=) Core.<$> select,
            ("TotalSegments" Core..=) Core.<$> totalSegments
          ]
      )

instance Core.AWSRequest Scan where
  type Rs Scan = ScanResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.Scan")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ScanResponse'
            Core.<$> (x Core..:? "ConsumedCapacity")
            Core.<*> (x Core..:? "Count")
            Core.<*> (x Core..:? "Items")
            Core.<*> (x Core..:? "LastEvaluatedKey")
            Core.<*> (x Core..:? "ScannedCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager Scan where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"lastEvaluatedKey") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"exclusiveStartKey"
            Lens..~ rs Lens.^. Lens.field @"lastEvaluatedKey"
        )

-- | Represents the output of a @Scan@ operation.
--
-- /See:/ 'mkScanResponse' smart constructor.
data ScanResponse = ScanResponse'
  { -- | The capacity units consumed by the @Scan@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
    consumedCapacity :: Core.Maybe Types.ConsumedCapacity,
    -- | The number of items in the response.
    --
    -- If you set @ScanFilter@ in the request, then @Count@ is the number of items returned after the filter was applied, and @ScannedCount@ is the number of matching items before the filter was applied.
    -- If you did not use a filter in the request, then @Count@ is the same as @ScannedCount@ .
    count :: Core.Maybe Core.Int,
    -- | An array of item attributes that match the scan criteria. Each element in this array consists of an attribute name and the value for that attribute.
    items :: Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue],
    -- | The primary key of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
    --
    -- If @LastEvaluatedKey@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved.
    -- If @LastEvaluatedKey@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedKey@ is empty.
    lastEvaluatedKey :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | The number of items evaluated, before any @ScanFilter@ is applied. A high @ScannedCount@ value with few, or no, @Count@ results indicates an inefficient @Scan@ operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount> in the /Amazon DynamoDB Developer Guide/ .
    --
    -- If you did not use a filter in the request, then @ScannedCount@ is the same as @Count@ .
    scannedCount :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScanResponse' value with any optional fields omitted.
mkScanResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ScanResponse
mkScanResponse responseStatus =
  ScanResponse'
    { consumedCapacity = Core.Nothing,
      count = Core.Nothing,
      items = Core.Nothing,
      lastEvaluatedKey = Core.Nothing,
      scannedCount = Core.Nothing,
      responseStatus
    }

-- | The capacity units consumed by the @Scan@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsConsumedCapacity :: Lens.Lens' ScanResponse (Core.Maybe Types.ConsumedCapacity)
srrsConsumedCapacity = Lens.field @"consumedCapacity"
{-# DEPRECATED srrsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | The number of items in the response.
--
-- If you set @ScanFilter@ in the request, then @Count@ is the number of items returned after the filter was applied, and @ScannedCount@ is the number of matching items before the filter was applied.
-- If you did not use a filter in the request, then @Count@ is the same as @ScannedCount@ .
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsCount :: Lens.Lens' ScanResponse (Core.Maybe Core.Int)
srrsCount = Lens.field @"count"
{-# DEPRECATED srrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | An array of item attributes that match the scan criteria. Each element in this array consists of an attribute name and the value for that attribute.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsItems :: Lens.Lens' ScanResponse (Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue])
srrsItems = Lens.field @"items"
{-# DEPRECATED srrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The primary key of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedKey@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved.
-- If @LastEvaluatedKey@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedKey@ is empty.
--
-- /Note:/ Consider using 'lastEvaluatedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsLastEvaluatedKey :: Lens.Lens' ScanResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
srrsLastEvaluatedKey = Lens.field @"lastEvaluatedKey"
{-# DEPRECATED srrsLastEvaluatedKey "Use generic-lens or generic-optics with 'lastEvaluatedKey' instead." #-}

-- | The number of items evaluated, before any @ScanFilter@ is applied. A high @ScannedCount@ value with few, or no, @Count@ results indicates an inefficient @Scan@ operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount> in the /Amazon DynamoDB Developer Guide/ .
--
-- If you did not use a filter in the request, then @ScannedCount@ is the same as @Count@ .
--
-- /Note:/ Consider using 'scannedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsScannedCount :: Lens.Lens' ScanResponse (Core.Maybe Core.Int)
srrsScannedCount = Lens.field @"scannedCount"
{-# DEPRECATED srrsScannedCount "Use generic-lens or generic-optics with 'scannedCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrsResponseStatus :: Lens.Lens' ScanResponse Core.Int
srrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
