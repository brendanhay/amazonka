{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Query
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @Query@ operation finds items based on primary key values. You can query any table or secondary index that has a composite primary key (a partition key and a sort key).
--
-- Use the @KeyConditionExpression@ parameter to provide a specific value for the partition key. The @Query@ operation will return all of the items from the table or index with that partition key value. You can optionally narrow the scope of the @Query@ operation by specifying a sort key value and a comparison operator in @KeyConditionExpression@ . To further refine the @Query@ results, you can optionally provide a @FilterExpression@ . A @FilterExpression@ determines which items within the results should be returned to you. All of the other results are discarded.
-- A @Query@ operation always returns a result set. If no matching items are found, the result set will be empty. Queries that do not return results consume the minimum number of read capacity units for that type of read operation.
-- @Query@ results are always sorted by the sort key value. If the data type of the sort key is Number, the results are returned in numeric order; otherwise, the results are returned in order of UTF-8 bytes. By default, the sort order is ascending. To reverse the order, set the @ScanIndexForward@ parameter to false.
-- A single @Query@ operation will read up to the maximum number of items set (if using the @Limit@ parameter) or a maximum of 1 MB of data and then apply any filtering to the results using @FilterExpression@ . If @LastEvaluatedKey@ is present in the response, you will need to paginate the result set. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Query.html#Query.Pagination Paginating the Results> in the /Amazon DynamoDB Developer Guide/ .
-- @FilterExpression@ is applied after a @Query@ finishes, but before the results are returned. A @FilterExpression@ cannot contain partition key or sort key attributes. You need to specify those attributes in the @KeyConditionExpression@ .
-- You can query a table, a local secondary index, or a global secondary index. For a query on a table or on a local secondary index, you can set the @ConsistentRead@ parameter to @true@ and obtain a strongly consistent result. Global secondary indexes support eventually consistent reads only, so do not specify @ConsistentRead@ when querying a global secondary index.
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.Query
  ( -- * Creating a request
    Query (..),
    mkQuery,

    -- ** Request lenses
    qTableName,
    qAttributesToGet,
    qConditionalOperator,
    qConsistentRead,
    qExclusiveStartKey,
    qExpressionAttributeNames,
    qExpressionAttributeValues,
    qFilterExpression,
    qIndexName,
    qKeyConditionExpression,
    qKeyConditions,
    qLimit,
    qProjectionExpression,
    qQueryFilter,
    qReturnConsumedCapacity,
    qScanIndexForward,
    qSelect,

    -- * Destructuring the response
    QueryResponse (..),
    mkQueryResponse,

    -- ** Response lenses
    qrrsConsumedCapacity,
    qrrsCount,
    qrrsItems,
    qrrsLastEvaluatedKey,
    qrrsScannedCount,
    qrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @Query@ operation.
--
-- /See:/ 'mkQuery' smart constructor.
data Query = Query'
  { -- | The name of the table containing the requested items.
    tableName :: Types.TableName,
    -- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
    attributesToGet :: Core.Maybe (Core.NonEmpty Types.AttributeName),
    -- | This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
    conditionalOperator :: Core.Maybe Types.ConditionalOperator,
    -- | Determines the read consistency model: If set to @true@ , then the operation uses strongly consistent reads; otherwise, the operation uses eventually consistent reads.
    --
    -- Strongly consistent reads are not supported on global secondary indexes. If you query a global secondary index with @ConsistentRead@ set to @true@ , you will receive a @ValidationException@ .
    consistentRead :: Core.Maybe Core.Bool,
    -- | The primary key of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedKey@ in the previous operation.
    --
    -- The data type for @ExclusiveStartKey@ must be String, Number, or Binary. No set data types are allowed.
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
    -- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:
    -- @Available | Backordered | Discontinued@
    -- You would first need to specify @ExpressionAttributeValues@ as follows:
    -- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
    -- You could then use these values in an expression, such as this:
    -- @ProductStatus IN (:avail, :back, :disc)@
    -- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
    expressionAttributeValues :: Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue),
    -- | A string that contains conditions that DynamoDB applies after the @Query@ operation, but before the data is returned to you. Items that do not satisfy the @FilterExpression@ criteria are not returned.
    --
    -- A @FilterExpression@ does not allow key attributes. You cannot define a filter expression based on a partition key or a sort key.
    -- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#FilteringResults Filter Expressions> in the /Amazon DynamoDB Developer Guide/ .
    filterExpression :: Core.Maybe Types.FilterExpression,
    -- | The name of an index to query. This index can be any local secondary index or global secondary index on the table. Note that if you use the @IndexName@ parameter, you must also provide @TableName.@
    indexName :: Core.Maybe Types.IndexName,
    -- | The condition that specifies the key values for items to be retrieved by the @Query@ action.
    --
    -- The condition must perform an equality test on a single partition key value.
    -- The condition can optionally perform one of several comparison tests on a single sort key value. This allows @Query@ to retrieve one item with a given partition key value and sort key value, or several items that have the same partition key value but different sort key values.
    -- The partition key equality test is required, and must be specified in the following format:
    -- @partitionKeyName@ /=/ @:partitionkeyval@
    -- If you also want to provide a condition for the sort key, it must be combined using @AND@ with the condition for the sort key. Following is an example, using the __=__ comparison operator for the sort key:
    -- @partitionKeyName@ @=@ @:partitionkeyval@ @AND@ @sortKeyName@ @=@ @:sortkeyval@
    -- Valid comparisons for the sort key condition are as follows:
    --
    --     * @sortKeyName@ @=@ @:sortkeyval@ - true if the sort key value is equal to @:sortkeyval@ .
    --
    --
    --     * @sortKeyName@ @<@ @:sortkeyval@ - true if the sort key value is less than @:sortkeyval@ .
    --
    --
    --     * @sortKeyName@ @<=@ @:sortkeyval@ - true if the sort key value is less than or equal to @:sortkeyval@ .
    --
    --
    --     * @sortKeyName@ @>@ @:sortkeyval@ - true if the sort key value is greater than @:sortkeyval@ .
    --
    --
    --     * @sortKeyName@ @>= @ @:sortkeyval@ - true if the sort key value is greater than or equal to @:sortkeyval@ .
    --
    --
    --     * @sortKeyName@ @BETWEEN@ @:sortkeyval1@ @AND@ @:sortkeyval2@ - true if the sort key value is greater than or equal to @:sortkeyval1@ , and less than or equal to @:sortkeyval2@ .
    --
    --
    --     * @begins_with (@ @sortKeyName@ , @:sortkeyval@ @)@ - true if the sort key value begins with a particular operand. (You cannot use this function with a sort key that is of type Number.) Note that the function name @begins_with@ is case-sensitive.
    --
    --
    -- Use the @ExpressionAttributeValues@ parameter to replace tokens such as @:partitionval@ and @:sortval@ with actual values at runtime.
    -- You can optionally use the @ExpressionAttributeNames@ parameter to replace the names of the partition key and sort key with placeholder tokens. This option might be necessary if an attribute name conflicts with a DynamoDB reserved word. For example, the following @KeyConditionExpression@ parameter causes an error because /Size/ is a reserved word:
    --
    --     * @Size = :myval@
    --
    --
    -- To work around this, define a placeholder (such a @#S@ ) to represent the attribute name /Size/ . @KeyConditionExpression@ then is as follows:
    --
    --     * @#S = :myval@
    --
    --
    -- For a list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ .
    -- For more information on @ExpressionAttributeNames@ and @ExpressionAttributeValues@ , see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values> in the /Amazon DynamoDB Developer Guide/ .
    keyConditionExpression :: Core.Maybe Types.KeyConditionExpression,
    -- | This is a legacy parameter. Use @KeyConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.KeyConditions.html KeyConditions> in the /Amazon DynamoDB Developer Guide/ .
    keyConditions :: Core.Maybe (Core.HashMap Types.AttributeName Types.Condition),
    -- | The maximum number of items to evaluate (not necessarily the number of matching items). If DynamoDB processes the number of items up to the limit while processing the results, it stops the operation and returns the matching values up to that point, and a key in @LastEvaluatedKey@ to apply in a subsequent operation, so that you can pick up where you left off. Also, if the processed dataset size exceeds 1 MB before DynamoDB reaches this limit, it stops the operation and returns the matching values up to the limit, and a key in @LastEvaluatedKey@ to apply in a subsequent operation to continue the operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan> in the /Amazon DynamoDB Developer Guide/ .
    limit :: Core.Maybe Core.Natural,
    -- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
    --
    -- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
    -- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
    projectionExpression :: Core.Maybe Types.ProjectionExpression,
    -- | This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.QueryFilter.html QueryFilter> in the /Amazon DynamoDB Developer Guide/ .
    queryFilter :: Core.Maybe (Core.HashMap Types.AttributeName Types.Condition),
    returnConsumedCapacity :: Core.Maybe Types.ReturnConsumedCapacity,
    -- | Specifies the order for index traversal: If @true@ (default), the traversal is performed in ascending order; if @false@ , the traversal is performed in descending order.
    --
    -- Items with the same partition key value are stored in sorted order by sort key. If the sort key data type is Number, the results are stored in numeric order. For type String, the results are stored in order of UTF-8 bytes. For type Binary, DynamoDB treats each byte of the binary data as unsigned.
    -- If @ScanIndexForward@ is @true@ , DynamoDB returns the results in the order in which they are stored (by sort key value). This is the default behavior. If @ScanIndexForward@ is @false@ , DynamoDB reads the results in reverse order by sort key value, and then returns the results to the client.
    scanIndexForward :: Core.Maybe Core.Bool,
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
    -- If you query or scan a local secondary index and request only attributes that are projected into that index, the operation will read only the index and not the table. If any of the requested attributes are not projected into the local secondary index, DynamoDB fetches each of these attributes from the parent table. This extra fetching incurs additional throughput cost and latency.
    -- If you query or scan a global secondary index, you can only request attributes that are projected into the index. Global secondary index queries cannot fetch attributes from the parent table.
    --
    --
    -- If neither @Select@ nor @AttributesToGet@ are specified, DynamoDB defaults to @ALL_ATTRIBUTES@ when accessing a table, and @ALL_PROJECTED_ATTRIBUTES@ when accessing an index. You cannot use both @Select@ and @AttributesToGet@ together in a single request, unless the value for @Select@ is @SPECIFIC_ATTRIBUTES@ . (This usage is equivalent to specifying @AttributesToGet@ without any value for @Select@ .)
    select :: Core.Maybe Types.Select
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Query' value with any optional fields omitted.
mkQuery ::
  -- | 'tableName'
  Types.TableName ->
  Query
mkQuery tableName =
  Query'
    { tableName,
      attributesToGet = Core.Nothing,
      conditionalOperator = Core.Nothing,
      consistentRead = Core.Nothing,
      exclusiveStartKey = Core.Nothing,
      expressionAttributeNames = Core.Nothing,
      expressionAttributeValues = Core.Nothing,
      filterExpression = Core.Nothing,
      indexName = Core.Nothing,
      keyConditionExpression = Core.Nothing,
      keyConditions = Core.Nothing,
      limit = Core.Nothing,
      projectionExpression = Core.Nothing,
      queryFilter = Core.Nothing,
      returnConsumedCapacity = Core.Nothing,
      scanIndexForward = Core.Nothing,
      select = Core.Nothing
    }

-- | The name of the table containing the requested items.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qTableName :: Lens.Lens' Query Types.TableName
qTableName = Lens.field @"tableName"
{-# DEPRECATED qTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'attributesToGet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qAttributesToGet :: Lens.Lens' Query (Core.Maybe (Core.NonEmpty Types.AttributeName))
qAttributesToGet = Lens.field @"attributesToGet"
{-# DEPRECATED qAttributesToGet "Use generic-lens or generic-optics with 'attributesToGet' instead." #-}

-- | This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'conditionalOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qConditionalOperator :: Lens.Lens' Query (Core.Maybe Types.ConditionalOperator)
qConditionalOperator = Lens.field @"conditionalOperator"
{-# DEPRECATED qConditionalOperator "Use generic-lens or generic-optics with 'conditionalOperator' instead." #-}

-- | Determines the read consistency model: If set to @true@ , then the operation uses strongly consistent reads; otherwise, the operation uses eventually consistent reads.
--
-- Strongly consistent reads are not supported on global secondary indexes. If you query a global secondary index with @ConsistentRead@ set to @true@ , you will receive a @ValidationException@ .
--
-- /Note:/ Consider using 'consistentRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qConsistentRead :: Lens.Lens' Query (Core.Maybe Core.Bool)
qConsistentRead = Lens.field @"consistentRead"
{-# DEPRECATED qConsistentRead "Use generic-lens or generic-optics with 'consistentRead' instead." #-}

-- | The primary key of the first item that this operation will evaluate. Use the value that was returned for @LastEvaluatedKey@ in the previous operation.
--
-- The data type for @ExclusiveStartKey@ must be String, Number, or Binary. No set data types are allowed.
--
-- /Note:/ Consider using 'exclusiveStartKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qExclusiveStartKey :: Lens.Lens' Query (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
qExclusiveStartKey = Lens.field @"exclusiveStartKey"
{-# DEPRECATED qExclusiveStartKey "Use generic-lens or generic-optics with 'exclusiveStartKey' instead." #-}

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
qExpressionAttributeNames :: Lens.Lens' Query (Core.Maybe (Core.HashMap Types.ExpressionAttributeNameVariable Types.AttributeName))
qExpressionAttributeNames = Lens.field @"expressionAttributeNames"
{-# DEPRECATED qExpressionAttributeNames "Use generic-lens or generic-optics with 'expressionAttributeNames' instead." #-}

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an attribute value. For example, suppose that you wanted to check whether the value of the /ProductStatus/ attribute was one of the following:
-- @Available | Backordered | Discontinued@
-- You would first need to specify @ExpressionAttributeValues@ as follows:
-- @{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }@
-- You could then use these values in an expression, such as this:
-- @ProductStatus IN (:avail, :back, :disc)@
-- For more information on expression attribute values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'expressionAttributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qExpressionAttributeValues :: Lens.Lens' Query (Core.Maybe (Core.HashMap Types.ExpressionAttributeValueVariable Types.AttributeValue))
qExpressionAttributeValues = Lens.field @"expressionAttributeValues"
{-# DEPRECATED qExpressionAttributeValues "Use generic-lens or generic-optics with 'expressionAttributeValues' instead." #-}

-- | A string that contains conditions that DynamoDB applies after the @Query@ operation, but before the data is returned to you. Items that do not satisfy the @FilterExpression@ criteria are not returned.
--
-- A @FilterExpression@ does not allow key attributes. You cannot define a filter expression based on a partition key or a sort key.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#FilteringResults Filter Expressions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qFilterExpression :: Lens.Lens' Query (Core.Maybe Types.FilterExpression)
qFilterExpression = Lens.field @"filterExpression"
{-# DEPRECATED qFilterExpression "Use generic-lens or generic-optics with 'filterExpression' instead." #-}

-- | The name of an index to query. This index can be any local secondary index or global secondary index on the table. Note that if you use the @IndexName@ parameter, you must also provide @TableName.@
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qIndexName :: Lens.Lens' Query (Core.Maybe Types.IndexName)
qIndexName = Lens.field @"indexName"
{-# DEPRECATED qIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The condition that specifies the key values for items to be retrieved by the @Query@ action.
--
-- The condition must perform an equality test on a single partition key value.
-- The condition can optionally perform one of several comparison tests on a single sort key value. This allows @Query@ to retrieve one item with a given partition key value and sort key value, or several items that have the same partition key value but different sort key values.
-- The partition key equality test is required, and must be specified in the following format:
-- @partitionKeyName@ /=/ @:partitionkeyval@
-- If you also want to provide a condition for the sort key, it must be combined using @AND@ with the condition for the sort key. Following is an example, using the __=__ comparison operator for the sort key:
-- @partitionKeyName@ @=@ @:partitionkeyval@ @AND@ @sortKeyName@ @=@ @:sortkeyval@
-- Valid comparisons for the sort key condition are as follows:
--
--     * @sortKeyName@ @=@ @:sortkeyval@ - true if the sort key value is equal to @:sortkeyval@ .
--
--
--     * @sortKeyName@ @<@ @:sortkeyval@ - true if the sort key value is less than @:sortkeyval@ .
--
--
--     * @sortKeyName@ @<=@ @:sortkeyval@ - true if the sort key value is less than or equal to @:sortkeyval@ .
--
--
--     * @sortKeyName@ @>@ @:sortkeyval@ - true if the sort key value is greater than @:sortkeyval@ .
--
--
--     * @sortKeyName@ @>= @ @:sortkeyval@ - true if the sort key value is greater than or equal to @:sortkeyval@ .
--
--
--     * @sortKeyName@ @BETWEEN@ @:sortkeyval1@ @AND@ @:sortkeyval2@ - true if the sort key value is greater than or equal to @:sortkeyval1@ , and less than or equal to @:sortkeyval2@ .
--
--
--     * @begins_with (@ @sortKeyName@ , @:sortkeyval@ @)@ - true if the sort key value begins with a particular operand. (You cannot use this function with a sort key that is of type Number.) Note that the function name @begins_with@ is case-sensitive.
--
--
-- Use the @ExpressionAttributeValues@ parameter to replace tokens such as @:partitionval@ and @:sortval@ with actual values at runtime.
-- You can optionally use the @ExpressionAttributeNames@ parameter to replace the names of the partition key and sort key with placeholder tokens. This option might be necessary if an attribute name conflicts with a DynamoDB reserved word. For example, the following @KeyConditionExpression@ parameter causes an error because /Size/ is a reserved word:
--
--     * @Size = :myval@
--
--
-- To work around this, define a placeholder (such a @#S@ ) to represent the attribute name /Size/ . @KeyConditionExpression@ then is as follows:
--
--     * @#S = :myval@
--
--
-- For a list of reserved words, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words> in the /Amazon DynamoDB Developer Guide/ .
-- For more information on @ExpressionAttributeNames@ and @ExpressionAttributeValues@ , see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'keyConditionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qKeyConditionExpression :: Lens.Lens' Query (Core.Maybe Types.KeyConditionExpression)
qKeyConditionExpression = Lens.field @"keyConditionExpression"
{-# DEPRECATED qKeyConditionExpression "Use generic-lens or generic-optics with 'keyConditionExpression' instead." #-}

-- | This is a legacy parameter. Use @KeyConditionExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.KeyConditions.html KeyConditions> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'keyConditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qKeyConditions :: Lens.Lens' Query (Core.Maybe (Core.HashMap Types.AttributeName Types.Condition))
qKeyConditions = Lens.field @"keyConditions"
{-# DEPRECATED qKeyConditions "Use generic-lens or generic-optics with 'keyConditions' instead." #-}

-- | The maximum number of items to evaluate (not necessarily the number of matching items). If DynamoDB processes the number of items up to the limit while processing the results, it stops the operation and returns the matching values up to that point, and a key in @LastEvaluatedKey@ to apply in a subsequent operation, so that you can pick up where you left off. Also, if the processed dataset size exceeds 1 MB before DynamoDB reaches this limit, it stops the operation and returns the matching values up to the limit, and a key in @LastEvaluatedKey@ to apply in a subsequent operation to continue the operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qLimit :: Lens.Lens' Query (Core.Maybe Core.Natural)
qLimit = Lens.field @"limit"
{-# DEPRECATED qLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A string that identifies one or more attributes to retrieve from the table. These attributes can include scalars, sets, or elements of a JSON document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be returned. If any of the requested attributes are not found, they will not appear in the result.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'projectionExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qProjectionExpression :: Lens.Lens' Query (Core.Maybe Types.ProjectionExpression)
qProjectionExpression = Lens.field @"projectionExpression"
{-# DEPRECATED qProjectionExpression "Use generic-lens or generic-optics with 'projectionExpression' instead." #-}

-- | This is a legacy parameter. Use @FilterExpression@ instead. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.QueryFilter.html QueryFilter> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'queryFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qQueryFilter :: Lens.Lens' Query (Core.Maybe (Core.HashMap Types.AttributeName Types.Condition))
qQueryFilter = Lens.field @"queryFilter"
{-# DEPRECATED qQueryFilter "Use generic-lens or generic-optics with 'queryFilter' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qReturnConsumedCapacity :: Lens.Lens' Query (Core.Maybe Types.ReturnConsumedCapacity)
qReturnConsumedCapacity = Lens.field @"returnConsumedCapacity"
{-# DEPRECATED qReturnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead." #-}

-- | Specifies the order for index traversal: If @true@ (default), the traversal is performed in ascending order; if @false@ , the traversal is performed in descending order.
--
-- Items with the same partition key value are stored in sorted order by sort key. If the sort key data type is Number, the results are stored in numeric order. For type String, the results are stored in order of UTF-8 bytes. For type Binary, DynamoDB treats each byte of the binary data as unsigned.
-- If @ScanIndexForward@ is @true@ , DynamoDB returns the results in the order in which they are stored (by sort key value). This is the default behavior. If @ScanIndexForward@ is @false@ , DynamoDB reads the results in reverse order by sort key value, and then returns the results to the client.
--
-- /Note:/ Consider using 'scanIndexForward' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qScanIndexForward :: Lens.Lens' Query (Core.Maybe Core.Bool)
qScanIndexForward = Lens.field @"scanIndexForward"
{-# DEPRECATED qScanIndexForward "Use generic-lens or generic-optics with 'scanIndexForward' instead." #-}

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
-- If you query or scan a local secondary index and request only attributes that are projected into that index, the operation will read only the index and not the table. If any of the requested attributes are not projected into the local secondary index, DynamoDB fetches each of these attributes from the parent table. This extra fetching incurs additional throughput cost and latency.
-- If you query or scan a global secondary index, you can only request attributes that are projected into the index. Global secondary index queries cannot fetch attributes from the parent table.
--
--
-- If neither @Select@ nor @AttributesToGet@ are specified, DynamoDB defaults to @ALL_ATTRIBUTES@ when accessing a table, and @ALL_PROJECTED_ATTRIBUTES@ when accessing an index. You cannot use both @Select@ and @AttributesToGet@ together in a single request, unless the value for @Select@ is @SPECIFIC_ATTRIBUTES@ . (This usage is equivalent to specifying @AttributesToGet@ without any value for @Select@ .)
--
-- /Note:/ Consider using 'select' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qSelect :: Lens.Lens' Query (Core.Maybe Types.Select)
qSelect = Lens.field @"select"
{-# DEPRECATED qSelect "Use generic-lens or generic-optics with 'select' instead." #-}

instance Core.FromJSON Query where
  toJSON Query {..} =
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
            ("KeyConditionExpression" Core..=) Core.<$> keyConditionExpression,
            ("KeyConditions" Core..=) Core.<$> keyConditions,
            ("Limit" Core..=) Core.<$> limit,
            ("ProjectionExpression" Core..=) Core.<$> projectionExpression,
            ("QueryFilter" Core..=) Core.<$> queryFilter,
            ("ReturnConsumedCapacity" Core..=) Core.<$> returnConsumedCapacity,
            ("ScanIndexForward" Core..=) Core.<$> scanIndexForward,
            ("Select" Core..=) Core.<$> select
          ]
      )

instance Core.AWSRequest Query where
  type Rs Query = QueryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.Query")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryResponse'
            Core.<$> (x Core..:? "ConsumedCapacity")
            Core.<*> (x Core..:? "Count")
            Core.<*> (x Core..:? "Items")
            Core.<*> (x Core..:? "LastEvaluatedKey")
            Core.<*> (x Core..:? "ScannedCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager Query where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"lastEvaluatedKey") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"exclusiveStartKey"
            Lens..~ rs Lens.^. Lens.field @"lastEvaluatedKey"
        )

-- | Represents the output of a @Query@ operation.
--
-- /See:/ 'mkQueryResponse' smart constructor.
data QueryResponse = QueryResponse'
  { -- | The capacity units consumed by the @Query@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
    consumedCapacity :: Core.Maybe Types.ConsumedCapacity,
    -- | The number of items in the response.
    --
    -- If you used a @QueryFilter@ in the request, then @Count@ is the number of items returned after the filter was applied, and @ScannedCount@ is the number of matching items before the filter was applied.
    -- If you did not use a filter in the request, then @Count@ and @ScannedCount@ are the same.
    count :: Core.Maybe Core.Int,
    -- | An array of item attributes that match the query criteria. Each element in this array consists of an attribute name and the value for that attribute.
    items :: Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue],
    -- | The primary key of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
    --
    -- If @LastEvaluatedKey@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved.
    -- If @LastEvaluatedKey@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedKey@ is empty.
    lastEvaluatedKey :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | The number of items evaluated, before any @QueryFilter@ is applied. A high @ScannedCount@ value with few, or no, @Count@ results indicates an inefficient @Query@ operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount> in the /Amazon DynamoDB Developer Guide/ .
    --
    -- If you did not use a filter in the request, then @ScannedCount@ is the same as @Count@ .
    scannedCount :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryResponse' value with any optional fields omitted.
mkQueryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  QueryResponse
mkQueryResponse responseStatus =
  QueryResponse'
    { consumedCapacity = Core.Nothing,
      count = Core.Nothing,
      items = Core.Nothing,
      lastEvaluatedKey = Core.Nothing,
      scannedCount = Core.Nothing,
      responseStatus
    }

-- | The capacity units consumed by the @Query@ operation. The data returned includes the total provisioned throughput consumed, along with statistics for the table and any indexes involved in the operation. @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@ parameter was specified. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrrsConsumedCapacity :: Lens.Lens' QueryResponse (Core.Maybe Types.ConsumedCapacity)
qrrsConsumedCapacity = Lens.field @"consumedCapacity"
{-# DEPRECATED qrrsConsumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead." #-}

-- | The number of items in the response.
--
-- If you used a @QueryFilter@ in the request, then @Count@ is the number of items returned after the filter was applied, and @ScannedCount@ is the number of matching items before the filter was applied.
-- If you did not use a filter in the request, then @Count@ and @ScannedCount@ are the same.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrrsCount :: Lens.Lens' QueryResponse (Core.Maybe Core.Int)
qrrsCount = Lens.field @"count"
{-# DEPRECATED qrrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | An array of item attributes that match the query criteria. Each element in this array consists of an attribute name and the value for that attribute.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrrsItems :: Lens.Lens' QueryResponse (Core.Maybe [Core.HashMap Types.AttributeName Types.AttributeValue])
qrrsItems = Lens.field @"items"
{-# DEPRECATED qrrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The primary key of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedKey@ is empty, then the "last page" of results has been processed and there is no more data to be retrieved.
-- If @LastEvaluatedKey@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedKey@ is empty.
--
-- /Note:/ Consider using 'lastEvaluatedKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrrsLastEvaluatedKey :: Lens.Lens' QueryResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
qrrsLastEvaluatedKey = Lens.field @"lastEvaluatedKey"
{-# DEPRECATED qrrsLastEvaluatedKey "Use generic-lens or generic-optics with 'lastEvaluatedKey' instead." #-}

-- | The number of items evaluated, before any @QueryFilter@ is applied. A high @ScannedCount@ value with few, or no, @Count@ results indicates an inefficient @Query@ operation. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount> in the /Amazon DynamoDB Developer Guide/ .
--
-- If you did not use a filter in the request, then @ScannedCount@ is the same as @Count@ .
--
-- /Note:/ Consider using 'scannedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrrsScannedCount :: Lens.Lens' QueryResponse (Core.Maybe Core.Int)
qrrsScannedCount = Lens.field @"scannedCount"
{-# DEPRECATED qrrsScannedCount "Use generic-lens or generic-optics with 'scannedCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qrrsResponseStatus :: Lens.Lens' QueryResponse Core.Int
qrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED qrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
