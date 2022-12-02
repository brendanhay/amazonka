{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDB.Scan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @Scan@ operation returns one or more items and item attributes by
-- accessing every item in a table or a secondary index. To have DynamoDB
-- return fewer items, you can provide a @FilterExpression@ operation.
--
-- If the total number of scanned items exceeds the maximum dataset size
-- limit of 1 MB, the scan stops and results are returned to the user as a
-- @LastEvaluatedKey@ value to continue the scan in a subsequent operation.
-- The results also include the number of items exceeding the limit. A scan
-- can result in no table data meeting the filter criteria.
--
-- A single @Scan@ operation reads up to the maximum number of items set
-- (if using the @Limit@ parameter) or a maximum of 1 MB of data and then
-- apply any filtering to the results using @FilterExpression@. If
-- @LastEvaluatedKey@ is present in the response, you need to paginate the
-- result set. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Scan.html#Scan.Pagination Paginating the Results>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- @Scan@ operations proceed sequentially; however, for faster performance
-- on a large table or secondary index, applications can request a parallel
-- @Scan@ operation by providing the @Segment@ and @TotalSegments@
-- parameters. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Scan.html#Scan.ParallelScan Parallel Scan>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- @Scan@ uses eventually consistent reads when accessing the data in a
-- table; therefore, the result set might not include the changes to data
-- in the table immediately before the operation began. If you need a
-- consistent copy of the data, as of the time that the @Scan@ begins, you
-- can set the @ConsistentRead@ parameter to @true@.
--
-- This operation returns paginated results.
module Amazonka.DynamoDB.Scan
  ( -- * Creating a Request
    Scan (..),
    newScan,

    -- * Request Lenses
    scan_scanFilter,
    scan_consistentRead,
    scan_expressionAttributeValues,
    scan_expressionAttributeNames,
    scan_conditionalOperator,
    scan_returnConsumedCapacity,
    scan_exclusiveStartKey,
    scan_filterExpression,
    scan_attributesToGet,
    scan_select,
    scan_segment,
    scan_limit,
    scan_indexName,
    scan_totalSegments,
    scan_projectionExpression,
    scan_tableName,

    -- * Destructuring the Response
    ScanResponse (..),
    newScanResponse,

    -- * Response Lenses
    scanResponse_items,
    scanResponse_lastEvaluatedKey,
    scanResponse_count,
    scanResponse_consumedCapacity,
    scanResponse_scannedCount,
    scanResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @Scan@ operation.
--
-- /See:/ 'newScan' smart constructor.
data Scan = Scan'
  { -- | This is a legacy parameter. Use @FilterExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ScanFilter.html ScanFilter>
    -- in the /Amazon DynamoDB Developer Guide/.
    scanFilter :: Prelude.Maybe (Prelude.HashMap Prelude.Text Condition),
    -- | A Boolean value that determines the read consistency model during the
    -- scan:
    --
    -- -   If @ConsistentRead@ is @false@, then the data returned from @Scan@
    --     might not contain the results from other recently completed write
    --     operations (@PutItem@, @UpdateItem@, or @DeleteItem@).
    --
    -- -   If @ConsistentRead@ is @true@, then all of the write operations that
    --     completed before the @Scan@ began are guaranteed to be contained in
    --     the @Scan@ response.
    --
    -- The default setting for @ConsistentRead@ is @false@.
    --
    -- The @ConsistentRead@ parameter is not supported on global secondary
    -- indexes. If you scan a global secondary index with @ConsistentRead@ set
    -- to true, you will receive a @ValidationException@.
    consistentRead :: Prelude.Maybe Prelude.Bool,
    -- | One or more values that can be substituted in an expression.
    --
    -- Use the __:__ (colon) character in an expression to dereference an
    -- attribute value. For example, suppose that you wanted to check whether
    -- the value of the @ProductStatus@ attribute was one of the following:
    --
    -- @Available | Backordered | Discontinued@
    --
    -- You would first need to specify @ExpressionAttributeValues@ as follows:
    --
    -- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
    --
    -- You could then use these values in an expression, such as this:
    --
    -- @ProductStatus IN (:avail, :back, :disc)@
    --
    -- For more information on expression attribute values, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
    -- in the /Amazon DynamoDB Developer Guide/.
    expressionAttributeValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | One or more substitution tokens for attribute names in an expression.
    -- The following are some use cases for using @ExpressionAttributeNames@:
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
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
    -- in the /Amazon DynamoDB Developer Guide/). To work around this, you
    -- could specify the following for @ExpressionAttributeNames@:
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
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    expressionAttributeNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This is a legacy parameter. Use @FilterExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
    -- in the /Amazon DynamoDB Developer Guide/.
    conditionalOperator :: Prelude.Maybe ConditionalOperator,
    returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
    -- | The primary key of the first item that this operation will evaluate. Use
    -- the value that was returned for @LastEvaluatedKey@ in the previous
    -- operation.
    --
    -- The data type for @ExclusiveStartKey@ must be String, Number or Binary.
    -- No set data types are allowed.
    --
    -- In a parallel scan, a @Scan@ request that includes @ExclusiveStartKey@
    -- must specify the same segment whose previous @Scan@ returned the
    -- corresponding value of @LastEvaluatedKey@.
    exclusiveStartKey :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | A string that contains conditions that DynamoDB applies after the @Scan@
    -- operation, but before the data is returned to you. Items that do not
    -- satisfy the @FilterExpression@ criteria are not returned.
    --
    -- A @FilterExpression@ is applied after the items have already been read;
    -- the process of filtering does not consume any additional read capacity
    -- units.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Query.FilterExpression Filter Expressions>
    -- in the /Amazon DynamoDB Developer Guide/.
    filterExpression :: Prelude.Maybe Prelude.Text,
    -- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
    -- in the /Amazon DynamoDB Developer Guide/.
    attributesToGet :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The attributes to be returned in the result. You can retrieve all item
    -- attributes, specific item attributes, the count of matching items, or in
    -- the case of an index, some or all of the attributes projected into the
    -- index.
    --
    -- -   @ALL_ATTRIBUTES@ - Returns all of the item attributes from the
    --     specified table or index. If you query a local secondary index, then
    --     for each matching item in the index, DynamoDB fetches the entire
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
    --     @ProjectionExpression@. This return value is equivalent to
    --     specifying @ProjectionExpression@ without specifying any value for
    --     @Select@.
    --
    --     If you query or scan a local secondary index and request only
    --     attributes that are projected into that index, the operation reads
    --     only the index and not the table. If any of the requested attributes
    --     are not projected into the local secondary index, DynamoDB fetches
    --     each of these attributes from the parent table. This extra fetching
    --     incurs additional throughput cost and latency.
    --
    --     If you query or scan a global secondary index, you can only request
    --     attributes that are projected into the index. Global secondary index
    --     queries cannot fetch attributes from the parent table.
    --
    -- If neither @Select@ nor @ProjectionExpression@ are specified, DynamoDB
    -- defaults to @ALL_ATTRIBUTES@ when accessing a table, and
    -- @ALL_PROJECTED_ATTRIBUTES@ when accessing an index. You cannot use both
    -- @Select@ and @ProjectionExpression@ together in a single request, unless
    -- the value for @Select@ is @SPECIFIC_ATTRIBUTES@. (This usage is
    -- equivalent to specifying @ProjectionExpression@ without any value for
    -- @Select@.)
    --
    -- If you use the @ProjectionExpression@ parameter, then the value for
    -- @Select@ can only be @SPECIFIC_ATTRIBUTES@. Any other value for @Select@
    -- will return an error.
    select :: Prelude.Maybe Select,
    -- | For a parallel @Scan@ request, @Segment@ identifies an individual
    -- segment to be scanned by an application worker.
    --
    -- Segment IDs are zero-based, so the first segment is always 0. For
    -- example, if you want to use four application threads to scan a table or
    -- an index, then the first thread specifies a @Segment@ value of 0, the
    -- second thread specifies 1, and so on.
    --
    -- The value of @LastEvaluatedKey@ returned from a parallel @Scan@ request
    -- must be used as @ExclusiveStartKey@ with the same segment ID in a
    -- subsequent @Scan@ operation.
    --
    -- The value for @Segment@ must be greater than or equal to 0, and less
    -- than the value provided for @TotalSegments@.
    --
    -- If you provide @Segment@, you must also provide @TotalSegments@.
    segment :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of items to evaluate (not necessarily the number of
    -- matching items). If DynamoDB processes the number of items up to the
    -- limit while processing the results, it stops the operation and returns
    -- the matching values up to that point, and a key in @LastEvaluatedKey@ to
    -- apply in a subsequent operation, so that you can pick up where you left
    -- off. Also, if the processed dataset size exceeds 1 MB before DynamoDB
    -- reaches this limit, it stops the operation and returns the matching
    -- values up to the limit, and a key in @LastEvaluatedKey@ to apply in a
    -- subsequent operation to continue the operation. For more information,
    -- see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Working with Queries>
    -- in the /Amazon DynamoDB Developer Guide/.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of a secondary index to scan. This index can be any local
    -- secondary index or global secondary index. Note that if you use the
    -- @IndexName@ parameter, you must also provide @TableName@.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | For a parallel @Scan@ request, @TotalSegments@ represents the total
    -- number of segments into which the @Scan@ operation will be divided. The
    -- value of @TotalSegments@ corresponds to the number of application
    -- workers that will perform the parallel scan. For example, if you want to
    -- use four application threads to scan a table or an index, specify a
    -- @TotalSegments@ value of 4.
    --
    -- The value for @TotalSegments@ must be greater than or equal to 1, and
    -- less than or equal to 1000000. If you specify a @TotalSegments@ value of
    -- 1, the @Scan@ operation will be sequential rather than parallel.
    --
    -- If you specify @TotalSegments@, you must also specify @Segment@.
    totalSegments :: Prelude.Maybe Prelude.Natural,
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
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    projectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The name of the table containing the requested items; or, if you provide
    -- @IndexName@, the name of the table to which that index belongs.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scanFilter', 'scan_scanFilter' - This is a legacy parameter. Use @FilterExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ScanFilter.html ScanFilter>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'consistentRead', 'scan_consistentRead' - A Boolean value that determines the read consistency model during the
-- scan:
--
-- -   If @ConsistentRead@ is @false@, then the data returned from @Scan@
--     might not contain the results from other recently completed write
--     operations (@PutItem@, @UpdateItem@, or @DeleteItem@).
--
-- -   If @ConsistentRead@ is @true@, then all of the write operations that
--     completed before the @Scan@ began are guaranteed to be contained in
--     the @Scan@ response.
--
-- The default setting for @ConsistentRead@ is @false@.
--
-- The @ConsistentRead@ parameter is not supported on global secondary
-- indexes. If you scan a global secondary index with @ConsistentRead@ set
-- to true, you will receive a @ValidationException@.
--
-- 'expressionAttributeValues', 'scan_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the @ProductStatus@ attribute was one of the following:
--
-- @Available | Backordered | Discontinued@
--
-- You would first need to specify @ExpressionAttributeValues@ as follows:
--
-- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
--
-- You could then use these values in an expression, such as this:
--
-- @ProductStatus IN (:avail, :back, :disc)@
--
-- For more information on expression attribute values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'expressionAttributeNames', 'scan_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using @ExpressionAttributeNames@:
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
-- could specify the following for @ExpressionAttributeNames@:
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'conditionalOperator', 'scan_conditionalOperator' - This is a legacy parameter. Use @FilterExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnConsumedCapacity', 'scan_returnConsumedCapacity' - Undocumented member.
--
-- 'exclusiveStartKey', 'scan_exclusiveStartKey' - The primary key of the first item that this operation will evaluate. Use
-- the value that was returned for @LastEvaluatedKey@ in the previous
-- operation.
--
-- The data type for @ExclusiveStartKey@ must be String, Number or Binary.
-- No set data types are allowed.
--
-- In a parallel scan, a @Scan@ request that includes @ExclusiveStartKey@
-- must specify the same segment whose previous @Scan@ returned the
-- corresponding value of @LastEvaluatedKey@.
--
-- 'filterExpression', 'scan_filterExpression' - A string that contains conditions that DynamoDB applies after the @Scan@
-- operation, but before the data is returned to you. Items that do not
-- satisfy the @FilterExpression@ criteria are not returned.
--
-- A @FilterExpression@ is applied after the items have already been read;
-- the process of filtering does not consume any additional read capacity
-- units.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Query.FilterExpression Filter Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'attributesToGet', 'scan_attributesToGet' - This is a legacy parameter. Use @ProjectionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'select', 'scan_select' - The attributes to be returned in the result. You can retrieve all item
-- attributes, specific item attributes, the count of matching items, or in
-- the case of an index, some or all of the attributes projected into the
-- index.
--
-- -   @ALL_ATTRIBUTES@ - Returns all of the item attributes from the
--     specified table or index. If you query a local secondary index, then
--     for each matching item in the index, DynamoDB fetches the entire
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
--     @ProjectionExpression@. This return value is equivalent to
--     specifying @ProjectionExpression@ without specifying any value for
--     @Select@.
--
--     If you query or scan a local secondary index and request only
--     attributes that are projected into that index, the operation reads
--     only the index and not the table. If any of the requested attributes
--     are not projected into the local secondary index, DynamoDB fetches
--     each of these attributes from the parent table. This extra fetching
--     incurs additional throughput cost and latency.
--
--     If you query or scan a global secondary index, you can only request
--     attributes that are projected into the index. Global secondary index
--     queries cannot fetch attributes from the parent table.
--
-- If neither @Select@ nor @ProjectionExpression@ are specified, DynamoDB
-- defaults to @ALL_ATTRIBUTES@ when accessing a table, and
-- @ALL_PROJECTED_ATTRIBUTES@ when accessing an index. You cannot use both
-- @Select@ and @ProjectionExpression@ together in a single request, unless
-- the value for @Select@ is @SPECIFIC_ATTRIBUTES@. (This usage is
-- equivalent to specifying @ProjectionExpression@ without any value for
-- @Select@.)
--
-- If you use the @ProjectionExpression@ parameter, then the value for
-- @Select@ can only be @SPECIFIC_ATTRIBUTES@. Any other value for @Select@
-- will return an error.
--
-- 'segment', 'scan_segment' - For a parallel @Scan@ request, @Segment@ identifies an individual
-- segment to be scanned by an application worker.
--
-- Segment IDs are zero-based, so the first segment is always 0. For
-- example, if you want to use four application threads to scan a table or
-- an index, then the first thread specifies a @Segment@ value of 0, the
-- second thread specifies 1, and so on.
--
-- The value of @LastEvaluatedKey@ returned from a parallel @Scan@ request
-- must be used as @ExclusiveStartKey@ with the same segment ID in a
-- subsequent @Scan@ operation.
--
-- The value for @Segment@ must be greater than or equal to 0, and less
-- than the value provided for @TotalSegments@.
--
-- If you provide @Segment@, you must also provide @TotalSegments@.
--
-- 'limit', 'scan_limit' - The maximum number of items to evaluate (not necessarily the number of
-- matching items). If DynamoDB processes the number of items up to the
-- limit while processing the results, it stops the operation and returns
-- the matching values up to that point, and a key in @LastEvaluatedKey@ to
-- apply in a subsequent operation, so that you can pick up where you left
-- off. Also, if the processed dataset size exceeds 1 MB before DynamoDB
-- reaches this limit, it stops the operation and returns the matching
-- values up to the limit, and a key in @LastEvaluatedKey@ to apply in a
-- subsequent operation to continue the operation. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Working with Queries>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'indexName', 'scan_indexName' - The name of a secondary index to scan. This index can be any local
-- secondary index or global secondary index. Note that if you use the
-- @IndexName@ parameter, you must also provide @TableName@.
--
-- 'totalSegments', 'scan_totalSegments' - For a parallel @Scan@ request, @TotalSegments@ represents the total
-- number of segments into which the @Scan@ operation will be divided. The
-- value of @TotalSegments@ corresponds to the number of application
-- workers that will perform the parallel scan. For example, if you want to
-- use four application threads to scan a table or an index, specify a
-- @TotalSegments@ value of 4.
--
-- The value for @TotalSegments@ must be greater than or equal to 1, and
-- less than or equal to 1000000. If you specify a @TotalSegments@ value of
-- 1, the @Scan@ operation will be sequential rather than parallel.
--
-- If you specify @TotalSegments@, you must also specify @Segment@.
--
-- 'projectionExpression', 'scan_projectionExpression' - A string that identifies one or more attributes to retrieve from the
-- specified table or index. These attributes can include scalars, sets, or
-- elements of a JSON document. The attributes in the expression must be
-- separated by commas.
--
-- If no attribute names are specified, then all attributes will be
-- returned. If any of the requested attributes are not found, they will
-- not appear in the result.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'tableName', 'scan_tableName' - The name of the table containing the requested items; or, if you provide
-- @IndexName@, the name of the table to which that index belongs.
newScan ::
  -- | 'tableName'
  Prelude.Text ->
  Scan
newScan pTableName_ =
  Scan'
    { scanFilter = Prelude.Nothing,
      consistentRead = Prelude.Nothing,
      expressionAttributeValues = Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      conditionalOperator = Prelude.Nothing,
      returnConsumedCapacity = Prelude.Nothing,
      exclusiveStartKey = Prelude.Nothing,
      filterExpression = Prelude.Nothing,
      attributesToGet = Prelude.Nothing,
      select = Prelude.Nothing,
      segment = Prelude.Nothing,
      limit = Prelude.Nothing,
      indexName = Prelude.Nothing,
      totalSegments = Prelude.Nothing,
      projectionExpression = Prelude.Nothing,
      tableName = pTableName_
    }

-- | This is a legacy parameter. Use @FilterExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ScanFilter.html ScanFilter>
-- in the /Amazon DynamoDB Developer Guide/.
scan_scanFilter :: Lens.Lens' Scan (Prelude.Maybe (Prelude.HashMap Prelude.Text Condition))
scan_scanFilter = Lens.lens (\Scan' {scanFilter} -> scanFilter) (\s@Scan' {} a -> s {scanFilter = a} :: Scan) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value that determines the read consistency model during the
-- scan:
--
-- -   If @ConsistentRead@ is @false@, then the data returned from @Scan@
--     might not contain the results from other recently completed write
--     operations (@PutItem@, @UpdateItem@, or @DeleteItem@).
--
-- -   If @ConsistentRead@ is @true@, then all of the write operations that
--     completed before the @Scan@ began are guaranteed to be contained in
--     the @Scan@ response.
--
-- The default setting for @ConsistentRead@ is @false@.
--
-- The @ConsistentRead@ parameter is not supported on global secondary
-- indexes. If you scan a global secondary index with @ConsistentRead@ set
-- to true, you will receive a @ValidationException@.
scan_consistentRead :: Lens.Lens' Scan (Prelude.Maybe Prelude.Bool)
scan_consistentRead = Lens.lens (\Scan' {consistentRead} -> consistentRead) (\s@Scan' {} a -> s {consistentRead = a} :: Scan)

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the @ProductStatus@ attribute was one of the following:
--
-- @Available | Backordered | Discontinued@
--
-- You would first need to specify @ExpressionAttributeValues@ as follows:
--
-- @{ \":avail\":{\"S\":\"Available\"}, \":back\":{\"S\":\"Backordered\"}, \":disc\":{\"S\":\"Discontinued\"} }@
--
-- You could then use these values in an expression, such as this:
--
-- @ProductStatus IN (:avail, :back, :disc)@
--
-- For more information on expression attribute values, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Condition Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
scan_expressionAttributeValues :: Lens.Lens' Scan (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
scan_expressionAttributeValues = Lens.lens (\Scan' {expressionAttributeValues} -> expressionAttributeValues) (\s@Scan' {} a -> s {expressionAttributeValues = a} :: Scan) Prelude.. Lens.mapping Lens.coerced

-- | One or more substitution tokens for attribute names in an expression.
-- The following are some use cases for using @ExpressionAttributeNames@:
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/). To work around this, you
-- could specify the following for @ExpressionAttributeNames@:
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
scan_expressionAttributeNames :: Lens.Lens' Scan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
scan_expressionAttributeNames = Lens.lens (\Scan' {expressionAttributeNames} -> expressionAttributeNames) (\s@Scan' {} a -> s {expressionAttributeNames = a} :: Scan) Prelude.. Lens.mapping Lens.coerced

-- | This is a legacy parameter. Use @FilterExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
-- in the /Amazon DynamoDB Developer Guide/.
scan_conditionalOperator :: Lens.Lens' Scan (Prelude.Maybe ConditionalOperator)
scan_conditionalOperator = Lens.lens (\Scan' {conditionalOperator} -> conditionalOperator) (\s@Scan' {} a -> s {conditionalOperator = a} :: Scan)

-- | Undocumented member.
scan_returnConsumedCapacity :: Lens.Lens' Scan (Prelude.Maybe ReturnConsumedCapacity)
scan_returnConsumedCapacity = Lens.lens (\Scan' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@Scan' {} a -> s {returnConsumedCapacity = a} :: Scan)

-- | The primary key of the first item that this operation will evaluate. Use
-- the value that was returned for @LastEvaluatedKey@ in the previous
-- operation.
--
-- The data type for @ExclusiveStartKey@ must be String, Number or Binary.
-- No set data types are allowed.
--
-- In a parallel scan, a @Scan@ request that includes @ExclusiveStartKey@
-- must specify the same segment whose previous @Scan@ returned the
-- corresponding value of @LastEvaluatedKey@.
scan_exclusiveStartKey :: Lens.Lens' Scan (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
scan_exclusiveStartKey = Lens.lens (\Scan' {exclusiveStartKey} -> exclusiveStartKey) (\s@Scan' {} a -> s {exclusiveStartKey = a} :: Scan) Prelude.. Lens.mapping Lens.coerced

-- | A string that contains conditions that DynamoDB applies after the @Scan@
-- operation, but before the data is returned to you. Items that do not
-- satisfy the @FilterExpression@ criteria are not returned.
--
-- A @FilterExpression@ is applied after the items have already been read;
-- the process of filtering does not consume any additional read capacity
-- units.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Query.FilterExpression Filter Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
scan_filterExpression :: Lens.Lens' Scan (Prelude.Maybe Prelude.Text)
scan_filterExpression = Lens.lens (\Scan' {filterExpression} -> filterExpression) (\s@Scan' {} a -> s {filterExpression = a} :: Scan)

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
-- in the /Amazon DynamoDB Developer Guide/.
scan_attributesToGet :: Lens.Lens' Scan (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
scan_attributesToGet = Lens.lens (\Scan' {attributesToGet} -> attributesToGet) (\s@Scan' {} a -> s {attributesToGet = a} :: Scan) Prelude.. Lens.mapping Lens.coerced

-- | The attributes to be returned in the result. You can retrieve all item
-- attributes, specific item attributes, the count of matching items, or in
-- the case of an index, some or all of the attributes projected into the
-- index.
--
-- -   @ALL_ATTRIBUTES@ - Returns all of the item attributes from the
--     specified table or index. If you query a local secondary index, then
--     for each matching item in the index, DynamoDB fetches the entire
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
--     @ProjectionExpression@. This return value is equivalent to
--     specifying @ProjectionExpression@ without specifying any value for
--     @Select@.
--
--     If you query or scan a local secondary index and request only
--     attributes that are projected into that index, the operation reads
--     only the index and not the table. If any of the requested attributes
--     are not projected into the local secondary index, DynamoDB fetches
--     each of these attributes from the parent table. This extra fetching
--     incurs additional throughput cost and latency.
--
--     If you query or scan a global secondary index, you can only request
--     attributes that are projected into the index. Global secondary index
--     queries cannot fetch attributes from the parent table.
--
-- If neither @Select@ nor @ProjectionExpression@ are specified, DynamoDB
-- defaults to @ALL_ATTRIBUTES@ when accessing a table, and
-- @ALL_PROJECTED_ATTRIBUTES@ when accessing an index. You cannot use both
-- @Select@ and @ProjectionExpression@ together in a single request, unless
-- the value for @Select@ is @SPECIFIC_ATTRIBUTES@. (This usage is
-- equivalent to specifying @ProjectionExpression@ without any value for
-- @Select@.)
--
-- If you use the @ProjectionExpression@ parameter, then the value for
-- @Select@ can only be @SPECIFIC_ATTRIBUTES@. Any other value for @Select@
-- will return an error.
scan_select :: Lens.Lens' Scan (Prelude.Maybe Select)
scan_select = Lens.lens (\Scan' {select} -> select) (\s@Scan' {} a -> s {select = a} :: Scan)

-- | For a parallel @Scan@ request, @Segment@ identifies an individual
-- segment to be scanned by an application worker.
--
-- Segment IDs are zero-based, so the first segment is always 0. For
-- example, if you want to use four application threads to scan a table or
-- an index, then the first thread specifies a @Segment@ value of 0, the
-- second thread specifies 1, and so on.
--
-- The value of @LastEvaluatedKey@ returned from a parallel @Scan@ request
-- must be used as @ExclusiveStartKey@ with the same segment ID in a
-- subsequent @Scan@ operation.
--
-- The value for @Segment@ must be greater than or equal to 0, and less
-- than the value provided for @TotalSegments@.
--
-- If you provide @Segment@, you must also provide @TotalSegments@.
scan_segment :: Lens.Lens' Scan (Prelude.Maybe Prelude.Natural)
scan_segment = Lens.lens (\Scan' {segment} -> segment) (\s@Scan' {} a -> s {segment = a} :: Scan)

-- | The maximum number of items to evaluate (not necessarily the number of
-- matching items). If DynamoDB processes the number of items up to the
-- limit while processing the results, it stops the operation and returns
-- the matching values up to that point, and a key in @LastEvaluatedKey@ to
-- apply in a subsequent operation, so that you can pick up where you left
-- off. Also, if the processed dataset size exceeds 1 MB before DynamoDB
-- reaches this limit, it stops the operation and returns the matching
-- values up to the limit, and a key in @LastEvaluatedKey@ to apply in a
-- subsequent operation to continue the operation. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Working with Queries>
-- in the /Amazon DynamoDB Developer Guide/.
scan_limit :: Lens.Lens' Scan (Prelude.Maybe Prelude.Natural)
scan_limit = Lens.lens (\Scan' {limit} -> limit) (\s@Scan' {} a -> s {limit = a} :: Scan)

-- | The name of a secondary index to scan. This index can be any local
-- secondary index or global secondary index. Note that if you use the
-- @IndexName@ parameter, you must also provide @TableName@.
scan_indexName :: Lens.Lens' Scan (Prelude.Maybe Prelude.Text)
scan_indexName = Lens.lens (\Scan' {indexName} -> indexName) (\s@Scan' {} a -> s {indexName = a} :: Scan)

-- | For a parallel @Scan@ request, @TotalSegments@ represents the total
-- number of segments into which the @Scan@ operation will be divided. The
-- value of @TotalSegments@ corresponds to the number of application
-- workers that will perform the parallel scan. For example, if you want to
-- use four application threads to scan a table or an index, specify a
-- @TotalSegments@ value of 4.
--
-- The value for @TotalSegments@ must be greater than or equal to 1, and
-- less than or equal to 1000000. If you specify a @TotalSegments@ value of
-- 1, the @Scan@ operation will be sequential rather than parallel.
--
-- If you specify @TotalSegments@, you must also specify @Segment@.
scan_totalSegments :: Lens.Lens' Scan (Prelude.Maybe Prelude.Natural)
scan_totalSegments = Lens.lens (\Scan' {totalSegments} -> totalSegments) (\s@Scan' {} a -> s {totalSegments = a} :: Scan)

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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Specifying Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
scan_projectionExpression :: Lens.Lens' Scan (Prelude.Maybe Prelude.Text)
scan_projectionExpression = Lens.lens (\Scan' {projectionExpression} -> projectionExpression) (\s@Scan' {} a -> s {projectionExpression = a} :: Scan)

-- | The name of the table containing the requested items; or, if you provide
-- @IndexName@, the name of the table to which that index belongs.
scan_tableName :: Lens.Lens' Scan Prelude.Text
scan_tableName = Lens.lens (\Scan' {tableName} -> tableName) (\s@Scan' {} a -> s {tableName = a} :: Scan)

instance Core.AWSPager Scan where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? scanResponse_lastEvaluatedKey Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& scan_exclusiveStartKey
          Lens..~ rs
          Lens.^? scanResponse_lastEvaluatedKey Prelude.. Lens._Just

instance Core.AWSRequest Scan where
  type AWSResponse Scan = ScanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ScanResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "LastEvaluatedKey"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Count")
            Prelude.<*> (x Data..?> "ConsumedCapacity")
            Prelude.<*> (x Data..?> "ScannedCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Scan where
  hashWithSalt _salt Scan' {..} =
    _salt `Prelude.hashWithSalt` scanFilter
      `Prelude.hashWithSalt` consistentRead
      `Prelude.hashWithSalt` expressionAttributeValues
      `Prelude.hashWithSalt` expressionAttributeNames
      `Prelude.hashWithSalt` conditionalOperator
      `Prelude.hashWithSalt` returnConsumedCapacity
      `Prelude.hashWithSalt` exclusiveStartKey
      `Prelude.hashWithSalt` filterExpression
      `Prelude.hashWithSalt` attributesToGet
      `Prelude.hashWithSalt` select
      `Prelude.hashWithSalt` segment
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` totalSegments
      `Prelude.hashWithSalt` projectionExpression
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData Scan where
  rnf Scan' {..} =
    Prelude.rnf scanFilter
      `Prelude.seq` Prelude.rnf consistentRead
      `Prelude.seq` Prelude.rnf expressionAttributeValues
      `Prelude.seq` Prelude.rnf expressionAttributeNames
      `Prelude.seq` Prelude.rnf conditionalOperator
      `Prelude.seq` Prelude.rnf returnConsumedCapacity
      `Prelude.seq` Prelude.rnf exclusiveStartKey
      `Prelude.seq` Prelude.rnf filterExpression
      `Prelude.seq` Prelude.rnf attributesToGet
      `Prelude.seq` Prelude.rnf select
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf totalSegments
      `Prelude.seq` Prelude.rnf projectionExpression
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders Scan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("DynamoDB_20120810.Scan" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Scan where
  toJSON Scan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ScanFilter" Data..=) Prelude.<$> scanFilter,
            ("ConsistentRead" Data..=)
              Prelude.<$> consistentRead,
            ("ExpressionAttributeValues" Data..=)
              Prelude.<$> expressionAttributeValues,
            ("ExpressionAttributeNames" Data..=)
              Prelude.<$> expressionAttributeNames,
            ("ConditionalOperator" Data..=)
              Prelude.<$> conditionalOperator,
            ("ReturnConsumedCapacity" Data..=)
              Prelude.<$> returnConsumedCapacity,
            ("ExclusiveStartKey" Data..=)
              Prelude.<$> exclusiveStartKey,
            ("FilterExpression" Data..=)
              Prelude.<$> filterExpression,
            ("AttributesToGet" Data..=)
              Prelude.<$> attributesToGet,
            ("Select" Data..=) Prelude.<$> select,
            ("Segment" Data..=) Prelude.<$> segment,
            ("Limit" Data..=) Prelude.<$> limit,
            ("IndexName" Data..=) Prelude.<$> indexName,
            ("TotalSegments" Data..=) Prelude.<$> totalSegments,
            ("ProjectionExpression" Data..=)
              Prelude.<$> projectionExpression,
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath Scan where
  toPath = Prelude.const "/"

instance Data.ToQuery Scan where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @Scan@ operation.
--
-- /See:/ 'newScanResponse' smart constructor.
data ScanResponse = ScanResponse'
  { -- | An array of item attributes that match the scan criteria. Each element
    -- in this array consists of an attribute name and the value for that
    -- attribute.
    items :: Prelude.Maybe [Prelude.HashMap Prelude.Text AttributeValue],
    -- | The primary key of the item where the operation stopped, inclusive of
    -- the previous result set. Use this value to start a new operation,
    -- excluding this value in the new request.
    --
    -- If @LastEvaluatedKey@ is empty, then the \"last page\" of results has
    -- been processed and there is no more data to be retrieved.
    --
    -- If @LastEvaluatedKey@ is not empty, it does not necessarily mean that
    -- there is more data in the result set. The only way to know when you have
    -- reached the end of the result set is when @LastEvaluatedKey@ is empty.
    lastEvaluatedKey :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | The number of items in the response.
    --
    -- If you set @ScanFilter@ in the request, then @Count@ is the number of
    -- items returned after the filter was applied, and @ScannedCount@ is the
    -- number of matching items before the filter was applied.
    --
    -- If you did not use a filter in the request, then @Count@ is the same as
    -- @ScannedCount@.
    count :: Prelude.Maybe Prelude.Int,
    -- | The capacity units consumed by the @Scan@ operation. The data returned
    -- includes the total provisioned throughput consumed, along with
    -- statistics for the table and any indexes involved in the operation.
    -- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
    -- parameter was specified. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
    -- in the /Amazon DynamoDB Developer Guide/.
    consumedCapacity :: Prelude.Maybe ConsumedCapacity,
    -- | The number of items evaluated, before any @ScanFilter@ is applied. A
    -- high @ScannedCount@ value with few, or no, @Count@ results indicates an
    -- inefficient @Scan@ operation. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount>
    -- in the /Amazon DynamoDB Developer Guide/.
    --
    -- If you did not use a filter in the request, then @ScannedCount@ is the
    -- same as @Count@.
    scannedCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'scanResponse_items' - An array of item attributes that match the scan criteria. Each element
-- in this array consists of an attribute name and the value for that
-- attribute.
--
-- 'lastEvaluatedKey', 'scanResponse_lastEvaluatedKey' - The primary key of the item where the operation stopped, inclusive of
-- the previous result set. Use this value to start a new operation,
-- excluding this value in the new request.
--
-- If @LastEvaluatedKey@ is empty, then the \"last page\" of results has
-- been processed and there is no more data to be retrieved.
--
-- If @LastEvaluatedKey@ is not empty, it does not necessarily mean that
-- there is more data in the result set. The only way to know when you have
-- reached the end of the result set is when @LastEvaluatedKey@ is empty.
--
-- 'count', 'scanResponse_count' - The number of items in the response.
--
-- If you set @ScanFilter@ in the request, then @Count@ is the number of
-- items returned after the filter was applied, and @ScannedCount@ is the
-- number of matching items before the filter was applied.
--
-- If you did not use a filter in the request, then @Count@ is the same as
-- @ScannedCount@.
--
-- 'consumedCapacity', 'scanResponse_consumedCapacity' - The capacity units consumed by the @Scan@ operation. The data returned
-- includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'scannedCount', 'scanResponse_scannedCount' - The number of items evaluated, before any @ScanFilter@ is applied. A
-- high @ScannedCount@ value with few, or no, @Count@ results indicates an
-- inefficient @Scan@ operation. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If you did not use a filter in the request, then @ScannedCount@ is the
-- same as @Count@.
--
-- 'httpStatus', 'scanResponse_httpStatus' - The response's http status code.
newScanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ScanResponse
newScanResponse pHttpStatus_ =
  ScanResponse'
    { items = Prelude.Nothing,
      lastEvaluatedKey = Prelude.Nothing,
      count = Prelude.Nothing,
      consumedCapacity = Prelude.Nothing,
      scannedCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of item attributes that match the scan criteria. Each element
-- in this array consists of an attribute name and the value for that
-- attribute.
scanResponse_items :: Lens.Lens' ScanResponse (Prelude.Maybe [Prelude.HashMap Prelude.Text AttributeValue])
scanResponse_items = Lens.lens (\ScanResponse' {items} -> items) (\s@ScanResponse' {} a -> s {items = a} :: ScanResponse) Prelude.. Lens.mapping Lens.coerced

-- | The primary key of the item where the operation stopped, inclusive of
-- the previous result set. Use this value to start a new operation,
-- excluding this value in the new request.
--
-- If @LastEvaluatedKey@ is empty, then the \"last page\" of results has
-- been processed and there is no more data to be retrieved.
--
-- If @LastEvaluatedKey@ is not empty, it does not necessarily mean that
-- there is more data in the result set. The only way to know when you have
-- reached the end of the result set is when @LastEvaluatedKey@ is empty.
scanResponse_lastEvaluatedKey :: Lens.Lens' ScanResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
scanResponse_lastEvaluatedKey = Lens.lens (\ScanResponse' {lastEvaluatedKey} -> lastEvaluatedKey) (\s@ScanResponse' {} a -> s {lastEvaluatedKey = a} :: ScanResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of items in the response.
--
-- If you set @ScanFilter@ in the request, then @Count@ is the number of
-- items returned after the filter was applied, and @ScannedCount@ is the
-- number of matching items before the filter was applied.
--
-- If you did not use a filter in the request, then @Count@ is the same as
-- @ScannedCount@.
scanResponse_count :: Lens.Lens' ScanResponse (Prelude.Maybe Prelude.Int)
scanResponse_count = Lens.lens (\ScanResponse' {count} -> count) (\s@ScanResponse' {} a -> s {count = a} :: ScanResponse)

-- | The capacity units consumed by the @Scan@ operation. The data returned
-- includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
-- in the /Amazon DynamoDB Developer Guide/.
scanResponse_consumedCapacity :: Lens.Lens' ScanResponse (Prelude.Maybe ConsumedCapacity)
scanResponse_consumedCapacity = Lens.lens (\ScanResponse' {consumedCapacity} -> consumedCapacity) (\s@ScanResponse' {} a -> s {consumedCapacity = a} :: ScanResponse)

-- | The number of items evaluated, before any @ScanFilter@ is applied. A
-- high @ScannedCount@ value with few, or no, @Count@ results indicates an
-- inefficient @Scan@ operation. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If you did not use a filter in the request, then @ScannedCount@ is the
-- same as @Count@.
scanResponse_scannedCount :: Lens.Lens' ScanResponse (Prelude.Maybe Prelude.Int)
scanResponse_scannedCount = Lens.lens (\ScanResponse' {scannedCount} -> scannedCount) (\s@ScanResponse' {} a -> s {scannedCount = a} :: ScanResponse)

-- | The response's http status code.
scanResponse_httpStatus :: Lens.Lens' ScanResponse Prelude.Int
scanResponse_httpStatus = Lens.lens (\ScanResponse' {httpStatus} -> httpStatus) (\s@ScanResponse' {} a -> s {httpStatus = a} :: ScanResponse)

instance Prelude.NFData ScanResponse where
  rnf ScanResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf lastEvaluatedKey
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf consumedCapacity
      `Prelude.seq` Prelude.rnf scannedCount
      `Prelude.seq` Prelude.rnf httpStatus
