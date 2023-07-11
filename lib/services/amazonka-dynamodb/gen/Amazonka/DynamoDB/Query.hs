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
-- Module      : Amazonka.DynamoDB.Query
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You must provide the name of the partition key attribute and a single
-- value for that attribute. @Query@ returns all items with that partition
-- key value. Optionally, you can provide a sort key attribute and use a
-- comparison operator to refine the search results.
--
-- Use the @KeyConditionExpression@ parameter to provide a specific value
-- for the partition key. The @Query@ operation will return all of the
-- items from the table or index with that partition key value. You can
-- optionally narrow the scope of the @Query@ operation by specifying a
-- sort key value and a comparison operator in @KeyConditionExpression@. To
-- further refine the @Query@ results, you can optionally provide a
-- @FilterExpression@. A @FilterExpression@ determines which items within
-- the results should be returned to you. All of the other results are
-- discarded.
--
-- A @Query@ operation always returns a result set. If no matching items
-- are found, the result set will be empty. Queries that do not return
-- results consume the minimum number of read capacity units for that type
-- of read operation.
--
-- DynamoDB calculates the number of read capacity units consumed based on
-- item size, not on the amount of data that is returned to an application.
-- The number of capacity units consumed will be the same whether you
-- request all of the attributes (the default behavior) or just some of
-- them (using a projection expression). The number will also be the same
-- whether or not you use a @FilterExpression@.
--
-- @Query@ results are always sorted by the sort key value. If the data
-- type of the sort key is Number, the results are returned in numeric
-- order; otherwise, the results are returned in order of UTF-8 bytes. By
-- default, the sort order is ascending. To reverse the order, set the
-- @ScanIndexForward@ parameter to false.
--
-- A single @Query@ operation will read up to the maximum number of items
-- set (if using the @Limit@ parameter) or a maximum of 1 MB of data and
-- then apply any filtering to the results using @FilterExpression@. If
-- @LastEvaluatedKey@ is present in the response, you will need to paginate
-- the result set. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Query.html#Query.Pagination Paginating the Results>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- @FilterExpression@ is applied after a @Query@ finishes, but before the
-- results are returned. A @FilterExpression@ cannot contain partition key
-- or sort key attributes. You need to specify those attributes in the
-- @KeyConditionExpression@.
--
-- A @Query@ operation can return an empty result set and a
-- @LastEvaluatedKey@ if all the items read for the page of results are
-- filtered out.
--
-- You can query a table, a local secondary index, or a global secondary
-- index. For a query on a table or on a local secondary index, you can set
-- the @ConsistentRead@ parameter to @true@ and obtain a strongly
-- consistent result. Global secondary indexes support eventually
-- consistent reads only, so do not specify @ConsistentRead@ when querying
-- a global secondary index.
--
-- This operation returns paginated results.
module Amazonka.DynamoDB.Query
  ( -- * Creating a Request
    Query (..),
    newQuery,

    -- * Request Lenses
    query_attributesToGet,
    query_conditionalOperator,
    query_consistentRead,
    query_exclusiveStartKey,
    query_expressionAttributeNames,
    query_expressionAttributeValues,
    query_filterExpression,
    query_indexName,
    query_keyConditionExpression,
    query_keyConditions,
    query_limit,
    query_projectionExpression,
    query_queryFilter,
    query_returnConsumedCapacity,
    query_scanIndexForward,
    query_select,
    query_tableName,

    -- * Destructuring the Response
    QueryResponse (..),
    newQueryResponse,

    -- * Response Lenses
    queryResponse_consumedCapacity,
    queryResponse_count,
    queryResponse_lastEvaluatedKey,
    queryResponse_scannedCount,
    queryResponse_httpStatus,
    queryResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @Query@ operation.
--
-- /See:/ 'newQuery' smart constructor.
data Query = Query'
  { -- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
    -- in the /Amazon DynamoDB Developer Guide/.
    attributesToGet :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | This is a legacy parameter. Use @FilterExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
    -- in the /Amazon DynamoDB Developer Guide/.
    conditionalOperator :: Prelude.Maybe ConditionalOperator,
    -- | Determines the read consistency model: If set to @true@, then the
    -- operation uses strongly consistent reads; otherwise, the operation uses
    -- eventually consistent reads.
    --
    -- Strongly consistent reads are not supported on global secondary indexes.
    -- If you query a global secondary index with @ConsistentRead@ set to
    -- @true@, you will receive a @ValidationException@.
    consistentRead :: Prelude.Maybe Prelude.Bool,
    -- | The primary key of the first item that this operation will evaluate. Use
    -- the value that was returned for @LastEvaluatedKey@ in the previous
    -- operation.
    --
    -- The data type for @ExclusiveStartKey@ must be String, Number, or Binary.
    -- No set data types are allowed.
    exclusiveStartKey :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
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
    -- | One or more values that can be substituted in an expression.
    --
    -- Use the __:__ (colon) character in an expression to dereference an
    -- attribute value. For example, suppose that you wanted to check whether
    -- the value of the /ProductStatus/ attribute was one of the following:
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
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions>
    -- in the /Amazon DynamoDB Developer Guide/.
    expressionAttributeValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | A string that contains conditions that DynamoDB applies after the
    -- @Query@ operation, but before the data is returned to you. Items that do
    -- not satisfy the @FilterExpression@ criteria are not returned.
    --
    -- A @FilterExpression@ does not allow key attributes. You cannot define a
    -- filter expression based on a partition key or a sort key.
    --
    -- A @FilterExpression@ is applied after the items have already been read;
    -- the process of filtering does not consume any additional read capacity
    -- units.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Query.FilterExpression Filter Expressions>
    -- in the /Amazon DynamoDB Developer Guide/.
    filterExpression :: Prelude.Maybe Prelude.Text,
    -- | The name of an index to query. This index can be any local secondary
    -- index or global secondary index on the table. Note that if you use the
    -- @IndexName@ parameter, you must also provide @TableName.@
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The condition that specifies the key values for items to be retrieved by
    -- the @Query@ action.
    --
    -- The condition must perform an equality test on a single partition key
    -- value.
    --
    -- The condition can optionally perform one of several comparison tests on
    -- a single sort key value. This allows @Query@ to retrieve one item with a
    -- given partition key value and sort key value, or several items that have
    -- the same partition key value but different sort key values.
    --
    -- The partition key equality test is required, and must be specified in
    -- the following format:
    --
    -- @partitionKeyName@ /=/ @:partitionkeyval@
    --
    -- If you also want to provide a condition for the sort key, it must be
    -- combined using @AND@ with the condition for the sort key. Following is
    -- an example, using the __=__ comparison operator for the sort key:
    --
    -- @partitionKeyName@ @=@ @:partitionkeyval@ @AND@ @sortKeyName@ @=@
    -- @:sortkeyval@
    --
    -- Valid comparisons for the sort key condition are as follows:
    --
    -- -   @sortKeyName@ @=@ @:sortkeyval@ - true if the sort key value is
    --     equal to @:sortkeyval@.
    --
    -- -   @sortKeyName@ @\<@ @:sortkeyval@ - true if the sort key value is
    --     less than @:sortkeyval@.
    --
    -- -   @sortKeyName@ @\<=@ @:sortkeyval@ - true if the sort key value is
    --     less than or equal to @:sortkeyval@.
    --
    -- -   @sortKeyName@ @>@ @:sortkeyval@ - true if the sort key value is
    --     greater than @:sortkeyval@.
    --
    -- -   @sortKeyName@ @>= @ @:sortkeyval@ - true if the sort key value is
    --     greater than or equal to @:sortkeyval@.
    --
    -- -   @sortKeyName@ @BETWEEN@ @:sortkeyval1@ @AND@ @:sortkeyval2@ - true
    --     if the sort key value is greater than or equal to @:sortkeyval1@,
    --     and less than or equal to @:sortkeyval2@.
    --
    -- -   @begins_with (@ @sortKeyName@, @:sortkeyval@ @)@ - true if the sort
    --     key value begins with a particular operand. (You cannot use this
    --     function with a sort key that is of type Number.) Note that the
    --     function name @begins_with@ is case-sensitive.
    --
    -- Use the @ExpressionAttributeValues@ parameter to replace tokens such as
    -- @:partitionval@ and @:sortval@ with actual values at runtime.
    --
    -- You can optionally use the @ExpressionAttributeNames@ parameter to
    -- replace the names of the partition key and sort key with placeholder
    -- tokens. This option might be necessary if an attribute name conflicts
    -- with a DynamoDB reserved word. For example, the following
    -- @KeyConditionExpression@ parameter causes an error because /Size/ is a
    -- reserved word:
    --
    -- -   @Size = :myval@
    --
    -- To work around this, define a placeholder (such a @#S@) to represent the
    -- attribute name /Size/. @KeyConditionExpression@ then is as follows:
    --
    -- -   @#S = :myval@
    --
    -- For a list of reserved words, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
    -- in the /Amazon DynamoDB Developer Guide/.
    --
    -- For more information on @ExpressionAttributeNames@ and
    -- @ExpressionAttributeValues@, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
    -- in the /Amazon DynamoDB Developer Guide/.
    keyConditionExpression :: Prelude.Maybe Prelude.Text,
    -- | This is a legacy parameter. Use @KeyConditionExpression@ instead. For
    -- more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.KeyConditions.html KeyConditions>
    -- in the /Amazon DynamoDB Developer Guide/.
    keyConditions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Condition),
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
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan>
    -- in the /Amazon DynamoDB Developer Guide/.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A string that identifies one or more attributes to retrieve from the
    -- table. These attributes can include scalars, sets, or elements of a JSON
    -- document. The attributes in the expression must be separated by commas.
    --
    -- If no attribute names are specified, then all attributes will be
    -- returned. If any of the requested attributes are not found, they will
    -- not appear in the result.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
    -- in the /Amazon DynamoDB Developer Guide/.
    projectionExpression :: Prelude.Maybe Prelude.Text,
    -- | This is a legacy parameter. Use @FilterExpression@ instead. For more
    -- information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.QueryFilter.html QueryFilter>
    -- in the /Amazon DynamoDB Developer Guide/.
    queryFilter :: Prelude.Maybe (Prelude.HashMap Prelude.Text Condition),
    returnConsumedCapacity :: Prelude.Maybe ReturnConsumedCapacity,
    -- | Specifies the order for index traversal: If @true@ (default), the
    -- traversal is performed in ascending order; if @false@, the traversal is
    -- performed in descending order.
    --
    -- Items with the same partition key value are stored in sorted order by
    -- sort key. If the sort key data type is Number, the results are stored in
    -- numeric order. For type String, the results are stored in order of UTF-8
    -- bytes. For type Binary, DynamoDB treats each byte of the binary data as
    -- unsigned.
    --
    -- If @ScanIndexForward@ is @true@, DynamoDB returns the results in the
    -- order in which they are stored (by sort key value). This is the default
    -- behavior. If @ScanIndexForward@ is @false@, DynamoDB reads the results
    -- in reverse order by sort key value, and then returns the results to the
    -- client.
    scanIndexForward :: Prelude.Maybe Prelude.Bool,
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
    --     attributes that are projected into that index, the operation will
    --     read only the index and not the table. If any of the requested
    --     attributes are not projected into the local secondary index,
    --     DynamoDB fetches each of these attributes from the parent table.
    --     This extra fetching incurs additional throughput cost and latency.
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
    -- | The name of the table containing the requested items.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Query' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributesToGet', 'query_attributesToGet' - This is a legacy parameter. Use @ProjectionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'conditionalOperator', 'query_conditionalOperator' - This is a legacy parameter. Use @FilterExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'consistentRead', 'query_consistentRead' - Determines the read consistency model: If set to @true@, then the
-- operation uses strongly consistent reads; otherwise, the operation uses
-- eventually consistent reads.
--
-- Strongly consistent reads are not supported on global secondary indexes.
-- If you query a global secondary index with @ConsistentRead@ set to
-- @true@, you will receive a @ValidationException@.
--
-- 'exclusiveStartKey', 'query_exclusiveStartKey' - The primary key of the first item that this operation will evaluate. Use
-- the value that was returned for @LastEvaluatedKey@ in the previous
-- operation.
--
-- The data type for @ExclusiveStartKey@ must be String, Number, or Binary.
-- No set data types are allowed.
--
-- 'expressionAttributeNames', 'query_expressionAttributeNames' - One or more substitution tokens for attribute names in an expression.
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
-- 'expressionAttributeValues', 'query_expressionAttributeValues' - One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the /ProductStatus/ attribute was one of the following:
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'filterExpression', 'query_filterExpression' - A string that contains conditions that DynamoDB applies after the
-- @Query@ operation, but before the data is returned to you. Items that do
-- not satisfy the @FilterExpression@ criteria are not returned.
--
-- A @FilterExpression@ does not allow key attributes. You cannot define a
-- filter expression based on a partition key or a sort key.
--
-- A @FilterExpression@ is applied after the items have already been read;
-- the process of filtering does not consume any additional read capacity
-- units.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Query.FilterExpression Filter Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'indexName', 'query_indexName' - The name of an index to query. This index can be any local secondary
-- index or global secondary index on the table. Note that if you use the
-- @IndexName@ parameter, you must also provide @TableName.@
--
-- 'keyConditionExpression', 'query_keyConditionExpression' - The condition that specifies the key values for items to be retrieved by
-- the @Query@ action.
--
-- The condition must perform an equality test on a single partition key
-- value.
--
-- The condition can optionally perform one of several comparison tests on
-- a single sort key value. This allows @Query@ to retrieve one item with a
-- given partition key value and sort key value, or several items that have
-- the same partition key value but different sort key values.
--
-- The partition key equality test is required, and must be specified in
-- the following format:
--
-- @partitionKeyName@ /=/ @:partitionkeyval@
--
-- If you also want to provide a condition for the sort key, it must be
-- combined using @AND@ with the condition for the sort key. Following is
-- an example, using the __=__ comparison operator for the sort key:
--
-- @partitionKeyName@ @=@ @:partitionkeyval@ @AND@ @sortKeyName@ @=@
-- @:sortkeyval@
--
-- Valid comparisons for the sort key condition are as follows:
--
-- -   @sortKeyName@ @=@ @:sortkeyval@ - true if the sort key value is
--     equal to @:sortkeyval@.
--
-- -   @sortKeyName@ @\<@ @:sortkeyval@ - true if the sort key value is
--     less than @:sortkeyval@.
--
-- -   @sortKeyName@ @\<=@ @:sortkeyval@ - true if the sort key value is
--     less than or equal to @:sortkeyval@.
--
-- -   @sortKeyName@ @>@ @:sortkeyval@ - true if the sort key value is
--     greater than @:sortkeyval@.
--
-- -   @sortKeyName@ @>= @ @:sortkeyval@ - true if the sort key value is
--     greater than or equal to @:sortkeyval@.
--
-- -   @sortKeyName@ @BETWEEN@ @:sortkeyval1@ @AND@ @:sortkeyval2@ - true
--     if the sort key value is greater than or equal to @:sortkeyval1@,
--     and less than or equal to @:sortkeyval2@.
--
-- -   @begins_with (@ @sortKeyName@, @:sortkeyval@ @)@ - true if the sort
--     key value begins with a particular operand. (You cannot use this
--     function with a sort key that is of type Number.) Note that the
--     function name @begins_with@ is case-sensitive.
--
-- Use the @ExpressionAttributeValues@ parameter to replace tokens such as
-- @:partitionval@ and @:sortval@ with actual values at runtime.
--
-- You can optionally use the @ExpressionAttributeNames@ parameter to
-- replace the names of the partition key and sort key with placeholder
-- tokens. This option might be necessary if an attribute name conflicts
-- with a DynamoDB reserved word. For example, the following
-- @KeyConditionExpression@ parameter causes an error because /Size/ is a
-- reserved word:
--
-- -   @Size = :myval@
--
-- To work around this, define a placeholder (such a @#S@) to represent the
-- attribute name /Size/. @KeyConditionExpression@ then is as follows:
--
-- -   @#S = :myval@
--
-- For a list of reserved words, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- For more information on @ExpressionAttributeNames@ and
-- @ExpressionAttributeValues@, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'keyConditions', 'query_keyConditions' - This is a legacy parameter. Use @KeyConditionExpression@ instead. For
-- more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.KeyConditions.html KeyConditions>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'limit', 'query_limit' - The maximum number of items to evaluate (not necessarily the number of
-- matching items). If DynamoDB processes the number of items up to the
-- limit while processing the results, it stops the operation and returns
-- the matching values up to that point, and a key in @LastEvaluatedKey@ to
-- apply in a subsequent operation, so that you can pick up where you left
-- off. Also, if the processed dataset size exceeds 1 MB before DynamoDB
-- reaches this limit, it stops the operation and returns the matching
-- values up to the limit, and a key in @LastEvaluatedKey@ to apply in a
-- subsequent operation to continue the operation. For more information,
-- see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'projectionExpression', 'query_projectionExpression' - A string that identifies one or more attributes to retrieve from the
-- table. These attributes can include scalars, sets, or elements of a JSON
-- document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be
-- returned. If any of the requested attributes are not found, they will
-- not appear in the result.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'queryFilter', 'query_queryFilter' - This is a legacy parameter. Use @FilterExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.QueryFilter.html QueryFilter>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'returnConsumedCapacity', 'query_returnConsumedCapacity' - Undocumented member.
--
-- 'scanIndexForward', 'query_scanIndexForward' - Specifies the order for index traversal: If @true@ (default), the
-- traversal is performed in ascending order; if @false@, the traversal is
-- performed in descending order.
--
-- Items with the same partition key value are stored in sorted order by
-- sort key. If the sort key data type is Number, the results are stored in
-- numeric order. For type String, the results are stored in order of UTF-8
-- bytes. For type Binary, DynamoDB treats each byte of the binary data as
-- unsigned.
--
-- If @ScanIndexForward@ is @true@, DynamoDB returns the results in the
-- order in which they are stored (by sort key value). This is the default
-- behavior. If @ScanIndexForward@ is @false@, DynamoDB reads the results
-- in reverse order by sort key value, and then returns the results to the
-- client.
--
-- 'select', 'query_select' - The attributes to be returned in the result. You can retrieve all item
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
--     attributes that are projected into that index, the operation will
--     read only the index and not the table. If any of the requested
--     attributes are not projected into the local secondary index,
--     DynamoDB fetches each of these attributes from the parent table.
--     This extra fetching incurs additional throughput cost and latency.
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
-- 'tableName', 'query_tableName' - The name of the table containing the requested items.
newQuery ::
  -- | 'tableName'
  Prelude.Text ->
  Query
newQuery pTableName_ =
  Query'
    { attributesToGet = Prelude.Nothing,
      conditionalOperator = Prelude.Nothing,
      consistentRead = Prelude.Nothing,
      exclusiveStartKey = Prelude.Nothing,
      expressionAttributeNames = Prelude.Nothing,
      expressionAttributeValues = Prelude.Nothing,
      filterExpression = Prelude.Nothing,
      indexName = Prelude.Nothing,
      keyConditionExpression = Prelude.Nothing,
      keyConditions = Prelude.Nothing,
      limit = Prelude.Nothing,
      projectionExpression = Prelude.Nothing,
      queryFilter = Prelude.Nothing,
      returnConsumedCapacity = Prelude.Nothing,
      scanIndexForward = Prelude.Nothing,
      select = Prelude.Nothing,
      tableName = pTableName_
    }

-- | This is a legacy parameter. Use @ProjectionExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.AttributesToGet.html AttributesToGet>
-- in the /Amazon DynamoDB Developer Guide/.
query_attributesToGet :: Lens.Lens' Query (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
query_attributesToGet = Lens.lens (\Query' {attributesToGet} -> attributesToGet) (\s@Query' {} a -> s {attributesToGet = a} :: Query) Prelude.. Lens.mapping Lens.coerced

-- | This is a legacy parameter. Use @FilterExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.ConditionalOperator.html ConditionalOperator>
-- in the /Amazon DynamoDB Developer Guide/.
query_conditionalOperator :: Lens.Lens' Query (Prelude.Maybe ConditionalOperator)
query_conditionalOperator = Lens.lens (\Query' {conditionalOperator} -> conditionalOperator) (\s@Query' {} a -> s {conditionalOperator = a} :: Query)

-- | Determines the read consistency model: If set to @true@, then the
-- operation uses strongly consistent reads; otherwise, the operation uses
-- eventually consistent reads.
--
-- Strongly consistent reads are not supported on global secondary indexes.
-- If you query a global secondary index with @ConsistentRead@ set to
-- @true@, you will receive a @ValidationException@.
query_consistentRead :: Lens.Lens' Query (Prelude.Maybe Prelude.Bool)
query_consistentRead = Lens.lens (\Query' {consistentRead} -> consistentRead) (\s@Query' {} a -> s {consistentRead = a} :: Query)

-- | The primary key of the first item that this operation will evaluate. Use
-- the value that was returned for @LastEvaluatedKey@ in the previous
-- operation.
--
-- The data type for @ExclusiveStartKey@ must be String, Number, or Binary.
-- No set data types are allowed.
query_exclusiveStartKey :: Lens.Lens' Query (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
query_exclusiveStartKey = Lens.lens (\Query' {exclusiveStartKey} -> exclusiveStartKey) (\s@Query' {} a -> s {exclusiveStartKey = a} :: Query) Prelude.. Lens.mapping Lens.coerced

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
query_expressionAttributeNames :: Lens.Lens' Query (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
query_expressionAttributeNames = Lens.lens (\Query' {expressionAttributeNames} -> expressionAttributeNames) (\s@Query' {} a -> s {expressionAttributeNames = a} :: Query) Prelude.. Lens.mapping Lens.coerced

-- | One or more values that can be substituted in an expression.
--
-- Use the __:__ (colon) character in an expression to dereference an
-- attribute value. For example, suppose that you wanted to check whether
-- the value of the /ProductStatus/ attribute was one of the following:
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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.SpecifyingConditions.html Specifying Conditions>
-- in the /Amazon DynamoDB Developer Guide/.
query_expressionAttributeValues :: Lens.Lens' Query (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
query_expressionAttributeValues = Lens.lens (\Query' {expressionAttributeValues} -> expressionAttributeValues) (\s@Query' {} a -> s {expressionAttributeValues = a} :: Query) Prelude.. Lens.mapping Lens.coerced

-- | A string that contains conditions that DynamoDB applies after the
-- @Query@ operation, but before the data is returned to you. Items that do
-- not satisfy the @FilterExpression@ criteria are not returned.
--
-- A @FilterExpression@ does not allow key attributes. You cannot define a
-- filter expression based on a partition key or a sort key.
--
-- A @FilterExpression@ is applied after the items have already been read;
-- the process of filtering does not consume any additional read capacity
-- units.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Query.FilterExpression Filter Expressions>
-- in the /Amazon DynamoDB Developer Guide/.
query_filterExpression :: Lens.Lens' Query (Prelude.Maybe Prelude.Text)
query_filterExpression = Lens.lens (\Query' {filterExpression} -> filterExpression) (\s@Query' {} a -> s {filterExpression = a} :: Query)

-- | The name of an index to query. This index can be any local secondary
-- index or global secondary index on the table. Note that if you use the
-- @IndexName@ parameter, you must also provide @TableName.@
query_indexName :: Lens.Lens' Query (Prelude.Maybe Prelude.Text)
query_indexName = Lens.lens (\Query' {indexName} -> indexName) (\s@Query' {} a -> s {indexName = a} :: Query)

-- | The condition that specifies the key values for items to be retrieved by
-- the @Query@ action.
--
-- The condition must perform an equality test on a single partition key
-- value.
--
-- The condition can optionally perform one of several comparison tests on
-- a single sort key value. This allows @Query@ to retrieve one item with a
-- given partition key value and sort key value, or several items that have
-- the same partition key value but different sort key values.
--
-- The partition key equality test is required, and must be specified in
-- the following format:
--
-- @partitionKeyName@ /=/ @:partitionkeyval@
--
-- If you also want to provide a condition for the sort key, it must be
-- combined using @AND@ with the condition for the sort key. Following is
-- an example, using the __=__ comparison operator for the sort key:
--
-- @partitionKeyName@ @=@ @:partitionkeyval@ @AND@ @sortKeyName@ @=@
-- @:sortkeyval@
--
-- Valid comparisons for the sort key condition are as follows:
--
-- -   @sortKeyName@ @=@ @:sortkeyval@ - true if the sort key value is
--     equal to @:sortkeyval@.
--
-- -   @sortKeyName@ @\<@ @:sortkeyval@ - true if the sort key value is
--     less than @:sortkeyval@.
--
-- -   @sortKeyName@ @\<=@ @:sortkeyval@ - true if the sort key value is
--     less than or equal to @:sortkeyval@.
--
-- -   @sortKeyName@ @>@ @:sortkeyval@ - true if the sort key value is
--     greater than @:sortkeyval@.
--
-- -   @sortKeyName@ @>= @ @:sortkeyval@ - true if the sort key value is
--     greater than or equal to @:sortkeyval@.
--
-- -   @sortKeyName@ @BETWEEN@ @:sortkeyval1@ @AND@ @:sortkeyval2@ - true
--     if the sort key value is greater than or equal to @:sortkeyval1@,
--     and less than or equal to @:sortkeyval2@.
--
-- -   @begins_with (@ @sortKeyName@, @:sortkeyval@ @)@ - true if the sort
--     key value begins with a particular operand. (You cannot use this
--     function with a sort key that is of type Number.) Note that the
--     function name @begins_with@ is case-sensitive.
--
-- Use the @ExpressionAttributeValues@ parameter to replace tokens such as
-- @:partitionval@ and @:sortval@ with actual values at runtime.
--
-- You can optionally use the @ExpressionAttributeNames@ parameter to
-- replace the names of the partition key and sort key with placeholder
-- tokens. This option might be necessary if an attribute name conflicts
-- with a DynamoDB reserved word. For example, the following
-- @KeyConditionExpression@ parameter causes an error because /Size/ is a
-- reserved word:
--
-- -   @Size = :myval@
--
-- To work around this, define a placeholder (such a @#S@) to represent the
-- attribute name /Size/. @KeyConditionExpression@ then is as follows:
--
-- -   @#S = :myval@
--
-- For a list of reserved words, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ReservedWords.html Reserved Words>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- For more information on @ExpressionAttributeNames@ and
-- @ExpressionAttributeValues@, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ExpressionPlaceholders.html Using Placeholders for Attribute Names and Values>
-- in the /Amazon DynamoDB Developer Guide/.
query_keyConditionExpression :: Lens.Lens' Query (Prelude.Maybe Prelude.Text)
query_keyConditionExpression = Lens.lens (\Query' {keyConditionExpression} -> keyConditionExpression) (\s@Query' {} a -> s {keyConditionExpression = a} :: Query)

-- | This is a legacy parameter. Use @KeyConditionExpression@ instead. For
-- more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.KeyConditions.html KeyConditions>
-- in the /Amazon DynamoDB Developer Guide/.
query_keyConditions :: Lens.Lens' Query (Prelude.Maybe (Prelude.HashMap Prelude.Text Condition))
query_keyConditions = Lens.lens (\Query' {keyConditions} -> keyConditions) (\s@Query' {} a -> s {keyConditions = a} :: Query) Prelude.. Lens.mapping Lens.coerced

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
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html Query and Scan>
-- in the /Amazon DynamoDB Developer Guide/.
query_limit :: Lens.Lens' Query (Prelude.Maybe Prelude.Natural)
query_limit = Lens.lens (\Query' {limit} -> limit) (\s@Query' {} a -> s {limit = a} :: Query)

-- | A string that identifies one or more attributes to retrieve from the
-- table. These attributes can include scalars, sets, or elements of a JSON
-- document. The attributes in the expression must be separated by commas.
--
-- If no attribute names are specified, then all attributes will be
-- returned. If any of the requested attributes are not found, they will
-- not appear in the result.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Expressions.AccessingItemAttributes.html Accessing Item Attributes>
-- in the /Amazon DynamoDB Developer Guide/.
query_projectionExpression :: Lens.Lens' Query (Prelude.Maybe Prelude.Text)
query_projectionExpression = Lens.lens (\Query' {projectionExpression} -> projectionExpression) (\s@Query' {} a -> s {projectionExpression = a} :: Query)

-- | This is a legacy parameter. Use @FilterExpression@ instead. For more
-- information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.QueryFilter.html QueryFilter>
-- in the /Amazon DynamoDB Developer Guide/.
query_queryFilter :: Lens.Lens' Query (Prelude.Maybe (Prelude.HashMap Prelude.Text Condition))
query_queryFilter = Lens.lens (\Query' {queryFilter} -> queryFilter) (\s@Query' {} a -> s {queryFilter = a} :: Query) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
query_returnConsumedCapacity :: Lens.Lens' Query (Prelude.Maybe ReturnConsumedCapacity)
query_returnConsumedCapacity = Lens.lens (\Query' {returnConsumedCapacity} -> returnConsumedCapacity) (\s@Query' {} a -> s {returnConsumedCapacity = a} :: Query)

-- | Specifies the order for index traversal: If @true@ (default), the
-- traversal is performed in ascending order; if @false@, the traversal is
-- performed in descending order.
--
-- Items with the same partition key value are stored in sorted order by
-- sort key. If the sort key data type is Number, the results are stored in
-- numeric order. For type String, the results are stored in order of UTF-8
-- bytes. For type Binary, DynamoDB treats each byte of the binary data as
-- unsigned.
--
-- If @ScanIndexForward@ is @true@, DynamoDB returns the results in the
-- order in which they are stored (by sort key value). This is the default
-- behavior. If @ScanIndexForward@ is @false@, DynamoDB reads the results
-- in reverse order by sort key value, and then returns the results to the
-- client.
query_scanIndexForward :: Lens.Lens' Query (Prelude.Maybe Prelude.Bool)
query_scanIndexForward = Lens.lens (\Query' {scanIndexForward} -> scanIndexForward) (\s@Query' {} a -> s {scanIndexForward = a} :: Query)

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
--     attributes that are projected into that index, the operation will
--     read only the index and not the table. If any of the requested
--     attributes are not projected into the local secondary index,
--     DynamoDB fetches each of these attributes from the parent table.
--     This extra fetching incurs additional throughput cost and latency.
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
query_select :: Lens.Lens' Query (Prelude.Maybe Select)
query_select = Lens.lens (\Query' {select} -> select) (\s@Query' {} a -> s {select = a} :: Query)

-- | The name of the table containing the requested items.
query_tableName :: Lens.Lens' Query Prelude.Text
query_tableName = Lens.lens (\Query' {tableName} -> tableName) (\s@Query' {} a -> s {tableName = a} :: Query)

instance Core.AWSPager Query where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? queryResponse_lastEvaluatedKey
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& query_exclusiveStartKey
          Lens..~ rs
          Lens.^? queryResponse_lastEvaluatedKey
          Prelude.. Lens._Just

instance Core.AWSRequest Query where
  type AWSResponse Query = QueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryResponse'
            Prelude.<$> (x Data..?> "ConsumedCapacity")
            Prelude.<*> (x Data..?> "Count")
            Prelude.<*> ( x
                            Data..?> "LastEvaluatedKey"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ScannedCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable Query where
  hashWithSalt _salt Query' {..} =
    _salt
      `Prelude.hashWithSalt` attributesToGet
      `Prelude.hashWithSalt` conditionalOperator
      `Prelude.hashWithSalt` consistentRead
      `Prelude.hashWithSalt` exclusiveStartKey
      `Prelude.hashWithSalt` expressionAttributeNames
      `Prelude.hashWithSalt` expressionAttributeValues
      `Prelude.hashWithSalt` filterExpression
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` keyConditionExpression
      `Prelude.hashWithSalt` keyConditions
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` projectionExpression
      `Prelude.hashWithSalt` queryFilter
      `Prelude.hashWithSalt` returnConsumedCapacity
      `Prelude.hashWithSalt` scanIndexForward
      `Prelude.hashWithSalt` select
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData Query where
  rnf Query' {..} =
    Prelude.rnf attributesToGet
      `Prelude.seq` Prelude.rnf conditionalOperator
      `Prelude.seq` Prelude.rnf consistentRead
      `Prelude.seq` Prelude.rnf exclusiveStartKey
      `Prelude.seq` Prelude.rnf expressionAttributeNames
      `Prelude.seq` Prelude.rnf expressionAttributeValues
      `Prelude.seq` Prelude.rnf filterExpression
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf keyConditionExpression
      `Prelude.seq` Prelude.rnf keyConditions
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf projectionExpression
      `Prelude.seq` Prelude.rnf queryFilter
      `Prelude.seq` Prelude.rnf returnConsumedCapacity
      `Prelude.seq` Prelude.rnf scanIndexForward
      `Prelude.seq` Prelude.rnf select
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders Query where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("DynamoDB_20120810.Query" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Query where
  toJSON Query' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributesToGet" Data..=)
              Prelude.<$> attributesToGet,
            ("ConditionalOperator" Data..=)
              Prelude.<$> conditionalOperator,
            ("ConsistentRead" Data..=)
              Prelude.<$> consistentRead,
            ("ExclusiveStartKey" Data..=)
              Prelude.<$> exclusiveStartKey,
            ("ExpressionAttributeNames" Data..=)
              Prelude.<$> expressionAttributeNames,
            ("ExpressionAttributeValues" Data..=)
              Prelude.<$> expressionAttributeValues,
            ("FilterExpression" Data..=)
              Prelude.<$> filterExpression,
            ("IndexName" Data..=) Prelude.<$> indexName,
            ("KeyConditionExpression" Data..=)
              Prelude.<$> keyConditionExpression,
            ("KeyConditions" Data..=) Prelude.<$> keyConditions,
            ("Limit" Data..=) Prelude.<$> limit,
            ("ProjectionExpression" Data..=)
              Prelude.<$> projectionExpression,
            ("QueryFilter" Data..=) Prelude.<$> queryFilter,
            ("ReturnConsumedCapacity" Data..=)
              Prelude.<$> returnConsumedCapacity,
            ("ScanIndexForward" Data..=)
              Prelude.<$> scanIndexForward,
            ("Select" Data..=) Prelude.<$> select,
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath Query where
  toPath = Prelude.const "/"

instance Data.ToQuery Query where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @Query@ operation.
--
-- /See:/ 'newQueryResponse' smart constructor.
data QueryResponse = QueryResponse'
  { -- | The capacity units consumed by the @Query@ operation. The data returned
    -- includes the total provisioned throughput consumed, along with
    -- statistics for the table and any indexes involved in the operation.
    -- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
    -- parameter was specified. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
    -- in the /Amazon DynamoDB Developer Guide/.
    consumedCapacity :: Prelude.Maybe ConsumedCapacity,
    -- | The number of items in the response.
    --
    -- If you used a @QueryFilter@ in the request, then @Count@ is the number
    -- of items returned after the filter was applied, and @ScannedCount@ is
    -- the number of matching items before the filter was applied.
    --
    -- If you did not use a filter in the request, then @Count@ and
    -- @ScannedCount@ are the same.
    count :: Prelude.Maybe Prelude.Int,
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
    -- | The number of items evaluated, before any @QueryFilter@ is applied. A
    -- high @ScannedCount@ value with few, or no, @Count@ results indicates an
    -- inefficient @Query@ operation. For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount>
    -- in the /Amazon DynamoDB Developer Guide/.
    --
    -- If you did not use a filter in the request, then @ScannedCount@ is the
    -- same as @Count@.
    scannedCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of item attributes that match the query criteria. Each element
    -- in this array consists of an attribute name and the value for that
    -- attribute.
    items :: [Prelude.HashMap Prelude.Text AttributeValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumedCapacity', 'queryResponse_consumedCapacity' - The capacity units consumed by the @Query@ operation. The data returned
-- includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'count', 'queryResponse_count' - The number of items in the response.
--
-- If you used a @QueryFilter@ in the request, then @Count@ is the number
-- of items returned after the filter was applied, and @ScannedCount@ is
-- the number of matching items before the filter was applied.
--
-- If you did not use a filter in the request, then @Count@ and
-- @ScannedCount@ are the same.
--
-- 'lastEvaluatedKey', 'queryResponse_lastEvaluatedKey' - The primary key of the item where the operation stopped, inclusive of
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
-- 'scannedCount', 'queryResponse_scannedCount' - The number of items evaluated, before any @QueryFilter@ is applied. A
-- high @ScannedCount@ value with few, or no, @Count@ results indicates an
-- inefficient @Query@ operation. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If you did not use a filter in the request, then @ScannedCount@ is the
-- same as @Count@.
--
-- 'httpStatus', 'queryResponse_httpStatus' - The response's http status code.
--
-- 'items', 'queryResponse_items' - An array of item attributes that match the query criteria. Each element
-- in this array consists of an attribute name and the value for that
-- attribute.
newQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  QueryResponse
newQueryResponse pHttpStatus_ =
  QueryResponse'
    { consumedCapacity = Prelude.Nothing,
      count = Prelude.Nothing,
      lastEvaluatedKey = Prelude.Nothing,
      scannedCount = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | The capacity units consumed by the @Query@ operation. The data returned
-- includes the total provisioned throughput consumed, along with
-- statistics for the table and any indexes involved in the operation.
-- @ConsumedCapacity@ is only returned if the @ReturnConsumedCapacity@
-- parameter was specified. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ProvisionedThroughputIntro.html Provisioned Throughput>
-- in the /Amazon DynamoDB Developer Guide/.
queryResponse_consumedCapacity :: Lens.Lens' QueryResponse (Prelude.Maybe ConsumedCapacity)
queryResponse_consumedCapacity = Lens.lens (\QueryResponse' {consumedCapacity} -> consumedCapacity) (\s@QueryResponse' {} a -> s {consumedCapacity = a} :: QueryResponse)

-- | The number of items in the response.
--
-- If you used a @QueryFilter@ in the request, then @Count@ is the number
-- of items returned after the filter was applied, and @ScannedCount@ is
-- the number of matching items before the filter was applied.
--
-- If you did not use a filter in the request, then @Count@ and
-- @ScannedCount@ are the same.
queryResponse_count :: Lens.Lens' QueryResponse (Prelude.Maybe Prelude.Int)
queryResponse_count = Lens.lens (\QueryResponse' {count} -> count) (\s@QueryResponse' {} a -> s {count = a} :: QueryResponse)

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
queryResponse_lastEvaluatedKey :: Lens.Lens' QueryResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
queryResponse_lastEvaluatedKey = Lens.lens (\QueryResponse' {lastEvaluatedKey} -> lastEvaluatedKey) (\s@QueryResponse' {} a -> s {lastEvaluatedKey = a} :: QueryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of items evaluated, before any @QueryFilter@ is applied. A
-- high @ScannedCount@ value with few, or no, @Count@ results indicates an
-- inefficient @Query@ operation. For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/QueryAndScan.html#Count Count and ScannedCount>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- If you did not use a filter in the request, then @ScannedCount@ is the
-- same as @Count@.
queryResponse_scannedCount :: Lens.Lens' QueryResponse (Prelude.Maybe Prelude.Int)
queryResponse_scannedCount = Lens.lens (\QueryResponse' {scannedCount} -> scannedCount) (\s@QueryResponse' {} a -> s {scannedCount = a} :: QueryResponse)

-- | The response's http status code.
queryResponse_httpStatus :: Lens.Lens' QueryResponse Prelude.Int
queryResponse_httpStatus = Lens.lens (\QueryResponse' {httpStatus} -> httpStatus) (\s@QueryResponse' {} a -> s {httpStatus = a} :: QueryResponse)

-- | An array of item attributes that match the query criteria. Each element
-- in this array consists of an attribute name and the value for that
-- attribute.
queryResponse_items :: Lens.Lens' QueryResponse [Prelude.HashMap Prelude.Text AttributeValue]
queryResponse_items = Lens.lens (\QueryResponse' {items} -> items) (\s@QueryResponse' {} a -> s {items = a} :: QueryResponse) Prelude.. Lens.coerced

instance Prelude.NFData QueryResponse where
  rnf QueryResponse' {..} =
    Prelude.rnf consumedCapacity
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf lastEvaluatedKey
      `Prelude.seq` Prelude.rnf scannedCount
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
