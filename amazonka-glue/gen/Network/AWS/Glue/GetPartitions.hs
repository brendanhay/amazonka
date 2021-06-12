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
-- Module      : Network.AWS.Glue.GetPartitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the partitions in a table.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetPartitions
  ( -- * Creating a Request
    GetPartitions (..),
    newGetPartitions,

    -- * Request Lenses
    getPartitions_nextToken,
    getPartitions_catalogId,
    getPartitions_maxResults,
    getPartitions_segment,
    getPartitions_excludeColumnSchema,
    getPartitions_expression,
    getPartitions_databaseName,
    getPartitions_tableName,

    -- * Destructuring the Response
    GetPartitionsResponse (..),
    newGetPartitionsResponse,

    -- * Response Lenses
    getPartitionsResponse_nextToken,
    getPartitionsResponse_partitions,
    getPartitionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPartitions' smart constructor.
data GetPartitions = GetPartitions'
  { -- | A continuation token, if this is not the first call to retrieve these
    -- partitions.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The maximum number of partitions to return in a single response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The segment of the table\'s partitions to scan in this request.
    segment :: Core.Maybe Segment,
    excludeColumnSchema :: Core.Maybe Core.Bool,
    -- | An expression that filters the partitions to be returned.
    --
    -- The expression uses SQL syntax similar to the SQL @WHERE@ filter clause.
    -- The SQL statement parser
    -- <http://jsqlparser.sourceforge.net/home.php JSQLParser> parses the
    -- expression.
    --
    -- /Operators/: The following are the operators that you can use in the
    -- @Expression@ API call:
    --
    -- [=]
    --     Checks whether the values of the two operands are equal; if yes,
    --     then the condition becomes true.
    --
    --     Example: Assume \'variable a\' holds 10 and \'variable b\' holds 20.
    --
    --     (a = b) is not true.
    --
    -- [\< >]
    --     Checks whether the values of two operands are equal; if the values
    --     are not equal, then the condition becomes true.
    --
    --     Example: (a \< > b) is true.
    --
    -- [>]
    --     Checks whether the value of the left operand is greater than the
    --     value of the right operand; if yes, then the condition becomes true.
    --
    --     Example: (a > b) is not true.
    --
    -- [\<]
    --     Checks whether the value of the left operand is less than the value
    --     of the right operand; if yes, then the condition becomes true.
    --
    --     Example: (a \< b) is true.
    --
    -- [>=]
    --     Checks whether the value of the left operand is greater than or
    --     equal to the value of the right operand; if yes, then the condition
    --     becomes true.
    --
    --     Example: (a >= b) is not true.
    --
    -- [\<=]
    --     Checks whether the value of the left operand is less than or equal
    --     to the value of the right operand; if yes, then the condition
    --     becomes true.
    --
    --     Example: (a \<= b) is true.
    --
    -- [AND, OR, IN, BETWEEN, LIKE, NOT, IS NULL]
    --     Logical operators.
    --
    -- /Supported Partition Key Types/: The following are the supported
    -- partition keys.
    --
    -- -   @string@
    --
    -- -   @date@
    --
    -- -   @timestamp@
    --
    -- -   @int@
    --
    -- -   @bigint@
    --
    -- -   @long@
    --
    -- -   @tinyint@
    --
    -- -   @smallint@
    --
    -- -   @decimal@
    --
    -- If an invalid type is encountered, an exception is thrown.
    --
    -- The following list shows the valid operators on each type. When you
    -- define a crawler, the @partitionKey@ type is created as a @STRING@, to
    -- be compatible with the catalog partitions.
    --
    -- /Sample API Call/:
    expression :: Core.Maybe Core.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Core.Text,
    -- | The name of the partitions\' table.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPartitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getPartitions_nextToken' - A continuation token, if this is not the first call to retrieve these
-- partitions.
--
-- 'catalogId', 'getPartitions_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is provided, the AWS account ID is used by default.
--
-- 'maxResults', 'getPartitions_maxResults' - The maximum number of partitions to return in a single response.
--
-- 'segment', 'getPartitions_segment' - The segment of the table\'s partitions to scan in this request.
--
-- 'excludeColumnSchema', 'getPartitions_excludeColumnSchema' - Undocumented member.
--
-- 'expression', 'getPartitions_expression' - An expression that filters the partitions to be returned.
--
-- The expression uses SQL syntax similar to the SQL @WHERE@ filter clause.
-- The SQL statement parser
-- <http://jsqlparser.sourceforge.net/home.php JSQLParser> parses the
-- expression.
--
-- /Operators/: The following are the operators that you can use in the
-- @Expression@ API call:
--
-- [=]
--     Checks whether the values of the two operands are equal; if yes,
--     then the condition becomes true.
--
--     Example: Assume \'variable a\' holds 10 and \'variable b\' holds 20.
--
--     (a = b) is not true.
--
-- [\< >]
--     Checks whether the values of two operands are equal; if the values
--     are not equal, then the condition becomes true.
--
--     Example: (a \< > b) is true.
--
-- [>]
--     Checks whether the value of the left operand is greater than the
--     value of the right operand; if yes, then the condition becomes true.
--
--     Example: (a > b) is not true.
--
-- [\<]
--     Checks whether the value of the left operand is less than the value
--     of the right operand; if yes, then the condition becomes true.
--
--     Example: (a \< b) is true.
--
-- [>=]
--     Checks whether the value of the left operand is greater than or
--     equal to the value of the right operand; if yes, then the condition
--     becomes true.
--
--     Example: (a >= b) is not true.
--
-- [\<=]
--     Checks whether the value of the left operand is less than or equal
--     to the value of the right operand; if yes, then the condition
--     becomes true.
--
--     Example: (a \<= b) is true.
--
-- [AND, OR, IN, BETWEEN, LIKE, NOT, IS NULL]
--     Logical operators.
--
-- /Supported Partition Key Types/: The following are the supported
-- partition keys.
--
-- -   @string@
--
-- -   @date@
--
-- -   @timestamp@
--
-- -   @int@
--
-- -   @bigint@
--
-- -   @long@
--
-- -   @tinyint@
--
-- -   @smallint@
--
-- -   @decimal@
--
-- If an invalid type is encountered, an exception is thrown.
--
-- The following list shows the valid operators on each type. When you
-- define a crawler, the @partitionKey@ type is created as a @STRING@, to
-- be compatible with the catalog partitions.
--
-- /Sample API Call/:
--
-- 'databaseName', 'getPartitions_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'getPartitions_tableName' - The name of the partitions\' table.
newGetPartitions ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  GetPartitions
newGetPartitions pDatabaseName_ pTableName_ =
  GetPartitions'
    { nextToken = Core.Nothing,
      catalogId = Core.Nothing,
      maxResults = Core.Nothing,
      segment = Core.Nothing,
      excludeColumnSchema = Core.Nothing,
      expression = Core.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | A continuation token, if this is not the first call to retrieve these
-- partitions.
getPartitions_nextToken :: Lens.Lens' GetPartitions (Core.Maybe Core.Text)
getPartitions_nextToken = Lens.lens (\GetPartitions' {nextToken} -> nextToken) (\s@GetPartitions' {} a -> s {nextToken = a} :: GetPartitions)

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is provided, the AWS account ID is used by default.
getPartitions_catalogId :: Lens.Lens' GetPartitions (Core.Maybe Core.Text)
getPartitions_catalogId = Lens.lens (\GetPartitions' {catalogId} -> catalogId) (\s@GetPartitions' {} a -> s {catalogId = a} :: GetPartitions)

-- | The maximum number of partitions to return in a single response.
getPartitions_maxResults :: Lens.Lens' GetPartitions (Core.Maybe Core.Natural)
getPartitions_maxResults = Lens.lens (\GetPartitions' {maxResults} -> maxResults) (\s@GetPartitions' {} a -> s {maxResults = a} :: GetPartitions)

-- | The segment of the table\'s partitions to scan in this request.
getPartitions_segment :: Lens.Lens' GetPartitions (Core.Maybe Segment)
getPartitions_segment = Lens.lens (\GetPartitions' {segment} -> segment) (\s@GetPartitions' {} a -> s {segment = a} :: GetPartitions)

-- | Undocumented member.
getPartitions_excludeColumnSchema :: Lens.Lens' GetPartitions (Core.Maybe Core.Bool)
getPartitions_excludeColumnSchema = Lens.lens (\GetPartitions' {excludeColumnSchema} -> excludeColumnSchema) (\s@GetPartitions' {} a -> s {excludeColumnSchema = a} :: GetPartitions)

-- | An expression that filters the partitions to be returned.
--
-- The expression uses SQL syntax similar to the SQL @WHERE@ filter clause.
-- The SQL statement parser
-- <http://jsqlparser.sourceforge.net/home.php JSQLParser> parses the
-- expression.
--
-- /Operators/: The following are the operators that you can use in the
-- @Expression@ API call:
--
-- [=]
--     Checks whether the values of the two operands are equal; if yes,
--     then the condition becomes true.
--
--     Example: Assume \'variable a\' holds 10 and \'variable b\' holds 20.
--
--     (a = b) is not true.
--
-- [\< >]
--     Checks whether the values of two operands are equal; if the values
--     are not equal, then the condition becomes true.
--
--     Example: (a \< > b) is true.
--
-- [>]
--     Checks whether the value of the left operand is greater than the
--     value of the right operand; if yes, then the condition becomes true.
--
--     Example: (a > b) is not true.
--
-- [\<]
--     Checks whether the value of the left operand is less than the value
--     of the right operand; if yes, then the condition becomes true.
--
--     Example: (a \< b) is true.
--
-- [>=]
--     Checks whether the value of the left operand is greater than or
--     equal to the value of the right operand; if yes, then the condition
--     becomes true.
--
--     Example: (a >= b) is not true.
--
-- [\<=]
--     Checks whether the value of the left operand is less than or equal
--     to the value of the right operand; if yes, then the condition
--     becomes true.
--
--     Example: (a \<= b) is true.
--
-- [AND, OR, IN, BETWEEN, LIKE, NOT, IS NULL]
--     Logical operators.
--
-- /Supported Partition Key Types/: The following are the supported
-- partition keys.
--
-- -   @string@
--
-- -   @date@
--
-- -   @timestamp@
--
-- -   @int@
--
-- -   @bigint@
--
-- -   @long@
--
-- -   @tinyint@
--
-- -   @smallint@
--
-- -   @decimal@
--
-- If an invalid type is encountered, an exception is thrown.
--
-- The following list shows the valid operators on each type. When you
-- define a crawler, the @partitionKey@ type is created as a @STRING@, to
-- be compatible with the catalog partitions.
--
-- /Sample API Call/:
getPartitions_expression :: Lens.Lens' GetPartitions (Core.Maybe Core.Text)
getPartitions_expression = Lens.lens (\GetPartitions' {expression} -> expression) (\s@GetPartitions' {} a -> s {expression = a} :: GetPartitions)

-- | The name of the catalog database where the partitions reside.
getPartitions_databaseName :: Lens.Lens' GetPartitions Core.Text
getPartitions_databaseName = Lens.lens (\GetPartitions' {databaseName} -> databaseName) (\s@GetPartitions' {} a -> s {databaseName = a} :: GetPartitions)

-- | The name of the partitions\' table.
getPartitions_tableName :: Lens.Lens' GetPartitions Core.Text
getPartitions_tableName = Lens.lens (\GetPartitions' {tableName} -> tableName) (\s@GetPartitions' {} a -> s {tableName = a} :: GetPartitions)

instance Core.AWSPager GetPartitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getPartitionsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getPartitionsResponse_partitions Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getPartitions_nextToken
          Lens..~ rs
          Lens.^? getPartitionsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetPartitions where
  type
    AWSResponse GetPartitions =
      GetPartitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPartitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Partitions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPartitions

instance Core.NFData GetPartitions

instance Core.ToHeaders GetPartitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetPartitions" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetPartitions where
  toJSON GetPartitions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Segment" Core..=) Core.<$> segment,
            ("ExcludeColumnSchema" Core..=)
              Core.<$> excludeColumnSchema,
            ("Expression" Core..=) Core.<$> expression,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.ToPath GetPartitions where
  toPath = Core.const "/"

instance Core.ToQuery GetPartitions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPartitionsResponse' smart constructor.
data GetPartitionsResponse = GetPartitionsResponse'
  { -- | A continuation token, if the returned list of partitions does not
    -- include the last one.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of requested partitions.
    partitions :: Core.Maybe [Partition],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPartitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getPartitionsResponse_nextToken' - A continuation token, if the returned list of partitions does not
-- include the last one.
--
-- 'partitions', 'getPartitionsResponse_partitions' - A list of requested partitions.
--
-- 'httpStatus', 'getPartitionsResponse_httpStatus' - The response's http status code.
newGetPartitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPartitionsResponse
newGetPartitionsResponse pHttpStatus_ =
  GetPartitionsResponse'
    { nextToken = Core.Nothing,
      partitions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the returned list of partitions does not
-- include the last one.
getPartitionsResponse_nextToken :: Lens.Lens' GetPartitionsResponse (Core.Maybe Core.Text)
getPartitionsResponse_nextToken = Lens.lens (\GetPartitionsResponse' {nextToken} -> nextToken) (\s@GetPartitionsResponse' {} a -> s {nextToken = a} :: GetPartitionsResponse)

-- | A list of requested partitions.
getPartitionsResponse_partitions :: Lens.Lens' GetPartitionsResponse (Core.Maybe [Partition])
getPartitionsResponse_partitions = Lens.lens (\GetPartitionsResponse' {partitions} -> partitions) (\s@GetPartitionsResponse' {} a -> s {partitions = a} :: GetPartitionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getPartitionsResponse_httpStatus :: Lens.Lens' GetPartitionsResponse Core.Int
getPartitionsResponse_httpStatus = Lens.lens (\GetPartitionsResponse' {httpStatus} -> httpStatus) (\s@GetPartitionsResponse' {} a -> s {httpStatus = a} :: GetPartitionsResponse)

instance Core.NFData GetPartitionsResponse
