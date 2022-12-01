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
-- Module      : Amazonka.Glue.GetPartitions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the partitions in a table.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetPartitions
  ( -- * Creating a Request
    GetPartitions (..),
    newGetPartitions,

    -- * Request Lenses
    getPartitions_nextToken,
    getPartitions_queryAsOfTime,
    getPartitions_expression,
    getPartitions_segment,
    getPartitions_maxResults,
    getPartitions_catalogId,
    getPartitions_transactionId,
    getPartitions_excludeColumnSchema,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPartitions' smart constructor.
data GetPartitions = GetPartitions'
  { -- | A continuation token, if this is not the first call to retrieve these
    -- partitions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time as of when to read the partition contents. If not set, the most
    -- recent transaction commit time will be used. Cannot be specified along
    -- with @TransactionId@.
    queryAsOfTime :: Prelude.Maybe Core.POSIX,
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
    -- If an type is encountered that is not valid, an exception is thrown.
    --
    -- The following list shows the valid operators on each type. When you
    -- define a crawler, the @partitionKey@ type is created as a @STRING@, to
    -- be compatible with the catalog partitions.
    --
    -- /Sample API Call/:
    expression :: Prelude.Maybe Prelude.Text,
    -- | The segment of the table\'s partitions to scan in this request.
    segment :: Prelude.Maybe Segment,
    -- | The maximum number of partitions to return in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The transaction ID at which to read the partition contents.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | When true, specifies not returning the partition column schema. Useful
    -- when you are interested only in other partition attributes such as
    -- partition values or location. This approach avoids the problem of a
    -- large response by not returning duplicate data.
    excludeColumnSchema :: Prelude.Maybe Prelude.Bool,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Prelude.Text,
    -- | The name of the partitions\' table.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'queryAsOfTime', 'getPartitions_queryAsOfTime' - The time as of when to read the partition contents. If not set, the most
-- recent transaction commit time will be used. Cannot be specified along
-- with @TransactionId@.
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
-- If an type is encountered that is not valid, an exception is thrown.
--
-- The following list shows the valid operators on each type. When you
-- define a crawler, the @partitionKey@ type is created as a @STRING@, to
-- be compatible with the catalog partitions.
--
-- /Sample API Call/:
--
-- 'segment', 'getPartitions_segment' - The segment of the table\'s partitions to scan in this request.
--
-- 'maxResults', 'getPartitions_maxResults' - The maximum number of partitions to return in a single response.
--
-- 'catalogId', 'getPartitions_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is provided, the Amazon Web Services account ID is used by default.
--
-- 'transactionId', 'getPartitions_transactionId' - The transaction ID at which to read the partition contents.
--
-- 'excludeColumnSchema', 'getPartitions_excludeColumnSchema' - When true, specifies not returning the partition column schema. Useful
-- when you are interested only in other partition attributes such as
-- partition values or location. This approach avoids the problem of a
-- large response by not returning duplicate data.
--
-- 'databaseName', 'getPartitions_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'getPartitions_tableName' - The name of the partitions\' table.
newGetPartitions ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GetPartitions
newGetPartitions pDatabaseName_ pTableName_ =
  GetPartitions'
    { nextToken = Prelude.Nothing,
      queryAsOfTime = Prelude.Nothing,
      expression = Prelude.Nothing,
      segment = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      transactionId = Prelude.Nothing,
      excludeColumnSchema = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | A continuation token, if this is not the first call to retrieve these
-- partitions.
getPartitions_nextToken :: Lens.Lens' GetPartitions (Prelude.Maybe Prelude.Text)
getPartitions_nextToken = Lens.lens (\GetPartitions' {nextToken} -> nextToken) (\s@GetPartitions' {} a -> s {nextToken = a} :: GetPartitions)

-- | The time as of when to read the partition contents. If not set, the most
-- recent transaction commit time will be used. Cannot be specified along
-- with @TransactionId@.
getPartitions_queryAsOfTime :: Lens.Lens' GetPartitions (Prelude.Maybe Prelude.UTCTime)
getPartitions_queryAsOfTime = Lens.lens (\GetPartitions' {queryAsOfTime} -> queryAsOfTime) (\s@GetPartitions' {} a -> s {queryAsOfTime = a} :: GetPartitions) Prelude.. Lens.mapping Core._Time

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
-- If an type is encountered that is not valid, an exception is thrown.
--
-- The following list shows the valid operators on each type. When you
-- define a crawler, the @partitionKey@ type is created as a @STRING@, to
-- be compatible with the catalog partitions.
--
-- /Sample API Call/:
getPartitions_expression :: Lens.Lens' GetPartitions (Prelude.Maybe Prelude.Text)
getPartitions_expression = Lens.lens (\GetPartitions' {expression} -> expression) (\s@GetPartitions' {} a -> s {expression = a} :: GetPartitions)

-- | The segment of the table\'s partitions to scan in this request.
getPartitions_segment :: Lens.Lens' GetPartitions (Prelude.Maybe Segment)
getPartitions_segment = Lens.lens (\GetPartitions' {segment} -> segment) (\s@GetPartitions' {} a -> s {segment = a} :: GetPartitions)

-- | The maximum number of partitions to return in a single response.
getPartitions_maxResults :: Lens.Lens' GetPartitions (Prelude.Maybe Prelude.Natural)
getPartitions_maxResults = Lens.lens (\GetPartitions' {maxResults} -> maxResults) (\s@GetPartitions' {} a -> s {maxResults = a} :: GetPartitions)

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is provided, the Amazon Web Services account ID is used by default.
getPartitions_catalogId :: Lens.Lens' GetPartitions (Prelude.Maybe Prelude.Text)
getPartitions_catalogId = Lens.lens (\GetPartitions' {catalogId} -> catalogId) (\s@GetPartitions' {} a -> s {catalogId = a} :: GetPartitions)

-- | The transaction ID at which to read the partition contents.
getPartitions_transactionId :: Lens.Lens' GetPartitions (Prelude.Maybe Prelude.Text)
getPartitions_transactionId = Lens.lens (\GetPartitions' {transactionId} -> transactionId) (\s@GetPartitions' {} a -> s {transactionId = a} :: GetPartitions)

-- | When true, specifies not returning the partition column schema. Useful
-- when you are interested only in other partition attributes such as
-- partition values or location. This approach avoids the problem of a
-- large response by not returning duplicate data.
getPartitions_excludeColumnSchema :: Lens.Lens' GetPartitions (Prelude.Maybe Prelude.Bool)
getPartitions_excludeColumnSchema = Lens.lens (\GetPartitions' {excludeColumnSchema} -> excludeColumnSchema) (\s@GetPartitions' {} a -> s {excludeColumnSchema = a} :: GetPartitions)

-- | The name of the catalog database where the partitions reside.
getPartitions_databaseName :: Lens.Lens' GetPartitions Prelude.Text
getPartitions_databaseName = Lens.lens (\GetPartitions' {databaseName} -> databaseName) (\s@GetPartitions' {} a -> s {databaseName = a} :: GetPartitions)

-- | The name of the partitions\' table.
getPartitions_tableName :: Lens.Lens' GetPartitions Prelude.Text
getPartitions_tableName = Lens.lens (\GetPartitions' {tableName} -> tableName) (\s@GetPartitions' {} a -> s {tableName = a} :: GetPartitions)

instance Core.AWSPager GetPartitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getPartitionsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getPartitionsResponse_partitions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getPartitions_nextToken
          Lens..~ rs
          Lens.^? getPartitionsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetPartitions where
  type
    AWSResponse GetPartitions =
      GetPartitionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPartitionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Partitions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPartitions where
  hashWithSalt _salt GetPartitions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` queryAsOfTime
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` segment
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` excludeColumnSchema
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData GetPartitions where
  rnf GetPartitions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queryAsOfTime
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf excludeColumnSchema
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName

instance Core.ToHeaders GetPartitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetPartitions" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPartitions where
  toJSON GetPartitions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("QueryAsOfTime" Core..=) Prelude.<$> queryAsOfTime,
            ("Expression" Core..=) Prelude.<$> expression,
            ("Segment" Core..=) Prelude.<$> segment,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CatalogId" Core..=) Prelude.<$> catalogId,
            ("TransactionId" Core..=) Prelude.<$> transactionId,
            ("ExcludeColumnSchema" Core..=)
              Prelude.<$> excludeColumnSchema,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.ToPath GetPartitions where
  toPath = Prelude.const "/"

instance Core.ToQuery GetPartitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPartitionsResponse' smart constructor.
data GetPartitionsResponse = GetPartitionsResponse'
  { -- | A continuation token, if the returned list of partitions does not
    -- include the last one.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of requested partitions.
    partitions :: Prelude.Maybe [Partition],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetPartitionsResponse
newGetPartitionsResponse pHttpStatus_ =
  GetPartitionsResponse'
    { nextToken = Prelude.Nothing,
      partitions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the returned list of partitions does not
-- include the last one.
getPartitionsResponse_nextToken :: Lens.Lens' GetPartitionsResponse (Prelude.Maybe Prelude.Text)
getPartitionsResponse_nextToken = Lens.lens (\GetPartitionsResponse' {nextToken} -> nextToken) (\s@GetPartitionsResponse' {} a -> s {nextToken = a} :: GetPartitionsResponse)

-- | A list of requested partitions.
getPartitionsResponse_partitions :: Lens.Lens' GetPartitionsResponse (Prelude.Maybe [Partition])
getPartitionsResponse_partitions = Lens.lens (\GetPartitionsResponse' {partitions} -> partitions) (\s@GetPartitionsResponse' {} a -> s {partitions = a} :: GetPartitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getPartitionsResponse_httpStatus :: Lens.Lens' GetPartitionsResponse Prelude.Int
getPartitionsResponse_httpStatus = Lens.lens (\GetPartitionsResponse' {httpStatus} -> httpStatus) (\s@GetPartitionsResponse' {} a -> s {httpStatus = a} :: GetPartitionsResponse)

instance Prelude.NFData GetPartitionsResponse where
  rnf GetPartitionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf partitions
      `Prelude.seq` Prelude.rnf httpStatus
