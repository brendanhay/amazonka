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
-- Module      : Amazonka.Glue.GetUnfilteredPartitionsMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves partition metadata from the Data Catalog that contains
-- unfiltered metadata.
--
-- For IAM authorization, the public IAM action associated with this API is
-- @glue:GetPartitions@.
module Amazonka.Glue.GetUnfilteredPartitionsMetadata
  ( -- * Creating a Request
    GetUnfilteredPartitionsMetadata (..),
    newGetUnfilteredPartitionsMetadata,

    -- * Request Lenses
    getUnfilteredPartitionsMetadata_auditContext,
    getUnfilteredPartitionsMetadata_expression,
    getUnfilteredPartitionsMetadata_maxResults,
    getUnfilteredPartitionsMetadata_nextToken,
    getUnfilteredPartitionsMetadata_segment,
    getUnfilteredPartitionsMetadata_catalogId,
    getUnfilteredPartitionsMetadata_databaseName,
    getUnfilteredPartitionsMetadata_tableName,
    getUnfilteredPartitionsMetadata_supportedPermissionTypes,

    -- * Destructuring the Response
    GetUnfilteredPartitionsMetadataResponse (..),
    newGetUnfilteredPartitionsMetadataResponse,

    -- * Response Lenses
    getUnfilteredPartitionsMetadataResponse_nextToken,
    getUnfilteredPartitionsMetadataResponse_unfilteredPartitions,
    getUnfilteredPartitionsMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUnfilteredPartitionsMetadata' smart constructor.
data GetUnfilteredPartitionsMetadata = GetUnfilteredPartitionsMetadata'
  { -- | A structure containing Lake Formation audit context information.
    auditContext :: Prelude.Maybe AuditContext,
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
    expression :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of partitions to return in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is not the first call to retrieve these
    -- partitions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The segment of the table\'s partitions to scan in this request.
    segment :: Prelude.Maybe Segment,
    -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is provided, the AWS account ID is used by default.
    catalogId :: Prelude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Prelude.Text,
    -- | The name of the table that contains the partition.
    tableName :: Prelude.Text,
    -- | A list of supported permission types.
    supportedPermissionTypes :: Prelude.NonEmpty PermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUnfilteredPartitionsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditContext', 'getUnfilteredPartitionsMetadata_auditContext' - A structure containing Lake Formation audit context information.
--
-- 'expression', 'getUnfilteredPartitionsMetadata_expression' - An expression that filters the partitions to be returned.
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
-- 'maxResults', 'getUnfilteredPartitionsMetadata_maxResults' - The maximum number of partitions to return in a single response.
--
-- 'nextToken', 'getUnfilteredPartitionsMetadata_nextToken' - A continuation token, if this is not the first call to retrieve these
-- partitions.
--
-- 'segment', 'getUnfilteredPartitionsMetadata_segment' - The segment of the table\'s partitions to scan in this request.
--
-- 'catalogId', 'getUnfilteredPartitionsMetadata_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is provided, the AWS account ID is used by default.
--
-- 'databaseName', 'getUnfilteredPartitionsMetadata_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'getUnfilteredPartitionsMetadata_tableName' - The name of the table that contains the partition.
--
-- 'supportedPermissionTypes', 'getUnfilteredPartitionsMetadata_supportedPermissionTypes' - A list of supported permission types.
newGetUnfilteredPartitionsMetadata ::
  -- | 'catalogId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'supportedPermissionTypes'
  Prelude.NonEmpty PermissionType ->
  GetUnfilteredPartitionsMetadata
newGetUnfilteredPartitionsMetadata
  pCatalogId_
  pDatabaseName_
  pTableName_
  pSupportedPermissionTypes_ =
    GetUnfilteredPartitionsMetadata'
      { auditContext =
          Prelude.Nothing,
        expression = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        segment = Prelude.Nothing,
        catalogId = pCatalogId_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        supportedPermissionTypes =
          Lens.coerced
            Lens.# pSupportedPermissionTypes_
      }

-- | A structure containing Lake Formation audit context information.
getUnfilteredPartitionsMetadata_auditContext :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.Maybe AuditContext)
getUnfilteredPartitionsMetadata_auditContext = Lens.lens (\GetUnfilteredPartitionsMetadata' {auditContext} -> auditContext) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {auditContext = a} :: GetUnfilteredPartitionsMetadata)

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
getUnfilteredPartitionsMetadata_expression :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.Maybe Prelude.Text)
getUnfilteredPartitionsMetadata_expression = Lens.lens (\GetUnfilteredPartitionsMetadata' {expression} -> expression) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {expression = a} :: GetUnfilteredPartitionsMetadata)

-- | The maximum number of partitions to return in a single response.
getUnfilteredPartitionsMetadata_maxResults :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.Maybe Prelude.Natural)
getUnfilteredPartitionsMetadata_maxResults = Lens.lens (\GetUnfilteredPartitionsMetadata' {maxResults} -> maxResults) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {maxResults = a} :: GetUnfilteredPartitionsMetadata)

-- | A continuation token, if this is not the first call to retrieve these
-- partitions.
getUnfilteredPartitionsMetadata_nextToken :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.Maybe Prelude.Text)
getUnfilteredPartitionsMetadata_nextToken = Lens.lens (\GetUnfilteredPartitionsMetadata' {nextToken} -> nextToken) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {nextToken = a} :: GetUnfilteredPartitionsMetadata)

-- | The segment of the table\'s partitions to scan in this request.
getUnfilteredPartitionsMetadata_segment :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.Maybe Segment)
getUnfilteredPartitionsMetadata_segment = Lens.lens (\GetUnfilteredPartitionsMetadata' {segment} -> segment) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {segment = a} :: GetUnfilteredPartitionsMetadata)

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is provided, the AWS account ID is used by default.
getUnfilteredPartitionsMetadata_catalogId :: Lens.Lens' GetUnfilteredPartitionsMetadata Prelude.Text
getUnfilteredPartitionsMetadata_catalogId = Lens.lens (\GetUnfilteredPartitionsMetadata' {catalogId} -> catalogId) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {catalogId = a} :: GetUnfilteredPartitionsMetadata)

-- | The name of the catalog database where the partitions reside.
getUnfilteredPartitionsMetadata_databaseName :: Lens.Lens' GetUnfilteredPartitionsMetadata Prelude.Text
getUnfilteredPartitionsMetadata_databaseName = Lens.lens (\GetUnfilteredPartitionsMetadata' {databaseName} -> databaseName) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {databaseName = a} :: GetUnfilteredPartitionsMetadata)

-- | The name of the table that contains the partition.
getUnfilteredPartitionsMetadata_tableName :: Lens.Lens' GetUnfilteredPartitionsMetadata Prelude.Text
getUnfilteredPartitionsMetadata_tableName = Lens.lens (\GetUnfilteredPartitionsMetadata' {tableName} -> tableName) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {tableName = a} :: GetUnfilteredPartitionsMetadata)

-- | A list of supported permission types.
getUnfilteredPartitionsMetadata_supportedPermissionTypes :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.NonEmpty PermissionType)
getUnfilteredPartitionsMetadata_supportedPermissionTypes = Lens.lens (\GetUnfilteredPartitionsMetadata' {supportedPermissionTypes} -> supportedPermissionTypes) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {supportedPermissionTypes = a} :: GetUnfilteredPartitionsMetadata) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    GetUnfilteredPartitionsMetadata
  where
  type
    AWSResponse GetUnfilteredPartitionsMetadata =
      GetUnfilteredPartitionsMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUnfilteredPartitionsMetadataResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "UnfilteredPartitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetUnfilteredPartitionsMetadata
  where
  hashWithSalt
    _salt
    GetUnfilteredPartitionsMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` auditContext
        `Prelude.hashWithSalt` expression
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` segment
        `Prelude.hashWithSalt` catalogId
        `Prelude.hashWithSalt` databaseName
        `Prelude.hashWithSalt` tableName
        `Prelude.hashWithSalt` supportedPermissionTypes

instance
  Prelude.NFData
    GetUnfilteredPartitionsMetadata
  where
  rnf GetUnfilteredPartitionsMetadata' {..} =
    Prelude.rnf auditContext
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf supportedPermissionTypes

instance
  Data.ToHeaders
    GetUnfilteredPartitionsMetadata
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetUnfilteredPartitionsMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetUnfilteredPartitionsMetadata where
  toJSON GetUnfilteredPartitionsMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuditContext" Data..=) Prelude.<$> auditContext,
            ("Expression" Data..=) Prelude.<$> expression,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Segment" Data..=) Prelude.<$> segment,
            Prelude.Just ("CatalogId" Data..= catalogId),
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ( "SupportedPermissionTypes"
                  Data..= supportedPermissionTypes
              )
          ]
      )

instance Data.ToPath GetUnfilteredPartitionsMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery GetUnfilteredPartitionsMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUnfilteredPartitionsMetadataResponse' smart constructor.
data GetUnfilteredPartitionsMetadataResponse = GetUnfilteredPartitionsMetadataResponse'
  { -- | A continuation token, if the returned list of partitions does not
    -- include the last one.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of requested partitions.
    unfilteredPartitions :: Prelude.Maybe [UnfilteredPartition],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUnfilteredPartitionsMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getUnfilteredPartitionsMetadataResponse_nextToken' - A continuation token, if the returned list of partitions does not
-- include the last one.
--
-- 'unfilteredPartitions', 'getUnfilteredPartitionsMetadataResponse_unfilteredPartitions' - A list of requested partitions.
--
-- 'httpStatus', 'getUnfilteredPartitionsMetadataResponse_httpStatus' - The response's http status code.
newGetUnfilteredPartitionsMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUnfilteredPartitionsMetadataResponse
newGetUnfilteredPartitionsMetadataResponse
  pHttpStatus_ =
    GetUnfilteredPartitionsMetadataResponse'
      { nextToken =
          Prelude.Nothing,
        unfilteredPartitions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A continuation token, if the returned list of partitions does not
-- include the last one.
getUnfilteredPartitionsMetadataResponse_nextToken :: Lens.Lens' GetUnfilteredPartitionsMetadataResponse (Prelude.Maybe Prelude.Text)
getUnfilteredPartitionsMetadataResponse_nextToken = Lens.lens (\GetUnfilteredPartitionsMetadataResponse' {nextToken} -> nextToken) (\s@GetUnfilteredPartitionsMetadataResponse' {} a -> s {nextToken = a} :: GetUnfilteredPartitionsMetadataResponse)

-- | A list of requested partitions.
getUnfilteredPartitionsMetadataResponse_unfilteredPartitions :: Lens.Lens' GetUnfilteredPartitionsMetadataResponse (Prelude.Maybe [UnfilteredPartition])
getUnfilteredPartitionsMetadataResponse_unfilteredPartitions = Lens.lens (\GetUnfilteredPartitionsMetadataResponse' {unfilteredPartitions} -> unfilteredPartitions) (\s@GetUnfilteredPartitionsMetadataResponse' {} a -> s {unfilteredPartitions = a} :: GetUnfilteredPartitionsMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getUnfilteredPartitionsMetadataResponse_httpStatus :: Lens.Lens' GetUnfilteredPartitionsMetadataResponse Prelude.Int
getUnfilteredPartitionsMetadataResponse_httpStatus = Lens.lens (\GetUnfilteredPartitionsMetadataResponse' {httpStatus} -> httpStatus) (\s@GetUnfilteredPartitionsMetadataResponse' {} a -> s {httpStatus = a} :: GetUnfilteredPartitionsMetadataResponse)

instance
  Prelude.NFData
    GetUnfilteredPartitionsMetadataResponse
  where
  rnf GetUnfilteredPartitionsMetadataResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf unfilteredPartitions
      `Prelude.seq` Prelude.rnf httpStatus
