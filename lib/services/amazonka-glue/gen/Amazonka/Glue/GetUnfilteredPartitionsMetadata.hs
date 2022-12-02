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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.Glue.GetUnfilteredPartitionsMetadata
  ( -- * Creating a Request
    GetUnfilteredPartitionsMetadata (..),
    newGetUnfilteredPartitionsMetadata,

    -- * Request Lenses
    getUnfilteredPartitionsMetadata_nextToken,
    getUnfilteredPartitionsMetadata_auditContext,
    getUnfilteredPartitionsMetadata_expression,
    getUnfilteredPartitionsMetadata_segment,
    getUnfilteredPartitionsMetadata_maxResults,
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
  { nextToken :: Prelude.Maybe Prelude.Text,
    auditContext :: Prelude.Maybe AuditContext,
    expression :: Prelude.Maybe Prelude.Text,
    segment :: Prelude.Maybe Segment,
    maxResults :: Prelude.Maybe Prelude.Natural,
    catalogId :: Prelude.Text,
    databaseName :: Prelude.Text,
    tableName :: Prelude.Text,
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
-- 'nextToken', 'getUnfilteredPartitionsMetadata_nextToken' - Undocumented member.
--
-- 'auditContext', 'getUnfilteredPartitionsMetadata_auditContext' - Undocumented member.
--
-- 'expression', 'getUnfilteredPartitionsMetadata_expression' - Undocumented member.
--
-- 'segment', 'getUnfilteredPartitionsMetadata_segment' - Undocumented member.
--
-- 'maxResults', 'getUnfilteredPartitionsMetadata_maxResults' - Undocumented member.
--
-- 'catalogId', 'getUnfilteredPartitionsMetadata_catalogId' - Undocumented member.
--
-- 'databaseName', 'getUnfilteredPartitionsMetadata_databaseName' - Undocumented member.
--
-- 'tableName', 'getUnfilteredPartitionsMetadata_tableName' - Undocumented member.
--
-- 'supportedPermissionTypes', 'getUnfilteredPartitionsMetadata_supportedPermissionTypes' - Undocumented member.
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
      { nextToken =
          Prelude.Nothing,
        auditContext = Prelude.Nothing,
        expression = Prelude.Nothing,
        segment = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        catalogId = pCatalogId_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        supportedPermissionTypes =
          Lens.coerced
            Lens.# pSupportedPermissionTypes_
      }

-- | Undocumented member.
getUnfilteredPartitionsMetadata_nextToken :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.Maybe Prelude.Text)
getUnfilteredPartitionsMetadata_nextToken = Lens.lens (\GetUnfilteredPartitionsMetadata' {nextToken} -> nextToken) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {nextToken = a} :: GetUnfilteredPartitionsMetadata)

-- | Undocumented member.
getUnfilteredPartitionsMetadata_auditContext :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.Maybe AuditContext)
getUnfilteredPartitionsMetadata_auditContext = Lens.lens (\GetUnfilteredPartitionsMetadata' {auditContext} -> auditContext) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {auditContext = a} :: GetUnfilteredPartitionsMetadata)

-- | Undocumented member.
getUnfilteredPartitionsMetadata_expression :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.Maybe Prelude.Text)
getUnfilteredPartitionsMetadata_expression = Lens.lens (\GetUnfilteredPartitionsMetadata' {expression} -> expression) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {expression = a} :: GetUnfilteredPartitionsMetadata)

-- | Undocumented member.
getUnfilteredPartitionsMetadata_segment :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.Maybe Segment)
getUnfilteredPartitionsMetadata_segment = Lens.lens (\GetUnfilteredPartitionsMetadata' {segment} -> segment) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {segment = a} :: GetUnfilteredPartitionsMetadata)

-- | Undocumented member.
getUnfilteredPartitionsMetadata_maxResults :: Lens.Lens' GetUnfilteredPartitionsMetadata (Prelude.Maybe Prelude.Natural)
getUnfilteredPartitionsMetadata_maxResults = Lens.lens (\GetUnfilteredPartitionsMetadata' {maxResults} -> maxResults) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {maxResults = a} :: GetUnfilteredPartitionsMetadata)

-- | Undocumented member.
getUnfilteredPartitionsMetadata_catalogId :: Lens.Lens' GetUnfilteredPartitionsMetadata Prelude.Text
getUnfilteredPartitionsMetadata_catalogId = Lens.lens (\GetUnfilteredPartitionsMetadata' {catalogId} -> catalogId) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {catalogId = a} :: GetUnfilteredPartitionsMetadata)

-- | Undocumented member.
getUnfilteredPartitionsMetadata_databaseName :: Lens.Lens' GetUnfilteredPartitionsMetadata Prelude.Text
getUnfilteredPartitionsMetadata_databaseName = Lens.lens (\GetUnfilteredPartitionsMetadata' {databaseName} -> databaseName) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {databaseName = a} :: GetUnfilteredPartitionsMetadata)

-- | Undocumented member.
getUnfilteredPartitionsMetadata_tableName :: Lens.Lens' GetUnfilteredPartitionsMetadata Prelude.Text
getUnfilteredPartitionsMetadata_tableName = Lens.lens (\GetUnfilteredPartitionsMetadata' {tableName} -> tableName) (\s@GetUnfilteredPartitionsMetadata' {} a -> s {tableName = a} :: GetUnfilteredPartitionsMetadata)

-- | Undocumented member.
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
            Prelude.<*> ( x Data..?> "UnfilteredPartitions"
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
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` auditContext
        `Prelude.hashWithSalt` expression
        `Prelude.hashWithSalt` segment
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` catalogId
        `Prelude.hashWithSalt` databaseName
        `Prelude.hashWithSalt` tableName
        `Prelude.hashWithSalt` supportedPermissionTypes

instance
  Prelude.NFData
    GetUnfilteredPartitionsMetadata
  where
  rnf GetUnfilteredPartitionsMetadata' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf auditContext
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf maxResults
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
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("AuditContext" Data..=) Prelude.<$> auditContext,
            ("Expression" Data..=) Prelude.<$> expression,
            ("Segment" Data..=) Prelude.<$> segment,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
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
  { nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'nextToken', 'getUnfilteredPartitionsMetadataResponse_nextToken' - Undocumented member.
--
-- 'unfilteredPartitions', 'getUnfilteredPartitionsMetadataResponse_unfilteredPartitions' - Undocumented member.
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

-- | Undocumented member.
getUnfilteredPartitionsMetadataResponse_nextToken :: Lens.Lens' GetUnfilteredPartitionsMetadataResponse (Prelude.Maybe Prelude.Text)
getUnfilteredPartitionsMetadataResponse_nextToken = Lens.lens (\GetUnfilteredPartitionsMetadataResponse' {nextToken} -> nextToken) (\s@GetUnfilteredPartitionsMetadataResponse' {} a -> s {nextToken = a} :: GetUnfilteredPartitionsMetadataResponse)

-- | Undocumented member.
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
