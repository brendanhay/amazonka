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
-- Module      : Amazonka.Glue.GetUnfilteredPartitionMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.Glue.GetUnfilteredPartitionMetadata
  ( -- * Creating a Request
    GetUnfilteredPartitionMetadata (..),
    newGetUnfilteredPartitionMetadata,

    -- * Request Lenses
    getUnfilteredPartitionMetadata_auditContext,
    getUnfilteredPartitionMetadata_catalogId,
    getUnfilteredPartitionMetadata_databaseName,
    getUnfilteredPartitionMetadata_tableName,
    getUnfilteredPartitionMetadata_partitionValues,
    getUnfilteredPartitionMetadata_supportedPermissionTypes,

    -- * Destructuring the Response
    GetUnfilteredPartitionMetadataResponse (..),
    newGetUnfilteredPartitionMetadataResponse,

    -- * Response Lenses
    getUnfilteredPartitionMetadataResponse_authorizedColumns,
    getUnfilteredPartitionMetadataResponse_partition,
    getUnfilteredPartitionMetadataResponse_isRegisteredWithLakeFormation,
    getUnfilteredPartitionMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUnfilteredPartitionMetadata' smart constructor.
data GetUnfilteredPartitionMetadata = GetUnfilteredPartitionMetadata'
  { auditContext :: Prelude.Maybe AuditContext,
    catalogId :: Prelude.Text,
    databaseName :: Prelude.Text,
    tableName :: Prelude.Text,
    partitionValues :: [Prelude.Text],
    supportedPermissionTypes :: Prelude.NonEmpty PermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUnfilteredPartitionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditContext', 'getUnfilteredPartitionMetadata_auditContext' - Undocumented member.
--
-- 'catalogId', 'getUnfilteredPartitionMetadata_catalogId' - Undocumented member.
--
-- 'databaseName', 'getUnfilteredPartitionMetadata_databaseName' - Undocumented member.
--
-- 'tableName', 'getUnfilteredPartitionMetadata_tableName' - Undocumented member.
--
-- 'partitionValues', 'getUnfilteredPartitionMetadata_partitionValues' - Undocumented member.
--
-- 'supportedPermissionTypes', 'getUnfilteredPartitionMetadata_supportedPermissionTypes' - Undocumented member.
newGetUnfilteredPartitionMetadata ::
  -- | 'catalogId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'supportedPermissionTypes'
  Prelude.NonEmpty PermissionType ->
  GetUnfilteredPartitionMetadata
newGetUnfilteredPartitionMetadata
  pCatalogId_
  pDatabaseName_
  pTableName_
  pSupportedPermissionTypes_ =
    GetUnfilteredPartitionMetadata'
      { auditContext =
          Prelude.Nothing,
        catalogId = pCatalogId_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        partitionValues = Prelude.mempty,
        supportedPermissionTypes =
          Lens.coerced
            Lens.# pSupportedPermissionTypes_
      }

-- | Undocumented member.
getUnfilteredPartitionMetadata_auditContext :: Lens.Lens' GetUnfilteredPartitionMetadata (Prelude.Maybe AuditContext)
getUnfilteredPartitionMetadata_auditContext = Lens.lens (\GetUnfilteredPartitionMetadata' {auditContext} -> auditContext) (\s@GetUnfilteredPartitionMetadata' {} a -> s {auditContext = a} :: GetUnfilteredPartitionMetadata)

-- | Undocumented member.
getUnfilteredPartitionMetadata_catalogId :: Lens.Lens' GetUnfilteredPartitionMetadata Prelude.Text
getUnfilteredPartitionMetadata_catalogId = Lens.lens (\GetUnfilteredPartitionMetadata' {catalogId} -> catalogId) (\s@GetUnfilteredPartitionMetadata' {} a -> s {catalogId = a} :: GetUnfilteredPartitionMetadata)

-- | Undocumented member.
getUnfilteredPartitionMetadata_databaseName :: Lens.Lens' GetUnfilteredPartitionMetadata Prelude.Text
getUnfilteredPartitionMetadata_databaseName = Lens.lens (\GetUnfilteredPartitionMetadata' {databaseName} -> databaseName) (\s@GetUnfilteredPartitionMetadata' {} a -> s {databaseName = a} :: GetUnfilteredPartitionMetadata)

-- | Undocumented member.
getUnfilteredPartitionMetadata_tableName :: Lens.Lens' GetUnfilteredPartitionMetadata Prelude.Text
getUnfilteredPartitionMetadata_tableName = Lens.lens (\GetUnfilteredPartitionMetadata' {tableName} -> tableName) (\s@GetUnfilteredPartitionMetadata' {} a -> s {tableName = a} :: GetUnfilteredPartitionMetadata)

-- | Undocumented member.
getUnfilteredPartitionMetadata_partitionValues :: Lens.Lens' GetUnfilteredPartitionMetadata [Prelude.Text]
getUnfilteredPartitionMetadata_partitionValues = Lens.lens (\GetUnfilteredPartitionMetadata' {partitionValues} -> partitionValues) (\s@GetUnfilteredPartitionMetadata' {} a -> s {partitionValues = a} :: GetUnfilteredPartitionMetadata) Prelude.. Lens.coerced

-- | Undocumented member.
getUnfilteredPartitionMetadata_supportedPermissionTypes :: Lens.Lens' GetUnfilteredPartitionMetadata (Prelude.NonEmpty PermissionType)
getUnfilteredPartitionMetadata_supportedPermissionTypes = Lens.lens (\GetUnfilteredPartitionMetadata' {supportedPermissionTypes} -> supportedPermissionTypes) (\s@GetUnfilteredPartitionMetadata' {} a -> s {supportedPermissionTypes = a} :: GetUnfilteredPartitionMetadata) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    GetUnfilteredPartitionMetadata
  where
  type
    AWSResponse GetUnfilteredPartitionMetadata =
      GetUnfilteredPartitionMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUnfilteredPartitionMetadataResponse'
            Prelude.<$> ( x Data..?> "AuthorizedColumns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Partition")
            Prelude.<*> (x Data..?> "IsRegisteredWithLakeFormation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetUnfilteredPartitionMetadata
  where
  hashWithSalt
    _salt
    GetUnfilteredPartitionMetadata' {..} =
      _salt `Prelude.hashWithSalt` auditContext
        `Prelude.hashWithSalt` catalogId
        `Prelude.hashWithSalt` databaseName
        `Prelude.hashWithSalt` tableName
        `Prelude.hashWithSalt` partitionValues
        `Prelude.hashWithSalt` supportedPermissionTypes

instance
  Prelude.NFData
    GetUnfilteredPartitionMetadata
  where
  rnf GetUnfilteredPartitionMetadata' {..} =
    Prelude.rnf auditContext
      `Prelude.seq` Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf partitionValues
      `Prelude.seq` Prelude.rnf supportedPermissionTypes

instance
  Data.ToHeaders
    GetUnfilteredPartitionMetadata
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetUnfilteredPartitionMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetUnfilteredPartitionMetadata where
  toJSON GetUnfilteredPartitionMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuditContext" Data..=) Prelude.<$> auditContext,
            Prelude.Just ("CatalogId" Data..= catalogId),
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ("PartitionValues" Data..= partitionValues),
            Prelude.Just
              ( "SupportedPermissionTypes"
                  Data..= supportedPermissionTypes
              )
          ]
      )

instance Data.ToPath GetUnfilteredPartitionMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery GetUnfilteredPartitionMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUnfilteredPartitionMetadataResponse' smart constructor.
data GetUnfilteredPartitionMetadataResponse = GetUnfilteredPartitionMetadataResponse'
  { authorizedColumns :: Prelude.Maybe [Prelude.Text],
    partition :: Prelude.Maybe Partition,
    isRegisteredWithLakeFormation :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUnfilteredPartitionMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizedColumns', 'getUnfilteredPartitionMetadataResponse_authorizedColumns' - Undocumented member.
--
-- 'partition', 'getUnfilteredPartitionMetadataResponse_partition' - Undocumented member.
--
-- 'isRegisteredWithLakeFormation', 'getUnfilteredPartitionMetadataResponse_isRegisteredWithLakeFormation' - Undocumented member.
--
-- 'httpStatus', 'getUnfilteredPartitionMetadataResponse_httpStatus' - The response's http status code.
newGetUnfilteredPartitionMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUnfilteredPartitionMetadataResponse
newGetUnfilteredPartitionMetadataResponse
  pHttpStatus_ =
    GetUnfilteredPartitionMetadataResponse'
      { authorizedColumns =
          Prelude.Nothing,
        partition = Prelude.Nothing,
        isRegisteredWithLakeFormation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
getUnfilteredPartitionMetadataResponse_authorizedColumns :: Lens.Lens' GetUnfilteredPartitionMetadataResponse (Prelude.Maybe [Prelude.Text])
getUnfilteredPartitionMetadataResponse_authorizedColumns = Lens.lens (\GetUnfilteredPartitionMetadataResponse' {authorizedColumns} -> authorizedColumns) (\s@GetUnfilteredPartitionMetadataResponse' {} a -> s {authorizedColumns = a} :: GetUnfilteredPartitionMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getUnfilteredPartitionMetadataResponse_partition :: Lens.Lens' GetUnfilteredPartitionMetadataResponse (Prelude.Maybe Partition)
getUnfilteredPartitionMetadataResponse_partition = Lens.lens (\GetUnfilteredPartitionMetadataResponse' {partition} -> partition) (\s@GetUnfilteredPartitionMetadataResponse' {} a -> s {partition = a} :: GetUnfilteredPartitionMetadataResponse)

-- | Undocumented member.
getUnfilteredPartitionMetadataResponse_isRegisteredWithLakeFormation :: Lens.Lens' GetUnfilteredPartitionMetadataResponse (Prelude.Maybe Prelude.Bool)
getUnfilteredPartitionMetadataResponse_isRegisteredWithLakeFormation = Lens.lens (\GetUnfilteredPartitionMetadataResponse' {isRegisteredWithLakeFormation} -> isRegisteredWithLakeFormation) (\s@GetUnfilteredPartitionMetadataResponse' {} a -> s {isRegisteredWithLakeFormation = a} :: GetUnfilteredPartitionMetadataResponse)

-- | The response's http status code.
getUnfilteredPartitionMetadataResponse_httpStatus :: Lens.Lens' GetUnfilteredPartitionMetadataResponse Prelude.Int
getUnfilteredPartitionMetadataResponse_httpStatus = Lens.lens (\GetUnfilteredPartitionMetadataResponse' {httpStatus} -> httpStatus) (\s@GetUnfilteredPartitionMetadataResponse' {} a -> s {httpStatus = a} :: GetUnfilteredPartitionMetadataResponse)

instance
  Prelude.NFData
    GetUnfilteredPartitionMetadataResponse
  where
  rnf GetUnfilteredPartitionMetadataResponse' {..} =
    Prelude.rnf authorizedColumns
      `Prelude.seq` Prelude.rnf partition
      `Prelude.seq` Prelude.rnf isRegisteredWithLakeFormation
      `Prelude.seq` Prelude.rnf httpStatus
