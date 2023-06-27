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
-- @glue:GetPartition@.
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
    getUnfilteredPartitionMetadataResponse_isRegisteredWithLakeFormation,
    getUnfilteredPartitionMetadataResponse_partition,
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
  { -- | A structure containing Lake Formation audit context information.
    auditContext :: Prelude.Maybe AuditContext,
    -- | The catalog ID where the partition resides.
    catalogId :: Prelude.Text,
    -- | (Required) Specifies the name of a database that contains the partition.
    databaseName :: Prelude.Text,
    -- | (Required) Specifies the name of a table that contains the partition.
    tableName :: Prelude.Text,
    -- | (Required) A list of partition key values.
    partitionValues :: [Prelude.Text],
    -- | (Required) A list of supported permission types.
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
-- 'auditContext', 'getUnfilteredPartitionMetadata_auditContext' - A structure containing Lake Formation audit context information.
--
-- 'catalogId', 'getUnfilteredPartitionMetadata_catalogId' - The catalog ID where the partition resides.
--
-- 'databaseName', 'getUnfilteredPartitionMetadata_databaseName' - (Required) Specifies the name of a database that contains the partition.
--
-- 'tableName', 'getUnfilteredPartitionMetadata_tableName' - (Required) Specifies the name of a table that contains the partition.
--
-- 'partitionValues', 'getUnfilteredPartitionMetadata_partitionValues' - (Required) A list of partition key values.
--
-- 'supportedPermissionTypes', 'getUnfilteredPartitionMetadata_supportedPermissionTypes' - (Required) A list of supported permission types.
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

-- | A structure containing Lake Formation audit context information.
getUnfilteredPartitionMetadata_auditContext :: Lens.Lens' GetUnfilteredPartitionMetadata (Prelude.Maybe AuditContext)
getUnfilteredPartitionMetadata_auditContext = Lens.lens (\GetUnfilteredPartitionMetadata' {auditContext} -> auditContext) (\s@GetUnfilteredPartitionMetadata' {} a -> s {auditContext = a} :: GetUnfilteredPartitionMetadata)

-- | The catalog ID where the partition resides.
getUnfilteredPartitionMetadata_catalogId :: Lens.Lens' GetUnfilteredPartitionMetadata Prelude.Text
getUnfilteredPartitionMetadata_catalogId = Lens.lens (\GetUnfilteredPartitionMetadata' {catalogId} -> catalogId) (\s@GetUnfilteredPartitionMetadata' {} a -> s {catalogId = a} :: GetUnfilteredPartitionMetadata)

-- | (Required) Specifies the name of a database that contains the partition.
getUnfilteredPartitionMetadata_databaseName :: Lens.Lens' GetUnfilteredPartitionMetadata Prelude.Text
getUnfilteredPartitionMetadata_databaseName = Lens.lens (\GetUnfilteredPartitionMetadata' {databaseName} -> databaseName) (\s@GetUnfilteredPartitionMetadata' {} a -> s {databaseName = a} :: GetUnfilteredPartitionMetadata)

-- | (Required) Specifies the name of a table that contains the partition.
getUnfilteredPartitionMetadata_tableName :: Lens.Lens' GetUnfilteredPartitionMetadata Prelude.Text
getUnfilteredPartitionMetadata_tableName = Lens.lens (\GetUnfilteredPartitionMetadata' {tableName} -> tableName) (\s@GetUnfilteredPartitionMetadata' {} a -> s {tableName = a} :: GetUnfilteredPartitionMetadata)

-- | (Required) A list of partition key values.
getUnfilteredPartitionMetadata_partitionValues :: Lens.Lens' GetUnfilteredPartitionMetadata [Prelude.Text]
getUnfilteredPartitionMetadata_partitionValues = Lens.lens (\GetUnfilteredPartitionMetadata' {partitionValues} -> partitionValues) (\s@GetUnfilteredPartitionMetadata' {} a -> s {partitionValues = a} :: GetUnfilteredPartitionMetadata) Prelude.. Lens.coerced

-- | (Required) A list of supported permission types.
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
            Prelude.<$> ( x
                            Data..?> "AuthorizedColumns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "IsRegisteredWithLakeFormation")
            Prelude.<*> (x Data..?> "Partition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetUnfilteredPartitionMetadata
  where
  hashWithSalt
    _salt
    GetUnfilteredPartitionMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` auditContext
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
  { -- | A list of column names that the user has been granted access to.
    authorizedColumns :: Prelude.Maybe [Prelude.Text],
    -- | A Boolean value that indicates whether the partition location is
    -- registered with Lake Formation.
    isRegisteredWithLakeFormation :: Prelude.Maybe Prelude.Bool,
    -- | A Partition object containing the partition metadata.
    partition :: Prelude.Maybe Partition,
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
-- 'authorizedColumns', 'getUnfilteredPartitionMetadataResponse_authorizedColumns' - A list of column names that the user has been granted access to.
--
-- 'isRegisteredWithLakeFormation', 'getUnfilteredPartitionMetadataResponse_isRegisteredWithLakeFormation' - A Boolean value that indicates whether the partition location is
-- registered with Lake Formation.
--
-- 'partition', 'getUnfilteredPartitionMetadataResponse_partition' - A Partition object containing the partition metadata.
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
        isRegisteredWithLakeFormation =
          Prelude.Nothing,
        partition = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of column names that the user has been granted access to.
getUnfilteredPartitionMetadataResponse_authorizedColumns :: Lens.Lens' GetUnfilteredPartitionMetadataResponse (Prelude.Maybe [Prelude.Text])
getUnfilteredPartitionMetadataResponse_authorizedColumns = Lens.lens (\GetUnfilteredPartitionMetadataResponse' {authorizedColumns} -> authorizedColumns) (\s@GetUnfilteredPartitionMetadataResponse' {} a -> s {authorizedColumns = a} :: GetUnfilteredPartitionMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value that indicates whether the partition location is
-- registered with Lake Formation.
getUnfilteredPartitionMetadataResponse_isRegisteredWithLakeFormation :: Lens.Lens' GetUnfilteredPartitionMetadataResponse (Prelude.Maybe Prelude.Bool)
getUnfilteredPartitionMetadataResponse_isRegisteredWithLakeFormation = Lens.lens (\GetUnfilteredPartitionMetadataResponse' {isRegisteredWithLakeFormation} -> isRegisteredWithLakeFormation) (\s@GetUnfilteredPartitionMetadataResponse' {} a -> s {isRegisteredWithLakeFormation = a} :: GetUnfilteredPartitionMetadataResponse)

-- | A Partition object containing the partition metadata.
getUnfilteredPartitionMetadataResponse_partition :: Lens.Lens' GetUnfilteredPartitionMetadataResponse (Prelude.Maybe Partition)
getUnfilteredPartitionMetadataResponse_partition = Lens.lens (\GetUnfilteredPartitionMetadataResponse' {partition} -> partition) (\s@GetUnfilteredPartitionMetadataResponse' {} a -> s {partition = a} :: GetUnfilteredPartitionMetadataResponse)

-- | The response's http status code.
getUnfilteredPartitionMetadataResponse_httpStatus :: Lens.Lens' GetUnfilteredPartitionMetadataResponse Prelude.Int
getUnfilteredPartitionMetadataResponse_httpStatus = Lens.lens (\GetUnfilteredPartitionMetadataResponse' {httpStatus} -> httpStatus) (\s@GetUnfilteredPartitionMetadataResponse' {} a -> s {httpStatus = a} :: GetUnfilteredPartitionMetadataResponse)

instance
  Prelude.NFData
    GetUnfilteredPartitionMetadataResponse
  where
  rnf GetUnfilteredPartitionMetadataResponse' {..} =
    Prelude.rnf authorizedColumns
      `Prelude.seq` Prelude.rnf isRegisteredWithLakeFormation
      `Prelude.seq` Prelude.rnf partition
      `Prelude.seq` Prelude.rnf httpStatus
