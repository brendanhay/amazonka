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
-- Module      : Amazonka.Glue.GetUnfilteredTableMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves table metadata from the Data Catalog that contains unfiltered
-- metadata.
--
-- For IAM authorization, the public IAM action associated with this API is
-- @glue:GetTable@.
module Amazonka.Glue.GetUnfilteredTableMetadata
  ( -- * Creating a Request
    GetUnfilteredTableMetadata (..),
    newGetUnfilteredTableMetadata,

    -- * Request Lenses
    getUnfilteredTableMetadata_auditContext,
    getUnfilteredTableMetadata_catalogId,
    getUnfilteredTableMetadata_databaseName,
    getUnfilteredTableMetadata_name,
    getUnfilteredTableMetadata_supportedPermissionTypes,

    -- * Destructuring the Response
    GetUnfilteredTableMetadataResponse (..),
    newGetUnfilteredTableMetadataResponse,

    -- * Response Lenses
    getUnfilteredTableMetadataResponse_authorizedColumns,
    getUnfilteredTableMetadataResponse_cellFilters,
    getUnfilteredTableMetadataResponse_isRegisteredWithLakeFormation,
    getUnfilteredTableMetadataResponse_table,
    getUnfilteredTableMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUnfilteredTableMetadata' smart constructor.
data GetUnfilteredTableMetadata = GetUnfilteredTableMetadata'
  { -- | A structure containing Lake Formation audit context information.
    auditContext :: Prelude.Maybe AuditContext,
    -- | The catalog ID where the table resides.
    catalogId :: Prelude.Text,
    -- | (Required) Specifies the name of a database that contains the table.
    databaseName :: Prelude.Text,
    -- | (Required) Specifies the name of a table for which you are requesting
    -- metadata.
    name :: Prelude.Text,
    -- | (Required) A list of supported permission types.
    supportedPermissionTypes :: Prelude.NonEmpty PermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUnfilteredTableMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditContext', 'getUnfilteredTableMetadata_auditContext' - A structure containing Lake Formation audit context information.
--
-- 'catalogId', 'getUnfilteredTableMetadata_catalogId' - The catalog ID where the table resides.
--
-- 'databaseName', 'getUnfilteredTableMetadata_databaseName' - (Required) Specifies the name of a database that contains the table.
--
-- 'name', 'getUnfilteredTableMetadata_name' - (Required) Specifies the name of a table for which you are requesting
-- metadata.
--
-- 'supportedPermissionTypes', 'getUnfilteredTableMetadata_supportedPermissionTypes' - (Required) A list of supported permission types.
newGetUnfilteredTableMetadata ::
  -- | 'catalogId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'supportedPermissionTypes'
  Prelude.NonEmpty PermissionType ->
  GetUnfilteredTableMetadata
newGetUnfilteredTableMetadata
  pCatalogId_
  pDatabaseName_
  pName_
  pSupportedPermissionTypes_ =
    GetUnfilteredTableMetadata'
      { auditContext =
          Prelude.Nothing,
        catalogId = pCatalogId_,
        databaseName = pDatabaseName_,
        name = pName_,
        supportedPermissionTypes =
          Lens.coerced
            Lens.# pSupportedPermissionTypes_
      }

-- | A structure containing Lake Formation audit context information.
getUnfilteredTableMetadata_auditContext :: Lens.Lens' GetUnfilteredTableMetadata (Prelude.Maybe AuditContext)
getUnfilteredTableMetadata_auditContext = Lens.lens (\GetUnfilteredTableMetadata' {auditContext} -> auditContext) (\s@GetUnfilteredTableMetadata' {} a -> s {auditContext = a} :: GetUnfilteredTableMetadata)

-- | The catalog ID where the table resides.
getUnfilteredTableMetadata_catalogId :: Lens.Lens' GetUnfilteredTableMetadata Prelude.Text
getUnfilteredTableMetadata_catalogId = Lens.lens (\GetUnfilteredTableMetadata' {catalogId} -> catalogId) (\s@GetUnfilteredTableMetadata' {} a -> s {catalogId = a} :: GetUnfilteredTableMetadata)

-- | (Required) Specifies the name of a database that contains the table.
getUnfilteredTableMetadata_databaseName :: Lens.Lens' GetUnfilteredTableMetadata Prelude.Text
getUnfilteredTableMetadata_databaseName = Lens.lens (\GetUnfilteredTableMetadata' {databaseName} -> databaseName) (\s@GetUnfilteredTableMetadata' {} a -> s {databaseName = a} :: GetUnfilteredTableMetadata)

-- | (Required) Specifies the name of a table for which you are requesting
-- metadata.
getUnfilteredTableMetadata_name :: Lens.Lens' GetUnfilteredTableMetadata Prelude.Text
getUnfilteredTableMetadata_name = Lens.lens (\GetUnfilteredTableMetadata' {name} -> name) (\s@GetUnfilteredTableMetadata' {} a -> s {name = a} :: GetUnfilteredTableMetadata)

-- | (Required) A list of supported permission types.
getUnfilteredTableMetadata_supportedPermissionTypes :: Lens.Lens' GetUnfilteredTableMetadata (Prelude.NonEmpty PermissionType)
getUnfilteredTableMetadata_supportedPermissionTypes = Lens.lens (\GetUnfilteredTableMetadata' {supportedPermissionTypes} -> supportedPermissionTypes) (\s@GetUnfilteredTableMetadata' {} a -> s {supportedPermissionTypes = a} :: GetUnfilteredTableMetadata) Prelude.. Lens.coerced

instance Core.AWSRequest GetUnfilteredTableMetadata where
  type
    AWSResponse GetUnfilteredTableMetadata =
      GetUnfilteredTableMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUnfilteredTableMetadataResponse'
            Prelude.<$> ( x
                            Data..?> "AuthorizedColumns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "CellFilters" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "IsRegisteredWithLakeFormation")
            Prelude.<*> (x Data..?> "Table")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUnfilteredTableMetadata where
  hashWithSalt _salt GetUnfilteredTableMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` auditContext
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` supportedPermissionTypes

instance Prelude.NFData GetUnfilteredTableMetadata where
  rnf GetUnfilteredTableMetadata' {..} =
    Prelude.rnf auditContext
      `Prelude.seq` Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf supportedPermissionTypes

instance Data.ToHeaders GetUnfilteredTableMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetUnfilteredTableMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetUnfilteredTableMetadata where
  toJSON GetUnfilteredTableMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuditContext" Data..=) Prelude.<$> auditContext,
            Prelude.Just ("CatalogId" Data..= catalogId),
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ( "SupportedPermissionTypes"
                  Data..= supportedPermissionTypes
              )
          ]
      )

instance Data.ToPath GetUnfilteredTableMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery GetUnfilteredTableMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUnfilteredTableMetadataResponse' smart constructor.
data GetUnfilteredTableMetadataResponse = GetUnfilteredTableMetadataResponse'
  { -- | A list of column names that the user has been granted access to.
    authorizedColumns :: Prelude.Maybe [Prelude.Text],
    -- | A list of column row filters.
    cellFilters :: Prelude.Maybe [ColumnRowFilter],
    -- | A Boolean value that indicates whether the partition location is
    -- registered with Lake Formation.
    isRegisteredWithLakeFormation :: Prelude.Maybe Prelude.Bool,
    -- | A Table object containing the table metadata.
    table :: Prelude.Maybe Table,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUnfilteredTableMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizedColumns', 'getUnfilteredTableMetadataResponse_authorizedColumns' - A list of column names that the user has been granted access to.
--
-- 'cellFilters', 'getUnfilteredTableMetadataResponse_cellFilters' - A list of column row filters.
--
-- 'isRegisteredWithLakeFormation', 'getUnfilteredTableMetadataResponse_isRegisteredWithLakeFormation' - A Boolean value that indicates whether the partition location is
-- registered with Lake Formation.
--
-- 'table', 'getUnfilteredTableMetadataResponse_table' - A Table object containing the table metadata.
--
-- 'httpStatus', 'getUnfilteredTableMetadataResponse_httpStatus' - The response's http status code.
newGetUnfilteredTableMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUnfilteredTableMetadataResponse
newGetUnfilteredTableMetadataResponse pHttpStatus_ =
  GetUnfilteredTableMetadataResponse'
    { authorizedColumns =
        Prelude.Nothing,
      cellFilters = Prelude.Nothing,
      isRegisteredWithLakeFormation =
        Prelude.Nothing,
      table = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of column names that the user has been granted access to.
getUnfilteredTableMetadataResponse_authorizedColumns :: Lens.Lens' GetUnfilteredTableMetadataResponse (Prelude.Maybe [Prelude.Text])
getUnfilteredTableMetadataResponse_authorizedColumns = Lens.lens (\GetUnfilteredTableMetadataResponse' {authorizedColumns} -> authorizedColumns) (\s@GetUnfilteredTableMetadataResponse' {} a -> s {authorizedColumns = a} :: GetUnfilteredTableMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of column row filters.
getUnfilteredTableMetadataResponse_cellFilters :: Lens.Lens' GetUnfilteredTableMetadataResponse (Prelude.Maybe [ColumnRowFilter])
getUnfilteredTableMetadataResponse_cellFilters = Lens.lens (\GetUnfilteredTableMetadataResponse' {cellFilters} -> cellFilters) (\s@GetUnfilteredTableMetadataResponse' {} a -> s {cellFilters = a} :: GetUnfilteredTableMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value that indicates whether the partition location is
-- registered with Lake Formation.
getUnfilteredTableMetadataResponse_isRegisteredWithLakeFormation :: Lens.Lens' GetUnfilteredTableMetadataResponse (Prelude.Maybe Prelude.Bool)
getUnfilteredTableMetadataResponse_isRegisteredWithLakeFormation = Lens.lens (\GetUnfilteredTableMetadataResponse' {isRegisteredWithLakeFormation} -> isRegisteredWithLakeFormation) (\s@GetUnfilteredTableMetadataResponse' {} a -> s {isRegisteredWithLakeFormation = a} :: GetUnfilteredTableMetadataResponse)

-- | A Table object containing the table metadata.
getUnfilteredTableMetadataResponse_table :: Lens.Lens' GetUnfilteredTableMetadataResponse (Prelude.Maybe Table)
getUnfilteredTableMetadataResponse_table = Lens.lens (\GetUnfilteredTableMetadataResponse' {table} -> table) (\s@GetUnfilteredTableMetadataResponse' {} a -> s {table = a} :: GetUnfilteredTableMetadataResponse)

-- | The response's http status code.
getUnfilteredTableMetadataResponse_httpStatus :: Lens.Lens' GetUnfilteredTableMetadataResponse Prelude.Int
getUnfilteredTableMetadataResponse_httpStatus = Lens.lens (\GetUnfilteredTableMetadataResponse' {httpStatus} -> httpStatus) (\s@GetUnfilteredTableMetadataResponse' {} a -> s {httpStatus = a} :: GetUnfilteredTableMetadataResponse)

instance
  Prelude.NFData
    GetUnfilteredTableMetadataResponse
  where
  rnf GetUnfilteredTableMetadataResponse' {..} =
    Prelude.rnf authorizedColumns
      `Prelude.seq` Prelude.rnf cellFilters
      `Prelude.seq` Prelude.rnf isRegisteredWithLakeFormation
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf httpStatus
