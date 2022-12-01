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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
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
    getUnfilteredTableMetadataResponse_cellFilters,
    getUnfilteredTableMetadataResponse_authorizedColumns,
    getUnfilteredTableMetadataResponse_isRegisteredWithLakeFormation,
    getUnfilteredTableMetadataResponse_table,
    getUnfilteredTableMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUnfilteredTableMetadata' smart constructor.
data GetUnfilteredTableMetadata = GetUnfilteredTableMetadata'
  { auditContext :: Prelude.Maybe AuditContext,
    catalogId :: Prelude.Text,
    databaseName :: Prelude.Text,
    name :: Prelude.Text,
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
-- 'auditContext', 'getUnfilteredTableMetadata_auditContext' - Undocumented member.
--
-- 'catalogId', 'getUnfilteredTableMetadata_catalogId' - Undocumented member.
--
-- 'databaseName', 'getUnfilteredTableMetadata_databaseName' - Undocumented member.
--
-- 'name', 'getUnfilteredTableMetadata_name' - Undocumented member.
--
-- 'supportedPermissionTypes', 'getUnfilteredTableMetadata_supportedPermissionTypes' - Undocumented member.
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

-- | Undocumented member.
getUnfilteredTableMetadata_auditContext :: Lens.Lens' GetUnfilteredTableMetadata (Prelude.Maybe AuditContext)
getUnfilteredTableMetadata_auditContext = Lens.lens (\GetUnfilteredTableMetadata' {auditContext} -> auditContext) (\s@GetUnfilteredTableMetadata' {} a -> s {auditContext = a} :: GetUnfilteredTableMetadata)

-- | Undocumented member.
getUnfilteredTableMetadata_catalogId :: Lens.Lens' GetUnfilteredTableMetadata Prelude.Text
getUnfilteredTableMetadata_catalogId = Lens.lens (\GetUnfilteredTableMetadata' {catalogId} -> catalogId) (\s@GetUnfilteredTableMetadata' {} a -> s {catalogId = a} :: GetUnfilteredTableMetadata)

-- | Undocumented member.
getUnfilteredTableMetadata_databaseName :: Lens.Lens' GetUnfilteredTableMetadata Prelude.Text
getUnfilteredTableMetadata_databaseName = Lens.lens (\GetUnfilteredTableMetadata' {databaseName} -> databaseName) (\s@GetUnfilteredTableMetadata' {} a -> s {databaseName = a} :: GetUnfilteredTableMetadata)

-- | Undocumented member.
getUnfilteredTableMetadata_name :: Lens.Lens' GetUnfilteredTableMetadata Prelude.Text
getUnfilteredTableMetadata_name = Lens.lens (\GetUnfilteredTableMetadata' {name} -> name) (\s@GetUnfilteredTableMetadata' {} a -> s {name = a} :: GetUnfilteredTableMetadata)

-- | Undocumented member.
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
            Prelude.<$> (x Core..?> "CellFilters" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "AuthorizedColumns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "IsRegisteredWithLakeFormation")
            Prelude.<*> (x Core..?> "Table")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUnfilteredTableMetadata where
  hashWithSalt _salt GetUnfilteredTableMetadata' {..} =
    _salt `Prelude.hashWithSalt` auditContext
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

instance Core.ToHeaders GetUnfilteredTableMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetUnfilteredTableMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetUnfilteredTableMetadata where
  toJSON GetUnfilteredTableMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AuditContext" Core..=) Prelude.<$> auditContext,
            Prelude.Just ("CatalogId" Core..= catalogId),
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ( "SupportedPermissionTypes"
                  Core..= supportedPermissionTypes
              )
          ]
      )

instance Core.ToPath GetUnfilteredTableMetadata where
  toPath = Prelude.const "/"

instance Core.ToQuery GetUnfilteredTableMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUnfilteredTableMetadataResponse' smart constructor.
data GetUnfilteredTableMetadataResponse = GetUnfilteredTableMetadataResponse'
  { cellFilters :: Prelude.Maybe [ColumnRowFilter],
    authorizedColumns :: Prelude.Maybe [Prelude.Text],
    isRegisteredWithLakeFormation :: Prelude.Maybe Prelude.Bool,
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
-- 'cellFilters', 'getUnfilteredTableMetadataResponse_cellFilters' - Undocumented member.
--
-- 'authorizedColumns', 'getUnfilteredTableMetadataResponse_authorizedColumns' - Undocumented member.
--
-- 'isRegisteredWithLakeFormation', 'getUnfilteredTableMetadataResponse_isRegisteredWithLakeFormation' - Undocumented member.
--
-- 'table', 'getUnfilteredTableMetadataResponse_table' - Undocumented member.
--
-- 'httpStatus', 'getUnfilteredTableMetadataResponse_httpStatus' - The response's http status code.
newGetUnfilteredTableMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUnfilteredTableMetadataResponse
newGetUnfilteredTableMetadataResponse pHttpStatus_ =
  GetUnfilteredTableMetadataResponse'
    { cellFilters =
        Prelude.Nothing,
      authorizedColumns = Prelude.Nothing,
      isRegisteredWithLakeFormation =
        Prelude.Nothing,
      table = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getUnfilteredTableMetadataResponse_cellFilters :: Lens.Lens' GetUnfilteredTableMetadataResponse (Prelude.Maybe [ColumnRowFilter])
getUnfilteredTableMetadataResponse_cellFilters = Lens.lens (\GetUnfilteredTableMetadataResponse' {cellFilters} -> cellFilters) (\s@GetUnfilteredTableMetadataResponse' {} a -> s {cellFilters = a} :: GetUnfilteredTableMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getUnfilteredTableMetadataResponse_authorizedColumns :: Lens.Lens' GetUnfilteredTableMetadataResponse (Prelude.Maybe [Prelude.Text])
getUnfilteredTableMetadataResponse_authorizedColumns = Lens.lens (\GetUnfilteredTableMetadataResponse' {authorizedColumns} -> authorizedColumns) (\s@GetUnfilteredTableMetadataResponse' {} a -> s {authorizedColumns = a} :: GetUnfilteredTableMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getUnfilteredTableMetadataResponse_isRegisteredWithLakeFormation :: Lens.Lens' GetUnfilteredTableMetadataResponse (Prelude.Maybe Prelude.Bool)
getUnfilteredTableMetadataResponse_isRegisteredWithLakeFormation = Lens.lens (\GetUnfilteredTableMetadataResponse' {isRegisteredWithLakeFormation} -> isRegisteredWithLakeFormation) (\s@GetUnfilteredTableMetadataResponse' {} a -> s {isRegisteredWithLakeFormation = a} :: GetUnfilteredTableMetadataResponse)

-- | Undocumented member.
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
    Prelude.rnf cellFilters
      `Prelude.seq` Prelude.rnf authorizedColumns
      `Prelude.seq` Prelude.rnf isRegisteredWithLakeFormation
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf httpStatus
