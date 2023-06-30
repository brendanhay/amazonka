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
-- Module      : Amazonka.Athena.GetTableMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns table metadata for the specified catalog, database, and table.
module Amazonka.Athena.GetTableMetadata
  ( -- * Creating a Request
    GetTableMetadata (..),
    newGetTableMetadata,

    -- * Request Lenses
    getTableMetadata_catalogName,
    getTableMetadata_databaseName,
    getTableMetadata_tableName,

    -- * Destructuring the Response
    GetTableMetadataResponse (..),
    newGetTableMetadataResponse,

    -- * Response Lenses
    getTableMetadataResponse_tableMetadata,
    getTableMetadataResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTableMetadata' smart constructor.
data GetTableMetadata = GetTableMetadata'
  { -- | The name of the data catalog that contains the database and table
    -- metadata to return.
    catalogName :: Prelude.Text,
    -- | The name of the database that contains the table metadata to return.
    databaseName :: Prelude.Text,
    -- | The name of the table for which metadata is returned.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTableMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogName', 'getTableMetadata_catalogName' - The name of the data catalog that contains the database and table
-- metadata to return.
--
-- 'databaseName', 'getTableMetadata_databaseName' - The name of the database that contains the table metadata to return.
--
-- 'tableName', 'getTableMetadata_tableName' - The name of the table for which metadata is returned.
newGetTableMetadata ::
  -- | 'catalogName'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GetTableMetadata
newGetTableMetadata
  pCatalogName_
  pDatabaseName_
  pTableName_ =
    GetTableMetadata'
      { catalogName = pCatalogName_,
        databaseName = pDatabaseName_,
        tableName = pTableName_
      }

-- | The name of the data catalog that contains the database and table
-- metadata to return.
getTableMetadata_catalogName :: Lens.Lens' GetTableMetadata Prelude.Text
getTableMetadata_catalogName = Lens.lens (\GetTableMetadata' {catalogName} -> catalogName) (\s@GetTableMetadata' {} a -> s {catalogName = a} :: GetTableMetadata)

-- | The name of the database that contains the table metadata to return.
getTableMetadata_databaseName :: Lens.Lens' GetTableMetadata Prelude.Text
getTableMetadata_databaseName = Lens.lens (\GetTableMetadata' {databaseName} -> databaseName) (\s@GetTableMetadata' {} a -> s {databaseName = a} :: GetTableMetadata)

-- | The name of the table for which metadata is returned.
getTableMetadata_tableName :: Lens.Lens' GetTableMetadata Prelude.Text
getTableMetadata_tableName = Lens.lens (\GetTableMetadata' {tableName} -> tableName) (\s@GetTableMetadata' {} a -> s {tableName = a} :: GetTableMetadata)

instance Core.AWSRequest GetTableMetadata where
  type
    AWSResponse GetTableMetadata =
      GetTableMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableMetadataResponse'
            Prelude.<$> (x Data..?> "TableMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTableMetadata where
  hashWithSalt _salt GetTableMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` catalogName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData GetTableMetadata where
  rnf GetTableMetadata' {..} =
    Prelude.rnf catalogName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders GetTableMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetTableMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTableMetadata where
  toJSON GetTableMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("CatalogName" Data..= catalogName),
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName)
          ]
      )

instance Data.ToPath GetTableMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTableMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTableMetadataResponse' smart constructor.
data GetTableMetadataResponse = GetTableMetadataResponse'
  { -- | An object that contains table metadata.
    tableMetadata :: Prelude.Maybe TableMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTableMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableMetadata', 'getTableMetadataResponse_tableMetadata' - An object that contains table metadata.
--
-- 'httpStatus', 'getTableMetadataResponse_httpStatus' - The response's http status code.
newGetTableMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTableMetadataResponse
newGetTableMetadataResponse pHttpStatus_ =
  GetTableMetadataResponse'
    { tableMetadata =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains table metadata.
getTableMetadataResponse_tableMetadata :: Lens.Lens' GetTableMetadataResponse (Prelude.Maybe TableMetadata)
getTableMetadataResponse_tableMetadata = Lens.lens (\GetTableMetadataResponse' {tableMetadata} -> tableMetadata) (\s@GetTableMetadataResponse' {} a -> s {tableMetadata = a} :: GetTableMetadataResponse)

-- | The response's http status code.
getTableMetadataResponse_httpStatus :: Lens.Lens' GetTableMetadataResponse Prelude.Int
getTableMetadataResponse_httpStatus = Lens.lens (\GetTableMetadataResponse' {httpStatus} -> httpStatus) (\s@GetTableMetadataResponse' {} a -> s {httpStatus = a} :: GetTableMetadataResponse)

instance Prelude.NFData GetTableMetadataResponse where
  rnf GetTableMetadataResponse' {..} =
    Prelude.rnf tableMetadata
      `Prelude.seq` Prelude.rnf httpStatus
