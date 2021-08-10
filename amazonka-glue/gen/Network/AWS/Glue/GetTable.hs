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
-- Module      : Network.AWS.Glue.GetTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @Table@ definition in a Data Catalog for a specified
-- table.
module Network.AWS.Glue.GetTable
  ( -- * Creating a Request
    GetTable (..),
    newGetTable,

    -- * Request Lenses
    getTable_catalogId,
    getTable_databaseName,
    getTable_name,

    -- * Destructuring the Response
    GetTableResponse (..),
    newGetTableResponse,

    -- * Response Lenses
    getTableResponse_table,
    getTableResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTable' smart constructor.
data GetTable = GetTable'
  { -- | The ID of the Data Catalog where the table resides. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database in the catalog in which the table resides. For
    -- Hive compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | The name of the table for which to retrieve the definition. For Hive
    -- compatibility, this name is entirely lowercase.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getTable_catalogId' - The ID of the Data Catalog where the table resides. If none is provided,
-- the AWS account ID is used by default.
--
-- 'databaseName', 'getTable_databaseName' - The name of the database in the catalog in which the table resides. For
-- Hive compatibility, this name is entirely lowercase.
--
-- 'name', 'getTable_name' - The name of the table for which to retrieve the definition. For Hive
-- compatibility, this name is entirely lowercase.
newGetTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  GetTable
newGetTable pDatabaseName_ pName_ =
  GetTable'
    { catalogId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      name = pName_
    }

-- | The ID of the Data Catalog where the table resides. If none is provided,
-- the AWS account ID is used by default.
getTable_catalogId :: Lens.Lens' GetTable (Prelude.Maybe Prelude.Text)
getTable_catalogId = Lens.lens (\GetTable' {catalogId} -> catalogId) (\s@GetTable' {} a -> s {catalogId = a} :: GetTable)

-- | The name of the database in the catalog in which the table resides. For
-- Hive compatibility, this name is entirely lowercase.
getTable_databaseName :: Lens.Lens' GetTable Prelude.Text
getTable_databaseName = Lens.lens (\GetTable' {databaseName} -> databaseName) (\s@GetTable' {} a -> s {databaseName = a} :: GetTable)

-- | The name of the table for which to retrieve the definition. For Hive
-- compatibility, this name is entirely lowercase.
getTable_name :: Lens.Lens' GetTable Prelude.Text
getTable_name = Lens.lens (\GetTable' {name} -> name) (\s@GetTable' {} a -> s {name = a} :: GetTable)

instance Core.AWSRequest GetTable where
  type AWSResponse GetTable = GetTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableResponse'
            Prelude.<$> (x Core..?> "Table")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTable

instance Prelude.NFData GetTable

instance Core.ToHeaders GetTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetTable" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTable where
  toJSON GetTable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetTable where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTableResponse' smart constructor.
data GetTableResponse = GetTableResponse'
  { -- | The @Table@ object that defines the specified table.
    table :: Prelude.Maybe Table,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'table', 'getTableResponse_table' - The @Table@ object that defines the specified table.
--
-- 'httpStatus', 'getTableResponse_httpStatus' - The response's http status code.
newGetTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTableResponse
newGetTableResponse pHttpStatus_ =
  GetTableResponse'
    { table = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Table@ object that defines the specified table.
getTableResponse_table :: Lens.Lens' GetTableResponse (Prelude.Maybe Table)
getTableResponse_table = Lens.lens (\GetTableResponse' {table} -> table) (\s@GetTableResponse' {} a -> s {table = a} :: GetTableResponse)

-- | The response's http status code.
getTableResponse_httpStatus :: Lens.Lens' GetTableResponse Prelude.Int
getTableResponse_httpStatus = Lens.lens (\GetTableResponse' {httpStatus} -> httpStatus) (\s@GetTableResponse' {} a -> s {httpStatus = a} :: GetTableResponse)

instance Prelude.NFData GetTableResponse
