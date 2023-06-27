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
-- Module      : Amazonka.LakeFormation.GetDataCellsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a data cells filter.
module Amazonka.LakeFormation.GetDataCellsFilter
  ( -- * Creating a Request
    GetDataCellsFilter (..),
    newGetDataCellsFilter,

    -- * Request Lenses
    getDataCellsFilter_tableCatalogId,
    getDataCellsFilter_databaseName,
    getDataCellsFilter_tableName,
    getDataCellsFilter_name,

    -- * Destructuring the Response
    GetDataCellsFilterResponse (..),
    newGetDataCellsFilterResponse,

    -- * Response Lenses
    getDataCellsFilterResponse_dataCellsFilter,
    getDataCellsFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataCellsFilter' smart constructor.
data GetDataCellsFilter = GetDataCellsFilter'
  { -- | The ID of the catalog to which the table belongs.
    tableCatalogId :: Prelude.Text,
    -- | A database in the Glue Data Catalog.
    databaseName :: Prelude.Text,
    -- | A table in the database.
    tableName :: Prelude.Text,
    -- | The name given by the user to the data filter cell.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataCellsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableCatalogId', 'getDataCellsFilter_tableCatalogId' - The ID of the catalog to which the table belongs.
--
-- 'databaseName', 'getDataCellsFilter_databaseName' - A database in the Glue Data Catalog.
--
-- 'tableName', 'getDataCellsFilter_tableName' - A table in the database.
--
-- 'name', 'getDataCellsFilter_name' - The name given by the user to the data filter cell.
newGetDataCellsFilter ::
  -- | 'tableCatalogId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  GetDataCellsFilter
newGetDataCellsFilter
  pTableCatalogId_
  pDatabaseName_
  pTableName_
  pName_ =
    GetDataCellsFilter'
      { tableCatalogId =
          pTableCatalogId_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        name = pName_
      }

-- | The ID of the catalog to which the table belongs.
getDataCellsFilter_tableCatalogId :: Lens.Lens' GetDataCellsFilter Prelude.Text
getDataCellsFilter_tableCatalogId = Lens.lens (\GetDataCellsFilter' {tableCatalogId} -> tableCatalogId) (\s@GetDataCellsFilter' {} a -> s {tableCatalogId = a} :: GetDataCellsFilter)

-- | A database in the Glue Data Catalog.
getDataCellsFilter_databaseName :: Lens.Lens' GetDataCellsFilter Prelude.Text
getDataCellsFilter_databaseName = Lens.lens (\GetDataCellsFilter' {databaseName} -> databaseName) (\s@GetDataCellsFilter' {} a -> s {databaseName = a} :: GetDataCellsFilter)

-- | A table in the database.
getDataCellsFilter_tableName :: Lens.Lens' GetDataCellsFilter Prelude.Text
getDataCellsFilter_tableName = Lens.lens (\GetDataCellsFilter' {tableName} -> tableName) (\s@GetDataCellsFilter' {} a -> s {tableName = a} :: GetDataCellsFilter)

-- | The name given by the user to the data filter cell.
getDataCellsFilter_name :: Lens.Lens' GetDataCellsFilter Prelude.Text
getDataCellsFilter_name = Lens.lens (\GetDataCellsFilter' {name} -> name) (\s@GetDataCellsFilter' {} a -> s {name = a} :: GetDataCellsFilter)

instance Core.AWSRequest GetDataCellsFilter where
  type
    AWSResponse GetDataCellsFilter =
      GetDataCellsFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataCellsFilterResponse'
            Prelude.<$> (x Data..?> "DataCellsFilter")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataCellsFilter where
  hashWithSalt _salt GetDataCellsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` tableCatalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetDataCellsFilter where
  rnf GetDataCellsFilter' {..} =
    Prelude.rnf tableCatalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetDataCellsFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDataCellsFilter where
  toJSON GetDataCellsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TableCatalogId" Data..= tableCatalogId),
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath GetDataCellsFilter where
  toPath = Prelude.const "/GetDataCellsFilter"

instance Data.ToQuery GetDataCellsFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataCellsFilterResponse' smart constructor.
data GetDataCellsFilterResponse = GetDataCellsFilterResponse'
  { -- | A structure that describes certain columns on certain rows.
    dataCellsFilter :: Prelude.Maybe DataCellsFilter,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataCellsFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataCellsFilter', 'getDataCellsFilterResponse_dataCellsFilter' - A structure that describes certain columns on certain rows.
--
-- 'httpStatus', 'getDataCellsFilterResponse_httpStatus' - The response's http status code.
newGetDataCellsFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataCellsFilterResponse
newGetDataCellsFilterResponse pHttpStatus_ =
  GetDataCellsFilterResponse'
    { dataCellsFilter =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that describes certain columns on certain rows.
getDataCellsFilterResponse_dataCellsFilter :: Lens.Lens' GetDataCellsFilterResponse (Prelude.Maybe DataCellsFilter)
getDataCellsFilterResponse_dataCellsFilter = Lens.lens (\GetDataCellsFilterResponse' {dataCellsFilter} -> dataCellsFilter) (\s@GetDataCellsFilterResponse' {} a -> s {dataCellsFilter = a} :: GetDataCellsFilterResponse)

-- | The response's http status code.
getDataCellsFilterResponse_httpStatus :: Lens.Lens' GetDataCellsFilterResponse Prelude.Int
getDataCellsFilterResponse_httpStatus = Lens.lens (\GetDataCellsFilterResponse' {httpStatus} -> httpStatus) (\s@GetDataCellsFilterResponse' {} a -> s {httpStatus = a} :: GetDataCellsFilterResponse)

instance Prelude.NFData GetDataCellsFilterResponse where
  rnf GetDataCellsFilterResponse' {..} =
    Prelude.rnf dataCellsFilter
      `Prelude.seq` Prelude.rnf httpStatus
