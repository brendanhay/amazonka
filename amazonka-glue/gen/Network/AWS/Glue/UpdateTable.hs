{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.UpdateTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a metadata table in the Data Catalog.
module Network.AWS.Glue.UpdateTable
  ( -- * Creating a Request
    UpdateTable (..),
    newUpdateTable,

    -- * Request Lenses
    updateTable_catalogId,
    updateTable_skipArchive,
    updateTable_databaseName,
    updateTable_tableInput,

    -- * Destructuring the Response
    UpdateTableResponse (..),
    newUpdateTableResponse,

    -- * Response Lenses
    updateTableResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { -- | The ID of the Data Catalog where the table resides. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | By default, @UpdateTable@ always creates an archived version of the
    -- table before updating it. However, if @skipArchive@ is set to true,
    -- @UpdateTable@ does not create the archived version.
    skipArchive :: Prelude.Maybe Prelude.Bool,
    -- | The name of the catalog database in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | An updated @TableInput@ object to define the metadata table in the
    -- catalog.
    tableInput :: TableInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateTable_catalogId' - The ID of the Data Catalog where the table resides. If none is provided,
-- the AWS account ID is used by default.
--
-- 'skipArchive', 'updateTable_skipArchive' - By default, @UpdateTable@ always creates an archived version of the
-- table before updating it. However, if @skipArchive@ is set to true,
-- @UpdateTable@ does not create the archived version.
--
-- 'databaseName', 'updateTable_databaseName' - The name of the catalog database in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
--
-- 'tableInput', 'updateTable_tableInput' - An updated @TableInput@ object to define the metadata table in the
-- catalog.
newUpdateTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableInput'
  TableInput ->
  UpdateTable
newUpdateTable pDatabaseName_ pTableInput_ =
  UpdateTable'
    { catalogId = Prelude.Nothing,
      skipArchive = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableInput = pTableInput_
    }

-- | The ID of the Data Catalog where the table resides. If none is provided,
-- the AWS account ID is used by default.
updateTable_catalogId :: Lens.Lens' UpdateTable (Prelude.Maybe Prelude.Text)
updateTable_catalogId = Lens.lens (\UpdateTable' {catalogId} -> catalogId) (\s@UpdateTable' {} a -> s {catalogId = a} :: UpdateTable)

-- | By default, @UpdateTable@ always creates an archived version of the
-- table before updating it. However, if @skipArchive@ is set to true,
-- @UpdateTable@ does not create the archived version.
updateTable_skipArchive :: Lens.Lens' UpdateTable (Prelude.Maybe Prelude.Bool)
updateTable_skipArchive = Lens.lens (\UpdateTable' {skipArchive} -> skipArchive) (\s@UpdateTable' {} a -> s {skipArchive = a} :: UpdateTable)

-- | The name of the catalog database in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
updateTable_databaseName :: Lens.Lens' UpdateTable Prelude.Text
updateTable_databaseName = Lens.lens (\UpdateTable' {databaseName} -> databaseName) (\s@UpdateTable' {} a -> s {databaseName = a} :: UpdateTable)

-- | An updated @TableInput@ object to define the metadata table in the
-- catalog.
updateTable_tableInput :: Lens.Lens' UpdateTable TableInput
updateTable_tableInput = Lens.lens (\UpdateTable' {tableInput} -> tableInput) (\s@UpdateTable' {} a -> s {tableInput = a} :: UpdateTable)

instance Prelude.AWSRequest UpdateTable where
  type Rs UpdateTable = UpdateTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTable

instance Prelude.NFData UpdateTable

instance Prelude.ToHeaders UpdateTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.UpdateTable" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateTable where
  toJSON UpdateTable' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            ("SkipArchive" Prelude..=) Prelude.<$> skipArchive,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just ("TableInput" Prelude..= tableInput)
          ]
      )

instance Prelude.ToPath UpdateTable where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTableResponse' smart constructor.
data UpdateTableResponse = UpdateTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTableResponse_httpStatus' - The response's http status code.
newUpdateTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTableResponse
newUpdateTableResponse pHttpStatus_ =
  UpdateTableResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateTableResponse_httpStatus :: Lens.Lens' UpdateTableResponse Prelude.Int
updateTableResponse_httpStatus = Lens.lens (\UpdateTableResponse' {httpStatus} -> httpStatus) (\s@UpdateTableResponse' {} a -> s {httpStatus = a} :: UpdateTableResponse)

instance Prelude.NFData UpdateTableResponse
