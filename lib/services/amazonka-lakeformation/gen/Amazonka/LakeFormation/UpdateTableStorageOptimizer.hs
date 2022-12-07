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
-- Module      : Amazonka.LakeFormation.UpdateTableStorageOptimizer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of the storage optimizers for a table.
module Amazonka.LakeFormation.UpdateTableStorageOptimizer
  ( -- * Creating a Request
    UpdateTableStorageOptimizer (..),
    newUpdateTableStorageOptimizer,

    -- * Request Lenses
    updateTableStorageOptimizer_catalogId,
    updateTableStorageOptimizer_databaseName,
    updateTableStorageOptimizer_tableName,
    updateTableStorageOptimizer_storageOptimizerConfig,

    -- * Destructuring the Response
    UpdateTableStorageOptimizerResponse (..),
    newUpdateTableStorageOptimizerResponse,

    -- * Response Lenses
    updateTableStorageOptimizerResponse_result,
    updateTableStorageOptimizerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTableStorageOptimizer' smart constructor.
data UpdateTableStorageOptimizer = UpdateTableStorageOptimizer'
  { -- | The Catalog ID of the table.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | Name of the database where the table is present.
    databaseName :: Prelude.Text,
    -- | Name of the table for which to enable the storage optimizer.
    tableName :: Prelude.Text,
    -- | Name of the table for which to enable the storage optimizer.
    storageOptimizerConfig :: Prelude.HashMap OptimizerType (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTableStorageOptimizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateTableStorageOptimizer_catalogId' - The Catalog ID of the table.
--
-- 'databaseName', 'updateTableStorageOptimizer_databaseName' - Name of the database where the table is present.
--
-- 'tableName', 'updateTableStorageOptimizer_tableName' - Name of the table for which to enable the storage optimizer.
--
-- 'storageOptimizerConfig', 'updateTableStorageOptimizer_storageOptimizerConfig' - Name of the table for which to enable the storage optimizer.
newUpdateTableStorageOptimizer ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  UpdateTableStorageOptimizer
newUpdateTableStorageOptimizer
  pDatabaseName_
  pTableName_ =
    UpdateTableStorageOptimizer'
      { catalogId =
          Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        storageOptimizerConfig = Prelude.mempty
      }

-- | The Catalog ID of the table.
updateTableStorageOptimizer_catalogId :: Lens.Lens' UpdateTableStorageOptimizer (Prelude.Maybe Prelude.Text)
updateTableStorageOptimizer_catalogId = Lens.lens (\UpdateTableStorageOptimizer' {catalogId} -> catalogId) (\s@UpdateTableStorageOptimizer' {} a -> s {catalogId = a} :: UpdateTableStorageOptimizer)

-- | Name of the database where the table is present.
updateTableStorageOptimizer_databaseName :: Lens.Lens' UpdateTableStorageOptimizer Prelude.Text
updateTableStorageOptimizer_databaseName = Lens.lens (\UpdateTableStorageOptimizer' {databaseName} -> databaseName) (\s@UpdateTableStorageOptimizer' {} a -> s {databaseName = a} :: UpdateTableStorageOptimizer)

-- | Name of the table for which to enable the storage optimizer.
updateTableStorageOptimizer_tableName :: Lens.Lens' UpdateTableStorageOptimizer Prelude.Text
updateTableStorageOptimizer_tableName = Lens.lens (\UpdateTableStorageOptimizer' {tableName} -> tableName) (\s@UpdateTableStorageOptimizer' {} a -> s {tableName = a} :: UpdateTableStorageOptimizer)

-- | Name of the table for which to enable the storage optimizer.
updateTableStorageOptimizer_storageOptimizerConfig :: Lens.Lens' UpdateTableStorageOptimizer (Prelude.HashMap OptimizerType (Prelude.HashMap Prelude.Text Prelude.Text))
updateTableStorageOptimizer_storageOptimizerConfig = Lens.lens (\UpdateTableStorageOptimizer' {storageOptimizerConfig} -> storageOptimizerConfig) (\s@UpdateTableStorageOptimizer' {} a -> s {storageOptimizerConfig = a} :: UpdateTableStorageOptimizer) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateTableStorageOptimizer where
  type
    AWSResponse UpdateTableStorageOptimizer =
      UpdateTableStorageOptimizerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTableStorageOptimizerResponse'
            Prelude.<$> (x Data..?> "Result")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTableStorageOptimizer where
  hashWithSalt _salt UpdateTableStorageOptimizer' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` storageOptimizerConfig

instance Prelude.NFData UpdateTableStorageOptimizer where
  rnf UpdateTableStorageOptimizer' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf storageOptimizerConfig

instance Data.ToHeaders UpdateTableStorageOptimizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTableStorageOptimizer where
  toJSON UpdateTableStorageOptimizer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ( "StorageOptimizerConfig"
                  Data..= storageOptimizerConfig
              )
          ]
      )

instance Data.ToPath UpdateTableStorageOptimizer where
  toPath = Prelude.const "/UpdateTableStorageOptimizer"

instance Data.ToQuery UpdateTableStorageOptimizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTableStorageOptimizerResponse' smart constructor.
data UpdateTableStorageOptimizerResponse = UpdateTableStorageOptimizerResponse'
  { -- | A response indicating the success of failure of the operation.
    result :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTableStorageOptimizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'updateTableStorageOptimizerResponse_result' - A response indicating the success of failure of the operation.
--
-- 'httpStatus', 'updateTableStorageOptimizerResponse_httpStatus' - The response's http status code.
newUpdateTableStorageOptimizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTableStorageOptimizerResponse
newUpdateTableStorageOptimizerResponse pHttpStatus_ =
  UpdateTableStorageOptimizerResponse'
    { result =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A response indicating the success of failure of the operation.
updateTableStorageOptimizerResponse_result :: Lens.Lens' UpdateTableStorageOptimizerResponse (Prelude.Maybe Prelude.Text)
updateTableStorageOptimizerResponse_result = Lens.lens (\UpdateTableStorageOptimizerResponse' {result} -> result) (\s@UpdateTableStorageOptimizerResponse' {} a -> s {result = a} :: UpdateTableStorageOptimizerResponse)

-- | The response's http status code.
updateTableStorageOptimizerResponse_httpStatus :: Lens.Lens' UpdateTableStorageOptimizerResponse Prelude.Int
updateTableStorageOptimizerResponse_httpStatus = Lens.lens (\UpdateTableStorageOptimizerResponse' {httpStatus} -> httpStatus) (\s@UpdateTableStorageOptimizerResponse' {} a -> s {httpStatus = a} :: UpdateTableStorageOptimizerResponse)

instance
  Prelude.NFData
    UpdateTableStorageOptimizerResponse
  where
  rnf UpdateTableStorageOptimizerResponse' {..} =
    Prelude.rnf result
      `Prelude.seq` Prelude.rnf httpStatus
