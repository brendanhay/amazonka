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
-- Module      : Amazonka.LakeFormation.UpdateTableObjects
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the manifest of Amazon S3 objects that make up the specified
-- governed table.
module Amazonka.LakeFormation.UpdateTableObjects
  ( -- * Creating a Request
    UpdateTableObjects (..),
    newUpdateTableObjects,

    -- * Request Lenses
    updateTableObjects_catalogId,
    updateTableObjects_transactionId,
    updateTableObjects_databaseName,
    updateTableObjects_tableName,
    updateTableObjects_writeOperations,

    -- * Destructuring the Response
    UpdateTableObjectsResponse (..),
    newUpdateTableObjectsResponse,

    -- * Response Lenses
    updateTableObjectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTableObjects' smart constructor.
data UpdateTableObjects = UpdateTableObjects'
  { -- | The catalog containing the governed table to update. Defaults to the
    -- caller’s account ID.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The transaction at which to do the write.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The database containing the governed table to update.
    databaseName :: Prelude.Text,
    -- | The governed table to update.
    tableName :: Prelude.Text,
    -- | A list of @WriteOperation@ objects that define an object to add to or
    -- delete from the manifest for a governed table.
    writeOperations :: Prelude.NonEmpty WriteOperation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTableObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateTableObjects_catalogId' - The catalog containing the governed table to update. Defaults to the
-- caller’s account ID.
--
-- 'transactionId', 'updateTableObjects_transactionId' - The transaction at which to do the write.
--
-- 'databaseName', 'updateTableObjects_databaseName' - The database containing the governed table to update.
--
-- 'tableName', 'updateTableObjects_tableName' - The governed table to update.
--
-- 'writeOperations', 'updateTableObjects_writeOperations' - A list of @WriteOperation@ objects that define an object to add to or
-- delete from the manifest for a governed table.
newUpdateTableObjects ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'writeOperations'
  Prelude.NonEmpty WriteOperation ->
  UpdateTableObjects
newUpdateTableObjects
  pDatabaseName_
  pTableName_
  pWriteOperations_ =
    UpdateTableObjects'
      { catalogId = Prelude.Nothing,
        transactionId = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        writeOperations =
          Lens.coerced Lens.# pWriteOperations_
      }

-- | The catalog containing the governed table to update. Defaults to the
-- caller’s account ID.
updateTableObjects_catalogId :: Lens.Lens' UpdateTableObjects (Prelude.Maybe Prelude.Text)
updateTableObjects_catalogId = Lens.lens (\UpdateTableObjects' {catalogId} -> catalogId) (\s@UpdateTableObjects' {} a -> s {catalogId = a} :: UpdateTableObjects)

-- | The transaction at which to do the write.
updateTableObjects_transactionId :: Lens.Lens' UpdateTableObjects (Prelude.Maybe Prelude.Text)
updateTableObjects_transactionId = Lens.lens (\UpdateTableObjects' {transactionId} -> transactionId) (\s@UpdateTableObjects' {} a -> s {transactionId = a} :: UpdateTableObjects)

-- | The database containing the governed table to update.
updateTableObjects_databaseName :: Lens.Lens' UpdateTableObjects Prelude.Text
updateTableObjects_databaseName = Lens.lens (\UpdateTableObjects' {databaseName} -> databaseName) (\s@UpdateTableObjects' {} a -> s {databaseName = a} :: UpdateTableObjects)

-- | The governed table to update.
updateTableObjects_tableName :: Lens.Lens' UpdateTableObjects Prelude.Text
updateTableObjects_tableName = Lens.lens (\UpdateTableObjects' {tableName} -> tableName) (\s@UpdateTableObjects' {} a -> s {tableName = a} :: UpdateTableObjects)

-- | A list of @WriteOperation@ objects that define an object to add to or
-- delete from the manifest for a governed table.
updateTableObjects_writeOperations :: Lens.Lens' UpdateTableObjects (Prelude.NonEmpty WriteOperation)
updateTableObjects_writeOperations = Lens.lens (\UpdateTableObjects' {writeOperations} -> writeOperations) (\s@UpdateTableObjects' {} a -> s {writeOperations = a} :: UpdateTableObjects) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateTableObjects where
  type
    AWSResponse UpdateTableObjects =
      UpdateTableObjectsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTableObjectsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTableObjects where
  hashWithSalt _salt UpdateTableObjects' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` writeOperations

instance Prelude.NFData UpdateTableObjects where
  rnf UpdateTableObjects' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf writeOperations

instance Data.ToHeaders UpdateTableObjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTableObjects where
  toJSON UpdateTableObjects' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("TransactionId" Data..=) Prelude.<$> transactionId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ("WriteOperations" Data..= writeOperations)
          ]
      )

instance Data.ToPath UpdateTableObjects where
  toPath = Prelude.const "/UpdateTableObjects"

instance Data.ToQuery UpdateTableObjects where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTableObjectsResponse' smart constructor.
data UpdateTableObjectsResponse = UpdateTableObjectsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTableObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTableObjectsResponse_httpStatus' - The response's http status code.
newUpdateTableObjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTableObjectsResponse
newUpdateTableObjectsResponse pHttpStatus_ =
  UpdateTableObjectsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateTableObjectsResponse_httpStatus :: Lens.Lens' UpdateTableObjectsResponse Prelude.Int
updateTableObjectsResponse_httpStatus = Lens.lens (\UpdateTableObjectsResponse' {httpStatus} -> httpStatus) (\s@UpdateTableObjectsResponse' {} a -> s {httpStatus = a} :: UpdateTableObjectsResponse)

instance Prelude.NFData UpdateTableObjectsResponse where
  rnf UpdateTableObjectsResponse' {..} =
    Prelude.rnf httpStatus
