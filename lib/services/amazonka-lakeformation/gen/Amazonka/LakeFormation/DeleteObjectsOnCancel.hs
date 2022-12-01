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
-- Module      : Amazonka.LakeFormation.DeleteObjectsOnCancel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a specific governed table, provides a list of Amazon S3 objects that
-- will be written during the current transaction and that can be
-- automatically deleted if the transaction is canceled. Without this call,
-- no Amazon S3 objects are automatically deleted when a transaction
-- cancels.
--
-- The Glue ETL library function @write_dynamic_frame.from_catalog()@
-- includes an option to automatically call @DeleteObjectsOnCancel@ before
-- writes. For more information, see
-- <https://docs.aws.amazon.com/lake-formation/latest/dg/transactions-data-operations.html#rolling-back-writes Rolling Back Amazon S3 Writes>.
module Amazonka.LakeFormation.DeleteObjectsOnCancel
  ( -- * Creating a Request
    DeleteObjectsOnCancel (..),
    newDeleteObjectsOnCancel,

    -- * Request Lenses
    deleteObjectsOnCancel_catalogId,
    deleteObjectsOnCancel_databaseName,
    deleteObjectsOnCancel_tableName,
    deleteObjectsOnCancel_transactionId,
    deleteObjectsOnCancel_objects,

    -- * Destructuring the Response
    DeleteObjectsOnCancelResponse (..),
    newDeleteObjectsOnCancelResponse,

    -- * Response Lenses
    deleteObjectsOnCancelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteObjectsOnCancel' smart constructor.
data DeleteObjectsOnCancel = DeleteObjectsOnCancel'
  { -- | The Glue data catalog that contains the governed table. Defaults to the
    -- current account ID.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The database that contains the governed table.
    databaseName :: Prelude.Text,
    -- | The name of the governed table.
    tableName :: Prelude.Text,
    -- | ID of the transaction that the writes occur in.
    transactionId :: Prelude.Text,
    -- | A list of VirtualObject structures, which indicates the Amazon S3
    -- objects to be deleted if the transaction cancels.
    objects :: Prelude.NonEmpty VirtualObject
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjectsOnCancel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteObjectsOnCancel_catalogId' - The Glue data catalog that contains the governed table. Defaults to the
-- current account ID.
--
-- 'databaseName', 'deleteObjectsOnCancel_databaseName' - The database that contains the governed table.
--
-- 'tableName', 'deleteObjectsOnCancel_tableName' - The name of the governed table.
--
-- 'transactionId', 'deleteObjectsOnCancel_transactionId' - ID of the transaction that the writes occur in.
--
-- 'objects', 'deleteObjectsOnCancel_objects' - A list of VirtualObject structures, which indicates the Amazon S3
-- objects to be deleted if the transaction cancels.
newDeleteObjectsOnCancel ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'transactionId'
  Prelude.Text ->
  -- | 'objects'
  Prelude.NonEmpty VirtualObject ->
  DeleteObjectsOnCancel
newDeleteObjectsOnCancel
  pDatabaseName_
  pTableName_
  pTransactionId_
  pObjects_ =
    DeleteObjectsOnCancel'
      { catalogId = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        transactionId = pTransactionId_,
        objects = Lens.coerced Lens.# pObjects_
      }

-- | The Glue data catalog that contains the governed table. Defaults to the
-- current account ID.
deleteObjectsOnCancel_catalogId :: Lens.Lens' DeleteObjectsOnCancel (Prelude.Maybe Prelude.Text)
deleteObjectsOnCancel_catalogId = Lens.lens (\DeleteObjectsOnCancel' {catalogId} -> catalogId) (\s@DeleteObjectsOnCancel' {} a -> s {catalogId = a} :: DeleteObjectsOnCancel)

-- | The database that contains the governed table.
deleteObjectsOnCancel_databaseName :: Lens.Lens' DeleteObjectsOnCancel Prelude.Text
deleteObjectsOnCancel_databaseName = Lens.lens (\DeleteObjectsOnCancel' {databaseName} -> databaseName) (\s@DeleteObjectsOnCancel' {} a -> s {databaseName = a} :: DeleteObjectsOnCancel)

-- | The name of the governed table.
deleteObjectsOnCancel_tableName :: Lens.Lens' DeleteObjectsOnCancel Prelude.Text
deleteObjectsOnCancel_tableName = Lens.lens (\DeleteObjectsOnCancel' {tableName} -> tableName) (\s@DeleteObjectsOnCancel' {} a -> s {tableName = a} :: DeleteObjectsOnCancel)

-- | ID of the transaction that the writes occur in.
deleteObjectsOnCancel_transactionId :: Lens.Lens' DeleteObjectsOnCancel Prelude.Text
deleteObjectsOnCancel_transactionId = Lens.lens (\DeleteObjectsOnCancel' {transactionId} -> transactionId) (\s@DeleteObjectsOnCancel' {} a -> s {transactionId = a} :: DeleteObjectsOnCancel)

-- | A list of VirtualObject structures, which indicates the Amazon S3
-- objects to be deleted if the transaction cancels.
deleteObjectsOnCancel_objects :: Lens.Lens' DeleteObjectsOnCancel (Prelude.NonEmpty VirtualObject)
deleteObjectsOnCancel_objects = Lens.lens (\DeleteObjectsOnCancel' {objects} -> objects) (\s@DeleteObjectsOnCancel' {} a -> s {objects = a} :: DeleteObjectsOnCancel) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteObjectsOnCancel where
  type
    AWSResponse DeleteObjectsOnCancel =
      DeleteObjectsOnCancelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteObjectsOnCancelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteObjectsOnCancel where
  hashWithSalt _salt DeleteObjectsOnCancel' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` objects

instance Prelude.NFData DeleteObjectsOnCancel where
  rnf DeleteObjectsOnCancel' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf objects

instance Core.ToHeaders DeleteObjectsOnCancel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteObjectsOnCancel where
  toJSON DeleteObjectsOnCancel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just ("TransactionId" Core..= transactionId),
            Prelude.Just ("Objects" Core..= objects)
          ]
      )

instance Core.ToPath DeleteObjectsOnCancel where
  toPath = Prelude.const "/DeleteObjectsOnCancel"

instance Core.ToQuery DeleteObjectsOnCancel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteObjectsOnCancelResponse' smart constructor.
data DeleteObjectsOnCancelResponse = DeleteObjectsOnCancelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjectsOnCancelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteObjectsOnCancelResponse_httpStatus' - The response's http status code.
newDeleteObjectsOnCancelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteObjectsOnCancelResponse
newDeleteObjectsOnCancelResponse pHttpStatus_ =
  DeleteObjectsOnCancelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteObjectsOnCancelResponse_httpStatus :: Lens.Lens' DeleteObjectsOnCancelResponse Prelude.Int
deleteObjectsOnCancelResponse_httpStatus = Lens.lens (\DeleteObjectsOnCancelResponse' {httpStatus} -> httpStatus) (\s@DeleteObjectsOnCancelResponse' {} a -> s {httpStatus = a} :: DeleteObjectsOnCancelResponse)

instance Prelude.NFData DeleteObjectsOnCancelResponse where
  rnf DeleteObjectsOnCancelResponse' {..} =
    Prelude.rnf httpStatus
