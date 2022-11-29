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
-- Module      : Amazonka.Glue.DeleteTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a table definition from the Data Catalog.
--
-- After completing this operation, you no longer have access to the table
-- versions and partitions that belong to the deleted table. Glue deletes
-- these \"orphaned\" resources asynchronously in a timely manner, at the
-- discretion of the service.
--
-- To ensure the immediate deletion of all related resources, before
-- calling @DeleteTable@, use @DeleteTableVersion@ or
-- @BatchDeleteTableVersion@, and @DeletePartition@ or
-- @BatchDeletePartition@, to delete any resources that belong to the
-- table.
module Amazonka.Glue.DeleteTable
  ( -- * Creating a Request
    DeleteTable (..),
    newDeleteTable,

    -- * Request Lenses
    deleteTable_catalogId,
    deleteTable_transactionId,
    deleteTable_databaseName,
    deleteTable_name,

    -- * Destructuring the Response
    DeleteTableResponse (..),
    newDeleteTableResponse,

    -- * Response Lenses
    deleteTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTable' smart constructor.
data DeleteTable = DeleteTable'
  { -- | The ID of the Data Catalog where the table resides. If none is provided,
    -- the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The transaction ID at which to delete the table contents.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | The name of the table to be deleted. For Hive compatibility, this name
    -- is entirely lowercase.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteTable_catalogId' - The ID of the Data Catalog where the table resides. If none is provided,
-- the Amazon Web Services account ID is used by default.
--
-- 'transactionId', 'deleteTable_transactionId' - The transaction ID at which to delete the table contents.
--
-- 'databaseName', 'deleteTable_databaseName' - The name of the catalog database in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
--
-- 'name', 'deleteTable_name' - The name of the table to be deleted. For Hive compatibility, this name
-- is entirely lowercase.
newDeleteTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  DeleteTable
newDeleteTable pDatabaseName_ pName_ =
  DeleteTable'
    { catalogId = Prelude.Nothing,
      transactionId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      name = pName_
    }

-- | The ID of the Data Catalog where the table resides. If none is provided,
-- the Amazon Web Services account ID is used by default.
deleteTable_catalogId :: Lens.Lens' DeleteTable (Prelude.Maybe Prelude.Text)
deleteTable_catalogId = Lens.lens (\DeleteTable' {catalogId} -> catalogId) (\s@DeleteTable' {} a -> s {catalogId = a} :: DeleteTable)

-- | The transaction ID at which to delete the table contents.
deleteTable_transactionId :: Lens.Lens' DeleteTable (Prelude.Maybe Prelude.Text)
deleteTable_transactionId = Lens.lens (\DeleteTable' {transactionId} -> transactionId) (\s@DeleteTable' {} a -> s {transactionId = a} :: DeleteTable)

-- | The name of the catalog database in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
deleteTable_databaseName :: Lens.Lens' DeleteTable Prelude.Text
deleteTable_databaseName = Lens.lens (\DeleteTable' {databaseName} -> databaseName) (\s@DeleteTable' {} a -> s {databaseName = a} :: DeleteTable)

-- | The name of the table to be deleted. For Hive compatibility, this name
-- is entirely lowercase.
deleteTable_name :: Lens.Lens' DeleteTable Prelude.Text
deleteTable_name = Lens.lens (\DeleteTable' {name} -> name) (\s@DeleteTable' {} a -> s {name = a} :: DeleteTable)

instance Core.AWSRequest DeleteTable where
  type AWSResponse DeleteTable = DeleteTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTable where
  hashWithSalt _salt DeleteTable' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteTable where
  rnf DeleteTable' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders DeleteTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.DeleteTable" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteTable where
  toJSON DeleteTable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            ("TransactionId" Core..=) Prelude.<$> transactionId,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath DeleteTable where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTableResponse' smart constructor.
data DeleteTableResponse = DeleteTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTableResponse_httpStatus' - The response's http status code.
newDeleteTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTableResponse
newDeleteTableResponse pHttpStatus_ =
  DeleteTableResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTableResponse_httpStatus :: Lens.Lens' DeleteTableResponse Prelude.Int
deleteTableResponse_httpStatus = Lens.lens (\DeleteTableResponse' {httpStatus} -> httpStatus) (\s@DeleteTableResponse' {} a -> s {httpStatus = a} :: DeleteTableResponse)

instance Prelude.NFData DeleteTableResponse where
  rnf DeleteTableResponse' {..} = Prelude.rnf httpStatus
