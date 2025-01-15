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
-- Module      : Amazonka.Glue.DeleteDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a specified database from a Data Catalog.
--
-- After completing this operation, you no longer have access to the tables
-- (and all table versions and partitions that might belong to the tables)
-- and the user-defined functions in the deleted database. Glue deletes
-- these \"orphaned\" resources asynchronously in a timely manner, at the
-- discretion of the service.
--
-- To ensure the immediate deletion of all related resources, before
-- calling @DeleteDatabase@, use @DeleteTableVersion@ or
-- @BatchDeleteTableVersion@, @DeletePartition@ or @BatchDeletePartition@,
-- @DeleteUserDefinedFunction@, and @DeleteTable@ or @BatchDeleteTable@, to
-- delete any resources that belong to the database.
module Amazonka.Glue.DeleteDatabase
  ( -- * Creating a Request
    DeleteDatabase (..),
    newDeleteDatabase,

    -- * Request Lenses
    deleteDatabase_catalogId,
    deleteDatabase_name,

    -- * Destructuring the Response
    DeleteDatabaseResponse (..),
    newDeleteDatabaseResponse,

    -- * Response Lenses
    deleteDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDatabase' smart constructor.
data DeleteDatabase = DeleteDatabase'
  { -- | The ID of the Data Catalog in which the database resides. If none is
    -- provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database to delete. For Hive compatibility, this must be
    -- all lowercase.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteDatabase_catalogId' - The ID of the Data Catalog in which the database resides. If none is
-- provided, the Amazon Web Services account ID is used by default.
--
-- 'name', 'deleteDatabase_name' - The name of the database to delete. For Hive compatibility, this must be
-- all lowercase.
newDeleteDatabase ::
  -- | 'name'
  Prelude.Text ->
  DeleteDatabase
newDeleteDatabase pName_ =
  DeleteDatabase'
    { catalogId = Prelude.Nothing,
      name = pName_
    }

-- | The ID of the Data Catalog in which the database resides. If none is
-- provided, the Amazon Web Services account ID is used by default.
deleteDatabase_catalogId :: Lens.Lens' DeleteDatabase (Prelude.Maybe Prelude.Text)
deleteDatabase_catalogId = Lens.lens (\DeleteDatabase' {catalogId} -> catalogId) (\s@DeleteDatabase' {} a -> s {catalogId = a} :: DeleteDatabase)

-- | The name of the database to delete. For Hive compatibility, this must be
-- all lowercase.
deleteDatabase_name :: Lens.Lens' DeleteDatabase Prelude.Text
deleteDatabase_name = Lens.lens (\DeleteDatabase' {name} -> name) (\s@DeleteDatabase' {} a -> s {name = a} :: DeleteDatabase)

instance Core.AWSRequest DeleteDatabase where
  type
    AWSResponse DeleteDatabase =
      DeleteDatabaseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDatabaseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDatabase where
  hashWithSalt _salt DeleteDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteDatabase where
  rnf DeleteDatabase' {..} =
    Prelude.rnf catalogId `Prelude.seq`
      Prelude.rnf name

instance Data.ToHeaders DeleteDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.DeleteDatabase" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDatabase where
  toJSON DeleteDatabase' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath DeleteDatabase where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDatabase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDatabaseResponse' smart constructor.
data DeleteDatabaseResponse = DeleteDatabaseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDatabaseResponse_httpStatus' - The response's http status code.
newDeleteDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDatabaseResponse
newDeleteDatabaseResponse pHttpStatus_ =
  DeleteDatabaseResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteDatabaseResponse_httpStatus :: Lens.Lens' DeleteDatabaseResponse Prelude.Int
deleteDatabaseResponse_httpStatus = Lens.lens (\DeleteDatabaseResponse' {httpStatus} -> httpStatus) (\s@DeleteDatabaseResponse' {} a -> s {httpStatus = a} :: DeleteDatabaseResponse)

instance Prelude.NFData DeleteDatabaseResponse where
  rnf DeleteDatabaseResponse' {..} =
    Prelude.rnf httpStatus
