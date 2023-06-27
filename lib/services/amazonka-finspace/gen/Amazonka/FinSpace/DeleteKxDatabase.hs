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
-- Module      : Amazonka.FinSpace.DeleteKxDatabase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified database and all of its associated data. This
-- action is irreversible. You must copy any data out of the database
-- before deleting it if the data is to be retained.
module Amazonka.FinSpace.DeleteKxDatabase
  ( -- * Creating a Request
    DeleteKxDatabase (..),
    newDeleteKxDatabase,

    -- * Request Lenses
    deleteKxDatabase_environmentId,
    deleteKxDatabase_databaseName,
    deleteKxDatabase_clientToken,

    -- * Destructuring the Response
    DeleteKxDatabaseResponse (..),
    newDeleteKxDatabaseResponse,

    -- * Response Lenses
    deleteKxDatabaseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKxDatabase' smart constructor.
data DeleteKxDatabase = DeleteKxDatabase'
  { -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | The name of the kdb database that you want to delete.
    databaseName :: Prelude.Text,
    -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKxDatabase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'deleteKxDatabase_environmentId' - A unique identifier for the kdb environment.
--
-- 'databaseName', 'deleteKxDatabase_databaseName' - The name of the kdb database that you want to delete.
--
-- 'clientToken', 'deleteKxDatabase_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
newDeleteKxDatabase ::
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  DeleteKxDatabase
newDeleteKxDatabase
  pEnvironmentId_
  pDatabaseName_
  pClientToken_ =
    DeleteKxDatabase'
      { environmentId = pEnvironmentId_,
        databaseName = pDatabaseName_,
        clientToken = pClientToken_
      }

-- | A unique identifier for the kdb environment.
deleteKxDatabase_environmentId :: Lens.Lens' DeleteKxDatabase Prelude.Text
deleteKxDatabase_environmentId = Lens.lens (\DeleteKxDatabase' {environmentId} -> environmentId) (\s@DeleteKxDatabase' {} a -> s {environmentId = a} :: DeleteKxDatabase)

-- | The name of the kdb database that you want to delete.
deleteKxDatabase_databaseName :: Lens.Lens' DeleteKxDatabase Prelude.Text
deleteKxDatabase_databaseName = Lens.lens (\DeleteKxDatabase' {databaseName} -> databaseName) (\s@DeleteKxDatabase' {} a -> s {databaseName = a} :: DeleteKxDatabase)

-- | A token that ensures idempotency. This token expires in 10 minutes.
deleteKxDatabase_clientToken :: Lens.Lens' DeleteKxDatabase Prelude.Text
deleteKxDatabase_clientToken = Lens.lens (\DeleteKxDatabase' {clientToken} -> clientToken) (\s@DeleteKxDatabase' {} a -> s {clientToken = a} :: DeleteKxDatabase)

instance Core.AWSRequest DeleteKxDatabase where
  type
    AWSResponse DeleteKxDatabase =
      DeleteKxDatabaseResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteKxDatabaseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteKxDatabase where
  hashWithSalt _salt DeleteKxDatabase' {..} =
    _salt
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData DeleteKxDatabase where
  rnf DeleteKxDatabase' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders DeleteKxDatabase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteKxDatabase where
  toPath DeleteKxDatabase' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/databases/",
        Data.toBS databaseName
      ]

instance Data.ToQuery DeleteKxDatabase where
  toQuery DeleteKxDatabase' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteKxDatabaseResponse' smart constructor.
data DeleteKxDatabaseResponse = DeleteKxDatabaseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKxDatabaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteKxDatabaseResponse_httpStatus' - The response's http status code.
newDeleteKxDatabaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteKxDatabaseResponse
newDeleteKxDatabaseResponse pHttpStatus_ =
  DeleteKxDatabaseResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteKxDatabaseResponse_httpStatus :: Lens.Lens' DeleteKxDatabaseResponse Prelude.Int
deleteKxDatabaseResponse_httpStatus = Lens.lens (\DeleteKxDatabaseResponse' {httpStatus} -> httpStatus) (\s@DeleteKxDatabaseResponse' {} a -> s {httpStatus = a} :: DeleteKxDatabaseResponse)

instance Prelude.NFData DeleteKxDatabaseResponse where
  rnf DeleteKxDatabaseResponse' {..} =
    Prelude.rnf httpStatus
