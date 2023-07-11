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
-- Module      : Amazonka.KeySpaces.DeleteTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteTable@ operation deletes a table and all of its data. After a
-- @DeleteTable@ request is received, the specified table is in the
-- @DELETING@ state until Amazon Keyspaces completes the deletion. If the
-- table is in the @ACTIVE@ state, you can delete it. If a table is either
-- in the @CREATING@ or @UPDATING@ states, then Amazon Keyspaces returns a
-- @ResourceInUseException@. If the specified table does not exist, Amazon
-- Keyspaces returns a @ResourceNotFoundException@. If the table is already
-- in the @DELETING@ state, no error is returned.
module Amazonka.KeySpaces.DeleteTable
  ( -- * Creating a Request
    DeleteTable (..),
    newDeleteTable,

    -- * Request Lenses
    deleteTable_keyspaceName,
    deleteTable_tableName,

    -- * Destructuring the Response
    DeleteTableResponse (..),
    newDeleteTableResponse,

    -- * Response Lenses
    deleteTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTable' smart constructor.
data DeleteTable = DeleteTable'
  { -- | The name of the keyspace of the to be deleted table.
    keyspaceName :: Prelude.Text,
    -- | The name of the table to be deleted.
    tableName :: Prelude.Text
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
-- 'keyspaceName', 'deleteTable_keyspaceName' - The name of the keyspace of the to be deleted table.
--
-- 'tableName', 'deleteTable_tableName' - The name of the table to be deleted.
newDeleteTable ::
  -- | 'keyspaceName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  DeleteTable
newDeleteTable pKeyspaceName_ pTableName_ =
  DeleteTable'
    { keyspaceName = pKeyspaceName_,
      tableName = pTableName_
    }

-- | The name of the keyspace of the to be deleted table.
deleteTable_keyspaceName :: Lens.Lens' DeleteTable Prelude.Text
deleteTable_keyspaceName = Lens.lens (\DeleteTable' {keyspaceName} -> keyspaceName) (\s@DeleteTable' {} a -> s {keyspaceName = a} :: DeleteTable)

-- | The name of the table to be deleted.
deleteTable_tableName :: Lens.Lens' DeleteTable Prelude.Text
deleteTable_tableName = Lens.lens (\DeleteTable' {tableName} -> tableName) (\s@DeleteTable' {} a -> s {tableName = a} :: DeleteTable)

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
    _salt
      `Prelude.hashWithSalt` keyspaceName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData DeleteTable where
  rnf DeleteTable' {..} =
    Prelude.rnf keyspaceName
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders DeleteTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KeyspacesService.DeleteTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTable where
  toJSON DeleteTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("keyspaceName" Data..= keyspaceName),
            Prelude.Just ("tableName" Data..= tableName)
          ]
      )

instance Data.ToPath DeleteTable where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTable where
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
