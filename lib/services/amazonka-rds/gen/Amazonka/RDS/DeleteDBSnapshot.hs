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
-- Module      : Amazonka.RDS.DeleteDBSnapshot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB snapshot. If the snapshot is being copied, the copy
-- operation is terminated.
--
-- The DB snapshot must be in the @available@ state to be deleted.
module Amazonka.RDS.DeleteDBSnapshot
  ( -- * Creating a Request
    DeleteDBSnapshot (..),
    newDeleteDBSnapshot,

    -- * Request Lenses
    deleteDBSnapshot_dbSnapshotIdentifier,

    -- * Destructuring the Response
    DeleteDBSnapshotResponse (..),
    newDeleteDBSnapshotResponse,

    -- * Response Lenses
    deleteDBSnapshotResponse_dbSnapshot,
    deleteDBSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteDBSnapshot' smart constructor.
data DeleteDBSnapshot = DeleteDBSnapshot'
  { -- | The DB snapshot identifier.
    --
    -- Constraints: Must be the name of an existing DB snapshot in the
    -- @available@ state.
    dbSnapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshotIdentifier', 'deleteDBSnapshot_dbSnapshotIdentifier' - The DB snapshot identifier.
--
-- Constraints: Must be the name of an existing DB snapshot in the
-- @available@ state.
newDeleteDBSnapshot ::
  -- | 'dbSnapshotIdentifier'
  Prelude.Text ->
  DeleteDBSnapshot
newDeleteDBSnapshot pDBSnapshotIdentifier_ =
  DeleteDBSnapshot'
    { dbSnapshotIdentifier =
        pDBSnapshotIdentifier_
    }

-- | The DB snapshot identifier.
--
-- Constraints: Must be the name of an existing DB snapshot in the
-- @available@ state.
deleteDBSnapshot_dbSnapshotIdentifier :: Lens.Lens' DeleteDBSnapshot Prelude.Text
deleteDBSnapshot_dbSnapshotIdentifier = Lens.lens (\DeleteDBSnapshot' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@DeleteDBSnapshot' {} a -> s {dbSnapshotIdentifier = a} :: DeleteDBSnapshot)

instance Core.AWSRequest DeleteDBSnapshot where
  type
    AWSResponse DeleteDBSnapshot =
      DeleteDBSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteDBSnapshotResult"
      ( \s h x ->
          DeleteDBSnapshotResponse'
            Prelude.<$> (x Data..@? "DBSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDBSnapshot where
  hashWithSalt _salt DeleteDBSnapshot' {..} =
    _salt `Prelude.hashWithSalt` dbSnapshotIdentifier

instance Prelude.NFData DeleteDBSnapshot where
  rnf DeleteDBSnapshot' {..} =
    Prelude.rnf dbSnapshotIdentifier

instance Data.ToHeaders DeleteDBSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDBSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDBSnapshot where
  toQuery DeleteDBSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDBSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBSnapshotIdentifier" Data.=: dbSnapshotIdentifier
      ]

-- | /See:/ 'newDeleteDBSnapshotResponse' smart constructor.
data DeleteDBSnapshotResponse = DeleteDBSnapshotResponse'
  { dbSnapshot :: Prelude.Maybe DBSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshot', 'deleteDBSnapshotResponse_dbSnapshot' - Undocumented member.
--
-- 'httpStatus', 'deleteDBSnapshotResponse_httpStatus' - The response's http status code.
newDeleteDBSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDBSnapshotResponse
newDeleteDBSnapshotResponse pHttpStatus_ =
  DeleteDBSnapshotResponse'
    { dbSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteDBSnapshotResponse_dbSnapshot :: Lens.Lens' DeleteDBSnapshotResponse (Prelude.Maybe DBSnapshot)
deleteDBSnapshotResponse_dbSnapshot = Lens.lens (\DeleteDBSnapshotResponse' {dbSnapshot} -> dbSnapshot) (\s@DeleteDBSnapshotResponse' {} a -> s {dbSnapshot = a} :: DeleteDBSnapshotResponse)

-- | The response's http status code.
deleteDBSnapshotResponse_httpStatus :: Lens.Lens' DeleteDBSnapshotResponse Prelude.Int
deleteDBSnapshotResponse_httpStatus = Lens.lens (\DeleteDBSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteDBSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteDBSnapshotResponse)

instance Prelude.NFData DeleteDBSnapshotResponse where
  rnf DeleteDBSnapshotResponse' {..} =
    Prelude.rnf dbSnapshot
      `Prelude.seq` Prelude.rnf httpStatus
