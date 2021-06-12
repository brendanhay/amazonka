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
-- Module      : Network.AWS.RDS.DeleteDBSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB snapshot. If the snapshot is being copied, the copy
-- operation is terminated.
--
-- The DB snapshot must be in the @available@ state to be deleted.
module Network.AWS.RDS.DeleteDBSnapshot
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteDBSnapshot' smart constructor.
data DeleteDBSnapshot = DeleteDBSnapshot'
  { -- | The DB snapshot identifier.
    --
    -- Constraints: Must be the name of an existing DB snapshot in the
    -- @available@ state.
    dbSnapshotIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
deleteDBSnapshot_dbSnapshotIdentifier :: Lens.Lens' DeleteDBSnapshot Core.Text
deleteDBSnapshot_dbSnapshotIdentifier = Lens.lens (\DeleteDBSnapshot' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@DeleteDBSnapshot' {} a -> s {dbSnapshotIdentifier = a} :: DeleteDBSnapshot)

instance Core.AWSRequest DeleteDBSnapshot where
  type
    AWSResponse DeleteDBSnapshot =
      DeleteDBSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteDBSnapshotResult"
      ( \s h x ->
          DeleteDBSnapshotResponse'
            Core.<$> (x Core..@? "DBSnapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDBSnapshot

instance Core.NFData DeleteDBSnapshot

instance Core.ToHeaders DeleteDBSnapshot where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteDBSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDBSnapshot where
  toQuery DeleteDBSnapshot' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteDBSnapshot" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBSnapshotIdentifier" Core.=: dbSnapshotIdentifier
      ]

-- | /See:/ 'newDeleteDBSnapshotResponse' smart constructor.
data DeleteDBSnapshotResponse = DeleteDBSnapshotResponse'
  { dbSnapshot :: Core.Maybe DBSnapshot,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteDBSnapshotResponse
newDeleteDBSnapshotResponse pHttpStatus_ =
  DeleteDBSnapshotResponse'
    { dbSnapshot =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteDBSnapshotResponse_dbSnapshot :: Lens.Lens' DeleteDBSnapshotResponse (Core.Maybe DBSnapshot)
deleteDBSnapshotResponse_dbSnapshot = Lens.lens (\DeleteDBSnapshotResponse' {dbSnapshot} -> dbSnapshot) (\s@DeleteDBSnapshotResponse' {} a -> s {dbSnapshot = a} :: DeleteDBSnapshotResponse)

-- | The response's http status code.
deleteDBSnapshotResponse_httpStatus :: Lens.Lens' DeleteDBSnapshotResponse Core.Int
deleteDBSnapshotResponse_httpStatus = Lens.lens (\DeleteDBSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteDBSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteDBSnapshotResponse)

instance Core.NFData DeleteDBSnapshotResponse
