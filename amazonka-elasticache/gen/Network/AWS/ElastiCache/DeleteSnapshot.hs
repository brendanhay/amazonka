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
-- Module      : Network.AWS.ElastiCache.DeleteSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing snapshot. When you receive a successful response
-- from this operation, ElastiCache immediately begins deleting the
-- snapshot; you cannot cancel or revert this operation.
--
-- This operation is valid for Redis only.
module Network.AWS.ElastiCache.DeleteSnapshot
  ( -- * Creating a Request
    DeleteSnapshot (..),
    newDeleteSnapshot,

    -- * Request Lenses
    deleteSnapshot_snapshotName,

    -- * Destructuring the Response
    DeleteSnapshotResponse (..),
    newDeleteSnapshotResponse,

    -- * Response Lenses
    deleteSnapshotResponse_snapshot,
    deleteSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteSnapshot@ operation.
--
-- /See:/ 'newDeleteSnapshot' smart constructor.
data DeleteSnapshot = DeleteSnapshot'
  { -- | The name of the snapshot to be deleted.
    snapshotName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotName', 'deleteSnapshot_snapshotName' - The name of the snapshot to be deleted.
newDeleteSnapshot ::
  -- | 'snapshotName'
  Core.Text ->
  DeleteSnapshot
newDeleteSnapshot pSnapshotName_ =
  DeleteSnapshot' {snapshotName = pSnapshotName_}

-- | The name of the snapshot to be deleted.
deleteSnapshot_snapshotName :: Lens.Lens' DeleteSnapshot Core.Text
deleteSnapshot_snapshotName = Lens.lens (\DeleteSnapshot' {snapshotName} -> snapshotName) (\s@DeleteSnapshot' {} a -> s {snapshotName = a} :: DeleteSnapshot)

instance Core.AWSRequest DeleteSnapshot where
  type
    AWSResponse DeleteSnapshot =
      DeleteSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteSnapshotResult"
      ( \s h x ->
          DeleteSnapshotResponse'
            Core.<$> (x Core..@? "Snapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSnapshot

instance Core.NFData DeleteSnapshot

instance Core.ToHeaders DeleteSnapshot where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSnapshot where
  toQuery DeleteSnapshot' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteSnapshot" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "SnapshotName" Core.=: snapshotName
      ]

-- | /See:/ 'newDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  { snapshot :: Core.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'deleteSnapshotResponse_snapshot' - Undocumented member.
--
-- 'httpStatus', 'deleteSnapshotResponse_httpStatus' - The response's http status code.
newDeleteSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteSnapshotResponse
newDeleteSnapshotResponse pHttpStatus_ =
  DeleteSnapshotResponse'
    { snapshot = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteSnapshotResponse_snapshot :: Lens.Lens' DeleteSnapshotResponse (Core.Maybe Snapshot)
deleteSnapshotResponse_snapshot = Lens.lens (\DeleteSnapshotResponse' {snapshot} -> snapshot) (\s@DeleteSnapshotResponse' {} a -> s {snapshot = a} :: DeleteSnapshotResponse)

-- | The response's http status code.
deleteSnapshotResponse_httpStatus :: Lens.Lens' DeleteSnapshotResponse Core.Int
deleteSnapshotResponse_httpStatus = Lens.lens (\DeleteSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteSnapshotResponse)

instance Core.NFData DeleteSnapshotResponse
