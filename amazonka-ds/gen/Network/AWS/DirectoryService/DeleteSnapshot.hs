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
-- Module      : Network.AWS.DirectoryService.DeleteSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a directory snapshot.
module Network.AWS.DirectoryService.DeleteSnapshot
  ( -- * Creating a Request
    DeleteSnapshot (..),
    newDeleteSnapshot,

    -- * Request Lenses
    deleteSnapshot_snapshotId,

    -- * Destructuring the Response
    DeleteSnapshotResponse (..),
    newDeleteSnapshotResponse,

    -- * Response Lenses
    deleteSnapshotResponse_snapshotId,
    deleteSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DeleteSnapshot operation.
--
-- /See:/ 'newDeleteSnapshot' smart constructor.
data DeleteSnapshot = DeleteSnapshot'
  { -- | The identifier of the directory snapshot to be deleted.
    snapshotId :: Core.Text
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
-- 'snapshotId', 'deleteSnapshot_snapshotId' - The identifier of the directory snapshot to be deleted.
newDeleteSnapshot ::
  -- | 'snapshotId'
  Core.Text ->
  DeleteSnapshot
newDeleteSnapshot pSnapshotId_ =
  DeleteSnapshot' {snapshotId = pSnapshotId_}

-- | The identifier of the directory snapshot to be deleted.
deleteSnapshot_snapshotId :: Lens.Lens' DeleteSnapshot Core.Text
deleteSnapshot_snapshotId = Lens.lens (\DeleteSnapshot' {snapshotId} -> snapshotId) (\s@DeleteSnapshot' {} a -> s {snapshotId = a} :: DeleteSnapshot)

instance Core.AWSRequest DeleteSnapshot where
  type
    AWSResponse DeleteSnapshot =
      DeleteSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSnapshotResponse'
            Core.<$> (x Core..?> "SnapshotId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSnapshot

instance Core.NFData DeleteSnapshot

instance Core.ToHeaders DeleteSnapshot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DeleteSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteSnapshot where
  toJSON DeleteSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SnapshotId" Core..= snapshotId)]
      )

instance Core.ToPath DeleteSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSnapshot where
  toQuery = Core.const Core.mempty

-- | Contains the results of the DeleteSnapshot operation.
--
-- /See:/ 'newDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  { -- | The identifier of the directory snapshot that was deleted.
    snapshotId :: Core.Maybe Core.Text,
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
-- 'snapshotId', 'deleteSnapshotResponse_snapshotId' - The identifier of the directory snapshot that was deleted.
--
-- 'httpStatus', 'deleteSnapshotResponse_httpStatus' - The response's http status code.
newDeleteSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteSnapshotResponse
newDeleteSnapshotResponse pHttpStatus_ =
  DeleteSnapshotResponse'
    { snapshotId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the directory snapshot that was deleted.
deleteSnapshotResponse_snapshotId :: Lens.Lens' DeleteSnapshotResponse (Core.Maybe Core.Text)
deleteSnapshotResponse_snapshotId = Lens.lens (\DeleteSnapshotResponse' {snapshotId} -> snapshotId) (\s@DeleteSnapshotResponse' {} a -> s {snapshotId = a} :: DeleteSnapshotResponse)

-- | The response's http status code.
deleteSnapshotResponse_httpStatus :: Lens.Lens' DeleteSnapshotResponse Core.Int
deleteSnapshotResponse_httpStatus = Lens.lens (\DeleteSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteSnapshotResponse)

instance Core.NFData DeleteSnapshotResponse
