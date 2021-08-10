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
-- Module      : Network.AWS.Lightsail.DeleteDiskSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified disk snapshot.
--
-- When you make periodic snapshots of a disk, the snapshots are
-- incremental, and only the blocks on the device that have changed since
-- your last snapshot are saved in the new snapshot. When you delete a
-- snapshot, only the data not needed for any other snapshot is removed. So
-- regardless of which prior snapshots have been deleted, all active
-- snapshots will have access to all the information needed to restore the
-- disk.
--
-- The @delete disk snapshot@ operation supports tag-based access control
-- via resource tags applied to the resource identified by
-- @disk snapshot name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteDiskSnapshot
  ( -- * Creating a Request
    DeleteDiskSnapshot (..),
    newDeleteDiskSnapshot,

    -- * Request Lenses
    deleteDiskSnapshot_diskSnapshotName,

    -- * Destructuring the Response
    DeleteDiskSnapshotResponse (..),
    newDeleteDiskSnapshotResponse,

    -- * Response Lenses
    deleteDiskSnapshotResponse_operations,
    deleteDiskSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDiskSnapshot' smart constructor.
data DeleteDiskSnapshot = DeleteDiskSnapshot'
  { -- | The name of the disk snapshot you want to delete (e.g.,
    -- @my-disk-snapshot@).
    diskSnapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDiskSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskSnapshotName', 'deleteDiskSnapshot_diskSnapshotName' - The name of the disk snapshot you want to delete (e.g.,
-- @my-disk-snapshot@).
newDeleteDiskSnapshot ::
  -- | 'diskSnapshotName'
  Prelude.Text ->
  DeleteDiskSnapshot
newDeleteDiskSnapshot pDiskSnapshotName_ =
  DeleteDiskSnapshot'
    { diskSnapshotName =
        pDiskSnapshotName_
    }

-- | The name of the disk snapshot you want to delete (e.g.,
-- @my-disk-snapshot@).
deleteDiskSnapshot_diskSnapshotName :: Lens.Lens' DeleteDiskSnapshot Prelude.Text
deleteDiskSnapshot_diskSnapshotName = Lens.lens (\DeleteDiskSnapshot' {diskSnapshotName} -> diskSnapshotName) (\s@DeleteDiskSnapshot' {} a -> s {diskSnapshotName = a} :: DeleteDiskSnapshot)

instance Core.AWSRequest DeleteDiskSnapshot where
  type
    AWSResponse DeleteDiskSnapshot =
      DeleteDiskSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDiskSnapshotResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDiskSnapshot

instance Prelude.NFData DeleteDiskSnapshot

instance Core.ToHeaders DeleteDiskSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteDiskSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteDiskSnapshot where
  toJSON DeleteDiskSnapshot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("diskSnapshotName" Core..= diskSnapshotName)
          ]
      )

instance Core.ToPath DeleteDiskSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDiskSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDiskSnapshotResponse' smart constructor.
data DeleteDiskSnapshotResponse = DeleteDiskSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDiskSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteDiskSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteDiskSnapshotResponse_httpStatus' - The response's http status code.
newDeleteDiskSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDiskSnapshotResponse
newDeleteDiskSnapshotResponse pHttpStatus_ =
  DeleteDiskSnapshotResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteDiskSnapshotResponse_operations :: Lens.Lens' DeleteDiskSnapshotResponse (Prelude.Maybe [Operation])
deleteDiskSnapshotResponse_operations = Lens.lens (\DeleteDiskSnapshotResponse' {operations} -> operations) (\s@DeleteDiskSnapshotResponse' {} a -> s {operations = a} :: DeleteDiskSnapshotResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteDiskSnapshotResponse_httpStatus :: Lens.Lens' DeleteDiskSnapshotResponse Prelude.Int
deleteDiskSnapshotResponse_httpStatus = Lens.lens (\DeleteDiskSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteDiskSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteDiskSnapshotResponse)

instance Prelude.NFData DeleteDiskSnapshotResponse
