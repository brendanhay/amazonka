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
-- Module      : Network.AWS.Lightsail.DeleteInstanceSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific snapshot of a virtual private server (or /instance/).
--
-- The @delete instance snapshot@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- @instance snapshot name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteInstanceSnapshot
  ( -- * Creating a Request
    DeleteInstanceSnapshot (..),
    newDeleteInstanceSnapshot,

    -- * Request Lenses
    deleteInstanceSnapshot_instanceSnapshotName,

    -- * Destructuring the Response
    DeleteInstanceSnapshotResponse (..),
    newDeleteInstanceSnapshotResponse,

    -- * Response Lenses
    deleteInstanceSnapshotResponse_operations,
    deleteInstanceSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteInstanceSnapshot' smart constructor.
data DeleteInstanceSnapshot = DeleteInstanceSnapshot'
  { -- | The name of the snapshot to delete.
    instanceSnapshotName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInstanceSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceSnapshotName', 'deleteInstanceSnapshot_instanceSnapshotName' - The name of the snapshot to delete.
newDeleteInstanceSnapshot ::
  -- | 'instanceSnapshotName'
  Core.Text ->
  DeleteInstanceSnapshot
newDeleteInstanceSnapshot pInstanceSnapshotName_ =
  DeleteInstanceSnapshot'
    { instanceSnapshotName =
        pInstanceSnapshotName_
    }

-- | The name of the snapshot to delete.
deleteInstanceSnapshot_instanceSnapshotName :: Lens.Lens' DeleteInstanceSnapshot Core.Text
deleteInstanceSnapshot_instanceSnapshotName = Lens.lens (\DeleteInstanceSnapshot' {instanceSnapshotName} -> instanceSnapshotName) (\s@DeleteInstanceSnapshot' {} a -> s {instanceSnapshotName = a} :: DeleteInstanceSnapshot)

instance Core.AWSRequest DeleteInstanceSnapshot where
  type
    AWSResponse DeleteInstanceSnapshot =
      DeleteInstanceSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInstanceSnapshotResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteInstanceSnapshot

instance Core.NFData DeleteInstanceSnapshot

instance Core.ToHeaders DeleteInstanceSnapshot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteInstanceSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteInstanceSnapshot where
  toJSON DeleteInstanceSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "instanceSnapshotName"
                  Core..= instanceSnapshotName
              )
          ]
      )

instance Core.ToPath DeleteInstanceSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery DeleteInstanceSnapshot where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteInstanceSnapshotResponse' smart constructor.
data DeleteInstanceSnapshotResponse = DeleteInstanceSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInstanceSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteInstanceSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteInstanceSnapshotResponse_httpStatus' - The response's http status code.
newDeleteInstanceSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteInstanceSnapshotResponse
newDeleteInstanceSnapshotResponse pHttpStatus_ =
  DeleteInstanceSnapshotResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteInstanceSnapshotResponse_operations :: Lens.Lens' DeleteInstanceSnapshotResponse (Core.Maybe [Operation])
deleteInstanceSnapshotResponse_operations = Lens.lens (\DeleteInstanceSnapshotResponse' {operations} -> operations) (\s@DeleteInstanceSnapshotResponse' {} a -> s {operations = a} :: DeleteInstanceSnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteInstanceSnapshotResponse_httpStatus :: Lens.Lens' DeleteInstanceSnapshotResponse Core.Int
deleteInstanceSnapshotResponse_httpStatus = Lens.lens (\DeleteInstanceSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteInstanceSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteInstanceSnapshotResponse)

instance Core.NFData DeleteInstanceSnapshotResponse
