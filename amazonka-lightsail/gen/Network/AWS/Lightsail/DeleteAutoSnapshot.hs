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
-- Module      : Network.AWS.Lightsail.DeleteAutoSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an automatic snapshot of an instance or disk. For more
-- information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteAutoSnapshot
  ( -- * Creating a Request
    DeleteAutoSnapshot (..),
    newDeleteAutoSnapshot,

    -- * Request Lenses
    deleteAutoSnapshot_resourceName,
    deleteAutoSnapshot_date,

    -- * Destructuring the Response
    DeleteAutoSnapshotResponse (..),
    newDeleteAutoSnapshotResponse,

    -- * Response Lenses
    deleteAutoSnapshotResponse_operations,
    deleteAutoSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAutoSnapshot' smart constructor.
data DeleteAutoSnapshot = DeleteAutoSnapshot'
  { -- | The name of the source instance or disk from which to delete the
    -- automatic snapshot.
    resourceName :: Core.Text,
    -- | The date of the automatic snapshot to delete in @YYYY-MM-DD@ format. Use
    -- the @get auto snapshots@ operation to get the available automatic
    -- snapshots for a resource.
    date :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAutoSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'deleteAutoSnapshot_resourceName' - The name of the source instance or disk from which to delete the
-- automatic snapshot.
--
-- 'date', 'deleteAutoSnapshot_date' - The date of the automatic snapshot to delete in @YYYY-MM-DD@ format. Use
-- the @get auto snapshots@ operation to get the available automatic
-- snapshots for a resource.
newDeleteAutoSnapshot ::
  -- | 'resourceName'
  Core.Text ->
  -- | 'date'
  Core.Text ->
  DeleteAutoSnapshot
newDeleteAutoSnapshot pResourceName_ pDate_ =
  DeleteAutoSnapshot'
    { resourceName = pResourceName_,
      date = pDate_
    }

-- | The name of the source instance or disk from which to delete the
-- automatic snapshot.
deleteAutoSnapshot_resourceName :: Lens.Lens' DeleteAutoSnapshot Core.Text
deleteAutoSnapshot_resourceName = Lens.lens (\DeleteAutoSnapshot' {resourceName} -> resourceName) (\s@DeleteAutoSnapshot' {} a -> s {resourceName = a} :: DeleteAutoSnapshot)

-- | The date of the automatic snapshot to delete in @YYYY-MM-DD@ format. Use
-- the @get auto snapshots@ operation to get the available automatic
-- snapshots for a resource.
deleteAutoSnapshot_date :: Lens.Lens' DeleteAutoSnapshot Core.Text
deleteAutoSnapshot_date = Lens.lens (\DeleteAutoSnapshot' {date} -> date) (\s@DeleteAutoSnapshot' {} a -> s {date = a} :: DeleteAutoSnapshot)

instance Core.AWSRequest DeleteAutoSnapshot where
  type
    AWSResponse DeleteAutoSnapshot =
      DeleteAutoSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAutoSnapshotResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAutoSnapshot

instance Core.NFData DeleteAutoSnapshot

instance Core.ToHeaders DeleteAutoSnapshot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteAutoSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAutoSnapshot where
  toJSON DeleteAutoSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceName" Core..= resourceName),
            Core.Just ("date" Core..= date)
          ]
      )

instance Core.ToPath DeleteAutoSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAutoSnapshot where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAutoSnapshotResponse' smart constructor.
data DeleteAutoSnapshotResponse = DeleteAutoSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAutoSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteAutoSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteAutoSnapshotResponse_httpStatus' - The response's http status code.
newDeleteAutoSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteAutoSnapshotResponse
newDeleteAutoSnapshotResponse pHttpStatus_ =
  DeleteAutoSnapshotResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteAutoSnapshotResponse_operations :: Lens.Lens' DeleteAutoSnapshotResponse (Core.Maybe [Operation])
deleteAutoSnapshotResponse_operations = Lens.lens (\DeleteAutoSnapshotResponse' {operations} -> operations) (\s@DeleteAutoSnapshotResponse' {} a -> s {operations = a} :: DeleteAutoSnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteAutoSnapshotResponse_httpStatus :: Lens.Lens' DeleteAutoSnapshotResponse Core.Int
deleteAutoSnapshotResponse_httpStatus = Lens.lens (\DeleteAutoSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteAutoSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteAutoSnapshotResponse)

instance Core.NFData DeleteAutoSnapshotResponse
