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
-- Module      : Network.AWS.Lightsail.DeleteDisk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified block storage disk. The disk must be in the
-- @available@ state (not attached to a Lightsail instance).
--
-- The disk may remain in the @deleting@ state for several minutes.
--
-- The @delete disk@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @disk name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteDisk
  ( -- * Creating a Request
    DeleteDisk (..),
    newDeleteDisk,

    -- * Request Lenses
    deleteDisk_forceDeleteAddOns,
    deleteDisk_diskName,

    -- * Destructuring the Response
    DeleteDiskResponse (..),
    newDeleteDiskResponse,

    -- * Response Lenses
    deleteDiskResponse_operations,
    deleteDiskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDisk' smart constructor.
data DeleteDisk = DeleteDisk'
  { -- | A Boolean value to indicate whether to delete the enabled add-ons for
    -- the disk.
    forceDeleteAddOns :: Core.Maybe Core.Bool,
    -- | The unique name of the disk you want to delete (e.g., @my-disk@).
    diskName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDeleteAddOns', 'deleteDisk_forceDeleteAddOns' - A Boolean value to indicate whether to delete the enabled add-ons for
-- the disk.
--
-- 'diskName', 'deleteDisk_diskName' - The unique name of the disk you want to delete (e.g., @my-disk@).
newDeleteDisk ::
  -- | 'diskName'
  Core.Text ->
  DeleteDisk
newDeleteDisk pDiskName_ =
  DeleteDisk'
    { forceDeleteAddOns = Core.Nothing,
      diskName = pDiskName_
    }

-- | A Boolean value to indicate whether to delete the enabled add-ons for
-- the disk.
deleteDisk_forceDeleteAddOns :: Lens.Lens' DeleteDisk (Core.Maybe Core.Bool)
deleteDisk_forceDeleteAddOns = Lens.lens (\DeleteDisk' {forceDeleteAddOns} -> forceDeleteAddOns) (\s@DeleteDisk' {} a -> s {forceDeleteAddOns = a} :: DeleteDisk)

-- | The unique name of the disk you want to delete (e.g., @my-disk@).
deleteDisk_diskName :: Lens.Lens' DeleteDisk Core.Text
deleteDisk_diskName = Lens.lens (\DeleteDisk' {diskName} -> diskName) (\s@DeleteDisk' {} a -> s {diskName = a} :: DeleteDisk)

instance Core.AWSRequest DeleteDisk where
  type AWSResponse DeleteDisk = DeleteDiskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDiskResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDisk

instance Core.NFData DeleteDisk

instance Core.ToHeaders DeleteDisk where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.DeleteDisk" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDisk where
  toJSON DeleteDisk' {..} =
    Core.object
      ( Core.catMaybes
          [ ("forceDeleteAddOns" Core..=)
              Core.<$> forceDeleteAddOns,
            Core.Just ("diskName" Core..= diskName)
          ]
      )

instance Core.ToPath DeleteDisk where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDisk where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDiskResponse' smart constructor.
data DeleteDiskResponse = DeleteDiskResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDiskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteDiskResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteDiskResponse_httpStatus' - The response's http status code.
newDeleteDiskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDiskResponse
newDeleteDiskResponse pHttpStatus_ =
  DeleteDiskResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteDiskResponse_operations :: Lens.Lens' DeleteDiskResponse (Core.Maybe [Operation])
deleteDiskResponse_operations = Lens.lens (\DeleteDiskResponse' {operations} -> operations) (\s@DeleteDiskResponse' {} a -> s {operations = a} :: DeleteDiskResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteDiskResponse_httpStatus :: Lens.Lens' DeleteDiskResponse Core.Int
deleteDiskResponse_httpStatus = Lens.lens (\DeleteDiskResponse' {httpStatus} -> httpStatus) (\s@DeleteDiskResponse' {} a -> s {httpStatus = a} :: DeleteDiskResponse)

instance Core.NFData DeleteDiskResponse
