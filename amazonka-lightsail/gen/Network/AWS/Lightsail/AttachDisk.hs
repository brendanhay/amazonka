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
-- Module      : Network.AWS.Lightsail.AttachDisk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a block storage disk to a running or stopped Lightsail instance
-- and exposes it to the instance with the specified disk name.
--
-- The @attach disk@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @disk name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.AttachDisk
  ( -- * Creating a Request
    AttachDisk (..),
    newAttachDisk,

    -- * Request Lenses
    attachDisk_diskName,
    attachDisk_instanceName,
    attachDisk_diskPath,

    -- * Destructuring the Response
    AttachDiskResponse (..),
    newAttachDiskResponse,

    -- * Response Lenses
    attachDiskResponse_operations,
    attachDiskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachDisk' smart constructor.
data AttachDisk = AttachDisk'
  { -- | The unique Lightsail disk name (e.g., @my-disk@).
    diskName :: Core.Text,
    -- | The name of the Lightsail instance where you want to utilize the storage
    -- disk.
    instanceName :: Core.Text,
    -- | The disk path to expose to the instance (e.g., @\/dev\/xvdf@).
    diskPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskName', 'attachDisk_diskName' - The unique Lightsail disk name (e.g., @my-disk@).
--
-- 'instanceName', 'attachDisk_instanceName' - The name of the Lightsail instance where you want to utilize the storage
-- disk.
--
-- 'diskPath', 'attachDisk_diskPath' - The disk path to expose to the instance (e.g., @\/dev\/xvdf@).
newAttachDisk ::
  -- | 'diskName'
  Core.Text ->
  -- | 'instanceName'
  Core.Text ->
  -- | 'diskPath'
  Core.Text ->
  AttachDisk
newAttachDisk pDiskName_ pInstanceName_ pDiskPath_ =
  AttachDisk'
    { diskName = pDiskName_,
      instanceName = pInstanceName_,
      diskPath = pDiskPath_
    }

-- | The unique Lightsail disk name (e.g., @my-disk@).
attachDisk_diskName :: Lens.Lens' AttachDisk Core.Text
attachDisk_diskName = Lens.lens (\AttachDisk' {diskName} -> diskName) (\s@AttachDisk' {} a -> s {diskName = a} :: AttachDisk)

-- | The name of the Lightsail instance where you want to utilize the storage
-- disk.
attachDisk_instanceName :: Lens.Lens' AttachDisk Core.Text
attachDisk_instanceName = Lens.lens (\AttachDisk' {instanceName} -> instanceName) (\s@AttachDisk' {} a -> s {instanceName = a} :: AttachDisk)

-- | The disk path to expose to the instance (e.g., @\/dev\/xvdf@).
attachDisk_diskPath :: Lens.Lens' AttachDisk Core.Text
attachDisk_diskPath = Lens.lens (\AttachDisk' {diskPath} -> diskPath) (\s@AttachDisk' {} a -> s {diskPath = a} :: AttachDisk)

instance Core.AWSRequest AttachDisk where
  type AWSResponse AttachDisk = AttachDiskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachDiskResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AttachDisk

instance Core.NFData AttachDisk

instance Core.ToHeaders AttachDisk where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.AttachDisk" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AttachDisk where
  toJSON AttachDisk' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("diskName" Core..= diskName),
            Core.Just ("instanceName" Core..= instanceName),
            Core.Just ("diskPath" Core..= diskPath)
          ]
      )

instance Core.ToPath AttachDisk where
  toPath = Core.const "/"

instance Core.ToQuery AttachDisk where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAttachDiskResponse' smart constructor.
data AttachDiskResponse = AttachDiskResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachDiskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'attachDiskResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'attachDiskResponse_httpStatus' - The response's http status code.
newAttachDiskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AttachDiskResponse
newAttachDiskResponse pHttpStatus_ =
  AttachDiskResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
attachDiskResponse_operations :: Lens.Lens' AttachDiskResponse (Core.Maybe [Operation])
attachDiskResponse_operations = Lens.lens (\AttachDiskResponse' {operations} -> operations) (\s@AttachDiskResponse' {} a -> s {operations = a} :: AttachDiskResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
attachDiskResponse_httpStatus :: Lens.Lens' AttachDiskResponse Core.Int
attachDiskResponse_httpStatus = Lens.lens (\AttachDiskResponse' {httpStatus} -> httpStatus) (\s@AttachDiskResponse' {} a -> s {httpStatus = a} :: AttachDiskResponse)

instance Core.NFData AttachDiskResponse
