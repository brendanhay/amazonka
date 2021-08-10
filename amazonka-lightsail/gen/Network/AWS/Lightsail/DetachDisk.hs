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
-- Module      : Network.AWS.Lightsail.DetachDisk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a stopped block storage disk from a Lightsail instance. Make
-- sure to unmount any file systems on the device within your operating
-- system before stopping the instance and detaching the disk.
--
-- The @detach disk@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @disk name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DetachDisk
  ( -- * Creating a Request
    DetachDisk (..),
    newDetachDisk,

    -- * Request Lenses
    detachDisk_diskName,

    -- * Destructuring the Response
    DetachDiskResponse (..),
    newDetachDiskResponse,

    -- * Response Lenses
    detachDiskResponse_operations,
    detachDiskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachDisk' smart constructor.
data DetachDisk = DetachDisk'
  { -- | The unique name of the disk you want to detach from your instance (e.g.,
    -- @my-disk@).
    diskName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskName', 'detachDisk_diskName' - The unique name of the disk you want to detach from your instance (e.g.,
-- @my-disk@).
newDetachDisk ::
  -- | 'diskName'
  Prelude.Text ->
  DetachDisk
newDetachDisk pDiskName_ =
  DetachDisk' {diskName = pDiskName_}

-- | The unique name of the disk you want to detach from your instance (e.g.,
-- @my-disk@).
detachDisk_diskName :: Lens.Lens' DetachDisk Prelude.Text
detachDisk_diskName = Lens.lens (\DetachDisk' {diskName} -> diskName) (\s@DetachDisk' {} a -> s {diskName = a} :: DetachDisk)

instance Core.AWSRequest DetachDisk where
  type AWSResponse DetachDisk = DetachDiskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachDiskResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetachDisk

instance Prelude.NFData DetachDisk

instance Core.ToHeaders DetachDisk where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DetachDisk" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DetachDisk where
  toJSON DetachDisk' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("diskName" Core..= diskName)]
      )

instance Core.ToPath DetachDisk where
  toPath = Prelude.const "/"

instance Core.ToQuery DetachDisk where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachDiskResponse' smart constructor.
data DetachDiskResponse = DetachDiskResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachDiskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'detachDiskResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'detachDiskResponse_httpStatus' - The response's http status code.
newDetachDiskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachDiskResponse
newDetachDiskResponse pHttpStatus_ =
  DetachDiskResponse'
    { operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
detachDiskResponse_operations :: Lens.Lens' DetachDiskResponse (Prelude.Maybe [Operation])
detachDiskResponse_operations = Lens.lens (\DetachDiskResponse' {operations} -> operations) (\s@DetachDiskResponse' {} a -> s {operations = a} :: DetachDiskResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detachDiskResponse_httpStatus :: Lens.Lens' DetachDiskResponse Prelude.Int
detachDiskResponse_httpStatus = Lens.lens (\DetachDiskResponse' {httpStatus} -> httpStatus) (\s@DetachDiskResponse' {} a -> s {httpStatus = a} :: DetachDiskResponse)

instance Prelude.NFData DetachDiskResponse
