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
-- Module      : Network.AWS.DeviceFarm.DeleteDevicePool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device pool given the pool ARN. Does not allow deletion of
-- curated pools owned by the system.
module Network.AWS.DeviceFarm.DeleteDevicePool
  ( -- * Creating a Request
    DeleteDevicePool (..),
    newDeleteDevicePool,

    -- * Request Lenses
    deleteDevicePool_arn,

    -- * Destructuring the Response
    DeleteDevicePoolResponse (..),
    newDeleteDevicePoolResponse,

    -- * Response Lenses
    deleteDevicePoolResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the delete device pool operation.
--
-- /See:/ 'newDeleteDevicePool' smart constructor.
data DeleteDevicePool = DeleteDevicePool'
  { -- | Represents the Amazon Resource Name (ARN) of the Device Farm device pool
    -- to delete.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDevicePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteDevicePool_arn' - Represents the Amazon Resource Name (ARN) of the Device Farm device pool
-- to delete.
newDeleteDevicePool ::
  -- | 'arn'
  Core.Text ->
  DeleteDevicePool
newDeleteDevicePool pArn_ =
  DeleteDevicePool' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm device pool
-- to delete.
deleteDevicePool_arn :: Lens.Lens' DeleteDevicePool Core.Text
deleteDevicePool_arn = Lens.lens (\DeleteDevicePool' {arn} -> arn) (\s@DeleteDevicePool' {} a -> s {arn = a} :: DeleteDevicePool)

instance Core.AWSRequest DeleteDevicePool where
  type
    AWSResponse DeleteDevicePool =
      DeleteDevicePoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDevicePoolResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDevicePool

instance Core.NFData DeleteDevicePool

instance Core.ToHeaders DeleteDevicePool where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.DeleteDevicePool" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDevicePool where
  toJSON DeleteDevicePool' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath DeleteDevicePool where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDevicePool where
  toQuery = Core.const Core.mempty

-- | Represents the result of a delete device pool request.
--
-- /See:/ 'newDeleteDevicePoolResponse' smart constructor.
data DeleteDevicePoolResponse = DeleteDevicePoolResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDevicePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDevicePoolResponse_httpStatus' - The response's http status code.
newDeleteDevicePoolResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDevicePoolResponse
newDeleteDevicePoolResponse pHttpStatus_ =
  DeleteDevicePoolResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDevicePoolResponse_httpStatus :: Lens.Lens' DeleteDevicePoolResponse Core.Int
deleteDevicePoolResponse_httpStatus = Lens.lens (\DeleteDevicePoolResponse' {httpStatus} -> httpStatus) (\s@DeleteDevicePoolResponse' {} a -> s {httpStatus = a} :: DeleteDevicePoolResponse)

instance Core.NFData DeleteDevicePoolResponse
