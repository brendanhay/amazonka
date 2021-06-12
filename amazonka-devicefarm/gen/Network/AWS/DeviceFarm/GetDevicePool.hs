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
-- Module      : Network.AWS.DeviceFarm.GetDevicePool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a device pool.
module Network.AWS.DeviceFarm.GetDevicePool
  ( -- * Creating a Request
    GetDevicePool (..),
    newGetDevicePool,

    -- * Request Lenses
    getDevicePool_arn,

    -- * Destructuring the Response
    GetDevicePoolResponse (..),
    newGetDevicePoolResponse,

    -- * Response Lenses
    getDevicePoolResponse_devicePool,
    getDevicePoolResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get device pool operation.
--
-- /See:/ 'newGetDevicePool' smart constructor.
data GetDevicePool = GetDevicePool'
  { -- | The device pool\'s ARN.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDevicePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getDevicePool_arn' - The device pool\'s ARN.
newGetDevicePool ::
  -- | 'arn'
  Core.Text ->
  GetDevicePool
newGetDevicePool pArn_ = GetDevicePool' {arn = pArn_}

-- | The device pool\'s ARN.
getDevicePool_arn :: Lens.Lens' GetDevicePool Core.Text
getDevicePool_arn = Lens.lens (\GetDevicePool' {arn} -> arn) (\s@GetDevicePool' {} a -> s {arn = a} :: GetDevicePool)

instance Core.AWSRequest GetDevicePool where
  type
    AWSResponse GetDevicePool =
      GetDevicePoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevicePoolResponse'
            Core.<$> (x Core..?> "devicePool")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDevicePool

instance Core.NFData GetDevicePool

instance Core.ToHeaders GetDevicePool where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetDevicePool" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDevicePool where
  toJSON GetDevicePool' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath GetDevicePool where
  toPath = Core.const "/"

instance Core.ToQuery GetDevicePool where
  toQuery = Core.const Core.mempty

-- | Represents the result of a get device pool request.
--
-- /See:/ 'newGetDevicePoolResponse' smart constructor.
data GetDevicePoolResponse = GetDevicePoolResponse'
  { -- | An object that contains information about the requested device pool.
    devicePool :: Core.Maybe DevicePool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDevicePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devicePool', 'getDevicePoolResponse_devicePool' - An object that contains information about the requested device pool.
--
-- 'httpStatus', 'getDevicePoolResponse_httpStatus' - The response's http status code.
newGetDevicePoolResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDevicePoolResponse
newGetDevicePoolResponse pHttpStatus_ =
  GetDevicePoolResponse'
    { devicePool = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about the requested device pool.
getDevicePoolResponse_devicePool :: Lens.Lens' GetDevicePoolResponse (Core.Maybe DevicePool)
getDevicePoolResponse_devicePool = Lens.lens (\GetDevicePoolResponse' {devicePool} -> devicePool) (\s@GetDevicePoolResponse' {} a -> s {devicePool = a} :: GetDevicePoolResponse)

-- | The response's http status code.
getDevicePoolResponse_httpStatus :: Lens.Lens' GetDevicePoolResponse Core.Int
getDevicePoolResponse_httpStatus = Lens.lens (\GetDevicePoolResponse' {httpStatus} -> httpStatus) (\s@GetDevicePoolResponse' {} a -> s {httpStatus = a} :: GetDevicePoolResponse)

instance Core.NFData GetDevicePoolResponse
