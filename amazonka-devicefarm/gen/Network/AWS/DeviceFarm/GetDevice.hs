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
-- Module      : Network.AWS.DeviceFarm.GetDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a unique device type.
module Network.AWS.DeviceFarm.GetDevice
  ( -- * Creating a Request
    GetDevice (..),
    newGetDevice,

    -- * Request Lenses
    getDevice_arn,

    -- * Destructuring the Response
    GetDeviceResponse (..),
    newGetDeviceResponse,

    -- * Response Lenses
    getDeviceResponse_device,
    getDeviceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get device request.
--
-- /See:/ 'newGetDevice' smart constructor.
data GetDevice = GetDevice'
  { -- | The device type\'s ARN.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getDevice_arn' - The device type\'s ARN.
newGetDevice ::
  -- | 'arn'
  Core.Text ->
  GetDevice
newGetDevice pArn_ = GetDevice' {arn = pArn_}

-- | The device type\'s ARN.
getDevice_arn :: Lens.Lens' GetDevice Core.Text
getDevice_arn = Lens.lens (\GetDevice' {arn} -> arn) (\s@GetDevice' {} a -> s {arn = a} :: GetDevice)

instance Core.AWSRequest GetDevice where
  type AWSResponse GetDevice = GetDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceResponse'
            Core.<$> (x Core..?> "device")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDevice

instance Core.NFData GetDevice

instance Core.ToHeaders GetDevice where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DeviceFarm_20150623.GetDevice" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDevice where
  toJSON GetDevice' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath GetDevice where
  toPath = Core.const "/"

instance Core.ToQuery GetDevice where
  toQuery = Core.const Core.mempty

-- | Represents the result of a get device request.
--
-- /See:/ 'newGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { -- | An object that contains information about the requested device.
    device :: Core.Maybe Device,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'device', 'getDeviceResponse_device' - An object that contains information about the requested device.
--
-- 'httpStatus', 'getDeviceResponse_httpStatus' - The response's http status code.
newGetDeviceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDeviceResponse
newGetDeviceResponse pHttpStatus_ =
  GetDeviceResponse'
    { device = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about the requested device.
getDeviceResponse_device :: Lens.Lens' GetDeviceResponse (Core.Maybe Device)
getDeviceResponse_device = Lens.lens (\GetDeviceResponse' {device} -> device) (\s@GetDeviceResponse' {} a -> s {device = a} :: GetDeviceResponse)

-- | The response's http status code.
getDeviceResponse_httpStatus :: Lens.Lens' GetDeviceResponse Core.Int
getDeviceResponse_httpStatus = Lens.lens (\GetDeviceResponse' {httpStatus} -> httpStatus) (\s@GetDeviceResponse' {} a -> s {httpStatus = a} :: GetDeviceResponse)

instance Core.NFData GetDeviceResponse
