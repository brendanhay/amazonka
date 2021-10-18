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
-- Module      : Network.AWS.AlexaBusiness.GetDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a device by device ARN.
module Network.AWS.AlexaBusiness.GetDevice
  ( -- * Creating a Request
    GetDevice (..),
    newGetDevice,

    -- * Request Lenses
    getDevice_deviceArn,

    -- * Destructuring the Response
    GetDeviceResponse (..),
    newGetDeviceResponse,

    -- * Response Lenses
    getDeviceResponse_device,
    getDeviceResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDevice' smart constructor.
data GetDevice = GetDevice'
  { -- | The ARN of the device for which to request details. Required.
    deviceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'getDevice_deviceArn' - The ARN of the device for which to request details. Required.
newGetDevice ::
  GetDevice
newGetDevice =
  GetDevice' {deviceArn = Prelude.Nothing}

-- | The ARN of the device for which to request details. Required.
getDevice_deviceArn :: Lens.Lens' GetDevice (Prelude.Maybe Prelude.Text)
getDevice_deviceArn = Lens.lens (\GetDevice' {deviceArn} -> deviceArn) (\s@GetDevice' {} a -> s {deviceArn = a} :: GetDevice)

instance Core.AWSRequest GetDevice where
  type AWSResponse GetDevice = GetDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceResponse'
            Prelude.<$> (x Core..?> "Device")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDevice

instance Prelude.NFData GetDevice

instance Core.ToHeaders GetDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.GetDevice" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDevice where
  toJSON GetDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [("DeviceArn" Core..=) Prelude.<$> deviceArn]
      )

instance Core.ToPath GetDevice where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { -- | The details of the device requested. Required.
    device :: Prelude.Maybe Device,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'device', 'getDeviceResponse_device' - The details of the device requested. Required.
--
-- 'httpStatus', 'getDeviceResponse_httpStatus' - The response's http status code.
newGetDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeviceResponse
newGetDeviceResponse pHttpStatus_ =
  GetDeviceResponse'
    { device = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the device requested. Required.
getDeviceResponse_device :: Lens.Lens' GetDeviceResponse (Prelude.Maybe Device)
getDeviceResponse_device = Lens.lens (\GetDeviceResponse' {device} -> device) (\s@GetDeviceResponse' {} a -> s {device = a} :: GetDeviceResponse)

-- | The response's http status code.
getDeviceResponse_httpStatus :: Lens.Lens' GetDeviceResponse Prelude.Int
getDeviceResponse_httpStatus = Lens.lens (\GetDeviceResponse' {httpStatus} -> httpStatus) (\s@GetDeviceResponse' {} a -> s {httpStatus = a} :: GetDeviceResponse)

instance Prelude.NFData GetDeviceResponse
