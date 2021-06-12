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
-- Module      : Network.AWS.CognitoIdentityProvider.GetDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the device.
module Network.AWS.CognitoIdentityProvider.GetDevice
  ( -- * Creating a Request
    GetDevice (..),
    newGetDevice,

    -- * Request Lenses
    getDevice_accessToken,
    getDevice_deviceKey,

    -- * Destructuring the Response
    GetDeviceResponse (..),
    newGetDeviceResponse,

    -- * Response Lenses
    getDeviceResponse_httpStatus,
    getDeviceResponse_device,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get the device.
--
-- /See:/ 'newGetDevice' smart constructor.
data GetDevice = GetDevice'
  { -- | The access token.
    accessToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The device key.
    deviceKey :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'getDevice_accessToken' - The access token.
--
-- 'deviceKey', 'getDevice_deviceKey' - The device key.
newGetDevice ::
  -- | 'deviceKey'
  Core.Text ->
  GetDevice
newGetDevice pDeviceKey_ =
  GetDevice'
    { accessToken = Core.Nothing,
      deviceKey = pDeviceKey_
    }

-- | The access token.
getDevice_accessToken :: Lens.Lens' GetDevice (Core.Maybe Core.Text)
getDevice_accessToken = Lens.lens (\GetDevice' {accessToken} -> accessToken) (\s@GetDevice' {} a -> s {accessToken = a} :: GetDevice) Core.. Lens.mapping Core._Sensitive

-- | The device key.
getDevice_deviceKey :: Lens.Lens' GetDevice Core.Text
getDevice_deviceKey = Lens.lens (\GetDevice' {deviceKey} -> deviceKey) (\s@GetDevice' {} a -> s {deviceKey = a} :: GetDevice)

instance Core.AWSRequest GetDevice where
  type AWSResponse GetDevice = GetDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Device")
      )

instance Core.Hashable GetDevice

instance Core.NFData GetDevice

instance Core.ToHeaders GetDevice where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetDevice" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDevice where
  toJSON GetDevice' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessToken" Core..=) Core.<$> accessToken,
            Core.Just ("DeviceKey" Core..= deviceKey)
          ]
      )

instance Core.ToPath GetDevice where
  toPath = Core.const "/"

instance Core.ToQuery GetDevice where
  toQuery = Core.const Core.mempty

-- | Gets the device response.
--
-- /See:/ 'newGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The device.
    device :: DeviceType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDeviceResponse_httpStatus' - The response's http status code.
--
-- 'device', 'getDeviceResponse_device' - The device.
newGetDeviceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'device'
  DeviceType ->
  GetDeviceResponse
newGetDeviceResponse pHttpStatus_ pDevice_ =
  GetDeviceResponse'
    { httpStatus = pHttpStatus_,
      device = pDevice_
    }

-- | The response's http status code.
getDeviceResponse_httpStatus :: Lens.Lens' GetDeviceResponse Core.Int
getDeviceResponse_httpStatus = Lens.lens (\GetDeviceResponse' {httpStatus} -> httpStatus) (\s@GetDeviceResponse' {} a -> s {httpStatus = a} :: GetDeviceResponse)

-- | The device.
getDeviceResponse_device :: Lens.Lens' GetDeviceResponse DeviceType
getDeviceResponse_device = Lens.lens (\GetDeviceResponse' {device} -> device) (\s@GetDeviceResponse' {} a -> s {device = a} :: GetDeviceResponse)

instance Core.NFData GetDeviceResponse
