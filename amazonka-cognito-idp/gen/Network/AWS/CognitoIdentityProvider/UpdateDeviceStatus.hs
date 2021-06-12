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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device status.
module Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
  ( -- * Creating a Request
    UpdateDeviceStatus (..),
    newUpdateDeviceStatus,

    -- * Request Lenses
    updateDeviceStatus_deviceRememberedStatus,
    updateDeviceStatus_accessToken,
    updateDeviceStatus_deviceKey,

    -- * Destructuring the Response
    UpdateDeviceStatusResponse (..),
    newUpdateDeviceStatusResponse,

    -- * Response Lenses
    updateDeviceStatusResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to update the device status.
--
-- /See:/ 'newUpdateDeviceStatus' smart constructor.
data UpdateDeviceStatus = UpdateDeviceStatus'
  { -- | The status of whether a device is remembered.
    deviceRememberedStatus :: Core.Maybe DeviceRememberedStatusType,
    -- | The access token.
    accessToken :: Core.Sensitive Core.Text,
    -- | The device key.
    deviceKey :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDeviceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceRememberedStatus', 'updateDeviceStatus_deviceRememberedStatus' - The status of whether a device is remembered.
--
-- 'accessToken', 'updateDeviceStatus_accessToken' - The access token.
--
-- 'deviceKey', 'updateDeviceStatus_deviceKey' - The device key.
newUpdateDeviceStatus ::
  -- | 'accessToken'
  Core.Text ->
  -- | 'deviceKey'
  Core.Text ->
  UpdateDeviceStatus
newUpdateDeviceStatus pAccessToken_ pDeviceKey_ =
  UpdateDeviceStatus'
    { deviceRememberedStatus =
        Core.Nothing,
      accessToken = Core._Sensitive Lens.# pAccessToken_,
      deviceKey = pDeviceKey_
    }

-- | The status of whether a device is remembered.
updateDeviceStatus_deviceRememberedStatus :: Lens.Lens' UpdateDeviceStatus (Core.Maybe DeviceRememberedStatusType)
updateDeviceStatus_deviceRememberedStatus = Lens.lens (\UpdateDeviceStatus' {deviceRememberedStatus} -> deviceRememberedStatus) (\s@UpdateDeviceStatus' {} a -> s {deviceRememberedStatus = a} :: UpdateDeviceStatus)

-- | The access token.
updateDeviceStatus_accessToken :: Lens.Lens' UpdateDeviceStatus Core.Text
updateDeviceStatus_accessToken = Lens.lens (\UpdateDeviceStatus' {accessToken} -> accessToken) (\s@UpdateDeviceStatus' {} a -> s {accessToken = a} :: UpdateDeviceStatus) Core.. Core._Sensitive

-- | The device key.
updateDeviceStatus_deviceKey :: Lens.Lens' UpdateDeviceStatus Core.Text
updateDeviceStatus_deviceKey = Lens.lens (\UpdateDeviceStatus' {deviceKey} -> deviceKey) (\s@UpdateDeviceStatus' {} a -> s {deviceKey = a} :: UpdateDeviceStatus)

instance Core.AWSRequest UpdateDeviceStatus where
  type
    AWSResponse UpdateDeviceStatus =
      UpdateDeviceStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeviceStatusResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDeviceStatus

instance Core.NFData UpdateDeviceStatus

instance Core.ToHeaders UpdateDeviceStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.UpdateDeviceStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDeviceStatus where
  toJSON UpdateDeviceStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeviceRememberedStatus" Core..=)
              Core.<$> deviceRememberedStatus,
            Core.Just ("AccessToken" Core..= accessToken),
            Core.Just ("DeviceKey" Core..= deviceKey)
          ]
      )

instance Core.ToPath UpdateDeviceStatus where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDeviceStatus where
  toQuery = Core.const Core.mempty

-- | The response to the request to update the device status.
--
-- /See:/ 'newUpdateDeviceStatusResponse' smart constructor.
data UpdateDeviceStatusResponse = UpdateDeviceStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDeviceStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDeviceStatusResponse_httpStatus' - The response's http status code.
newUpdateDeviceStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDeviceStatusResponse
newUpdateDeviceStatusResponse pHttpStatus_ =
  UpdateDeviceStatusResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDeviceStatusResponse_httpStatus :: Lens.Lens' UpdateDeviceStatusResponse Core.Int
updateDeviceStatusResponse_httpStatus = Lens.lens (\UpdateDeviceStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateDeviceStatusResponse' {} a -> s {httpStatus = a} :: UpdateDeviceStatusResponse)

instance Core.NFData UpdateDeviceStatusResponse
