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
-- Module      : Network.AWS.AlexaBusiness.UpdateDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device name by device ARN.
module Network.AWS.AlexaBusiness.UpdateDevice
  ( -- * Creating a Request
    UpdateDevice (..),
    newUpdateDevice,

    -- * Request Lenses
    updateDevice_deviceArn,
    updateDevice_deviceName,

    -- * Destructuring the Response
    UpdateDeviceResponse (..),
    newUpdateDeviceResponse,

    -- * Response Lenses
    updateDeviceResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDevice' smart constructor.
data UpdateDevice = UpdateDevice'
  { -- | The ARN of the device to update. Required.
    deviceArn :: Core.Maybe Core.Text,
    -- | The updated device name. Required.
    deviceName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'updateDevice_deviceArn' - The ARN of the device to update. Required.
--
-- 'deviceName', 'updateDevice_deviceName' - The updated device name. Required.
newUpdateDevice ::
  UpdateDevice
newUpdateDevice =
  UpdateDevice'
    { deviceArn = Core.Nothing,
      deviceName = Core.Nothing
    }

-- | The ARN of the device to update. Required.
updateDevice_deviceArn :: Lens.Lens' UpdateDevice (Core.Maybe Core.Text)
updateDevice_deviceArn = Lens.lens (\UpdateDevice' {deviceArn} -> deviceArn) (\s@UpdateDevice' {} a -> s {deviceArn = a} :: UpdateDevice)

-- | The updated device name. Required.
updateDevice_deviceName :: Lens.Lens' UpdateDevice (Core.Maybe Core.Text)
updateDevice_deviceName = Lens.lens (\UpdateDevice' {deviceName} -> deviceName) (\s@UpdateDevice' {} a -> s {deviceName = a} :: UpdateDevice)

instance Core.AWSRequest UpdateDevice where
  type AWSResponse UpdateDevice = UpdateDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeviceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDevice

instance Core.NFData UpdateDevice

instance Core.ToHeaders UpdateDevice where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.UpdateDevice" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDevice where
  toJSON UpdateDevice' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeviceArn" Core..=) Core.<$> deviceArn,
            ("DeviceName" Core..=) Core.<$> deviceName
          ]
      )

instance Core.ToPath UpdateDevice where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDevice where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDeviceResponse' smart constructor.
data UpdateDeviceResponse = UpdateDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDeviceResponse_httpStatus' - The response's http status code.
newUpdateDeviceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDeviceResponse
newUpdateDeviceResponse pHttpStatus_ =
  UpdateDeviceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateDeviceResponse_httpStatus :: Lens.Lens' UpdateDeviceResponse Core.Int
updateDeviceResponse_httpStatus = Lens.lens (\UpdateDeviceResponse' {httpStatus} -> httpStatus) (\s@UpdateDeviceResponse' {} a -> s {httpStatus = a} :: UpdateDeviceResponse)

instance Core.NFData UpdateDeviceResponse
