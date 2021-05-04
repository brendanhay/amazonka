{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDevice' smart constructor.
data UpdateDevice = UpdateDevice'
  { -- | The ARN of the device to update. Required.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The updated device name. Required.
    deviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { deviceArn = Prelude.Nothing,
      deviceName = Prelude.Nothing
    }

-- | The ARN of the device to update. Required.
updateDevice_deviceArn :: Lens.Lens' UpdateDevice (Prelude.Maybe Prelude.Text)
updateDevice_deviceArn = Lens.lens (\UpdateDevice' {deviceArn} -> deviceArn) (\s@UpdateDevice' {} a -> s {deviceArn = a} :: UpdateDevice)

-- | The updated device name. Required.
updateDevice_deviceName :: Lens.Lens' UpdateDevice (Prelude.Maybe Prelude.Text)
updateDevice_deviceName = Lens.lens (\UpdateDevice' {deviceName} -> deviceName) (\s@UpdateDevice' {} a -> s {deviceName = a} :: UpdateDevice)

instance Prelude.AWSRequest UpdateDevice where
  type Rs UpdateDevice = UpdateDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDevice

instance Prelude.NFData UpdateDevice

instance Prelude.ToHeaders UpdateDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.UpdateDevice" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDevice where
  toJSON UpdateDevice' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeviceArn" Prelude..=) Prelude.<$> deviceArn,
            ("DeviceName" Prelude..=) Prelude.<$> deviceName
          ]
      )

instance Prelude.ToPath UpdateDevice where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDeviceResponse' smart constructor.
data UpdateDeviceResponse = UpdateDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateDeviceResponse
newUpdateDeviceResponse pHttpStatus_ =
  UpdateDeviceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateDeviceResponse_httpStatus :: Lens.Lens' UpdateDeviceResponse Prelude.Int
updateDeviceResponse_httpStatus = Lens.lens (\UpdateDeviceResponse' {httpStatus} -> httpStatus) (\s@UpdateDeviceResponse' {} a -> s {httpStatus = a} :: UpdateDeviceResponse)

instance Prelude.NFData UpdateDeviceResponse
