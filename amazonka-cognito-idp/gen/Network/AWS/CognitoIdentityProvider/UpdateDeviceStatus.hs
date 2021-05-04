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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to update the device status.
--
-- /See:/ 'newUpdateDeviceStatus' smart constructor.
data UpdateDeviceStatus = UpdateDeviceStatus'
  { -- | The status of whether a device is remembered.
    deviceRememberedStatus :: Prelude.Maybe DeviceRememberedStatusType,
    -- | The access token.
    accessToken :: Prelude.Sensitive Prelude.Text,
    -- | The device key.
    deviceKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'deviceKey'
  Prelude.Text ->
  UpdateDeviceStatus
newUpdateDeviceStatus pAccessToken_ pDeviceKey_ =
  UpdateDeviceStatus'
    { deviceRememberedStatus =
        Prelude.Nothing,
      accessToken =
        Prelude._Sensitive Lens.# pAccessToken_,
      deviceKey = pDeviceKey_
    }

-- | The status of whether a device is remembered.
updateDeviceStatus_deviceRememberedStatus :: Lens.Lens' UpdateDeviceStatus (Prelude.Maybe DeviceRememberedStatusType)
updateDeviceStatus_deviceRememberedStatus = Lens.lens (\UpdateDeviceStatus' {deviceRememberedStatus} -> deviceRememberedStatus) (\s@UpdateDeviceStatus' {} a -> s {deviceRememberedStatus = a} :: UpdateDeviceStatus)

-- | The access token.
updateDeviceStatus_accessToken :: Lens.Lens' UpdateDeviceStatus Prelude.Text
updateDeviceStatus_accessToken = Lens.lens (\UpdateDeviceStatus' {accessToken} -> accessToken) (\s@UpdateDeviceStatus' {} a -> s {accessToken = a} :: UpdateDeviceStatus) Prelude.. Prelude._Sensitive

-- | The device key.
updateDeviceStatus_deviceKey :: Lens.Lens' UpdateDeviceStatus Prelude.Text
updateDeviceStatus_deviceKey = Lens.lens (\UpdateDeviceStatus' {deviceKey} -> deviceKey) (\s@UpdateDeviceStatus' {} a -> s {deviceKey = a} :: UpdateDeviceStatus)

instance Prelude.AWSRequest UpdateDeviceStatus where
  type
    Rs UpdateDeviceStatus =
      UpdateDeviceStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeviceStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDeviceStatus

instance Prelude.NFData UpdateDeviceStatus

instance Prelude.ToHeaders UpdateDeviceStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.UpdateDeviceStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDeviceStatus where
  toJSON UpdateDeviceStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeviceRememberedStatus" Prelude..=)
              Prelude.<$> deviceRememberedStatus,
            Prelude.Just ("AccessToken" Prelude..= accessToken),
            Prelude.Just ("DeviceKey" Prelude..= deviceKey)
          ]
      )

instance Prelude.ToPath UpdateDeviceStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDeviceStatus where
  toQuery = Prelude.const Prelude.mempty

-- | The response to the request to update the device status.
--
-- /See:/ 'newUpdateDeviceStatusResponse' smart constructor.
data UpdateDeviceStatusResponse = UpdateDeviceStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateDeviceStatusResponse
newUpdateDeviceStatusResponse pHttpStatus_ =
  UpdateDeviceStatusResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDeviceStatusResponse_httpStatus :: Lens.Lens' UpdateDeviceStatusResponse Prelude.Int
updateDeviceStatusResponse_httpStatus = Lens.lens (\UpdateDeviceStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateDeviceStatusResponse' {} a -> s {httpStatus = a} :: UpdateDeviceStatusResponse)

instance Prelude.NFData UpdateDeviceStatusResponse
