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
-- Module      : Amazonka.CognitoIdentityProvider.UpdateDeviceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device status.
module Amazonka.CognitoIdentityProvider.UpdateDeviceStatus
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to update the device status.
--
-- /See:/ 'newUpdateDeviceStatus' smart constructor.
data UpdateDeviceStatus = UpdateDeviceStatus'
  { -- | The status of whether a device is remembered.
    deviceRememberedStatus :: Prelude.Maybe DeviceRememberedStatusType,
    -- | A valid access token that Amazon Cognito issued to the user whose device
    -- status you want to update.
    accessToken :: Data.Sensitive Prelude.Text,
    -- | The device key.
    deviceKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'accessToken', 'updateDeviceStatus_accessToken' - A valid access token that Amazon Cognito issued to the user whose device
-- status you want to update.
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
      accessToken = Data._Sensitive Lens.# pAccessToken_,
      deviceKey = pDeviceKey_
    }

-- | The status of whether a device is remembered.
updateDeviceStatus_deviceRememberedStatus :: Lens.Lens' UpdateDeviceStatus (Prelude.Maybe DeviceRememberedStatusType)
updateDeviceStatus_deviceRememberedStatus = Lens.lens (\UpdateDeviceStatus' {deviceRememberedStatus} -> deviceRememberedStatus) (\s@UpdateDeviceStatus' {} a -> s {deviceRememberedStatus = a} :: UpdateDeviceStatus)

-- | A valid access token that Amazon Cognito issued to the user whose device
-- status you want to update.
updateDeviceStatus_accessToken :: Lens.Lens' UpdateDeviceStatus Prelude.Text
updateDeviceStatus_accessToken = Lens.lens (\UpdateDeviceStatus' {accessToken} -> accessToken) (\s@UpdateDeviceStatus' {} a -> s {accessToken = a} :: UpdateDeviceStatus) Prelude.. Data._Sensitive

-- | The device key.
updateDeviceStatus_deviceKey :: Lens.Lens' UpdateDeviceStatus Prelude.Text
updateDeviceStatus_deviceKey = Lens.lens (\UpdateDeviceStatus' {deviceKey} -> deviceKey) (\s@UpdateDeviceStatus' {} a -> s {deviceKey = a} :: UpdateDeviceStatus)

instance Core.AWSRequest UpdateDeviceStatus where
  type
    AWSResponse UpdateDeviceStatus =
      UpdateDeviceStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeviceStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDeviceStatus where
  hashWithSalt _salt UpdateDeviceStatus' {..} =
    _salt
      `Prelude.hashWithSalt` deviceRememberedStatus
      `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` deviceKey

instance Prelude.NFData UpdateDeviceStatus where
  rnf UpdateDeviceStatus' {..} =
    Prelude.rnf deviceRememberedStatus
      `Prelude.seq` Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf deviceKey

instance Data.ToHeaders UpdateDeviceStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.UpdateDeviceStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDeviceStatus where
  toJSON UpdateDeviceStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceRememberedStatus" Data..=)
              Prelude.<$> deviceRememberedStatus,
            Prelude.Just ("AccessToken" Data..= accessToken),
            Prelude.Just ("DeviceKey" Data..= deviceKey)
          ]
      )

instance Data.ToPath UpdateDeviceStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDeviceStatus where
  toQuery = Prelude.const Prelude.mempty

-- | The response to the request to update the device status.
--
-- /See:/ 'newUpdateDeviceStatusResponse' smart constructor.
data UpdateDeviceStatusResponse = UpdateDeviceStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData UpdateDeviceStatusResponse where
  rnf UpdateDeviceStatusResponse' {..} =
    Prelude.rnf httpStatus
