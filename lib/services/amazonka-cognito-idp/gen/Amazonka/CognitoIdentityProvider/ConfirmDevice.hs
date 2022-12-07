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
-- Module      : Amazonka.CognitoIdentityProvider.ConfirmDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms tracking of the device. This API call is the call that begins
-- device tracking.
module Amazonka.CognitoIdentityProvider.ConfirmDevice
  ( -- * Creating a Request
    ConfirmDevice (..),
    newConfirmDevice,

    -- * Request Lenses
    confirmDevice_deviceSecretVerifierConfig,
    confirmDevice_deviceName,
    confirmDevice_accessToken,
    confirmDevice_deviceKey,

    -- * Destructuring the Response
    ConfirmDeviceResponse (..),
    newConfirmDeviceResponse,

    -- * Response Lenses
    confirmDeviceResponse_userConfirmationNecessary,
    confirmDeviceResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Confirms the device request.
--
-- /See:/ 'newConfirmDevice' smart constructor.
data ConfirmDevice = ConfirmDevice'
  { -- | The configuration of the device secret verifier.
    deviceSecretVerifierConfig :: Prelude.Maybe DeviceSecretVerifierConfigType,
    -- | The device name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | A valid access token that Amazon Cognito issued to the user whose device
    -- you want to confirm.
    accessToken :: Data.Sensitive Prelude.Text,
    -- | The device key.
    deviceKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceSecretVerifierConfig', 'confirmDevice_deviceSecretVerifierConfig' - The configuration of the device secret verifier.
--
-- 'deviceName', 'confirmDevice_deviceName' - The device name.
--
-- 'accessToken', 'confirmDevice_accessToken' - A valid access token that Amazon Cognito issued to the user whose device
-- you want to confirm.
--
-- 'deviceKey', 'confirmDevice_deviceKey' - The device key.
newConfirmDevice ::
  -- | 'accessToken'
  Prelude.Text ->
  -- | 'deviceKey'
  Prelude.Text ->
  ConfirmDevice
newConfirmDevice pAccessToken_ pDeviceKey_ =
  ConfirmDevice'
    { deviceSecretVerifierConfig =
        Prelude.Nothing,
      deviceName = Prelude.Nothing,
      accessToken = Data._Sensitive Lens.# pAccessToken_,
      deviceKey = pDeviceKey_
    }

-- | The configuration of the device secret verifier.
confirmDevice_deviceSecretVerifierConfig :: Lens.Lens' ConfirmDevice (Prelude.Maybe DeviceSecretVerifierConfigType)
confirmDevice_deviceSecretVerifierConfig = Lens.lens (\ConfirmDevice' {deviceSecretVerifierConfig} -> deviceSecretVerifierConfig) (\s@ConfirmDevice' {} a -> s {deviceSecretVerifierConfig = a} :: ConfirmDevice)

-- | The device name.
confirmDevice_deviceName :: Lens.Lens' ConfirmDevice (Prelude.Maybe Prelude.Text)
confirmDevice_deviceName = Lens.lens (\ConfirmDevice' {deviceName} -> deviceName) (\s@ConfirmDevice' {} a -> s {deviceName = a} :: ConfirmDevice)

-- | A valid access token that Amazon Cognito issued to the user whose device
-- you want to confirm.
confirmDevice_accessToken :: Lens.Lens' ConfirmDevice Prelude.Text
confirmDevice_accessToken = Lens.lens (\ConfirmDevice' {accessToken} -> accessToken) (\s@ConfirmDevice' {} a -> s {accessToken = a} :: ConfirmDevice) Prelude.. Data._Sensitive

-- | The device key.
confirmDevice_deviceKey :: Lens.Lens' ConfirmDevice Prelude.Text
confirmDevice_deviceKey = Lens.lens (\ConfirmDevice' {deviceKey} -> deviceKey) (\s@ConfirmDevice' {} a -> s {deviceKey = a} :: ConfirmDevice)

instance Core.AWSRequest ConfirmDevice where
  type
    AWSResponse ConfirmDevice =
      ConfirmDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfirmDeviceResponse'
            Prelude.<$> (x Data..?> "UserConfirmationNecessary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfirmDevice where
  hashWithSalt _salt ConfirmDevice' {..} =
    _salt
      `Prelude.hashWithSalt` deviceSecretVerifierConfig
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` deviceKey

instance Prelude.NFData ConfirmDevice where
  rnf ConfirmDevice' {..} =
    Prelude.rnf deviceSecretVerifierConfig
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf deviceKey

instance Data.ToHeaders ConfirmDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ConfirmDevice" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConfirmDevice where
  toJSON ConfirmDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceSecretVerifierConfig" Data..=)
              Prelude.<$> deviceSecretVerifierConfig,
            ("DeviceName" Data..=) Prelude.<$> deviceName,
            Prelude.Just ("AccessToken" Data..= accessToken),
            Prelude.Just ("DeviceKey" Data..= deviceKey)
          ]
      )

instance Data.ToPath ConfirmDevice where
  toPath = Prelude.const "/"

instance Data.ToQuery ConfirmDevice where
  toQuery = Prelude.const Prelude.mempty

-- | Confirms the device response.
--
-- /See:/ 'newConfirmDeviceResponse' smart constructor.
data ConfirmDeviceResponse = ConfirmDeviceResponse'
  { -- | Indicates whether the user confirmation must confirm the device
    -- response.
    userConfirmationNecessary :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userConfirmationNecessary', 'confirmDeviceResponse_userConfirmationNecessary' - Indicates whether the user confirmation must confirm the device
-- response.
--
-- 'httpStatus', 'confirmDeviceResponse_httpStatus' - The response's http status code.
newConfirmDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConfirmDeviceResponse
newConfirmDeviceResponse pHttpStatus_ =
  ConfirmDeviceResponse'
    { userConfirmationNecessary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether the user confirmation must confirm the device
-- response.
confirmDeviceResponse_userConfirmationNecessary :: Lens.Lens' ConfirmDeviceResponse (Prelude.Maybe Prelude.Bool)
confirmDeviceResponse_userConfirmationNecessary = Lens.lens (\ConfirmDeviceResponse' {userConfirmationNecessary} -> userConfirmationNecessary) (\s@ConfirmDeviceResponse' {} a -> s {userConfirmationNecessary = a} :: ConfirmDeviceResponse)

-- | The response's http status code.
confirmDeviceResponse_httpStatus :: Lens.Lens' ConfirmDeviceResponse Prelude.Int
confirmDeviceResponse_httpStatus = Lens.lens (\ConfirmDeviceResponse' {httpStatus} -> httpStatus) (\s@ConfirmDeviceResponse' {} a -> s {httpStatus = a} :: ConfirmDeviceResponse)

instance Prelude.NFData ConfirmDeviceResponse where
  rnf ConfirmDeviceResponse' {..} =
    Prelude.rnf userConfirmationNecessary
      `Prelude.seq` Prelude.rnf httpStatus
