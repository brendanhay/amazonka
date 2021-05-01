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
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms tracking of the device. This API call is the call that begins
-- device tracking.
module Network.AWS.CognitoIdentityProvider.ConfirmDevice
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Confirms the device request.
--
-- /See:/ 'newConfirmDevice' smart constructor.
data ConfirmDevice = ConfirmDevice'
  { -- | The configuration of the device secret verifier.
    deviceSecretVerifierConfig :: Prelude.Maybe DeviceSecretVerifierConfigType,
    -- | The device name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The access token.
    accessToken :: Prelude.Sensitive Prelude.Text,
    -- | The device key.
    deviceKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'accessToken', 'confirmDevice_accessToken' - The access token.
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
      accessToken =
        Prelude._Sensitive Lens.# pAccessToken_,
      deviceKey = pDeviceKey_
    }

-- | The configuration of the device secret verifier.
confirmDevice_deviceSecretVerifierConfig :: Lens.Lens' ConfirmDevice (Prelude.Maybe DeviceSecretVerifierConfigType)
confirmDevice_deviceSecretVerifierConfig = Lens.lens (\ConfirmDevice' {deviceSecretVerifierConfig} -> deviceSecretVerifierConfig) (\s@ConfirmDevice' {} a -> s {deviceSecretVerifierConfig = a} :: ConfirmDevice)

-- | The device name.
confirmDevice_deviceName :: Lens.Lens' ConfirmDevice (Prelude.Maybe Prelude.Text)
confirmDevice_deviceName = Lens.lens (\ConfirmDevice' {deviceName} -> deviceName) (\s@ConfirmDevice' {} a -> s {deviceName = a} :: ConfirmDevice)

-- | The access token.
confirmDevice_accessToken :: Lens.Lens' ConfirmDevice Prelude.Text
confirmDevice_accessToken = Lens.lens (\ConfirmDevice' {accessToken} -> accessToken) (\s@ConfirmDevice' {} a -> s {accessToken = a} :: ConfirmDevice) Prelude.. Prelude._Sensitive

-- | The device key.
confirmDevice_deviceKey :: Lens.Lens' ConfirmDevice Prelude.Text
confirmDevice_deviceKey = Lens.lens (\ConfirmDevice' {deviceKey} -> deviceKey) (\s@ConfirmDevice' {} a -> s {deviceKey = a} :: ConfirmDevice)

instance Prelude.AWSRequest ConfirmDevice where
  type Rs ConfirmDevice = ConfirmDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfirmDeviceResponse'
            Prelude.<$> (x Prelude..?> "UserConfirmationNecessary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfirmDevice

instance Prelude.NFData ConfirmDevice

instance Prelude.ToHeaders ConfirmDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.ConfirmDevice" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ConfirmDevice where
  toJSON ConfirmDevice' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeviceSecretVerifierConfig" Prelude..=)
              Prelude.<$> deviceSecretVerifierConfig,
            ("DeviceName" Prelude..=) Prelude.<$> deviceName,
            Prelude.Just ("AccessToken" Prelude..= accessToken),
            Prelude.Just ("DeviceKey" Prelude..= deviceKey)
          ]
      )

instance Prelude.ToPath ConfirmDevice where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ConfirmDevice where
  toQuery = Prelude.const Prelude.mempty

-- | Confirms the device response.
--
-- /See:/ 'newConfirmDeviceResponse' smart constructor.
data ConfirmDeviceResponse = ConfirmDeviceResponse'
  { -- | Indicates whether the user confirmation is necessary to confirm the
    -- device response.
    userConfirmationNecessary :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfirmDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userConfirmationNecessary', 'confirmDeviceResponse_userConfirmationNecessary' - Indicates whether the user confirmation is necessary to confirm the
-- device response.
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

-- | Indicates whether the user confirmation is necessary to confirm the
-- device response.
confirmDeviceResponse_userConfirmationNecessary :: Lens.Lens' ConfirmDeviceResponse (Prelude.Maybe Prelude.Bool)
confirmDeviceResponse_userConfirmationNecessary = Lens.lens (\ConfirmDeviceResponse' {userConfirmationNecessary} -> userConfirmationNecessary) (\s@ConfirmDeviceResponse' {} a -> s {userConfirmationNecessary = a} :: ConfirmDeviceResponse)

-- | The response's http status code.
confirmDeviceResponse_httpStatus :: Lens.Lens' ConfirmDeviceResponse Prelude.Int
confirmDeviceResponse_httpStatus = Lens.lens (\ConfirmDeviceResponse' {httpStatus} -> httpStatus) (\s@ConfirmDeviceResponse' {} a -> s {httpStatus = a} :: ConfirmDeviceResponse)

instance Prelude.NFData ConfirmDeviceResponse
