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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Confirms the device request.
--
-- /See:/ 'newConfirmDevice' smart constructor.
data ConfirmDevice = ConfirmDevice'
  { -- | The configuration of the device secret verifier.
    deviceSecretVerifierConfig :: Core.Maybe DeviceSecretVerifierConfigType,
    -- | The device name.
    deviceName :: Core.Maybe Core.Text,
    -- | The access token.
    accessToken :: Core.Sensitive Core.Text,
    -- | The device key.
    deviceKey :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'deviceKey'
  Core.Text ->
  ConfirmDevice
newConfirmDevice pAccessToken_ pDeviceKey_ =
  ConfirmDevice'
    { deviceSecretVerifierConfig =
        Core.Nothing,
      deviceName = Core.Nothing,
      accessToken = Core._Sensitive Lens.# pAccessToken_,
      deviceKey = pDeviceKey_
    }

-- | The configuration of the device secret verifier.
confirmDevice_deviceSecretVerifierConfig :: Lens.Lens' ConfirmDevice (Core.Maybe DeviceSecretVerifierConfigType)
confirmDevice_deviceSecretVerifierConfig = Lens.lens (\ConfirmDevice' {deviceSecretVerifierConfig} -> deviceSecretVerifierConfig) (\s@ConfirmDevice' {} a -> s {deviceSecretVerifierConfig = a} :: ConfirmDevice)

-- | The device name.
confirmDevice_deviceName :: Lens.Lens' ConfirmDevice (Core.Maybe Core.Text)
confirmDevice_deviceName = Lens.lens (\ConfirmDevice' {deviceName} -> deviceName) (\s@ConfirmDevice' {} a -> s {deviceName = a} :: ConfirmDevice)

-- | The access token.
confirmDevice_accessToken :: Lens.Lens' ConfirmDevice Core.Text
confirmDevice_accessToken = Lens.lens (\ConfirmDevice' {accessToken} -> accessToken) (\s@ConfirmDevice' {} a -> s {accessToken = a} :: ConfirmDevice) Core.. Core._Sensitive

-- | The device key.
confirmDevice_deviceKey :: Lens.Lens' ConfirmDevice Core.Text
confirmDevice_deviceKey = Lens.lens (\ConfirmDevice' {deviceKey} -> deviceKey) (\s@ConfirmDevice' {} a -> s {deviceKey = a} :: ConfirmDevice)

instance Core.AWSRequest ConfirmDevice where
  type
    AWSResponse ConfirmDevice =
      ConfirmDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfirmDeviceResponse'
            Core.<$> (x Core..?> "UserConfirmationNecessary")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ConfirmDevice

instance Core.NFData ConfirmDevice

instance Core.ToHeaders ConfirmDevice where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ConfirmDevice" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ConfirmDevice where
  toJSON ConfirmDevice' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeviceSecretVerifierConfig" Core..=)
              Core.<$> deviceSecretVerifierConfig,
            ("DeviceName" Core..=) Core.<$> deviceName,
            Core.Just ("AccessToken" Core..= accessToken),
            Core.Just ("DeviceKey" Core..= deviceKey)
          ]
      )

instance Core.ToPath ConfirmDevice where
  toPath = Core.const "/"

instance Core.ToQuery ConfirmDevice where
  toQuery = Core.const Core.mempty

-- | Confirms the device response.
--
-- /See:/ 'newConfirmDeviceResponse' smart constructor.
data ConfirmDeviceResponse = ConfirmDeviceResponse'
  { -- | Indicates whether the user confirmation is necessary to confirm the
    -- device response.
    userConfirmationNecessary :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ConfirmDeviceResponse
newConfirmDeviceResponse pHttpStatus_ =
  ConfirmDeviceResponse'
    { userConfirmationNecessary =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether the user confirmation is necessary to confirm the
-- device response.
confirmDeviceResponse_userConfirmationNecessary :: Lens.Lens' ConfirmDeviceResponse (Core.Maybe Core.Bool)
confirmDeviceResponse_userConfirmationNecessary = Lens.lens (\ConfirmDeviceResponse' {userConfirmationNecessary} -> userConfirmationNecessary) (\s@ConfirmDeviceResponse' {} a -> s {userConfirmationNecessary = a} :: ConfirmDeviceResponse)

-- | The response's http status code.
confirmDeviceResponse_httpStatus :: Lens.Lens' ConfirmDeviceResponse Core.Int
confirmDeviceResponse_httpStatus = Lens.lens (\ConfirmDeviceResponse' {httpStatus} -> httpStatus) (\s@ConfirmDeviceResponse' {} a -> s {httpStatus = a} :: ConfirmDeviceResponse)

instance Core.NFData ConfirmDeviceResponse
