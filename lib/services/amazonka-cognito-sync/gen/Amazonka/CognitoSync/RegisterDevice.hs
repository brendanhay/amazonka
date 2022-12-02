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
-- Module      : Amazonka.CognitoSync.RegisterDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a device to receive push sync notifications.
--
-- This API can only be called with temporary credentials provided by
-- Cognito Identity. You cannot call this API with developer credentials.
module Amazonka.CognitoSync.RegisterDevice
  ( -- * Creating a Request
    RegisterDevice (..),
    newRegisterDevice,

    -- * Request Lenses
    registerDevice_identityPoolId,
    registerDevice_identityId,
    registerDevice_platform,
    registerDevice_token,

    -- * Destructuring the Response
    RegisterDeviceResponse (..),
    newRegisterDeviceResponse,

    -- * Response Lenses
    registerDeviceResponse_deviceId,
    registerDeviceResponse_httpStatus,
  )
where

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to RegisterDevice.
--
-- /See:/ 'newRegisterDevice' smart constructor.
data RegisterDevice = RegisterDevice'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. Here, the ID of the pool that the identity belongs to.
    identityPoolId :: Prelude.Text,
    -- | The unique ID for this identity.
    identityId :: Prelude.Text,
    -- | The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
    platform :: Platform,
    -- | The push token.
    token :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'registerDevice_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. Here, the ID of the pool that the identity belongs to.
--
-- 'identityId', 'registerDevice_identityId' - The unique ID for this identity.
--
-- 'platform', 'registerDevice_platform' - The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
--
-- 'token', 'registerDevice_token' - The push token.
newRegisterDevice ::
  -- | 'identityPoolId'
  Prelude.Text ->
  -- | 'identityId'
  Prelude.Text ->
  -- | 'platform'
  Platform ->
  -- | 'token'
  Prelude.Text ->
  RegisterDevice
newRegisterDevice
  pIdentityPoolId_
  pIdentityId_
  pPlatform_
  pToken_ =
    RegisterDevice'
      { identityPoolId = pIdentityPoolId_,
        identityId = pIdentityId_,
        platform = pPlatform_,
        token = pToken_
      }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. Here, the ID of the pool that the identity belongs to.
registerDevice_identityPoolId :: Lens.Lens' RegisterDevice Prelude.Text
registerDevice_identityPoolId = Lens.lens (\RegisterDevice' {identityPoolId} -> identityPoolId) (\s@RegisterDevice' {} a -> s {identityPoolId = a} :: RegisterDevice)

-- | The unique ID for this identity.
registerDevice_identityId :: Lens.Lens' RegisterDevice Prelude.Text
registerDevice_identityId = Lens.lens (\RegisterDevice' {identityId} -> identityId) (\s@RegisterDevice' {} a -> s {identityId = a} :: RegisterDevice)

-- | The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
registerDevice_platform :: Lens.Lens' RegisterDevice Platform
registerDevice_platform = Lens.lens (\RegisterDevice' {platform} -> platform) (\s@RegisterDevice' {} a -> s {platform = a} :: RegisterDevice)

-- | The push token.
registerDevice_token :: Lens.Lens' RegisterDevice Prelude.Text
registerDevice_token = Lens.lens (\RegisterDevice' {token} -> token) (\s@RegisterDevice' {} a -> s {token = a} :: RegisterDevice)

instance Core.AWSRequest RegisterDevice where
  type
    AWSResponse RegisterDevice =
      RegisterDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterDeviceResponse'
            Prelude.<$> (x Data..?> "DeviceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterDevice where
  hashWithSalt _salt RegisterDevice' {..} =
    _salt `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` token

instance Prelude.NFData RegisterDevice where
  rnf RegisterDevice' {..} =
    Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf token

instance Data.ToHeaders RegisterDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterDevice where
  toJSON RegisterDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Platform" Data..= platform),
            Prelude.Just ("Token" Data..= token)
          ]
      )

instance Data.ToPath RegisterDevice where
  toPath RegisterDevice' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Data.toBS identityPoolId,
        "/identity/",
        Data.toBS identityId,
        "/device"
      ]

instance Data.ToQuery RegisterDevice where
  toQuery = Prelude.const Prelude.mempty

-- | Response to a RegisterDevice request.
--
-- /See:/ 'newRegisterDeviceResponse' smart constructor.
data RegisterDeviceResponse = RegisterDeviceResponse'
  { -- | The unique ID generated for this device by Cognito.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'registerDeviceResponse_deviceId' - The unique ID generated for this device by Cognito.
--
-- 'httpStatus', 'registerDeviceResponse_httpStatus' - The response's http status code.
newRegisterDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterDeviceResponse
newRegisterDeviceResponse pHttpStatus_ =
  RegisterDeviceResponse'
    { deviceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID generated for this device by Cognito.
registerDeviceResponse_deviceId :: Lens.Lens' RegisterDeviceResponse (Prelude.Maybe Prelude.Text)
registerDeviceResponse_deviceId = Lens.lens (\RegisterDeviceResponse' {deviceId} -> deviceId) (\s@RegisterDeviceResponse' {} a -> s {deviceId = a} :: RegisterDeviceResponse)

-- | The response's http status code.
registerDeviceResponse_httpStatus :: Lens.Lens' RegisterDeviceResponse Prelude.Int
registerDeviceResponse_httpStatus = Lens.lens (\RegisterDeviceResponse' {httpStatus} -> httpStatus) (\s@RegisterDeviceResponse' {} a -> s {httpStatus = a} :: RegisterDeviceResponse)

instance Prelude.NFData RegisterDeviceResponse where
  rnf RegisterDeviceResponse' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf httpStatus
