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
-- Module      : Amazonka.CognitoIdentityProvider.GetDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the device.
module Amazonka.CognitoIdentityProvider.GetDevice
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to get the device.
--
-- /See:/ 'newGetDevice' smart constructor.
data GetDevice = GetDevice'
  { -- | A valid access token that Amazon Cognito issued to the user whose device
    -- information you want to request.
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The device key.
    deviceKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'getDevice_accessToken' - A valid access token that Amazon Cognito issued to the user whose device
-- information you want to request.
--
-- 'deviceKey', 'getDevice_deviceKey' - The device key.
newGetDevice ::
  -- | 'deviceKey'
  Prelude.Text ->
  GetDevice
newGetDevice pDeviceKey_ =
  GetDevice'
    { accessToken = Prelude.Nothing,
      deviceKey = pDeviceKey_
    }

-- | A valid access token that Amazon Cognito issued to the user whose device
-- information you want to request.
getDevice_accessToken :: Lens.Lens' GetDevice (Prelude.Maybe Prelude.Text)
getDevice_accessToken = Lens.lens (\GetDevice' {accessToken} -> accessToken) (\s@GetDevice' {} a -> s {accessToken = a} :: GetDevice) Prelude.. Lens.mapping Data._Sensitive

-- | The device key.
getDevice_deviceKey :: Lens.Lens' GetDevice Prelude.Text
getDevice_deviceKey = Lens.lens (\GetDevice' {deviceKey} -> deviceKey) (\s@GetDevice' {} a -> s {deviceKey = a} :: GetDevice)

instance Core.AWSRequest GetDevice where
  type AWSResponse GetDevice = GetDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Device")
      )

instance Prelude.Hashable GetDevice where
  hashWithSalt _salt GetDevice' {..} =
    _salt
      `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` deviceKey

instance Prelude.NFData GetDevice where
  rnf GetDevice' {..} =
    Prelude.rnf accessToken `Prelude.seq`
      Prelude.rnf deviceKey

instance Data.ToHeaders GetDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.GetDevice" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDevice where
  toJSON GetDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessToken" Data..=) Prelude.<$> accessToken,
            Prelude.Just ("DeviceKey" Data..= deviceKey)
          ]
      )

instance Data.ToPath GetDevice where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDevice where
  toQuery = Prelude.const Prelude.mempty

-- | Gets the device response.
--
-- /See:/ 'newGetDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The device.
    device :: DeviceType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'device'
  DeviceType ->
  GetDeviceResponse
newGetDeviceResponse pHttpStatus_ pDevice_ =
  GetDeviceResponse'
    { httpStatus = pHttpStatus_,
      device = pDevice_
    }

-- | The response's http status code.
getDeviceResponse_httpStatus :: Lens.Lens' GetDeviceResponse Prelude.Int
getDeviceResponse_httpStatus = Lens.lens (\GetDeviceResponse' {httpStatus} -> httpStatus) (\s@GetDeviceResponse' {} a -> s {httpStatus = a} :: GetDeviceResponse)

-- | The device.
getDeviceResponse_device :: Lens.Lens' GetDeviceResponse DeviceType
getDeviceResponse_device = Lens.lens (\GetDeviceResponse' {device} -> device) (\s@GetDeviceResponse' {} a -> s {device = a} :: GetDeviceResponse)

instance Prelude.NFData GetDeviceResponse where
  rnf GetDeviceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf device
