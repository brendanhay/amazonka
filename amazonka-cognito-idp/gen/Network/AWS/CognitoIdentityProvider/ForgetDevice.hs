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
-- Module      : Network.AWS.CognitoIdentityProvider.ForgetDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets the specified device.
module Network.AWS.CognitoIdentityProvider.ForgetDevice
  ( -- * Creating a Request
    ForgetDevice (..),
    newForgetDevice,

    -- * Request Lenses
    forgetDevice_accessToken,
    forgetDevice_deviceKey,

    -- * Destructuring the Response
    ForgetDeviceResponse (..),
    newForgetDeviceResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to forget the device.
--
-- /See:/ 'newForgetDevice' smart constructor.
data ForgetDevice = ForgetDevice'
  { -- | The access token for the forgotten device request.
    accessToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The device key.
    deviceKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ForgetDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'forgetDevice_accessToken' - The access token for the forgotten device request.
--
-- 'deviceKey', 'forgetDevice_deviceKey' - The device key.
newForgetDevice ::
  -- | 'deviceKey'
  Prelude.Text ->
  ForgetDevice
newForgetDevice pDeviceKey_ =
  ForgetDevice'
    { accessToken = Prelude.Nothing,
      deviceKey = pDeviceKey_
    }

-- | The access token for the forgotten device request.
forgetDevice_accessToken :: Lens.Lens' ForgetDevice (Prelude.Maybe Prelude.Text)
forgetDevice_accessToken = Lens.lens (\ForgetDevice' {accessToken} -> accessToken) (\s@ForgetDevice' {} a -> s {accessToken = a} :: ForgetDevice) Prelude.. Lens.mapping Prelude._Sensitive

-- | The device key.
forgetDevice_deviceKey :: Lens.Lens' ForgetDevice Prelude.Text
forgetDevice_deviceKey = Lens.lens (\ForgetDevice' {deviceKey} -> deviceKey) (\s@ForgetDevice' {} a -> s {deviceKey = a} :: ForgetDevice)

instance Prelude.AWSRequest ForgetDevice where
  type Rs ForgetDevice = ForgetDeviceResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull ForgetDeviceResponse'

instance Prelude.Hashable ForgetDevice

instance Prelude.NFData ForgetDevice

instance Prelude.ToHeaders ForgetDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.ForgetDevice" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ForgetDevice where
  toJSON ForgetDevice' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccessToken" Prelude..=) Prelude.<$> accessToken,
            Prelude.Just ("DeviceKey" Prelude..= deviceKey)
          ]
      )

instance Prelude.ToPath ForgetDevice where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ForgetDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newForgetDeviceResponse' smart constructor.
data ForgetDeviceResponse = ForgetDeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ForgetDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newForgetDeviceResponse ::
  ForgetDeviceResponse
newForgetDeviceResponse = ForgetDeviceResponse'

instance Prelude.NFData ForgetDeviceResponse
