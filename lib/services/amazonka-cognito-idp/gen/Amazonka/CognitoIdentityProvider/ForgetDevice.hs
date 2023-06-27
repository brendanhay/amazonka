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
-- Module      : Amazonka.CognitoIdentityProvider.ForgetDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets the specified device.
module Amazonka.CognitoIdentityProvider.ForgetDevice
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to forget the device.
--
-- /See:/ 'newForgetDevice' smart constructor.
data ForgetDevice = ForgetDevice'
  { -- | A valid access token that Amazon Cognito issued to the user whose
    -- registered device you want to forget.
    accessToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The device key.
    deviceKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForgetDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessToken', 'forgetDevice_accessToken' - A valid access token that Amazon Cognito issued to the user whose
-- registered device you want to forget.
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

-- | A valid access token that Amazon Cognito issued to the user whose
-- registered device you want to forget.
forgetDevice_accessToken :: Lens.Lens' ForgetDevice (Prelude.Maybe Prelude.Text)
forgetDevice_accessToken = Lens.lens (\ForgetDevice' {accessToken} -> accessToken) (\s@ForgetDevice' {} a -> s {accessToken = a} :: ForgetDevice) Prelude.. Lens.mapping Data._Sensitive

-- | The device key.
forgetDevice_deviceKey :: Lens.Lens' ForgetDevice Prelude.Text
forgetDevice_deviceKey = Lens.lens (\ForgetDevice' {deviceKey} -> deviceKey) (\s@ForgetDevice' {} a -> s {deviceKey = a} :: ForgetDevice)

instance Core.AWSRequest ForgetDevice where
  type AWSResponse ForgetDevice = ForgetDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull ForgetDeviceResponse'

instance Prelude.Hashable ForgetDevice where
  hashWithSalt _salt ForgetDevice' {..} =
    _salt
      `Prelude.hashWithSalt` accessToken
      `Prelude.hashWithSalt` deviceKey

instance Prelude.NFData ForgetDevice where
  rnf ForgetDevice' {..} =
    Prelude.rnf accessToken
      `Prelude.seq` Prelude.rnf deviceKey

instance Data.ToHeaders ForgetDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ForgetDevice" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ForgetDevice where
  toJSON ForgetDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessToken" Data..=) Prelude.<$> accessToken,
            Prelude.Just ("DeviceKey" Data..= deviceKey)
          ]
      )

instance Data.ToPath ForgetDevice where
  toPath = Prelude.const "/"

instance Data.ToQuery ForgetDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newForgetDeviceResponse' smart constructor.
data ForgetDeviceResponse = ForgetDeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForgetDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newForgetDeviceResponse ::
  ForgetDeviceResponse
newForgetDeviceResponse = ForgetDeviceResponse'

instance Prelude.NFData ForgetDeviceResponse where
  rnf _ = ()
