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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminForgetDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets the device, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminForgetDevice
  ( -- * Creating a Request
    AdminForgetDevice (..),
    newAdminForgetDevice,

    -- * Request Lenses
    adminForgetDevice_userPoolId,
    adminForgetDevice_username,
    adminForgetDevice_deviceKey,

    -- * Destructuring the Response
    AdminForgetDeviceResponse (..),
    newAdminForgetDeviceResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Sends the forgot device request, as an administrator.
--
-- /See:/ 'newAdminForgetDevice' smart constructor.
data AdminForgetDevice = AdminForgetDevice'
  { -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The user name.
    username :: Prelude.Sensitive Prelude.Text,
    -- | The device key.
    deviceKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminForgetDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminForgetDevice_userPoolId' - The user pool ID.
--
-- 'username', 'adminForgetDevice_username' - The user name.
--
-- 'deviceKey', 'adminForgetDevice_deviceKey' - The device key.
newAdminForgetDevice ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  -- | 'deviceKey'
  Prelude.Text ->
  AdminForgetDevice
newAdminForgetDevice
  pUserPoolId_
  pUsername_
  pDeviceKey_ =
    AdminForgetDevice'
      { userPoolId = pUserPoolId_,
        username = Prelude._Sensitive Lens.# pUsername_,
        deviceKey = pDeviceKey_
      }

-- | The user pool ID.
adminForgetDevice_userPoolId :: Lens.Lens' AdminForgetDevice Prelude.Text
adminForgetDevice_userPoolId = Lens.lens (\AdminForgetDevice' {userPoolId} -> userPoolId) (\s@AdminForgetDevice' {} a -> s {userPoolId = a} :: AdminForgetDevice)

-- | The user name.
adminForgetDevice_username :: Lens.Lens' AdminForgetDevice Prelude.Text
adminForgetDevice_username = Lens.lens (\AdminForgetDevice' {username} -> username) (\s@AdminForgetDevice' {} a -> s {username = a} :: AdminForgetDevice) Prelude.. Prelude._Sensitive

-- | The device key.
adminForgetDevice_deviceKey :: Lens.Lens' AdminForgetDevice Prelude.Text
adminForgetDevice_deviceKey = Lens.lens (\AdminForgetDevice' {deviceKey} -> deviceKey) (\s@AdminForgetDevice' {} a -> s {deviceKey = a} :: AdminForgetDevice)

instance Prelude.AWSRequest AdminForgetDevice where
  type Rs AdminForgetDevice = AdminForgetDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AdminForgetDeviceResponse'

instance Prelude.Hashable AdminForgetDevice

instance Prelude.NFData AdminForgetDevice

instance Prelude.ToHeaders AdminForgetDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminForgetDevice" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminForgetDevice where
  toJSON AdminForgetDevice' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("Username" Prelude..= username),
            Prelude.Just ("DeviceKey" Prelude..= deviceKey)
          ]
      )

instance Prelude.ToPath AdminForgetDevice where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminForgetDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminForgetDeviceResponse' smart constructor.
data AdminForgetDeviceResponse = AdminForgetDeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminForgetDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAdminForgetDeviceResponse ::
  AdminForgetDeviceResponse
newAdminForgetDeviceResponse =
  AdminForgetDeviceResponse'

instance Prelude.NFData AdminForgetDeviceResponse
