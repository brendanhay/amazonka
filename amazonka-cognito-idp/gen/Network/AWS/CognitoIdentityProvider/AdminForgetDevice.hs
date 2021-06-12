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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Sends the forgot device request, as an administrator.
--
-- /See:/ 'newAdminForgetDevice' smart constructor.
data AdminForgetDevice = AdminForgetDevice'
  { -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The user name.
    username :: Core.Sensitive Core.Text,
    -- | The device key.
    deviceKey :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'username'
  Core.Text ->
  -- | 'deviceKey'
  Core.Text ->
  AdminForgetDevice
newAdminForgetDevice
  pUserPoolId_
  pUsername_
  pDeviceKey_ =
    AdminForgetDevice'
      { userPoolId = pUserPoolId_,
        username = Core._Sensitive Lens.# pUsername_,
        deviceKey = pDeviceKey_
      }

-- | The user pool ID.
adminForgetDevice_userPoolId :: Lens.Lens' AdminForgetDevice Core.Text
adminForgetDevice_userPoolId = Lens.lens (\AdminForgetDevice' {userPoolId} -> userPoolId) (\s@AdminForgetDevice' {} a -> s {userPoolId = a} :: AdminForgetDevice)

-- | The user name.
adminForgetDevice_username :: Lens.Lens' AdminForgetDevice Core.Text
adminForgetDevice_username = Lens.lens (\AdminForgetDevice' {username} -> username) (\s@AdminForgetDevice' {} a -> s {username = a} :: AdminForgetDevice) Core.. Core._Sensitive

-- | The device key.
adminForgetDevice_deviceKey :: Lens.Lens' AdminForgetDevice Core.Text
adminForgetDevice_deviceKey = Lens.lens (\AdminForgetDevice' {deviceKey} -> deviceKey) (\s@AdminForgetDevice' {} a -> s {deviceKey = a} :: AdminForgetDevice)

instance Core.AWSRequest AdminForgetDevice where
  type
    AWSResponse AdminForgetDevice =
      AdminForgetDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AdminForgetDeviceResponse'

instance Core.Hashable AdminForgetDevice

instance Core.NFData AdminForgetDevice

instance Core.ToHeaders AdminForgetDevice where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminForgetDevice" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminForgetDevice where
  toJSON AdminForgetDevice' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("DeviceKey" Core..= deviceKey)
          ]
      )

instance Core.ToPath AdminForgetDevice where
  toPath = Core.const "/"

instance Core.ToQuery AdminForgetDevice where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAdminForgetDeviceResponse' smart constructor.
data AdminForgetDeviceResponse = AdminForgetDeviceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminForgetDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAdminForgetDeviceResponse ::
  AdminForgetDeviceResponse
newAdminForgetDeviceResponse =
  AdminForgetDeviceResponse'

instance Core.NFData AdminForgetDeviceResponse
