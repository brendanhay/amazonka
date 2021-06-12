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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminGetDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the device, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminGetDevice
  ( -- * Creating a Request
    AdminGetDevice (..),
    newAdminGetDevice,

    -- * Request Lenses
    adminGetDevice_deviceKey,
    adminGetDevice_userPoolId,
    adminGetDevice_username,

    -- * Destructuring the Response
    AdminGetDeviceResponse (..),
    newAdminGetDeviceResponse,

    -- * Response Lenses
    adminGetDeviceResponse_httpStatus,
    adminGetDeviceResponse_device,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to get the device, as an administrator.
--
-- /See:/ 'newAdminGetDevice' smart constructor.
data AdminGetDevice = AdminGetDevice'
  { -- | The device key.
    deviceKey :: Core.Text,
    -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The user name.
    username :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminGetDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceKey', 'adminGetDevice_deviceKey' - The device key.
--
-- 'userPoolId', 'adminGetDevice_userPoolId' - The user pool ID.
--
-- 'username', 'adminGetDevice_username' - The user name.
newAdminGetDevice ::
  -- | 'deviceKey'
  Core.Text ->
  -- | 'userPoolId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  AdminGetDevice
newAdminGetDevice pDeviceKey_ pUserPoolId_ pUsername_ =
  AdminGetDevice'
    { deviceKey = pDeviceKey_,
      userPoolId = pUserPoolId_,
      username = Core._Sensitive Lens.# pUsername_
    }

-- | The device key.
adminGetDevice_deviceKey :: Lens.Lens' AdminGetDevice Core.Text
adminGetDevice_deviceKey = Lens.lens (\AdminGetDevice' {deviceKey} -> deviceKey) (\s@AdminGetDevice' {} a -> s {deviceKey = a} :: AdminGetDevice)

-- | The user pool ID.
adminGetDevice_userPoolId :: Lens.Lens' AdminGetDevice Core.Text
adminGetDevice_userPoolId = Lens.lens (\AdminGetDevice' {userPoolId} -> userPoolId) (\s@AdminGetDevice' {} a -> s {userPoolId = a} :: AdminGetDevice)

-- | The user name.
adminGetDevice_username :: Lens.Lens' AdminGetDevice Core.Text
adminGetDevice_username = Lens.lens (\AdminGetDevice' {username} -> username) (\s@AdminGetDevice' {} a -> s {username = a} :: AdminGetDevice) Core.. Core._Sensitive

instance Core.AWSRequest AdminGetDevice where
  type
    AWSResponse AdminGetDevice =
      AdminGetDeviceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminGetDeviceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Device")
      )

instance Core.Hashable AdminGetDevice

instance Core.NFData AdminGetDevice

instance Core.ToHeaders AdminGetDevice where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminGetDevice" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminGetDevice where
  toJSON AdminGetDevice' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeviceKey" Core..= deviceKey),
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username)
          ]
      )

instance Core.ToPath AdminGetDevice where
  toPath = Core.const "/"

instance Core.ToQuery AdminGetDevice where
  toQuery = Core.const Core.mempty

-- | Gets the device response, as an administrator.
--
-- /See:/ 'newAdminGetDeviceResponse' smart constructor.
data AdminGetDeviceResponse = AdminGetDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The device.
    device :: DeviceType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminGetDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminGetDeviceResponse_httpStatus' - The response's http status code.
--
-- 'device', 'adminGetDeviceResponse_device' - The device.
newAdminGetDeviceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'device'
  DeviceType ->
  AdminGetDeviceResponse
newAdminGetDeviceResponse pHttpStatus_ pDevice_ =
  AdminGetDeviceResponse'
    { httpStatus = pHttpStatus_,
      device = pDevice_
    }

-- | The response's http status code.
adminGetDeviceResponse_httpStatus :: Lens.Lens' AdminGetDeviceResponse Core.Int
adminGetDeviceResponse_httpStatus = Lens.lens (\AdminGetDeviceResponse' {httpStatus} -> httpStatus) (\s@AdminGetDeviceResponse' {} a -> s {httpStatus = a} :: AdminGetDeviceResponse)

-- | The device.
adminGetDeviceResponse_device :: Lens.Lens' AdminGetDeviceResponse DeviceType
adminGetDeviceResponse_device = Lens.lens (\AdminGetDeviceResponse' {device} -> device) (\s@AdminGetDeviceResponse' {} a -> s {device = a} :: AdminGetDeviceResponse)

instance Core.NFData AdminGetDeviceResponse
