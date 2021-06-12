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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device status as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
  ( -- * Creating a Request
    AdminUpdateDeviceStatus (..),
    newAdminUpdateDeviceStatus,

    -- * Request Lenses
    adminUpdateDeviceStatus_deviceRememberedStatus,
    adminUpdateDeviceStatus_userPoolId,
    adminUpdateDeviceStatus_username,
    adminUpdateDeviceStatus_deviceKey,

    -- * Destructuring the Response
    AdminUpdateDeviceStatusResponse (..),
    newAdminUpdateDeviceStatusResponse,

    -- * Response Lenses
    adminUpdateDeviceStatusResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update the device status, as an administrator.
--
-- /See:/ 'newAdminUpdateDeviceStatus' smart constructor.
data AdminUpdateDeviceStatus = AdminUpdateDeviceStatus'
  { -- | The status indicating whether a device has been remembered or not.
    deviceRememberedStatus :: Core.Maybe DeviceRememberedStatusType,
    -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The user name.
    username :: Core.Sensitive Core.Text,
    -- | The device key.
    deviceKey :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminUpdateDeviceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceRememberedStatus', 'adminUpdateDeviceStatus_deviceRememberedStatus' - The status indicating whether a device has been remembered or not.
--
-- 'userPoolId', 'adminUpdateDeviceStatus_userPoolId' - The user pool ID.
--
-- 'username', 'adminUpdateDeviceStatus_username' - The user name.
--
-- 'deviceKey', 'adminUpdateDeviceStatus_deviceKey' - The device key.
newAdminUpdateDeviceStatus ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  -- | 'deviceKey'
  Core.Text ->
  AdminUpdateDeviceStatus
newAdminUpdateDeviceStatus
  pUserPoolId_
  pUsername_
  pDeviceKey_ =
    AdminUpdateDeviceStatus'
      { deviceRememberedStatus =
          Core.Nothing,
        userPoolId = pUserPoolId_,
        username = Core._Sensitive Lens.# pUsername_,
        deviceKey = pDeviceKey_
      }

-- | The status indicating whether a device has been remembered or not.
adminUpdateDeviceStatus_deviceRememberedStatus :: Lens.Lens' AdminUpdateDeviceStatus (Core.Maybe DeviceRememberedStatusType)
adminUpdateDeviceStatus_deviceRememberedStatus = Lens.lens (\AdminUpdateDeviceStatus' {deviceRememberedStatus} -> deviceRememberedStatus) (\s@AdminUpdateDeviceStatus' {} a -> s {deviceRememberedStatus = a} :: AdminUpdateDeviceStatus)

-- | The user pool ID.
adminUpdateDeviceStatus_userPoolId :: Lens.Lens' AdminUpdateDeviceStatus Core.Text
adminUpdateDeviceStatus_userPoolId = Lens.lens (\AdminUpdateDeviceStatus' {userPoolId} -> userPoolId) (\s@AdminUpdateDeviceStatus' {} a -> s {userPoolId = a} :: AdminUpdateDeviceStatus)

-- | The user name.
adminUpdateDeviceStatus_username :: Lens.Lens' AdminUpdateDeviceStatus Core.Text
adminUpdateDeviceStatus_username = Lens.lens (\AdminUpdateDeviceStatus' {username} -> username) (\s@AdminUpdateDeviceStatus' {} a -> s {username = a} :: AdminUpdateDeviceStatus) Core.. Core._Sensitive

-- | The device key.
adminUpdateDeviceStatus_deviceKey :: Lens.Lens' AdminUpdateDeviceStatus Core.Text
adminUpdateDeviceStatus_deviceKey = Lens.lens (\AdminUpdateDeviceStatus' {deviceKey} -> deviceKey) (\s@AdminUpdateDeviceStatus' {} a -> s {deviceKey = a} :: AdminUpdateDeviceStatus)

instance Core.AWSRequest AdminUpdateDeviceStatus where
  type
    AWSResponse AdminUpdateDeviceStatus =
      AdminUpdateDeviceStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminUpdateDeviceStatusResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AdminUpdateDeviceStatus

instance Core.NFData AdminUpdateDeviceStatus

instance Core.ToHeaders AdminUpdateDeviceStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminUpdateDeviceStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminUpdateDeviceStatus where
  toJSON AdminUpdateDeviceStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeviceRememberedStatus" Core..=)
              Core.<$> deviceRememberedStatus,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username),
            Core.Just ("DeviceKey" Core..= deviceKey)
          ]
      )

instance Core.ToPath AdminUpdateDeviceStatus where
  toPath = Core.const "/"

instance Core.ToQuery AdminUpdateDeviceStatus where
  toQuery = Core.const Core.mempty

-- | The status response from the request to update the device, as an
-- administrator.
--
-- /See:/ 'newAdminUpdateDeviceStatusResponse' smart constructor.
data AdminUpdateDeviceStatusResponse = AdminUpdateDeviceStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminUpdateDeviceStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminUpdateDeviceStatusResponse_httpStatus' - The response's http status code.
newAdminUpdateDeviceStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AdminUpdateDeviceStatusResponse
newAdminUpdateDeviceStatusResponse pHttpStatus_ =
  AdminUpdateDeviceStatusResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminUpdateDeviceStatusResponse_httpStatus :: Lens.Lens' AdminUpdateDeviceStatusResponse Core.Int
adminUpdateDeviceStatusResponse_httpStatus = Lens.lens (\AdminUpdateDeviceStatusResponse' {httpStatus} -> httpStatus) (\s@AdminUpdateDeviceStatusResponse' {} a -> s {httpStatus = a} :: AdminUpdateDeviceStatusResponse)

instance Core.NFData AdminUpdateDeviceStatusResponse
