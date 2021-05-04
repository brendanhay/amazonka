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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update the device status, as an administrator.
--
-- /See:/ 'newAdminUpdateDeviceStatus' smart constructor.
data AdminUpdateDeviceStatus = AdminUpdateDeviceStatus'
  { -- | The status indicating whether a device has been remembered or not.
    deviceRememberedStatus :: Prelude.Maybe DeviceRememberedStatusType,
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The user name.
    username :: Prelude.Sensitive Prelude.Text,
    -- | The device key.
    deviceKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  -- | 'deviceKey'
  Prelude.Text ->
  AdminUpdateDeviceStatus
newAdminUpdateDeviceStatus
  pUserPoolId_
  pUsername_
  pDeviceKey_ =
    AdminUpdateDeviceStatus'
      { deviceRememberedStatus =
          Prelude.Nothing,
        userPoolId = pUserPoolId_,
        username = Prelude._Sensitive Lens.# pUsername_,
        deviceKey = pDeviceKey_
      }

-- | The status indicating whether a device has been remembered or not.
adminUpdateDeviceStatus_deviceRememberedStatus :: Lens.Lens' AdminUpdateDeviceStatus (Prelude.Maybe DeviceRememberedStatusType)
adminUpdateDeviceStatus_deviceRememberedStatus = Lens.lens (\AdminUpdateDeviceStatus' {deviceRememberedStatus} -> deviceRememberedStatus) (\s@AdminUpdateDeviceStatus' {} a -> s {deviceRememberedStatus = a} :: AdminUpdateDeviceStatus)

-- | The user pool ID.
adminUpdateDeviceStatus_userPoolId :: Lens.Lens' AdminUpdateDeviceStatus Prelude.Text
adminUpdateDeviceStatus_userPoolId = Lens.lens (\AdminUpdateDeviceStatus' {userPoolId} -> userPoolId) (\s@AdminUpdateDeviceStatus' {} a -> s {userPoolId = a} :: AdminUpdateDeviceStatus)

-- | The user name.
adminUpdateDeviceStatus_username :: Lens.Lens' AdminUpdateDeviceStatus Prelude.Text
adminUpdateDeviceStatus_username = Lens.lens (\AdminUpdateDeviceStatus' {username} -> username) (\s@AdminUpdateDeviceStatus' {} a -> s {username = a} :: AdminUpdateDeviceStatus) Prelude.. Prelude._Sensitive

-- | The device key.
adminUpdateDeviceStatus_deviceKey :: Lens.Lens' AdminUpdateDeviceStatus Prelude.Text
adminUpdateDeviceStatus_deviceKey = Lens.lens (\AdminUpdateDeviceStatus' {deviceKey} -> deviceKey) (\s@AdminUpdateDeviceStatus' {} a -> s {deviceKey = a} :: AdminUpdateDeviceStatus)

instance Prelude.AWSRequest AdminUpdateDeviceStatus where
  type
    Rs AdminUpdateDeviceStatus =
      AdminUpdateDeviceStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminUpdateDeviceStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminUpdateDeviceStatus

instance Prelude.NFData AdminUpdateDeviceStatus

instance Prelude.ToHeaders AdminUpdateDeviceStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminUpdateDeviceStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminUpdateDeviceStatus where
  toJSON AdminUpdateDeviceStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeviceRememberedStatus" Prelude..=)
              Prelude.<$> deviceRememberedStatus,
            Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("Username" Prelude..= username),
            Prelude.Just ("DeviceKey" Prelude..= deviceKey)
          ]
      )

instance Prelude.ToPath AdminUpdateDeviceStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminUpdateDeviceStatus where
  toQuery = Prelude.const Prelude.mempty

-- | The status response from the request to update the device, as an
-- administrator.
--
-- /See:/ 'newAdminUpdateDeviceStatusResponse' smart constructor.
data AdminUpdateDeviceStatusResponse = AdminUpdateDeviceStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AdminUpdateDeviceStatusResponse
newAdminUpdateDeviceStatusResponse pHttpStatus_ =
  AdminUpdateDeviceStatusResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminUpdateDeviceStatusResponse_httpStatus :: Lens.Lens' AdminUpdateDeviceStatusResponse Prelude.Int
adminUpdateDeviceStatusResponse_httpStatus = Lens.lens (\AdminUpdateDeviceStatusResponse' {httpStatus} -> httpStatus) (\s@AdminUpdateDeviceStatusResponse' {} a -> s {httpStatus = a} :: AdminUpdateDeviceStatusResponse)

instance
  Prelude.NFData
    AdminUpdateDeviceStatusResponse
