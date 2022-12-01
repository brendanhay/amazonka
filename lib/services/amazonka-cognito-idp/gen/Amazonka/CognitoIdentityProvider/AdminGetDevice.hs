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
-- Module      : Amazonka.CognitoIdentityProvider.AdminGetDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the device, as an administrator.
--
-- Calling this action requires developer credentials.
module Amazonka.CognitoIdentityProvider.AdminGetDevice
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to get the device, as an administrator.
--
-- /See:/ 'newAdminGetDevice' smart constructor.
data AdminGetDevice = AdminGetDevice'
  { -- | The device key.
    deviceKey :: Prelude.Text,
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The user name.
    username :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminGetDevice
newAdminGetDevice pDeviceKey_ pUserPoolId_ pUsername_ =
  AdminGetDevice'
    { deviceKey = pDeviceKey_,
      userPoolId = pUserPoolId_,
      username = Core._Sensitive Lens.# pUsername_
    }

-- | The device key.
adminGetDevice_deviceKey :: Lens.Lens' AdminGetDevice Prelude.Text
adminGetDevice_deviceKey = Lens.lens (\AdminGetDevice' {deviceKey} -> deviceKey) (\s@AdminGetDevice' {} a -> s {deviceKey = a} :: AdminGetDevice)

-- | The user pool ID.
adminGetDevice_userPoolId :: Lens.Lens' AdminGetDevice Prelude.Text
adminGetDevice_userPoolId = Lens.lens (\AdminGetDevice' {userPoolId} -> userPoolId) (\s@AdminGetDevice' {} a -> s {userPoolId = a} :: AdminGetDevice)

-- | The user name.
adminGetDevice_username :: Lens.Lens' AdminGetDevice Prelude.Text
adminGetDevice_username = Lens.lens (\AdminGetDevice' {username} -> username) (\s@AdminGetDevice' {} a -> s {username = a} :: AdminGetDevice) Prelude.. Core._Sensitive

instance Core.AWSRequest AdminGetDevice where
  type
    AWSResponse AdminGetDevice =
      AdminGetDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminGetDeviceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Device")
      )

instance Prelude.Hashable AdminGetDevice where
  hashWithSalt _salt AdminGetDevice' {..} =
    _salt `Prelude.hashWithSalt` deviceKey
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username

instance Prelude.NFData AdminGetDevice where
  rnf AdminGetDevice' {..} =
    Prelude.rnf deviceKey
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf username

instance Core.ToHeaders AdminGetDevice where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminGetDevice" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AdminGetDevice where
  toJSON AdminGetDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DeviceKey" Core..= deviceKey),
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("Username" Core..= username)
          ]
      )

instance Core.ToPath AdminGetDevice where
  toPath = Prelude.const "/"

instance Core.ToQuery AdminGetDevice where
  toQuery = Prelude.const Prelude.mempty

-- | Gets the device response, as an administrator.
--
-- /See:/ 'newAdminGetDeviceResponse' smart constructor.
data AdminGetDeviceResponse = AdminGetDeviceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The device.
    device :: DeviceType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'device'
  DeviceType ->
  AdminGetDeviceResponse
newAdminGetDeviceResponse pHttpStatus_ pDevice_ =
  AdminGetDeviceResponse'
    { httpStatus = pHttpStatus_,
      device = pDevice_
    }

-- | The response's http status code.
adminGetDeviceResponse_httpStatus :: Lens.Lens' AdminGetDeviceResponse Prelude.Int
adminGetDeviceResponse_httpStatus = Lens.lens (\AdminGetDeviceResponse' {httpStatus} -> httpStatus) (\s@AdminGetDeviceResponse' {} a -> s {httpStatus = a} :: AdminGetDeviceResponse)

-- | The device.
adminGetDeviceResponse_device :: Lens.Lens' AdminGetDeviceResponse DeviceType
adminGetDeviceResponse_device = Lens.lens (\AdminGetDeviceResponse' {device} -> device) (\s@AdminGetDeviceResponse' {} a -> s {device = a} :: AdminGetDeviceResponse)

instance Prelude.NFData AdminGetDeviceResponse where
  rnf AdminGetDeviceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf device
