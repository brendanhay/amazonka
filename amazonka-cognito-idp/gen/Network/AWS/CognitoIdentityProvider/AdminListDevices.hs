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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminListDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists devices, as an administrator.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminListDevices
  ( -- * Creating a Request
    AdminListDevices (..),
    newAdminListDevices,

    -- * Request Lenses
    adminListDevices_paginationToken,
    adminListDevices_limit,
    adminListDevices_userPoolId,
    adminListDevices_username,

    -- * Destructuring the Response
    AdminListDevicesResponse (..),
    newAdminListDevicesResponse,

    -- * Response Lenses
    adminListDevicesResponse_devices,
    adminListDevicesResponse_paginationToken,
    adminListDevicesResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list devices, as an administrator.
--
-- /See:/ 'newAdminListDevices' smart constructor.
data AdminListDevices = AdminListDevices'
  { -- | The pagination token.
    paginationToken :: Core.Maybe Core.Text,
    -- | The limit of the devices request.
    limit :: Core.Maybe Core.Natural,
    -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The user name.
    username :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminListDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'adminListDevices_paginationToken' - The pagination token.
--
-- 'limit', 'adminListDevices_limit' - The limit of the devices request.
--
-- 'userPoolId', 'adminListDevices_userPoolId' - The user pool ID.
--
-- 'username', 'adminListDevices_username' - The user name.
newAdminListDevices ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  AdminListDevices
newAdminListDevices pUserPoolId_ pUsername_ =
  AdminListDevices'
    { paginationToken = Core.Nothing,
      limit = Core.Nothing,
      userPoolId = pUserPoolId_,
      username = Core._Sensitive Lens.# pUsername_
    }

-- | The pagination token.
adminListDevices_paginationToken :: Lens.Lens' AdminListDevices (Core.Maybe Core.Text)
adminListDevices_paginationToken = Lens.lens (\AdminListDevices' {paginationToken} -> paginationToken) (\s@AdminListDevices' {} a -> s {paginationToken = a} :: AdminListDevices)

-- | The limit of the devices request.
adminListDevices_limit :: Lens.Lens' AdminListDevices (Core.Maybe Core.Natural)
adminListDevices_limit = Lens.lens (\AdminListDevices' {limit} -> limit) (\s@AdminListDevices' {} a -> s {limit = a} :: AdminListDevices)

-- | The user pool ID.
adminListDevices_userPoolId :: Lens.Lens' AdminListDevices Core.Text
adminListDevices_userPoolId = Lens.lens (\AdminListDevices' {userPoolId} -> userPoolId) (\s@AdminListDevices' {} a -> s {userPoolId = a} :: AdminListDevices)

-- | The user name.
adminListDevices_username :: Lens.Lens' AdminListDevices Core.Text
adminListDevices_username = Lens.lens (\AdminListDevices' {username} -> username) (\s@AdminListDevices' {} a -> s {username = a} :: AdminListDevices) Core.. Core._Sensitive

instance Core.AWSRequest AdminListDevices where
  type
    AWSResponse AdminListDevices =
      AdminListDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminListDevicesResponse'
            Core.<$> (x Core..?> "Devices" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "PaginationToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AdminListDevices

instance Core.NFData AdminListDevices

instance Core.ToHeaders AdminListDevices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminListDevices" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminListDevices where
  toJSON AdminListDevices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PaginationToken" Core..=)
              Core.<$> paginationToken,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username)
          ]
      )

instance Core.ToPath AdminListDevices where
  toPath = Core.const "/"

instance Core.ToQuery AdminListDevices where
  toQuery = Core.const Core.mempty

-- | Lists the device\'s response, as an administrator.
--
-- /See:/ 'newAdminListDevicesResponse' smart constructor.
data AdminListDevicesResponse = AdminListDevicesResponse'
  { -- | The devices in the list of devices response.
    devices :: Core.Maybe [DeviceType],
    -- | The pagination token.
    paginationToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminListDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devices', 'adminListDevicesResponse_devices' - The devices in the list of devices response.
--
-- 'paginationToken', 'adminListDevicesResponse_paginationToken' - The pagination token.
--
-- 'httpStatus', 'adminListDevicesResponse_httpStatus' - The response's http status code.
newAdminListDevicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AdminListDevicesResponse
newAdminListDevicesResponse pHttpStatus_ =
  AdminListDevicesResponse'
    { devices = Core.Nothing,
      paginationToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The devices in the list of devices response.
adminListDevicesResponse_devices :: Lens.Lens' AdminListDevicesResponse (Core.Maybe [DeviceType])
adminListDevicesResponse_devices = Lens.lens (\AdminListDevicesResponse' {devices} -> devices) (\s@AdminListDevicesResponse' {} a -> s {devices = a} :: AdminListDevicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token.
adminListDevicesResponse_paginationToken :: Lens.Lens' AdminListDevicesResponse (Core.Maybe Core.Text)
adminListDevicesResponse_paginationToken = Lens.lens (\AdminListDevicesResponse' {paginationToken} -> paginationToken) (\s@AdminListDevicesResponse' {} a -> s {paginationToken = a} :: AdminListDevicesResponse)

-- | The response's http status code.
adminListDevicesResponse_httpStatus :: Lens.Lens' AdminListDevicesResponse Core.Int
adminListDevicesResponse_httpStatus = Lens.lens (\AdminListDevicesResponse' {httpStatus} -> httpStatus) (\s@AdminListDevicesResponse' {} a -> s {httpStatus = a} :: AdminListDevicesResponse)

instance Core.NFData AdminListDevicesResponse
