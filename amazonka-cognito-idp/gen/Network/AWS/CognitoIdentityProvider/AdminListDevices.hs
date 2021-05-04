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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list devices, as an administrator.
--
-- /See:/ 'newAdminListDevices' smart constructor.
data AdminListDevices = AdminListDevices'
  { -- | The pagination token.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The limit of the devices request.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The user name.
    username :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  AdminListDevices
newAdminListDevices pUserPoolId_ pUsername_ =
  AdminListDevices'
    { paginationToken =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      username = Prelude._Sensitive Lens.# pUsername_
    }

-- | The pagination token.
adminListDevices_paginationToken :: Lens.Lens' AdminListDevices (Prelude.Maybe Prelude.Text)
adminListDevices_paginationToken = Lens.lens (\AdminListDevices' {paginationToken} -> paginationToken) (\s@AdminListDevices' {} a -> s {paginationToken = a} :: AdminListDevices)

-- | The limit of the devices request.
adminListDevices_limit :: Lens.Lens' AdminListDevices (Prelude.Maybe Prelude.Natural)
adminListDevices_limit = Lens.lens (\AdminListDevices' {limit} -> limit) (\s@AdminListDevices' {} a -> s {limit = a} :: AdminListDevices)

-- | The user pool ID.
adminListDevices_userPoolId :: Lens.Lens' AdminListDevices Prelude.Text
adminListDevices_userPoolId = Lens.lens (\AdminListDevices' {userPoolId} -> userPoolId) (\s@AdminListDevices' {} a -> s {userPoolId = a} :: AdminListDevices)

-- | The user name.
adminListDevices_username :: Lens.Lens' AdminListDevices Prelude.Text
adminListDevices_username = Lens.lens (\AdminListDevices' {username} -> username) (\s@AdminListDevices' {} a -> s {username = a} :: AdminListDevices) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest AdminListDevices where
  type Rs AdminListDevices = AdminListDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AdminListDevicesResponse'
            Prelude.<$> (x Prelude..?> "Devices" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "PaginationToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AdminListDevices

instance Prelude.NFData AdminListDevices

instance Prelude.ToHeaders AdminListDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminListDevices" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminListDevices where
  toJSON AdminListDevices' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PaginationToken" Prelude..=)
              Prelude.<$> paginationToken,
            ("Limit" Prelude..=) Prelude.<$> limit,
            Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("Username" Prelude..= username)
          ]
      )

instance Prelude.ToPath AdminListDevices where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminListDevices where
  toQuery = Prelude.const Prelude.mempty

-- | Lists the device\'s response, as an administrator.
--
-- /See:/ 'newAdminListDevicesResponse' smart constructor.
data AdminListDevicesResponse = AdminListDevicesResponse'
  { -- | The devices in the list of devices response.
    devices :: Prelude.Maybe [DeviceType],
    -- | The pagination token.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AdminListDevicesResponse
newAdminListDevicesResponse pHttpStatus_ =
  AdminListDevicesResponse'
    { devices =
        Prelude.Nothing,
      paginationToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The devices in the list of devices response.
adminListDevicesResponse_devices :: Lens.Lens' AdminListDevicesResponse (Prelude.Maybe [DeviceType])
adminListDevicesResponse_devices = Lens.lens (\AdminListDevicesResponse' {devices} -> devices) (\s@AdminListDevicesResponse' {} a -> s {devices = a} :: AdminListDevicesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token.
adminListDevicesResponse_paginationToken :: Lens.Lens' AdminListDevicesResponse (Prelude.Maybe Prelude.Text)
adminListDevicesResponse_paginationToken = Lens.lens (\AdminListDevicesResponse' {paginationToken} -> paginationToken) (\s@AdminListDevicesResponse' {} a -> s {paginationToken = a} :: AdminListDevicesResponse)

-- | The response's http status code.
adminListDevicesResponse_httpStatus :: Lens.Lens' AdminListDevicesResponse Prelude.Int
adminListDevicesResponse_httpStatus = Lens.lens (\AdminListDevicesResponse' {httpStatus} -> httpStatus) (\s@AdminListDevicesResponse' {} a -> s {httpStatus = a} :: AdminListDevicesResponse)

instance Prelude.NFData AdminListDevicesResponse
