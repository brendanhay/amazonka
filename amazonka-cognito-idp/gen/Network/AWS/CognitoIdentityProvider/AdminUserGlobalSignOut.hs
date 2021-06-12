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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signs out users from all devices, as an administrator. It also
-- invalidates all refresh tokens issued to a user. The user\'s current
-- access and Id tokens remain valid until their expiry. Access and Id
-- tokens expire one hour after they are issued.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
  ( -- * Creating a Request
    AdminUserGlobalSignOut (..),
    newAdminUserGlobalSignOut,

    -- * Request Lenses
    adminUserGlobalSignOut_userPoolId,
    adminUserGlobalSignOut_username,

    -- * Destructuring the Response
    AdminUserGlobalSignOutResponse (..),
    newAdminUserGlobalSignOutResponse,

    -- * Response Lenses
    adminUserGlobalSignOutResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to sign out of all devices, as an administrator.
--
-- /See:/ 'newAdminUserGlobalSignOut' smart constructor.
data AdminUserGlobalSignOut = AdminUserGlobalSignOut'
  { -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The user name.
    username :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminUserGlobalSignOut' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminUserGlobalSignOut_userPoolId' - The user pool ID.
--
-- 'username', 'adminUserGlobalSignOut_username' - The user name.
newAdminUserGlobalSignOut ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'username'
  Core.Text ->
  AdminUserGlobalSignOut
newAdminUserGlobalSignOut pUserPoolId_ pUsername_ =
  AdminUserGlobalSignOut'
    { userPoolId = pUserPoolId_,
      username = Core._Sensitive Lens.# pUsername_
    }

-- | The user pool ID.
adminUserGlobalSignOut_userPoolId :: Lens.Lens' AdminUserGlobalSignOut Core.Text
adminUserGlobalSignOut_userPoolId = Lens.lens (\AdminUserGlobalSignOut' {userPoolId} -> userPoolId) (\s@AdminUserGlobalSignOut' {} a -> s {userPoolId = a} :: AdminUserGlobalSignOut)

-- | The user name.
adminUserGlobalSignOut_username :: Lens.Lens' AdminUserGlobalSignOut Core.Text
adminUserGlobalSignOut_username = Lens.lens (\AdminUserGlobalSignOut' {username} -> username) (\s@AdminUserGlobalSignOut' {} a -> s {username = a} :: AdminUserGlobalSignOut) Core.. Core._Sensitive

instance Core.AWSRequest AdminUserGlobalSignOut where
  type
    AWSResponse AdminUserGlobalSignOut =
      AdminUserGlobalSignOutResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminUserGlobalSignOutResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AdminUserGlobalSignOut

instance Core.NFData AdminUserGlobalSignOut

instance Core.ToHeaders AdminUserGlobalSignOut where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.AdminUserGlobalSignOut" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AdminUserGlobalSignOut where
  toJSON AdminUserGlobalSignOut' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("Username" Core..= username)
          ]
      )

instance Core.ToPath AdminUserGlobalSignOut where
  toPath = Core.const "/"

instance Core.ToQuery AdminUserGlobalSignOut where
  toQuery = Core.const Core.mempty

-- | The global sign-out response, as an administrator.
--
-- /See:/ 'newAdminUserGlobalSignOutResponse' smart constructor.
data AdminUserGlobalSignOutResponse = AdminUserGlobalSignOutResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdminUserGlobalSignOutResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminUserGlobalSignOutResponse_httpStatus' - The response's http status code.
newAdminUserGlobalSignOutResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AdminUserGlobalSignOutResponse
newAdminUserGlobalSignOutResponse pHttpStatus_ =
  AdminUserGlobalSignOutResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminUserGlobalSignOutResponse_httpStatus :: Lens.Lens' AdminUserGlobalSignOutResponse Core.Int
adminUserGlobalSignOutResponse_httpStatus = Lens.lens (\AdminUserGlobalSignOutResponse' {httpStatus} -> httpStatus) (\s@AdminUserGlobalSignOutResponse' {} a -> s {httpStatus = a} :: AdminUserGlobalSignOutResponse)

instance Core.NFData AdminUserGlobalSignOutResponse
