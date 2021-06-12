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
-- Module      : Network.AWS.AppStream.EnableUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a user in the user pool. After being enabled, users can sign in
-- to AppStream 2.0 and open applications from the stacks to which they are
-- assigned.
module Network.AWS.AppStream.EnableUser
  ( -- * Creating a Request
    EnableUser (..),
    newEnableUser,

    -- * Request Lenses
    enableUser_userName,
    enableUser_authenticationType,

    -- * Destructuring the Response
    EnableUserResponse (..),
    newEnableUserResponse,

    -- * Response Lenses
    enableUserResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableUser' smart constructor.
data EnableUser = EnableUser'
  { -- | The email address of the user.
    --
    -- Users\' email addresses are case-sensitive. During login, if they
    -- specify an email address that doesn\'t use the same capitalization as
    -- the email address specified when their user pool account was created, a
    -- \"user does not exist\" error message displays.
    userName :: Core.Sensitive Core.Text,
    -- | The authentication type for the user. You must specify USERPOOL.
    authenticationType :: AuthenticationType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'enableUser_userName' - The email address of the user.
--
-- Users\' email addresses are case-sensitive. During login, if they
-- specify an email address that doesn\'t use the same capitalization as
-- the email address specified when their user pool account was created, a
-- \"user does not exist\" error message displays.
--
-- 'authenticationType', 'enableUser_authenticationType' - The authentication type for the user. You must specify USERPOOL.
newEnableUser ::
  -- | 'userName'
  Core.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  EnableUser
newEnableUser pUserName_ pAuthenticationType_ =
  EnableUser'
    { userName =
        Core._Sensitive Lens.# pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | The email address of the user.
--
-- Users\' email addresses are case-sensitive. During login, if they
-- specify an email address that doesn\'t use the same capitalization as
-- the email address specified when their user pool account was created, a
-- \"user does not exist\" error message displays.
enableUser_userName :: Lens.Lens' EnableUser Core.Text
enableUser_userName = Lens.lens (\EnableUser' {userName} -> userName) (\s@EnableUser' {} a -> s {userName = a} :: EnableUser) Core.. Core._Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
enableUser_authenticationType :: Lens.Lens' EnableUser AuthenticationType
enableUser_authenticationType = Lens.lens (\EnableUser' {authenticationType} -> authenticationType) (\s@EnableUser' {} a -> s {authenticationType = a} :: EnableUser)

instance Core.AWSRequest EnableUser where
  type AWSResponse EnableUser = EnableUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableUserResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable EnableUser

instance Core.NFData EnableUser

instance Core.ToHeaders EnableUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.EnableUser" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON EnableUser where
  toJSON EnableUser' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserName" Core..= userName),
            Core.Just
              ("AuthenticationType" Core..= authenticationType)
          ]
      )

instance Core.ToPath EnableUser where
  toPath = Core.const "/"

instance Core.ToQuery EnableUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newEnableUserResponse' smart constructor.
data EnableUserResponse = EnableUserResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableUserResponse_httpStatus' - The response's http status code.
newEnableUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EnableUserResponse
newEnableUserResponse pHttpStatus_ =
  EnableUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
enableUserResponse_httpStatus :: Lens.Lens' EnableUserResponse Core.Int
enableUserResponse_httpStatus = Lens.lens (\EnableUserResponse' {httpStatus} -> httpStatus) (\s@EnableUserResponse' {} a -> s {httpStatus = a} :: EnableUserResponse)

instance Core.NFData EnableUserResponse
