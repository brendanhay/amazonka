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
-- Module      : Network.AWS.AppStream.DisableUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified user in the user pool. Users can\'t sign in to
-- AppStream 2.0 until they are re-enabled. This action does not delete the
-- user.
module Network.AWS.AppStream.DisableUser
  ( -- * Creating a Request
    DisableUser (..),
    newDisableUser,

    -- * Request Lenses
    disableUser_userName,
    disableUser_authenticationType,

    -- * Destructuring the Response
    DisableUserResponse (..),
    newDisableUserResponse,

    -- * Response Lenses
    disableUserResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableUser' smart constructor.
data DisableUser = DisableUser'
  { -- | The email address of the user.
    --
    -- Users\' email addresses are case-sensitive.
    userName :: Core.Sensitive Core.Text,
    -- | The authentication type for the user. You must specify USERPOOL.
    authenticationType :: AuthenticationType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'disableUser_userName' - The email address of the user.
--
-- Users\' email addresses are case-sensitive.
--
-- 'authenticationType', 'disableUser_authenticationType' - The authentication type for the user. You must specify USERPOOL.
newDisableUser ::
  -- | 'userName'
  Core.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  DisableUser
newDisableUser pUserName_ pAuthenticationType_ =
  DisableUser'
    { userName =
        Core._Sensitive Lens.# pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | The email address of the user.
--
-- Users\' email addresses are case-sensitive.
disableUser_userName :: Lens.Lens' DisableUser Core.Text
disableUser_userName = Lens.lens (\DisableUser' {userName} -> userName) (\s@DisableUser' {} a -> s {userName = a} :: DisableUser) Core.. Core._Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
disableUser_authenticationType :: Lens.Lens' DisableUser AuthenticationType
disableUser_authenticationType = Lens.lens (\DisableUser' {authenticationType} -> authenticationType) (\s@DisableUser' {} a -> s {authenticationType = a} :: DisableUser)

instance Core.AWSRequest DisableUser where
  type AWSResponse DisableUser = DisableUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableUserResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisableUser

instance Core.NFData DisableUser

instance Core.ToHeaders DisableUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DisableUser" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisableUser where
  toJSON DisableUser' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserName" Core..= userName),
            Core.Just
              ("AuthenticationType" Core..= authenticationType)
          ]
      )

instance Core.ToPath DisableUser where
  toPath = Core.const "/"

instance Core.ToQuery DisableUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisableUserResponse' smart constructor.
data DisableUserResponse = DisableUserResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableUserResponse_httpStatus' - The response's http status code.
newDisableUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisableUserResponse
newDisableUserResponse pHttpStatus_ =
  DisableUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
disableUserResponse_httpStatus :: Lens.Lens' DisableUserResponse Core.Int
disableUserResponse_httpStatus = Lens.lens (\DisableUserResponse' {httpStatus} -> httpStatus) (\s@DisableUserResponse' {} a -> s {httpStatus = a} :: DisableUserResponse)

instance Core.NFData DisableUserResponse
