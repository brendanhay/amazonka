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
-- Module      : Network.AWS.WorkDocs.ActivateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the specified user. Only active users can access Amazon
-- WorkDocs.
module Network.AWS.WorkDocs.ActivateUser
  ( -- * Creating a Request
    ActivateUser (..),
    newActivateUser,

    -- * Request Lenses
    activateUser_authenticationToken,
    activateUser_userId,

    -- * Destructuring the Response
    ActivateUserResponse (..),
    newActivateUserResponse,

    -- * Response Lenses
    activateUserResponse_user,
    activateUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newActivateUser' smart constructor.
data ActivateUser = ActivateUser'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the user.
    userId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActivateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'activateUser_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'userId', 'activateUser_userId' - The ID of the user.
newActivateUser ::
  -- | 'userId'
  Core.Text ->
  ActivateUser
newActivateUser pUserId_ =
  ActivateUser'
    { authenticationToken = Core.Nothing,
      userId = pUserId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
activateUser_authenticationToken :: Lens.Lens' ActivateUser (Core.Maybe Core.Text)
activateUser_authenticationToken = Lens.lens (\ActivateUser' {authenticationToken} -> authenticationToken) (\s@ActivateUser' {} a -> s {authenticationToken = a} :: ActivateUser) Core.. Lens.mapping Core._Sensitive

-- | The ID of the user.
activateUser_userId :: Lens.Lens' ActivateUser Core.Text
activateUser_userId = Lens.lens (\ActivateUser' {userId} -> userId) (\s@ActivateUser' {} a -> s {userId = a} :: ActivateUser)

instance Core.AWSRequest ActivateUser where
  type AWSResponse ActivateUser = ActivateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ActivateUserResponse'
            Core.<$> (x Core..?> "User")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ActivateUser

instance Core.NFData ActivateUser

instance Core.ToHeaders ActivateUser where
  toHeaders ActivateUser' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON ActivateUser where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath ActivateUser where
  toPath ActivateUser' {..} =
    Core.mconcat
      ["/api/v1/users/", Core.toBS userId, "/activation"]

instance Core.ToQuery ActivateUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newActivateUserResponse' smart constructor.
data ActivateUserResponse = ActivateUserResponse'
  { -- | The user information.
    user :: Core.Maybe User,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActivateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'activateUserResponse_user' - The user information.
--
-- 'httpStatus', 'activateUserResponse_httpStatus' - The response's http status code.
newActivateUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ActivateUserResponse
newActivateUserResponse pHttpStatus_ =
  ActivateUserResponse'
    { user = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user information.
activateUserResponse_user :: Lens.Lens' ActivateUserResponse (Core.Maybe User)
activateUserResponse_user = Lens.lens (\ActivateUserResponse' {user} -> user) (\s@ActivateUserResponse' {} a -> s {user = a} :: ActivateUserResponse)

-- | The response's http status code.
activateUserResponse_httpStatus :: Lens.Lens' ActivateUserResponse Core.Int
activateUserResponse_httpStatus = Lens.lens (\ActivateUserResponse' {httpStatus} -> httpStatus) (\s@ActivateUserResponse' {} a -> s {httpStatus = a} :: ActivateUserResponse)

instance Core.NFData ActivateUserResponse
