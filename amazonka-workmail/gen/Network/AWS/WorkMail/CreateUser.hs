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
-- Module      : Network.AWS.WorkMail.CreateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user who can be used in Amazon WorkMail by calling the
-- RegisterToWorkMail operation.
module Network.AWS.WorkMail.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_organizationId,
    createUser_name,
    createUser_displayName,
    createUser_password,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_userId,
    createUserResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The identifier of the organization for which the user is created.
    organizationId :: Core.Text,
    -- | The name for the new user. WorkMail directory user names have a maximum
    -- length of 64. All others have a maximum length of 20.
    name :: Core.Text,
    -- | The display name for the new user.
    displayName :: Core.Text,
    -- | The password for the new user.
    password :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'createUser_organizationId' - The identifier of the organization for which the user is created.
--
-- 'name', 'createUser_name' - The name for the new user. WorkMail directory user names have a maximum
-- length of 64. All others have a maximum length of 20.
--
-- 'displayName', 'createUser_displayName' - The display name for the new user.
--
-- 'password', 'createUser_password' - The password for the new user.
newCreateUser ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'displayName'
  Core.Text ->
  -- | 'password'
  Core.Text ->
  CreateUser
newCreateUser
  pOrganizationId_
  pName_
  pDisplayName_
  pPassword_ =
    CreateUser'
      { organizationId = pOrganizationId_,
        name = pName_,
        displayName = pDisplayName_,
        password = Core._Sensitive Lens.# pPassword_
      }

-- | The identifier of the organization for which the user is created.
createUser_organizationId :: Lens.Lens' CreateUser Core.Text
createUser_organizationId = Lens.lens (\CreateUser' {organizationId} -> organizationId) (\s@CreateUser' {} a -> s {organizationId = a} :: CreateUser)

-- | The name for the new user. WorkMail directory user names have a maximum
-- length of 64. All others have a maximum length of 20.
createUser_name :: Lens.Lens' CreateUser Core.Text
createUser_name = Lens.lens (\CreateUser' {name} -> name) (\s@CreateUser' {} a -> s {name = a} :: CreateUser)

-- | The display name for the new user.
createUser_displayName :: Lens.Lens' CreateUser Core.Text
createUser_displayName = Lens.lens (\CreateUser' {displayName} -> displayName) (\s@CreateUser' {} a -> s {displayName = a} :: CreateUser)

-- | The password for the new user.
createUser_password :: Lens.Lens' CreateUser Core.Text
createUser_password = Lens.lens (\CreateUser' {password} -> password) (\s@CreateUser' {} a -> s {password = a} :: CreateUser) Core.. Core._Sensitive

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Core.<$> (x Core..?> "UserId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUser

instance Core.NFData CreateUser

instance Core.ToHeaders CreateUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("WorkMailService.CreateUser" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("Name" Core..= name),
            Core.Just ("DisplayName" Core..= displayName),
            Core.Just ("Password" Core..= password)
          ]
      )

instance Core.ToPath CreateUser where
  toPath = Core.const "/"

instance Core.ToQuery CreateUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The identifier for the new user.
    userId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'createUserResponse_userId' - The identifier for the new user.
--
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse'
    { userId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the new user.
createUserResponse_userId :: Lens.Lens' CreateUserResponse (Core.Maybe Core.Text)
createUserResponse_userId = Lens.lens (\CreateUserResponse' {userId} -> userId) (\s@CreateUserResponse' {} a -> s {userId = a} :: CreateUserResponse)

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Core.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Core.NFData CreateUserResponse
