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
-- Module      : Network.AWS.AlexaBusiness.CreateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user.
module Network.AWS.AlexaBusiness.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_email,
    createUser_tags,
    createUser_clientRequestToken,
    createUser_firstName,
    createUser_lastName,
    createUser_userId,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_userArn,
    createUserResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The email address for the user.
    email :: Core.Maybe Core.Text,
    -- | The tags for the user.
    tags :: Core.Maybe [Tag],
    -- | A unique, user-specified identifier for this request that ensures
    -- idempotency.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The first name for the user.
    firstName :: Core.Maybe Core.Text,
    -- | The last name for the user.
    lastName :: Core.Maybe Core.Text,
    -- | The ARN for the user.
    userId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'email', 'createUser_email' - The email address for the user.
--
-- 'tags', 'createUser_tags' - The tags for the user.
--
-- 'clientRequestToken', 'createUser_clientRequestToken' - A unique, user-specified identifier for this request that ensures
-- idempotency.
--
-- 'firstName', 'createUser_firstName' - The first name for the user.
--
-- 'lastName', 'createUser_lastName' - The last name for the user.
--
-- 'userId', 'createUser_userId' - The ARN for the user.
newCreateUser ::
  -- | 'userId'
  Core.Text ->
  CreateUser
newCreateUser pUserId_ =
  CreateUser'
    { email = Core.Nothing,
      tags = Core.Nothing,
      clientRequestToken = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing,
      userId = pUserId_
    }

-- | The email address for the user.
createUser_email :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_email = Lens.lens (\CreateUser' {email} -> email) (\s@CreateUser' {} a -> s {email = a} :: CreateUser)

-- | The tags for the user.
createUser_tags :: Lens.Lens' CreateUser (Core.Maybe [Tag])
createUser_tags = Lens.lens (\CreateUser' {tags} -> tags) (\s@CreateUser' {} a -> s {tags = a} :: CreateUser) Core.. Lens.mapping Lens._Coerce

-- | A unique, user-specified identifier for this request that ensures
-- idempotency.
createUser_clientRequestToken :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_clientRequestToken = Lens.lens (\CreateUser' {clientRequestToken} -> clientRequestToken) (\s@CreateUser' {} a -> s {clientRequestToken = a} :: CreateUser)

-- | The first name for the user.
createUser_firstName :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_firstName = Lens.lens (\CreateUser' {firstName} -> firstName) (\s@CreateUser' {} a -> s {firstName = a} :: CreateUser)

-- | The last name for the user.
createUser_lastName :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_lastName = Lens.lens (\CreateUser' {lastName} -> lastName) (\s@CreateUser' {} a -> s {lastName = a} :: CreateUser)

-- | The ARN for the user.
createUser_userId :: Lens.Lens' CreateUser Core.Text
createUser_userId = Lens.lens (\CreateUser' {userId} -> userId) (\s@CreateUser' {} a -> s {userId = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Core.<$> (x Core..?> "UserArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUser

instance Core.NFData CreateUser

instance Core.ToHeaders CreateUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.CreateUser" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Email" Core..=) Core.<$> email,
            ("Tags" Core..=) Core.<$> tags,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("FirstName" Core..=) Core.<$> firstName,
            ("LastName" Core..=) Core.<$> lastName,
            Core.Just ("UserId" Core..= userId)
          ]
      )

instance Core.ToPath CreateUser where
  toPath = Core.const "/"

instance Core.ToQuery CreateUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The ARN of the newly created user in the response.
    userArn :: Core.Maybe Core.Text,
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
-- 'userArn', 'createUserResponse_userArn' - The ARN of the newly created user in the response.
--
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse'
    { userArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created user in the response.
createUserResponse_userArn :: Lens.Lens' CreateUserResponse (Core.Maybe Core.Text)
createUserResponse_userArn = Lens.lens (\CreateUserResponse' {userArn} -> userArn) (\s@CreateUserResponse' {} a -> s {userArn = a} :: CreateUserResponse)

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Core.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Core.NFData CreateUserResponse
