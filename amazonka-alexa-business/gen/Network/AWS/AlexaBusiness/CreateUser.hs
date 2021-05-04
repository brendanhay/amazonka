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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The email address for the user.
    email :: Prelude.Maybe Prelude.Text,
    -- | The tags for the user.
    tags :: Prelude.Maybe [Tag],
    -- | A unique, user-specified identifier for this request that ensures
    -- idempotency.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The first name for the user.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The last name for the user.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateUser
newCreateUser pUserId_ =
  CreateUser'
    { email = Prelude.Nothing,
      tags = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastName = Prelude.Nothing,
      userId = pUserId_
    }

-- | The email address for the user.
createUser_email :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_email = Lens.lens (\CreateUser' {email} -> email) (\s@CreateUser' {} a -> s {email = a} :: CreateUser)

-- | The tags for the user.
createUser_tags :: Lens.Lens' CreateUser (Prelude.Maybe [Tag])
createUser_tags = Lens.lens (\CreateUser' {tags} -> tags) (\s@CreateUser' {} a -> s {tags = a} :: CreateUser) Prelude.. Lens.mapping Prelude._Coerce

-- | A unique, user-specified identifier for this request that ensures
-- idempotency.
createUser_clientRequestToken :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_clientRequestToken = Lens.lens (\CreateUser' {clientRequestToken} -> clientRequestToken) (\s@CreateUser' {} a -> s {clientRequestToken = a} :: CreateUser)

-- | The first name for the user.
createUser_firstName :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_firstName = Lens.lens (\CreateUser' {firstName} -> firstName) (\s@CreateUser' {} a -> s {firstName = a} :: CreateUser)

-- | The last name for the user.
createUser_lastName :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_lastName = Lens.lens (\CreateUser' {lastName} -> lastName) (\s@CreateUser' {} a -> s {lastName = a} :: CreateUser)

-- | The ARN for the user.
createUser_userId :: Lens.Lens' CreateUser Prelude.Text
createUser_userId = Lens.lens (\CreateUser' {userId} -> userId) (\s@CreateUser' {} a -> s {userId = a} :: CreateUser)

instance Prelude.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (x Prelude..?> "UserArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser

instance Prelude.NFData CreateUser

instance Prelude.ToHeaders CreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.CreateUser" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Email" Prelude..=) Prelude.<$> email,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("ClientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            ("FirstName" Prelude..=) Prelude.<$> firstName,
            ("LastName" Prelude..=) Prelude.<$> lastName,
            Prelude.Just ("UserId" Prelude..= userId)
          ]
      )

instance Prelude.ToPath CreateUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The ARN of the newly created user in the response.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse'
    { userArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created user in the response.
createUserResponse_userArn :: Lens.Lens' CreateUserResponse (Prelude.Maybe Prelude.Text)
createUserResponse_userArn = Lens.lens (\CreateUserResponse' {userArn} -> userArn) (\s@CreateUserResponse' {} a -> s {userArn = a} :: CreateUserResponse)

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse
