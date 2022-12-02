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
-- Module      : Amazonka.AlexaBusiness.CreateUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user.
module Amazonka.AlexaBusiness.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_tags,
    createUser_clientRequestToken,
    createUser_firstName,
    createUser_email,
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The tags for the user.
    tags :: Prelude.Maybe [Tag],
    -- | A unique, user-specified identifier for this request that ensures
    -- idempotency.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The first name for the user.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The email address for the user.
    email :: Prelude.Maybe Prelude.Text,
    -- | The last name for the user.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createUser_tags' - The tags for the user.
--
-- 'clientRequestToken', 'createUser_clientRequestToken' - A unique, user-specified identifier for this request that ensures
-- idempotency.
--
-- 'firstName', 'createUser_firstName' - The first name for the user.
--
-- 'email', 'createUser_email' - The email address for the user.
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
    { tags = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      firstName = Prelude.Nothing,
      email = Prelude.Nothing,
      lastName = Prelude.Nothing,
      userId = pUserId_
    }

-- | The tags for the user.
createUser_tags :: Lens.Lens' CreateUser (Prelude.Maybe [Tag])
createUser_tags = Lens.lens (\CreateUser' {tags} -> tags) (\s@CreateUser' {} a -> s {tags = a} :: CreateUser) Prelude.. Lens.mapping Lens.coerced

-- | A unique, user-specified identifier for this request that ensures
-- idempotency.
createUser_clientRequestToken :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_clientRequestToken = Lens.lens (\CreateUser' {clientRequestToken} -> clientRequestToken) (\s@CreateUser' {} a -> s {clientRequestToken = a} :: CreateUser)

-- | The first name for the user.
createUser_firstName :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_firstName = Lens.lens (\CreateUser' {firstName} -> firstName) (\s@CreateUser' {} a -> s {firstName = a} :: CreateUser)

-- | The email address for the user.
createUser_email :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_email = Lens.lens (\CreateUser' {email} -> email) (\s@CreateUser' {} a -> s {email = a} :: CreateUser)

-- | The last name for the user.
createUser_lastName :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_lastName = Lens.lens (\CreateUser' {lastName} -> lastName) (\s@CreateUser' {} a -> s {lastName = a} :: CreateUser)

-- | The ARN for the user.
createUser_userId :: Lens.Lens' CreateUser Prelude.Text
createUser_userId = Lens.lens (\CreateUser' {userId} -> userId) (\s@CreateUser' {} a -> s {userId = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (x Data..?> "UserArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser where
  hashWithSalt _salt CreateUser' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` userId

instance Prelude.NFData CreateUser where
  rnf CreateUser' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders CreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.CreateUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("FirstName" Data..=) Prelude.<$> firstName,
            ("Email" Data..=) Prelude.<$> email,
            ("LastName" Data..=) Prelude.<$> lastName,
            Prelude.Just ("UserId" Data..= userId)
          ]
      )

instance Data.ToPath CreateUser where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The ARN of the newly created user in the response.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateUserResponse where
  rnf CreateUserResponse' {..} =
    Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf httpStatus
