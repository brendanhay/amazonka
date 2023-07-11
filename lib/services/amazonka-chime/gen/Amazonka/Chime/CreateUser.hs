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
-- Module      : Amazonka.Chime.CreateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user under the specified Amazon Chime account.
module Amazonka.Chime.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_email,
    createUser_userType,
    createUser_username,
    createUser_accountId,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_user,
    createUserResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The user\'s email address.
    email :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The user type.
    userType :: Prelude.Maybe UserType,
    -- | The user name.
    username :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'email', 'createUser_email' - The user\'s email address.
--
-- 'userType', 'createUser_userType' - The user type.
--
-- 'username', 'createUser_username' - The user name.
--
-- 'accountId', 'createUser_accountId' - The Amazon Chime account ID.
newCreateUser ::
  -- | 'accountId'
  Prelude.Text ->
  CreateUser
newCreateUser pAccountId_ =
  CreateUser'
    { email = Prelude.Nothing,
      userType = Prelude.Nothing,
      username = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | The user\'s email address.
createUser_email :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_email = Lens.lens (\CreateUser' {email} -> email) (\s@CreateUser' {} a -> s {email = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | The user type.
createUser_userType :: Lens.Lens' CreateUser (Prelude.Maybe UserType)
createUser_userType = Lens.lens (\CreateUser' {userType} -> userType) (\s@CreateUser' {} a -> s {userType = a} :: CreateUser)

-- | The user name.
createUser_username :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_username = Lens.lens (\CreateUser' {username} -> username) (\s@CreateUser' {} a -> s {username = a} :: CreateUser)

-- | The Amazon Chime account ID.
createUser_accountId :: Lens.Lens' CreateUser Prelude.Text
createUser_accountId = Lens.lens (\CreateUser' {accountId} -> accountId) (\s@CreateUser' {} a -> s {accountId = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (x Data..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser where
  hashWithSalt _salt CreateUser' {..} =
    _salt
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` userType
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData CreateUser where
  rnf CreateUser' {..} =
    Prelude.rnf email
      `Prelude.seq` Prelude.rnf userType
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf accountId

instance Data.ToHeaders CreateUser where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Email" Data..=) Prelude.<$> email,
            ("UserType" Data..=) Prelude.<$> userType,
            ("Username" Data..=) Prelude.<$> username
          ]
      )

instance Data.ToPath CreateUser where
  toPath CreateUser' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS accountId, "/users"]

instance Data.ToQuery CreateUser where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=create"])

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { user :: Prelude.Maybe User,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'createUserResponse_user' - Undocumented member.
--
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse'
    { user = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createUserResponse_user :: Lens.Lens' CreateUserResponse (Prelude.Maybe User)
createUserResponse_user = Lens.lens (\CreateUserResponse' {user} -> user) (\s@CreateUserResponse' {} a -> s {user = a} :: CreateUserResponse)

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse where
  rnf CreateUserResponse' {..} =
    Prelude.rnf user
      `Prelude.seq` Prelude.rnf httpStatus
