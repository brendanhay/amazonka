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
-- Module      : Amazonka.WorkDocs.CreateUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user in a Simple AD or Microsoft AD directory. The status of a
-- newly created user is \"ACTIVE\". New users can access Amazon WorkDocs.
module Amazonka.WorkDocs.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_timeZoneId,
    createUser_storageRule,
    createUser_authenticationToken,
    createUser_organizationId,
    createUser_emailAddress,
    createUser_username,
    createUser_givenName,
    createUser_surname,
    createUser_password,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_user,
    createUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The time zone ID of the user.
    timeZoneId :: Prelude.Maybe Prelude.Text,
    -- | The amount of storage for the user.
    storageRule :: Prelude.Maybe StorageRuleType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The email address of the user.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The login name of the user.
    username :: Prelude.Text,
    -- | The given name of the user.
    givenName :: Prelude.Text,
    -- | The surname of the user.
    surname :: Prelude.Text,
    -- | The password of the user.
    password :: Data.Sensitive Prelude.Text
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
-- 'timeZoneId', 'createUser_timeZoneId' - The time zone ID of the user.
--
-- 'storageRule', 'createUser_storageRule' - The amount of storage for the user.
--
-- 'authenticationToken', 'createUser_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'organizationId', 'createUser_organizationId' - The ID of the organization.
--
-- 'emailAddress', 'createUser_emailAddress' - The email address of the user.
--
-- 'username', 'createUser_username' - The login name of the user.
--
-- 'givenName', 'createUser_givenName' - The given name of the user.
--
-- 'surname', 'createUser_surname' - The surname of the user.
--
-- 'password', 'createUser_password' - The password of the user.
newCreateUser ::
  -- | 'username'
  Prelude.Text ->
  -- | 'givenName'
  Prelude.Text ->
  -- | 'surname'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  CreateUser
newCreateUser
  pUsername_
  pGivenName_
  pSurname_
  pPassword_ =
    CreateUser'
      { timeZoneId = Prelude.Nothing,
        storageRule = Prelude.Nothing,
        authenticationToken = Prelude.Nothing,
        organizationId = Prelude.Nothing,
        emailAddress = Prelude.Nothing,
        username = pUsername_,
        givenName = pGivenName_,
        surname = pSurname_,
        password = Data._Sensitive Lens.# pPassword_
      }

-- | The time zone ID of the user.
createUser_timeZoneId :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_timeZoneId = Lens.lens (\CreateUser' {timeZoneId} -> timeZoneId) (\s@CreateUser' {} a -> s {timeZoneId = a} :: CreateUser)

-- | The amount of storage for the user.
createUser_storageRule :: Lens.Lens' CreateUser (Prelude.Maybe StorageRuleType)
createUser_storageRule = Lens.lens (\CreateUser' {storageRule} -> storageRule) (\s@CreateUser' {} a -> s {storageRule = a} :: CreateUser)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
createUser_authenticationToken :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_authenticationToken = Lens.lens (\CreateUser' {authenticationToken} -> authenticationToken) (\s@CreateUser' {} a -> s {authenticationToken = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the organization.
createUser_organizationId :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_organizationId = Lens.lens (\CreateUser' {organizationId} -> organizationId) (\s@CreateUser' {} a -> s {organizationId = a} :: CreateUser)

-- | The email address of the user.
createUser_emailAddress :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_emailAddress = Lens.lens (\CreateUser' {emailAddress} -> emailAddress) (\s@CreateUser' {} a -> s {emailAddress = a} :: CreateUser) Prelude.. Lens.mapping Data._Sensitive

-- | The login name of the user.
createUser_username :: Lens.Lens' CreateUser Prelude.Text
createUser_username = Lens.lens (\CreateUser' {username} -> username) (\s@CreateUser' {} a -> s {username = a} :: CreateUser)

-- | The given name of the user.
createUser_givenName :: Lens.Lens' CreateUser Prelude.Text
createUser_givenName = Lens.lens (\CreateUser' {givenName} -> givenName) (\s@CreateUser' {} a -> s {givenName = a} :: CreateUser)

-- | The surname of the user.
createUser_surname :: Lens.Lens' CreateUser Prelude.Text
createUser_surname = Lens.lens (\CreateUser' {surname} -> surname) (\s@CreateUser' {} a -> s {surname = a} :: CreateUser)

-- | The password of the user.
createUser_password :: Lens.Lens' CreateUser Prelude.Text
createUser_password = Lens.lens (\CreateUser' {password} -> password) (\s@CreateUser' {} a -> s {password = a} :: CreateUser) Prelude.. Data._Sensitive

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
    _salt `Prelude.hashWithSalt` timeZoneId
      `Prelude.hashWithSalt` storageRule
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` givenName
      `Prelude.hashWithSalt` surname
      `Prelude.hashWithSalt` password

instance Prelude.NFData CreateUser where
  rnf CreateUser' {..} =
    Prelude.rnf timeZoneId
      `Prelude.seq` Prelude.rnf storageRule
      `Prelude.seq` Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf givenName
      `Prelude.seq` Prelude.rnf surname
      `Prelude.seq` Prelude.rnf password

instance Data.ToHeaders CreateUser where
  toHeaders CreateUser' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TimeZoneId" Data..=) Prelude.<$> timeZoneId,
            ("StorageRule" Data..=) Prelude.<$> storageRule,
            ("OrganizationId" Data..=)
              Prelude.<$> organizationId,
            ("EmailAddress" Data..=) Prelude.<$> emailAddress,
            Prelude.Just ("Username" Data..= username),
            Prelude.Just ("GivenName" Data..= givenName),
            Prelude.Just ("Surname" Data..= surname),
            Prelude.Just ("Password" Data..= password)
          ]
      )

instance Data.ToPath CreateUser where
  toPath = Prelude.const "/api/v1/users"

instance Data.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The user information.
    user :: Prelude.Maybe User,
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
-- 'user', 'createUserResponse_user' - The user information.
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

-- | The user information.
createUserResponse_user :: Lens.Lens' CreateUserResponse (Prelude.Maybe User)
createUserResponse_user = Lens.lens (\CreateUserResponse' {user} -> user) (\s@CreateUserResponse' {} a -> s {user = a} :: CreateUserResponse)

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse where
  rnf CreateUserResponse' {..} =
    Prelude.rnf user
      `Prelude.seq` Prelude.rnf httpStatus
