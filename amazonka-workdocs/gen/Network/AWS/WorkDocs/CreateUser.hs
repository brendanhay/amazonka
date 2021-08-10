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
-- Module      : Network.AWS.WorkDocs.CreateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user in a Simple AD or Microsoft AD directory. The status of a
-- newly created user is \"ACTIVE\". New users can access Amazon WorkDocs.
module Network.AWS.WorkDocs.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_storageRule,
    createUser_organizationId,
    createUser_timeZoneId,
    createUser_authenticationToken,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The amount of storage for the user.
    storageRule :: Prelude.Maybe StorageRuleType,
    -- | The ID of the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The time zone ID of the user.
    timeZoneId :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The email address of the user.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The login name of the user.
    username :: Prelude.Text,
    -- | The given name of the user.
    givenName :: Prelude.Text,
    -- | The surname of the user.
    surname :: Prelude.Text,
    -- | The password of the user.
    password :: Core.Sensitive Prelude.Text
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
-- 'storageRule', 'createUser_storageRule' - The amount of storage for the user.
--
-- 'organizationId', 'createUser_organizationId' - The ID of the organization.
--
-- 'timeZoneId', 'createUser_timeZoneId' - The time zone ID of the user.
--
-- 'authenticationToken', 'createUser_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
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
      { storageRule = Prelude.Nothing,
        organizationId = Prelude.Nothing,
        timeZoneId = Prelude.Nothing,
        authenticationToken = Prelude.Nothing,
        emailAddress = Prelude.Nothing,
        username = pUsername_,
        givenName = pGivenName_,
        surname = pSurname_,
        password = Core._Sensitive Lens.# pPassword_
      }

-- | The amount of storage for the user.
createUser_storageRule :: Lens.Lens' CreateUser (Prelude.Maybe StorageRuleType)
createUser_storageRule = Lens.lens (\CreateUser' {storageRule} -> storageRule) (\s@CreateUser' {} a -> s {storageRule = a} :: CreateUser)

-- | The ID of the organization.
createUser_organizationId :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_organizationId = Lens.lens (\CreateUser' {organizationId} -> organizationId) (\s@CreateUser' {} a -> s {organizationId = a} :: CreateUser)

-- | The time zone ID of the user.
createUser_timeZoneId :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_timeZoneId = Lens.lens (\CreateUser' {timeZoneId} -> timeZoneId) (\s@CreateUser' {} a -> s {timeZoneId = a} :: CreateUser)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
createUser_authenticationToken :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_authenticationToken = Lens.lens (\CreateUser' {authenticationToken} -> authenticationToken) (\s@CreateUser' {} a -> s {authenticationToken = a} :: CreateUser) Prelude.. Lens.mapping Core._Sensitive

-- | The email address of the user.
createUser_emailAddress :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_emailAddress = Lens.lens (\CreateUser' {emailAddress} -> emailAddress) (\s@CreateUser' {} a -> s {emailAddress = a} :: CreateUser)

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
createUser_password = Lens.lens (\CreateUser' {password} -> password) (\s@CreateUser' {} a -> s {password = a} :: CreateUser) Prelude.. Core._Sensitive

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (x Core..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser

instance Prelude.NFData CreateUser

instance Core.ToHeaders CreateUser where
  toHeaders CreateUser' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StorageRule" Core..=) Prelude.<$> storageRule,
            ("OrganizationId" Core..=)
              Prelude.<$> organizationId,
            ("TimeZoneId" Core..=) Prelude.<$> timeZoneId,
            ("EmailAddress" Core..=) Prelude.<$> emailAddress,
            Prelude.Just ("Username" Core..= username),
            Prelude.Just ("GivenName" Core..= givenName),
            Prelude.Just ("Surname" Core..= surname),
            Prelude.Just ("Password" Core..= password)
          ]
      )

instance Core.ToPath CreateUser where
  toPath = Prelude.const "/api/v1/users"

instance Core.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The user information.
    user :: Prelude.Maybe User,
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

instance Prelude.NFData CreateUserResponse
