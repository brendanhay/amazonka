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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The amount of storage for the user.
    storageRule :: Core.Maybe StorageRuleType,
    -- | The ID of the organization.
    organizationId :: Core.Maybe Core.Text,
    -- | The time zone ID of the user.
    timeZoneId :: Core.Maybe Core.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The email address of the user.
    emailAddress :: Core.Maybe Core.Text,
    -- | The login name of the user.
    username :: Core.Text,
    -- | The given name of the user.
    givenName :: Core.Text,
    -- | The surname of the user.
    surname :: Core.Text,
    -- | The password of the user.
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
  Core.Text ->
  -- | 'givenName'
  Core.Text ->
  -- | 'surname'
  Core.Text ->
  -- | 'password'
  Core.Text ->
  CreateUser
newCreateUser
  pUsername_
  pGivenName_
  pSurname_
  pPassword_ =
    CreateUser'
      { storageRule = Core.Nothing,
        organizationId = Core.Nothing,
        timeZoneId = Core.Nothing,
        authenticationToken = Core.Nothing,
        emailAddress = Core.Nothing,
        username = pUsername_,
        givenName = pGivenName_,
        surname = pSurname_,
        password = Core._Sensitive Lens.# pPassword_
      }

-- | The amount of storage for the user.
createUser_storageRule :: Lens.Lens' CreateUser (Core.Maybe StorageRuleType)
createUser_storageRule = Lens.lens (\CreateUser' {storageRule} -> storageRule) (\s@CreateUser' {} a -> s {storageRule = a} :: CreateUser)

-- | The ID of the organization.
createUser_organizationId :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_organizationId = Lens.lens (\CreateUser' {organizationId} -> organizationId) (\s@CreateUser' {} a -> s {organizationId = a} :: CreateUser)

-- | The time zone ID of the user.
createUser_timeZoneId :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_timeZoneId = Lens.lens (\CreateUser' {timeZoneId} -> timeZoneId) (\s@CreateUser' {} a -> s {timeZoneId = a} :: CreateUser)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
createUser_authenticationToken :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_authenticationToken = Lens.lens (\CreateUser' {authenticationToken} -> authenticationToken) (\s@CreateUser' {} a -> s {authenticationToken = a} :: CreateUser) Core.. Lens.mapping Core._Sensitive

-- | The email address of the user.
createUser_emailAddress :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_emailAddress = Lens.lens (\CreateUser' {emailAddress} -> emailAddress) (\s@CreateUser' {} a -> s {emailAddress = a} :: CreateUser)

-- | The login name of the user.
createUser_username :: Lens.Lens' CreateUser Core.Text
createUser_username = Lens.lens (\CreateUser' {username} -> username) (\s@CreateUser' {} a -> s {username = a} :: CreateUser)

-- | The given name of the user.
createUser_givenName :: Lens.Lens' CreateUser Core.Text
createUser_givenName = Lens.lens (\CreateUser' {givenName} -> givenName) (\s@CreateUser' {} a -> s {givenName = a} :: CreateUser)

-- | The surname of the user.
createUser_surname :: Lens.Lens' CreateUser Core.Text
createUser_surname = Lens.lens (\CreateUser' {surname} -> surname) (\s@CreateUser' {} a -> s {surname = a} :: CreateUser)

-- | The password of the user.
createUser_password :: Lens.Lens' CreateUser Core.Text
createUser_password = Lens.lens (\CreateUser' {password} -> password) (\s@CreateUser' {} a -> s {password = a} :: CreateUser) Core.. Core._Sensitive

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserResponse'
            Core.<$> (x Core..?> "User")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUser

instance Core.NFData CreateUser

instance Core.ToHeaders CreateUser where
  toHeaders CreateUser' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StorageRule" Core..=) Core.<$> storageRule,
            ("OrganizationId" Core..=) Core.<$> organizationId,
            ("TimeZoneId" Core..=) Core.<$> timeZoneId,
            ("EmailAddress" Core..=) Core.<$> emailAddress,
            Core.Just ("Username" Core..= username),
            Core.Just ("GivenName" Core..= givenName),
            Core.Just ("Surname" Core..= surname),
            Core.Just ("Password" Core..= password)
          ]
      )

instance Core.ToPath CreateUser where
  toPath = Core.const "/api/v1/users"

instance Core.ToQuery CreateUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The user information.
    user :: Core.Maybe User,
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
-- 'user', 'createUserResponse_user' - The user information.
--
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse'
    { user = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user information.
createUserResponse_user :: Lens.Lens' CreateUserResponse (Core.Maybe User)
createUserResponse_user = Lens.lens (\CreateUserResponse' {user} -> user) (\s@CreateUserResponse' {} a -> s {user = a} :: CreateUserResponse)

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Core.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Core.NFData CreateUserResponse
