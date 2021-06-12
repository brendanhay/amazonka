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
-- Module      : Network.AWS.AppStream.CreateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user in the user pool.
module Network.AWS.AppStream.CreateUser
  ( -- * Creating a Request
    CreateUser (..),
    newCreateUser,

    -- * Request Lenses
    createUser_messageAction,
    createUser_firstName,
    createUser_lastName,
    createUser_userName,
    createUser_authenticationType,

    -- * Destructuring the Response
    CreateUserResponse (..),
    newCreateUserResponse,

    -- * Response Lenses
    createUserResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateUser' smart constructor.
data CreateUser = CreateUser'
  { -- | The action to take for the welcome email that is sent to a user after
    -- the user is created in the user pool. If you specify SUPPRESS, no email
    -- is sent. If you specify RESEND, do not specify the first name or last
    -- name of the user. If the value is null, the email is sent.
    --
    -- The temporary password in the welcome email is valid for only 7 days. If
    -- users don’t set their passwords within 7 days, you must send them a new
    -- welcome email.
    messageAction :: Core.Maybe MessageAction,
    -- | The first name, or given name, of the user.
    firstName :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The last name, or surname, of the user.
    lastName :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The email address of the user.
    --
    -- Users\' email addresses are case-sensitive. During login, if they
    -- specify an email address that doesn\'t use the same capitalization as
    -- the email address specified when their user pool account was created, a
    -- \"user does not exist\" error message displays.
    userName :: Core.Sensitive Core.Text,
    -- | The authentication type for the user. You must specify USERPOOL.
    authenticationType :: AuthenticationType
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
-- 'messageAction', 'createUser_messageAction' - The action to take for the welcome email that is sent to a user after
-- the user is created in the user pool. If you specify SUPPRESS, no email
-- is sent. If you specify RESEND, do not specify the first name or last
-- name of the user. If the value is null, the email is sent.
--
-- The temporary password in the welcome email is valid for only 7 days. If
-- users don’t set their passwords within 7 days, you must send them a new
-- welcome email.
--
-- 'firstName', 'createUser_firstName' - The first name, or given name, of the user.
--
-- 'lastName', 'createUser_lastName' - The last name, or surname, of the user.
--
-- 'userName', 'createUser_userName' - The email address of the user.
--
-- Users\' email addresses are case-sensitive. During login, if they
-- specify an email address that doesn\'t use the same capitalization as
-- the email address specified when their user pool account was created, a
-- \"user does not exist\" error message displays.
--
-- 'authenticationType', 'createUser_authenticationType' - The authentication type for the user. You must specify USERPOOL.
newCreateUser ::
  -- | 'userName'
  Core.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  CreateUser
newCreateUser pUserName_ pAuthenticationType_ =
  CreateUser'
    { messageAction = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing,
      userName = Core._Sensitive Lens.# pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | The action to take for the welcome email that is sent to a user after
-- the user is created in the user pool. If you specify SUPPRESS, no email
-- is sent. If you specify RESEND, do not specify the first name or last
-- name of the user. If the value is null, the email is sent.
--
-- The temporary password in the welcome email is valid for only 7 days. If
-- users don’t set their passwords within 7 days, you must send them a new
-- welcome email.
createUser_messageAction :: Lens.Lens' CreateUser (Core.Maybe MessageAction)
createUser_messageAction = Lens.lens (\CreateUser' {messageAction} -> messageAction) (\s@CreateUser' {} a -> s {messageAction = a} :: CreateUser)

-- | The first name, or given name, of the user.
createUser_firstName :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_firstName = Lens.lens (\CreateUser' {firstName} -> firstName) (\s@CreateUser' {} a -> s {firstName = a} :: CreateUser) Core.. Lens.mapping Core._Sensitive

-- | The last name, or surname, of the user.
createUser_lastName :: Lens.Lens' CreateUser (Core.Maybe Core.Text)
createUser_lastName = Lens.lens (\CreateUser' {lastName} -> lastName) (\s@CreateUser' {} a -> s {lastName = a} :: CreateUser) Core.. Lens.mapping Core._Sensitive

-- | The email address of the user.
--
-- Users\' email addresses are case-sensitive. During login, if they
-- specify an email address that doesn\'t use the same capitalization as
-- the email address specified when their user pool account was created, a
-- \"user does not exist\" error message displays.
createUser_userName :: Lens.Lens' CreateUser Core.Text
createUser_userName = Lens.lens (\CreateUser' {userName} -> userName) (\s@CreateUser' {} a -> s {userName = a} :: CreateUser) Core.. Core._Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
createUser_authenticationType :: Lens.Lens' CreateUser AuthenticationType
createUser_authenticationType = Lens.lens (\CreateUser' {authenticationType} -> authenticationType) (\s@CreateUser' {} a -> s {authenticationType = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateUserResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateUser

instance Core.NFData CreateUser

instance Core.ToHeaders CreateUser where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CreateUser" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MessageAction" Core..=) Core.<$> messageAction,
            ("FirstName" Core..=) Core.<$> firstName,
            ("LastName" Core..=) Core.<$> lastName,
            Core.Just ("UserName" Core..= userName),
            Core.Just
              ("AuthenticationType" Core..= authenticationType)
          ]
      )

instance Core.ToPath CreateUser where
  toPath = Core.const "/"

instance Core.ToQuery CreateUser where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Core.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Core.NFData CreateUserResponse
