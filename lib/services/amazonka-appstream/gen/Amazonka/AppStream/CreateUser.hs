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
-- Module      : Amazonka.AppStream.CreateUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user in the user pool.
module Amazonka.AppStream.CreateUser
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    messageAction :: Prelude.Maybe MessageAction,
    -- | The first name, or given name, of the user.
    firstName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The last name, or surname, of the user.
    lastName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The email address of the user.
    --
    -- Users\' email addresses are case-sensitive. During login, if they
    -- specify an email address that doesn\'t use the same capitalization as
    -- the email address specified when their user pool account was created, a
    -- \"user does not exist\" error message displays.
    userName :: Core.Sensitive Prelude.Text,
    -- | The authentication type for the user. You must specify USERPOOL.
    authenticationType :: AuthenticationType
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
  Prelude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  CreateUser
newCreateUser pUserName_ pAuthenticationType_ =
  CreateUser'
    { messageAction = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastName = Prelude.Nothing,
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
createUser_messageAction :: Lens.Lens' CreateUser (Prelude.Maybe MessageAction)
createUser_messageAction = Lens.lens (\CreateUser' {messageAction} -> messageAction) (\s@CreateUser' {} a -> s {messageAction = a} :: CreateUser)

-- | The first name, or given name, of the user.
createUser_firstName :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_firstName = Lens.lens (\CreateUser' {firstName} -> firstName) (\s@CreateUser' {} a -> s {firstName = a} :: CreateUser) Prelude.. Lens.mapping Core._Sensitive

-- | The last name, or surname, of the user.
createUser_lastName :: Lens.Lens' CreateUser (Prelude.Maybe Prelude.Text)
createUser_lastName = Lens.lens (\CreateUser' {lastName} -> lastName) (\s@CreateUser' {} a -> s {lastName = a} :: CreateUser) Prelude.. Lens.mapping Core._Sensitive

-- | The email address of the user.
--
-- Users\' email addresses are case-sensitive. During login, if they
-- specify an email address that doesn\'t use the same capitalization as
-- the email address specified when their user pool account was created, a
-- \"user does not exist\" error message displays.
createUser_userName :: Lens.Lens' CreateUser Prelude.Text
createUser_userName = Lens.lens (\CreateUser' {userName} -> userName) (\s@CreateUser' {} a -> s {userName = a} :: CreateUser) Prelude.. Core._Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
createUser_authenticationType :: Lens.Lens' CreateUser AuthenticationType
createUser_authenticationType = Lens.lens (\CreateUser' {authenticationType} -> authenticationType) (\s@CreateUser' {} a -> s {authenticationType = a} :: CreateUser)

instance Core.AWSRequest CreateUser where
  type AWSResponse CreateUser = CreateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUser where
  hashWithSalt _salt CreateUser' {..} =
    _salt `Prelude.hashWithSalt` messageAction
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` authenticationType

instance Prelude.NFData CreateUser where
  rnf CreateUser' {..} =
    Prelude.rnf messageAction
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf authenticationType

instance Core.ToHeaders CreateUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CreateUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateUser where
  toJSON CreateUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MessageAction" Core..=) Prelude.<$> messageAction,
            ("FirstName" Core..=) Prelude.<$> firstName,
            ("LastName" Core..=) Prelude.<$> lastName,
            Prelude.Just ("UserName" Core..= userName),
            Prelude.Just
              ("AuthenticationType" Core..= authenticationType)
          ]
      )

instance Core.ToPath CreateUser where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'createUserResponse_httpStatus' - The response's http status code.
newCreateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserResponse
newCreateUserResponse pHttpStatus_ =
  CreateUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createUserResponse_httpStatus :: Lens.Lens' CreateUserResponse Prelude.Int
createUserResponse_httpStatus = Lens.lens (\CreateUserResponse' {httpStatus} -> httpStatus) (\s@CreateUserResponse' {} a -> s {httpStatus = a} :: CreateUserResponse)

instance Prelude.NFData CreateUserResponse where
  rnf CreateUserResponse' {..} = Prelude.rnf httpStatus
