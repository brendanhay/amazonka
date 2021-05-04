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
-- Module      : Network.AWS.AppStream.EnableUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a user in the user pool. After being enabled, users can sign in
-- to AppStream 2.0 and open applications from the stacks to which they are
-- assigned.
module Network.AWS.AppStream.EnableUser
  ( -- * Creating a Request
    EnableUser (..),
    newEnableUser,

    -- * Request Lenses
    enableUser_userName,
    enableUser_authenticationType,

    -- * Destructuring the Response
    EnableUserResponse (..),
    newEnableUserResponse,

    -- * Response Lenses
    enableUserResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableUser' smart constructor.
data EnableUser = EnableUser'
  { -- | The email address of the user.
    --
    -- Users\' email addresses are case-sensitive. During login, if they
    -- specify an email address that doesn\'t use the same capitalization as
    -- the email address specified when their user pool account was created, a
    -- \"user does not exist\" error message displays.
    userName :: Prelude.Sensitive Prelude.Text,
    -- | The authentication type for the user. You must specify USERPOOL.
    authenticationType :: AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'enableUser_userName' - The email address of the user.
--
-- Users\' email addresses are case-sensitive. During login, if they
-- specify an email address that doesn\'t use the same capitalization as
-- the email address specified when their user pool account was created, a
-- \"user does not exist\" error message displays.
--
-- 'authenticationType', 'enableUser_authenticationType' - The authentication type for the user. You must specify USERPOOL.
newEnableUser ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  EnableUser
newEnableUser pUserName_ pAuthenticationType_ =
  EnableUser'
    { userName =
        Prelude._Sensitive Lens.# pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | The email address of the user.
--
-- Users\' email addresses are case-sensitive. During login, if they
-- specify an email address that doesn\'t use the same capitalization as
-- the email address specified when their user pool account was created, a
-- \"user does not exist\" error message displays.
enableUser_userName :: Lens.Lens' EnableUser Prelude.Text
enableUser_userName = Lens.lens (\EnableUser' {userName} -> userName) (\s@EnableUser' {} a -> s {userName = a} :: EnableUser) Prelude.. Prelude._Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
enableUser_authenticationType :: Lens.Lens' EnableUser AuthenticationType
enableUser_authenticationType = Lens.lens (\EnableUser' {authenticationType} -> authenticationType) (\s@EnableUser' {} a -> s {authenticationType = a} :: EnableUser)

instance Prelude.AWSRequest EnableUser where
  type Rs EnableUser = EnableUserResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableUser

instance Prelude.NFData EnableUser

instance Prelude.ToHeaders EnableUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.EnableUser" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON EnableUser where
  toJSON EnableUser' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserName" Prelude..= userName),
            Prelude.Just
              ( "AuthenticationType"
                  Prelude..= authenticationType
              )
          ]
      )

instance Prelude.ToPath EnableUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableUserResponse' smart constructor.
data EnableUserResponse = EnableUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableUserResponse_httpStatus' - The response's http status code.
newEnableUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableUserResponse
newEnableUserResponse pHttpStatus_ =
  EnableUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
enableUserResponse_httpStatus :: Lens.Lens' EnableUserResponse Prelude.Int
enableUserResponse_httpStatus = Lens.lens (\EnableUserResponse' {httpStatus} -> httpStatus) (\s@EnableUserResponse' {} a -> s {httpStatus = a} :: EnableUserResponse)

instance Prelude.NFData EnableUserResponse
