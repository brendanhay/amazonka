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
-- Module      : Amazonka.IAM.CreateLoginProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a password for the specified IAM user. A password allows an IAM
-- user to access Amazon Web Services services through the Amazon Web
-- Services Management Console.
--
-- You can use the CLI, the Amazon Web Services API, or the __Users__ page
-- in the IAM console to create a password for any IAM user. Use
-- ChangePassword to update your own existing password in the __My Security
-- Credentials__ page in the Amazon Web Services Management Console.
--
-- For more information about managing passwords, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing passwords>
-- in the /IAM User Guide/.
module Amazonka.IAM.CreateLoginProfile
  ( -- * Creating a Request
    CreateLoginProfile (..),
    newCreateLoginProfile,

    -- * Request Lenses
    createLoginProfile_passwordResetRequired,
    createLoginProfile_userName,
    createLoginProfile_password,

    -- * Destructuring the Response
    CreateLoginProfileResponse (..),
    newCreateLoginProfileResponse,

    -- * Response Lenses
    createLoginProfileResponse_httpStatus,
    createLoginProfileResponse_loginProfile,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLoginProfile' smart constructor.
data CreateLoginProfile = CreateLoginProfile'
  { -- | Specifies whether the user is required to set a new password on next
    -- sign-in.
    passwordResetRequired :: Prelude.Maybe Prelude.Bool,
    -- | The name of the IAM user to create a password for. The user must already
    -- exist.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
    -- | The new password for the user.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of characters. That string can
    -- include almost any printable ASCII character from the space (@\\u0020@)
    -- through the end of the ASCII character range (@\\u00FF@). You can also
    -- include the tab (@\\u0009@), line feed (@\\u000A@), and carriage return
    -- (@\\u000D@) characters. Any of these characters are valid in a password.
    -- However, many tools, such as the Amazon Web Services Management Console,
    -- might restrict the ability to type certain characters because they have
    -- special meaning within that tool.
    password :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoginProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordResetRequired', 'createLoginProfile_passwordResetRequired' - Specifies whether the user is required to set a new password on next
-- sign-in.
--
-- 'userName', 'createLoginProfile_userName' - The name of the IAM user to create a password for. The user must already
-- exist.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'password', 'createLoginProfile_password' - The new password for the user.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of characters. That string can
-- include almost any printable ASCII character from the space (@\\u0020@)
-- through the end of the ASCII character range (@\\u00FF@). You can also
-- include the tab (@\\u0009@), line feed (@\\u000A@), and carriage return
-- (@\\u000D@) characters. Any of these characters are valid in a password.
-- However, many tools, such as the Amazon Web Services Management Console,
-- might restrict the ability to type certain characters because they have
-- special meaning within that tool.
newCreateLoginProfile ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  CreateLoginProfile
newCreateLoginProfile pUserName_ pPassword_ =
  CreateLoginProfile'
    { passwordResetRequired =
        Prelude.Nothing,
      userName = pUserName_,
      password = Core._Sensitive Lens.# pPassword_
    }

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
createLoginProfile_passwordResetRequired :: Lens.Lens' CreateLoginProfile (Prelude.Maybe Prelude.Bool)
createLoginProfile_passwordResetRequired = Lens.lens (\CreateLoginProfile' {passwordResetRequired} -> passwordResetRequired) (\s@CreateLoginProfile' {} a -> s {passwordResetRequired = a} :: CreateLoginProfile)

-- | The name of the IAM user to create a password for. The user must already
-- exist.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
createLoginProfile_userName :: Lens.Lens' CreateLoginProfile Prelude.Text
createLoginProfile_userName = Lens.lens (\CreateLoginProfile' {userName} -> userName) (\s@CreateLoginProfile' {} a -> s {userName = a} :: CreateLoginProfile)

-- | The new password for the user.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of characters. That string can
-- include almost any printable ASCII character from the space (@\\u0020@)
-- through the end of the ASCII character range (@\\u00FF@). You can also
-- include the tab (@\\u0009@), line feed (@\\u000A@), and carriage return
-- (@\\u000D@) characters. Any of these characters are valid in a password.
-- However, many tools, such as the Amazon Web Services Management Console,
-- might restrict the ability to type certain characters because they have
-- special meaning within that tool.
createLoginProfile_password :: Lens.Lens' CreateLoginProfile Prelude.Text
createLoginProfile_password = Lens.lens (\CreateLoginProfile' {password} -> password) (\s@CreateLoginProfile' {} a -> s {password = a} :: CreateLoginProfile) Prelude.. Core._Sensitive

instance Core.AWSRequest CreateLoginProfile where
  type
    AWSResponse CreateLoginProfile =
      CreateLoginProfileResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateLoginProfileResult"
      ( \s h x ->
          CreateLoginProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "LoginProfile")
      )

instance Prelude.Hashable CreateLoginProfile where
  hashWithSalt _salt CreateLoginProfile' {..} =
    _salt `Prelude.hashWithSalt` passwordResetRequired
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` password

instance Prelude.NFData CreateLoginProfile where
  rnf CreateLoginProfile' {..} =
    Prelude.rnf passwordResetRequired
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf password

instance Core.ToHeaders CreateLoginProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateLoginProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLoginProfile where
  toQuery CreateLoginProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateLoginProfile" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "PasswordResetRequired"
          Core.=: passwordResetRequired,
        "UserName" Core.=: userName,
        "Password" Core.=: password
      ]

-- | Contains the response to a successful CreateLoginProfile request.
--
-- /See:/ 'newCreateLoginProfileResponse' smart constructor.
data CreateLoginProfileResponse = CreateLoginProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing the user name and password create date.
    loginProfile :: LoginProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoginProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLoginProfileResponse_httpStatus' - The response's http status code.
--
-- 'loginProfile', 'createLoginProfileResponse_loginProfile' - A structure containing the user name and password create date.
newCreateLoginProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'loginProfile'
  LoginProfile ->
  CreateLoginProfileResponse
newCreateLoginProfileResponse
  pHttpStatus_
  pLoginProfile_ =
    CreateLoginProfileResponse'
      { httpStatus =
          pHttpStatus_,
        loginProfile = pLoginProfile_
      }

-- | The response's http status code.
createLoginProfileResponse_httpStatus :: Lens.Lens' CreateLoginProfileResponse Prelude.Int
createLoginProfileResponse_httpStatus = Lens.lens (\CreateLoginProfileResponse' {httpStatus} -> httpStatus) (\s@CreateLoginProfileResponse' {} a -> s {httpStatus = a} :: CreateLoginProfileResponse)

-- | A structure containing the user name and password create date.
createLoginProfileResponse_loginProfile :: Lens.Lens' CreateLoginProfileResponse LoginProfile
createLoginProfileResponse_loginProfile = Lens.lens (\CreateLoginProfileResponse' {loginProfile} -> loginProfile) (\s@CreateLoginProfileResponse' {} a -> s {loginProfile = a} :: CreateLoginProfileResponse)

instance Prelude.NFData CreateLoginProfileResponse where
  rnf CreateLoginProfileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf loginProfile
