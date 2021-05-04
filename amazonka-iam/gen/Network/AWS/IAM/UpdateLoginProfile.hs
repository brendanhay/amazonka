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
-- Module      : Network.AWS.IAM.UpdateLoginProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password for the specified IAM user. You can use the AWS
-- CLI, the AWS API, or the __Users__ page in the IAM console to change the
-- password for any IAM user. Use ChangePassword to change your own
-- password in the __My Security Credentials__ page in the AWS Management
-- Console.
--
-- For more information about modifying passwords, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing passwords>
-- in the /IAM User Guide/.
module Network.AWS.IAM.UpdateLoginProfile
  ( -- * Creating a Request
    UpdateLoginProfile (..),
    newUpdateLoginProfile,

    -- * Request Lenses
    updateLoginProfile_passwordResetRequired,
    updateLoginProfile_password,
    updateLoginProfile_userName,

    -- * Destructuring the Response
    UpdateLoginProfileResponse (..),
    newUpdateLoginProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLoginProfile' smart constructor.
data UpdateLoginProfile = UpdateLoginProfile'
  { -- | Allows this new password to be used only once by requiring the specified
    -- IAM user to set a new password on next sign-in.
    passwordResetRequired :: Prelude.Maybe Prelude.Bool,
    -- | The new password for the specified IAM user.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
    -- this parameter is a string of characters consisting of the following:
    --
    -- -   Any printable ASCII character ranging from the space character
    --     (@\\u0020@) through the end of the ASCII character range
    --
    -- -   The printable characters in the Basic Latin and Latin-1 Supplement
    --     character set (through @\\u00FF@)
    --
    -- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
    --     carriage return (@\\u000D@)
    --
    -- However, the format can be further restricted by the account
    -- administrator by setting a password policy on the AWS account. For more
    -- information, see UpdateAccountPasswordPolicy.
    password :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The name of the user whose password you want to update.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoginProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordResetRequired', 'updateLoginProfile_passwordResetRequired' - Allows this new password to be used only once by requiring the specified
-- IAM user to set a new password on next sign-in.
--
-- 'password', 'updateLoginProfile_password' - The new password for the specified IAM user.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
--
-- However, the format can be further restricted by the account
-- administrator by setting a password policy on the AWS account. For more
-- information, see UpdateAccountPasswordPolicy.
--
-- 'userName', 'updateLoginProfile_userName' - The name of the user whose password you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newUpdateLoginProfile ::
  -- | 'userName'
  Prelude.Text ->
  UpdateLoginProfile
newUpdateLoginProfile pUserName_ =
  UpdateLoginProfile'
    { passwordResetRequired =
        Prelude.Nothing,
      password = Prelude.Nothing,
      userName = pUserName_
    }

-- | Allows this new password to be used only once by requiring the specified
-- IAM user to set a new password on next sign-in.
updateLoginProfile_passwordResetRequired :: Lens.Lens' UpdateLoginProfile (Prelude.Maybe Prelude.Bool)
updateLoginProfile_passwordResetRequired = Lens.lens (\UpdateLoginProfile' {passwordResetRequired} -> passwordResetRequired) (\s@UpdateLoginProfile' {} a -> s {passwordResetRequired = a} :: UpdateLoginProfile)

-- | The new password for the specified IAM user.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
--
-- However, the format can be further restricted by the account
-- administrator by setting a password policy on the AWS account. For more
-- information, see UpdateAccountPasswordPolicy.
updateLoginProfile_password :: Lens.Lens' UpdateLoginProfile (Prelude.Maybe Prelude.Text)
updateLoginProfile_password = Lens.lens (\UpdateLoginProfile' {password} -> password) (\s@UpdateLoginProfile' {} a -> s {password = a} :: UpdateLoginProfile) Prelude.. Lens.mapping Prelude._Sensitive

-- | The name of the user whose password you want to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateLoginProfile_userName :: Lens.Lens' UpdateLoginProfile Prelude.Text
updateLoginProfile_userName = Lens.lens (\UpdateLoginProfile' {userName} -> userName) (\s@UpdateLoginProfile' {} a -> s {userName = a} :: UpdateLoginProfile)

instance Prelude.AWSRequest UpdateLoginProfile where
  type
    Rs UpdateLoginProfile =
      UpdateLoginProfileResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull UpdateLoginProfileResponse'

instance Prelude.Hashable UpdateLoginProfile

instance Prelude.NFData UpdateLoginProfile

instance Prelude.ToHeaders UpdateLoginProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateLoginProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateLoginProfile where
  toQuery UpdateLoginProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateLoginProfile" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "PasswordResetRequired"
          Prelude.=: passwordResetRequired,
        "Password" Prelude.=: password,
        "UserName" Prelude.=: userName
      ]

-- | /See:/ 'newUpdateLoginProfileResponse' smart constructor.
data UpdateLoginProfileResponse = UpdateLoginProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoginProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateLoginProfileResponse ::
  UpdateLoginProfileResponse
newUpdateLoginProfileResponse =
  UpdateLoginProfileResponse'

instance Prelude.NFData UpdateLoginProfileResponse
