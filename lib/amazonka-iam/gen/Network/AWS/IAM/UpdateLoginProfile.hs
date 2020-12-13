{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateLoginProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password for the specified IAM user.
--
-- IAM users can change their own passwords by calling 'ChangePassword' . For more information about modifying passwords, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords> in the /IAM User Guide/ .
module Network.AWS.IAM.UpdateLoginProfile
  ( -- * Creating a request
    UpdateLoginProfile (..),
    mkUpdateLoginProfile,

    -- ** Request lenses
    ulpUserName,
    ulpPassword,
    ulpPasswordResetRequired,

    -- * Destructuring the response
    UpdateLoginProfileResponse (..),
    mkUpdateLoginProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateLoginProfile' smart constructor.
data UpdateLoginProfile = UpdateLoginProfile'
  { -- | The name of the user whose password you want to update.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text,
    -- | The new password for the specified IAM user.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
    --
    --     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
    --
    --
    --     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
    --
    --
    --     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
    --
    --
    -- However, the format can be further restricted by the account administrator by setting a password policy on the AWS account. For more information, see 'UpdateAccountPasswordPolicy' .
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Allows this new password to be used only once by requiring the specified IAM user to set a new password on next sign-in.
    passwordResetRequired :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateLoginProfile' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user whose password you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'password' - The new password for the specified IAM user.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
-- However, the format can be further restricted by the account administrator by setting a password policy on the AWS account. For more information, see 'UpdateAccountPasswordPolicy' .
-- * 'passwordResetRequired' - Allows this new password to be used only once by requiring the specified IAM user to set a new password on next sign-in.
mkUpdateLoginProfile ::
  -- | 'userName'
  Lude.Text ->
  UpdateLoginProfile
mkUpdateLoginProfile pUserName_ =
  UpdateLoginProfile'
    { userName = pUserName_,
      password = Lude.Nothing,
      passwordResetRequired = Lude.Nothing
    }

-- | The name of the user whose password you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpUserName :: Lens.Lens' UpdateLoginProfile Lude.Text
ulpUserName = Lens.lens (userName :: UpdateLoginProfile -> Lude.Text) (\s a -> s {userName = a} :: UpdateLoginProfile)
{-# DEPRECATED ulpUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The new password for the specified IAM user.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
-- However, the format can be further restricted by the account administrator by setting a password policy on the AWS account. For more information, see 'UpdateAccountPasswordPolicy' .
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpPassword :: Lens.Lens' UpdateLoginProfile (Lude.Maybe (Lude.Sensitive Lude.Text))
ulpPassword = Lens.lens (password :: UpdateLoginProfile -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: UpdateLoginProfile)
{-# DEPRECATED ulpPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Allows this new password to be used only once by requiring the specified IAM user to set a new password on next sign-in.
--
-- /Note:/ Consider using 'passwordResetRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulpPasswordResetRequired :: Lens.Lens' UpdateLoginProfile (Lude.Maybe Lude.Bool)
ulpPasswordResetRequired = Lens.lens (passwordResetRequired :: UpdateLoginProfile -> Lude.Maybe Lude.Bool) (\s a -> s {passwordResetRequired = a} :: UpdateLoginProfile)
{-# DEPRECATED ulpPasswordResetRequired "Use generic-lens or generic-optics with 'passwordResetRequired' instead." #-}

instance Lude.AWSRequest UpdateLoginProfile where
  type Rs UpdateLoginProfile = UpdateLoginProfileResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UpdateLoginProfileResponse'

instance Lude.ToHeaders UpdateLoginProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateLoginProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateLoginProfile where
  toQuery UpdateLoginProfile' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateLoginProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "Password" Lude.=: password,
        "PasswordResetRequired" Lude.=: passwordResetRequired
      ]

-- | /See:/ 'mkUpdateLoginProfileResponse' smart constructor.
data UpdateLoginProfileResponse = UpdateLoginProfileResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateLoginProfileResponse' with the minimum fields required to make a request.
mkUpdateLoginProfileResponse ::
  UpdateLoginProfileResponse
mkUpdateLoginProfileResponse = UpdateLoginProfileResponse'
