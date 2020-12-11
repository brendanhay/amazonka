{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ChangePassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password of the IAM user who is calling this operation. The AWS account root user password is not affected by this operation.
--
-- To change the password for a different user, see 'UpdateLoginProfile' . For more information about modifying passwords, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords> in the /IAM User Guide/ .
module Network.AWS.IAM.ChangePassword
  ( -- * Creating a request
    ChangePassword (..),
    mkChangePassword,

    -- ** Request lenses
    cpOldPassword,
    cpNewPassword,

    -- * Destructuring the response
    ChangePasswordResponse (..),
    mkChangePasswordResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkChangePassword' smart constructor.
data ChangePassword = ChangePassword'
  { oldPassword ::
      Lude.Sensitive Lude.Text,
    newPassword :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangePassword' with the minimum fields required to make a request.
--
-- * 'newPassword' - The new password. The new password must conform to the AWS account's password policy, if one exists.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (@\u0020@ ) through the end of the ASCII character range (@\u00FF@ ). You can also include the tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ ) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
-- * 'oldPassword' - The IAM user's current password.
mkChangePassword ::
  -- | 'oldPassword'
  Lude.Sensitive Lude.Text ->
  -- | 'newPassword'
  Lude.Sensitive Lude.Text ->
  ChangePassword
mkChangePassword pOldPassword_ pNewPassword_ =
  ChangePassword'
    { oldPassword = pOldPassword_,
      newPassword = pNewPassword_
    }

-- | The IAM user's current password.
--
-- /Note:/ Consider using 'oldPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOldPassword :: Lens.Lens' ChangePassword (Lude.Sensitive Lude.Text)
cpOldPassword = Lens.lens (oldPassword :: ChangePassword -> Lude.Sensitive Lude.Text) (\s a -> s {oldPassword = a} :: ChangePassword)
{-# DEPRECATED cpOldPassword "Use generic-lens or generic-optics with 'oldPassword' instead." #-}

-- | The new password. The new password must conform to the AWS account's password policy, if one exists.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (@\u0020@ ) through the end of the ASCII character range (@\u00FF@ ). You can also include the tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ ) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
--
-- /Note:/ Consider using 'newPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpNewPassword :: Lens.Lens' ChangePassword (Lude.Sensitive Lude.Text)
cpNewPassword = Lens.lens (newPassword :: ChangePassword -> Lude.Sensitive Lude.Text) (\s a -> s {newPassword = a} :: ChangePassword)
{-# DEPRECATED cpNewPassword "Use generic-lens or generic-optics with 'newPassword' instead." #-}

instance Lude.AWSRequest ChangePassword where
  type Rs ChangePassword = ChangePasswordResponse
  request = Req.postQuery iamService
  response = Res.receiveNull ChangePasswordResponse'

instance Lude.ToHeaders ChangePassword where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ChangePassword where
  toPath = Lude.const "/"

instance Lude.ToQuery ChangePassword where
  toQuery ChangePassword' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ChangePassword" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "OldPassword" Lude.=: oldPassword,
        "NewPassword" Lude.=: newPassword
      ]

-- | /See:/ 'mkChangePasswordResponse' smart constructor.
data ChangePasswordResponse = ChangePasswordResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangePasswordResponse' with the minimum fields required to make a request.
mkChangePasswordResponse ::
  ChangePasswordResponse
mkChangePasswordResponse = ChangePasswordResponse'
