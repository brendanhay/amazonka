{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateLoginProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a password for the specified user, giving the user the ability to access AWS services through the AWS Management Console. For more information about managing passwords, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords> in the /IAM User Guide/ .
module Network.AWS.IAM.CreateLoginProfile
  ( -- * Creating a request
    CreateLoginProfile (..),
    mkCreateLoginProfile,

    -- ** Request lenses
    clpPasswordResetRequired,
    clpUserName,
    clpPassword,

    -- * Destructuring the response
    CreateLoginProfileResponse (..),
    mkCreateLoginProfileResponse,

    -- ** Response lenses
    clprsResponseStatus,
    clprsLoginProfile,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLoginProfile' smart constructor.
data CreateLoginProfile = CreateLoginProfile'
  { passwordResetRequired ::
      Lude.Maybe Lude.Bool,
    userName :: Lude.Text,
    password :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoginProfile' with the minimum fields required to make a request.
--
-- * 'password' - The new password for the user.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (@\u0020@ ) through the end of the ASCII character range (@\u00FF@ ). You can also include the tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ ) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
-- * 'passwordResetRequired' - Specifies whether the user is required to set a new password on next sign-in.
-- * 'userName' - The name of the IAM user to create a password for. The user must already exist.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkCreateLoginProfile ::
  -- | 'userName'
  Lude.Text ->
  -- | 'password'
  Lude.Sensitive Lude.Text ->
  CreateLoginProfile
mkCreateLoginProfile pUserName_ pPassword_ =
  CreateLoginProfile'
    { passwordResetRequired = Lude.Nothing,
      userName = pUserName_,
      password = pPassword_
    }

-- | Specifies whether the user is required to set a new password on next sign-in.
--
-- /Note:/ Consider using 'passwordResetRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clpPasswordResetRequired :: Lens.Lens' CreateLoginProfile (Lude.Maybe Lude.Bool)
clpPasswordResetRequired = Lens.lens (passwordResetRequired :: CreateLoginProfile -> Lude.Maybe Lude.Bool) (\s a -> s {passwordResetRequired = a} :: CreateLoginProfile)
{-# DEPRECATED clpPasswordResetRequired "Use generic-lens or generic-optics with 'passwordResetRequired' instead." #-}

-- | The name of the IAM user to create a password for. The user must already exist.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clpUserName :: Lens.Lens' CreateLoginProfile Lude.Text
clpUserName = Lens.lens (userName :: CreateLoginProfile -> Lude.Text) (\s a -> s {userName = a} :: CreateLoginProfile)
{-# DEPRECATED clpUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The new password for the user.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (@\u0020@ ) through the end of the ASCII character range (@\u00FF@ ). You can also include the tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ ) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clpPassword :: Lens.Lens' CreateLoginProfile (Lude.Sensitive Lude.Text)
clpPassword = Lens.lens (password :: CreateLoginProfile -> Lude.Sensitive Lude.Text) (\s a -> s {password = a} :: CreateLoginProfile)
{-# DEPRECATED clpPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.AWSRequest CreateLoginProfile where
  type Rs CreateLoginProfile = CreateLoginProfileResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreateLoginProfileResult"
      ( \s h x ->
          CreateLoginProfileResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "LoginProfile")
      )

instance Lude.ToHeaders CreateLoginProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLoginProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLoginProfile where
  toQuery CreateLoginProfile' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateLoginProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PasswordResetRequired" Lude.=: passwordResetRequired,
        "UserName" Lude.=: userName,
        "Password" Lude.=: password
      ]

-- | Contains the response to a successful 'CreateLoginProfile' request.
--
-- /See:/ 'mkCreateLoginProfileResponse' smart constructor.
data CreateLoginProfileResponse = CreateLoginProfileResponse'
  { responseStatus ::
      Lude.Int,
    loginProfile :: LoginProfile
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoginProfileResponse' with the minimum fields required to make a request.
--
-- * 'loginProfile' - A structure containing the user name and password create date.
-- * 'responseStatus' - The response status code.
mkCreateLoginProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'loginProfile'
  LoginProfile ->
  CreateLoginProfileResponse
mkCreateLoginProfileResponse pResponseStatus_ pLoginProfile_ =
  CreateLoginProfileResponse'
    { responseStatus = pResponseStatus_,
      loginProfile = pLoginProfile_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clprsResponseStatus :: Lens.Lens' CreateLoginProfileResponse Lude.Int
clprsResponseStatus = Lens.lens (responseStatus :: CreateLoginProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLoginProfileResponse)
{-# DEPRECATED clprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A structure containing the user name and password create date.
--
-- /Note:/ Consider using 'loginProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clprsLoginProfile :: Lens.Lens' CreateLoginProfileResponse LoginProfile
clprsLoginProfile = Lens.lens (loginProfile :: CreateLoginProfileResponse -> LoginProfile) (\s a -> s {loginProfile = a} :: CreateLoginProfileResponse)
{-# DEPRECATED clprsLoginProfile "Use generic-lens or generic-optics with 'loginProfile' instead." #-}
