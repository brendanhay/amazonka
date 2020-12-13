{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.LoginProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.LoginProfile
  ( LoginProfile (..),

    -- * Smart constructor
    mkLoginProfile,

    -- * Lenses
    lpCreateDate,
    lpUserName,
    lpPasswordResetRequired,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the user name and password create date for a user.
--
-- This data type is used as a response element in the 'CreateLoginProfile' and 'GetLoginProfile' operations.
--
-- /See:/ 'mkLoginProfile' smart constructor.
data LoginProfile = LoginProfile'
  { -- | The date when the password for the user was created.
    createDate :: Lude.DateTime,
    -- | The name of the user, which can be used for signing in to the AWS Management Console.
    userName :: Lude.Text,
    -- | Specifies whether the user is required to set a new password on next sign-in.
    passwordResetRequired :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoginProfile' with the minimum fields required to make a request.
--
-- * 'createDate' - The date when the password for the user was created.
-- * 'userName' - The name of the user, which can be used for signing in to the AWS Management Console.
-- * 'passwordResetRequired' - Specifies whether the user is required to set a new password on next sign-in.
mkLoginProfile ::
  -- | 'createDate'
  Lude.DateTime ->
  -- | 'userName'
  Lude.Text ->
  LoginProfile
mkLoginProfile pCreateDate_ pUserName_ =
  LoginProfile'
    { createDate = pCreateDate_,
      userName = pUserName_,
      passwordResetRequired = Lude.Nothing
    }

-- | The date when the password for the user was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpCreateDate :: Lens.Lens' LoginProfile Lude.DateTime
lpCreateDate = Lens.lens (createDate :: LoginProfile -> Lude.DateTime) (\s a -> s {createDate = a} :: LoginProfile)
{-# DEPRECATED lpCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The name of the user, which can be used for signing in to the AWS Management Console.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpUserName :: Lens.Lens' LoginProfile Lude.Text
lpUserName = Lens.lens (userName :: LoginProfile -> Lude.Text) (\s a -> s {userName = a} :: LoginProfile)
{-# DEPRECATED lpUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Specifies whether the user is required to set a new password on next sign-in.
--
-- /Note:/ Consider using 'passwordResetRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPasswordResetRequired :: Lens.Lens' LoginProfile (Lude.Maybe Lude.Bool)
lpPasswordResetRequired = Lens.lens (passwordResetRequired :: LoginProfile -> Lude.Maybe Lude.Bool) (\s a -> s {passwordResetRequired = a} :: LoginProfile)
{-# DEPRECATED lpPasswordResetRequired "Use generic-lens or generic-optics with 'passwordResetRequired' instead." #-}

instance Lude.FromXML LoginProfile where
  parseXML x =
    LoginProfile'
      Lude.<$> (x Lude..@ "CreateDate")
      Lude.<*> (x Lude..@ "UserName")
      Lude.<*> (x Lude..@? "PasswordResetRequired")
