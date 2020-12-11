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
    lpPasswordResetRequired,
    lpUserName,
    lpCreateDate,
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
  { passwordResetRequired ::
      Lude.Maybe Lude.Bool,
    userName :: Lude.Text,
    createDate :: Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoginProfile' with the minimum fields required to make a request.
--
-- * 'createDate' - The date when the password for the user was created.
-- * 'passwordResetRequired' - Specifies whether the user is required to set a new password on next sign-in.
-- * 'userName' - The name of the user, which can be used for signing in to the AWS Management Console.
mkLoginProfile ::
  -- | 'userName'
  Lude.Text ->
  -- | 'createDate'
  Lude.ISO8601 ->
  LoginProfile
mkLoginProfile pUserName_ pCreateDate_ =
  LoginProfile'
    { passwordResetRequired = Lude.Nothing,
      userName = pUserName_,
      createDate = pCreateDate_
    }

-- | Specifies whether the user is required to set a new password on next sign-in.
--
-- /Note:/ Consider using 'passwordResetRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPasswordResetRequired :: Lens.Lens' LoginProfile (Lude.Maybe Lude.Bool)
lpPasswordResetRequired = Lens.lens (passwordResetRequired :: LoginProfile -> Lude.Maybe Lude.Bool) (\s a -> s {passwordResetRequired = a} :: LoginProfile)
{-# DEPRECATED lpPasswordResetRequired "Use generic-lens or generic-optics with 'passwordResetRequired' instead." #-}

-- | The name of the user, which can be used for signing in to the AWS Management Console.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpUserName :: Lens.Lens' LoginProfile Lude.Text
lpUserName = Lens.lens (userName :: LoginProfile -> Lude.Text) (\s a -> s {userName = a} :: LoginProfile)
{-# DEPRECATED lpUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The date when the password for the user was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpCreateDate :: Lens.Lens' LoginProfile Lude.ISO8601
lpCreateDate = Lens.lens (createDate :: LoginProfile -> Lude.ISO8601) (\s a -> s {createDate = a} :: LoginProfile)
{-# DEPRECATED lpCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

instance Lude.FromXML LoginProfile where
  parseXML x =
    LoginProfile'
      Lude.<$> (x Lude..@? "PasswordResetRequired")
      Lude.<*> (x Lude..@ "UserName")
      Lude.<*> (x Lude..@ "CreateDate")
