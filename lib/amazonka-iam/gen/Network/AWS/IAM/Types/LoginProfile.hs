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
    lpUserName,
    lpCreateDate,
    lpPasswordResetRequired,
  )
where

import qualified Network.AWS.IAM.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the user name and password create date for a user.
--
-- This data type is used as a response element in the 'CreateLoginProfile' and 'GetLoginProfile' operations.
--
-- /See:/ 'mkLoginProfile' smart constructor.
data LoginProfile = LoginProfile'
  { -- | The name of the user, which can be used for signing in to the AWS Management Console.
    userName :: Types.UserName,
    -- | The date when the password for the user was created.
    createDate :: Core.UTCTime,
    -- | Specifies whether the user is required to set a new password on next sign-in.
    passwordResetRequired :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LoginProfile' value with any optional fields omitted.
mkLoginProfile ::
  -- | 'userName'
  Types.UserName ->
  -- | 'createDate'
  Core.UTCTime ->
  LoginProfile
mkLoginProfile userName createDate =
  LoginProfile'
    { userName,
      createDate,
      passwordResetRequired = Core.Nothing
    }

-- | The name of the user, which can be used for signing in to the AWS Management Console.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpUserName :: Lens.Lens' LoginProfile Types.UserName
lpUserName = Lens.field @"userName"
{-# DEPRECATED lpUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The date when the password for the user was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpCreateDate :: Lens.Lens' LoginProfile Core.UTCTime
lpCreateDate = Lens.field @"createDate"
{-# DEPRECATED lpCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | Specifies whether the user is required to set a new password on next sign-in.
--
-- /Note:/ Consider using 'passwordResetRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPasswordResetRequired :: Lens.Lens' LoginProfile (Core.Maybe Core.Bool)
lpPasswordResetRequired = Lens.field @"passwordResetRequired"
{-# DEPRECATED lpPasswordResetRequired "Use generic-lens or generic-optics with 'passwordResetRequired' instead." #-}

instance Core.FromXML LoginProfile where
  parseXML x =
    LoginProfile'
      Core.<$> (x Core..@ "UserName")
      Core.<*> (x Core..@ "CreateDate")
      Core.<*> (x Core..@? "PasswordResetRequired")
