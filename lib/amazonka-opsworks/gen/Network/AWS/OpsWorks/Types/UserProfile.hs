{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.UserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.UserProfile
  ( UserProfile (..),

    -- * Smart constructor
    mkUserProfile,

    -- * Lenses
    upAllowSelfManagement,
    upIamUserArn,
    upName,
    upSshPublicKey,
    upSshUsername,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a user's SSH information.
--
-- /See:/ 'mkUserProfile' smart constructor.
data UserProfile = UserProfile'
  { -- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
    allowSelfManagement :: Core.Maybe Core.Bool,
    -- | The user's IAM ARN.
    iamUserArn :: Core.Maybe Types.String,
    -- | The user's name.
    name :: Core.Maybe Types.String,
    -- | The user's SSH public key.
    sshPublicKey :: Core.Maybe Types.String,
    -- | The user's SSH user name.
    sshUsername :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserProfile' value with any optional fields omitted.
mkUserProfile ::
  UserProfile
mkUserProfile =
  UserProfile'
    { allowSelfManagement = Core.Nothing,
      iamUserArn = Core.Nothing,
      name = Core.Nothing,
      sshPublicKey = Core.Nothing,
      sshUsername = Core.Nothing
    }

-- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
--
-- /Note:/ Consider using 'allowSelfManagement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upAllowSelfManagement :: Lens.Lens' UserProfile (Core.Maybe Core.Bool)
upAllowSelfManagement = Lens.field @"allowSelfManagement"
{-# DEPRECATED upAllowSelfManagement "Use generic-lens or generic-optics with 'allowSelfManagement' instead." #-}

-- | The user's IAM ARN.
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upIamUserArn :: Lens.Lens' UserProfile (Core.Maybe Types.String)
upIamUserArn = Lens.field @"iamUserArn"
{-# DEPRECATED upIamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead." #-}

-- | The user's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UserProfile (Core.Maybe Types.String)
upName = Lens.field @"name"
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The user's SSH public key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSshPublicKey :: Lens.Lens' UserProfile (Core.Maybe Types.String)
upSshPublicKey = Lens.field @"sshPublicKey"
{-# DEPRECATED upSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The user's SSH user name.
--
-- /Note:/ Consider using 'sshUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSshUsername :: Lens.Lens' UserProfile (Core.Maybe Types.String)
upSshUsername = Lens.field @"sshUsername"
{-# DEPRECATED upSshUsername "Use generic-lens or generic-optics with 'sshUsername' instead." #-}

instance Core.FromJSON UserProfile where
  parseJSON =
    Core.withObject "UserProfile" Core.$
      \x ->
        UserProfile'
          Core.<$> (x Core..:? "AllowSelfManagement")
          Core.<*> (x Core..:? "IamUserArn")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "SshPublicKey")
          Core.<*> (x Core..:? "SshUsername")
