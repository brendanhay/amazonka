{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.SelfUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.SelfUserProfile
  ( SelfUserProfile (..),

    -- * Smart constructor
    mkSelfUserProfile,

    -- * Lenses
    supIamUserArn,
    supName,
    supSshPublicKey,
    supSshUsername,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a user's SSH information.
--
-- /See:/ 'mkSelfUserProfile' smart constructor.
data SelfUserProfile = SelfUserProfile'
  { -- | The user's IAM ARN.
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

-- | Creates a 'SelfUserProfile' value with any optional fields omitted.
mkSelfUserProfile ::
  SelfUserProfile
mkSelfUserProfile =
  SelfUserProfile'
    { iamUserArn = Core.Nothing,
      name = Core.Nothing,
      sshPublicKey = Core.Nothing,
      sshUsername = Core.Nothing
    }

-- | The user's IAM ARN.
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supIamUserArn :: Lens.Lens' SelfUserProfile (Core.Maybe Types.String)
supIamUserArn = Lens.field @"iamUserArn"
{-# DEPRECATED supIamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead." #-}

-- | The user's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supName :: Lens.Lens' SelfUserProfile (Core.Maybe Types.String)
supName = Lens.field @"name"
{-# DEPRECATED supName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The user's SSH public key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supSshPublicKey :: Lens.Lens' SelfUserProfile (Core.Maybe Types.String)
supSshPublicKey = Lens.field @"sshPublicKey"
{-# DEPRECATED supSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The user's SSH user name.
--
-- /Note:/ Consider using 'sshUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
supSshUsername :: Lens.Lens' SelfUserProfile (Core.Maybe Types.String)
supSshUsername = Lens.field @"sshUsername"
{-# DEPRECATED supSshUsername "Use generic-lens or generic-optics with 'sshUsername' instead." #-}

instance Core.FromJSON SelfUserProfile where
  parseJSON =
    Core.withObject "SelfUserProfile" Core.$
      \x ->
        SelfUserProfile'
          Core.<$> (x Core..:? "IamUserArn")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "SshPublicKey")
          Core.<*> (x Core..:? "SshUsername")
