{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Permission
  ( Permission (..),

    -- * Smart constructor
    mkPermission,

    -- * Lenses
    pAllowSsh,
    pAllowSudo,
    pIamUserArn,
    pLevel,
    pStackId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes stack or user permissions.
--
-- /See:/ 'mkPermission' smart constructor.
data Permission = Permission'
  { -- | Whether the user can use SSH.
    allowSsh :: Core.Maybe Core.Bool,
    -- | Whether the user can use __sudo__ .
    allowSudo :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
    iamUserArn :: Core.Maybe Types.String,
    -- | The user's permission level, which must be the following:
    --
    --
    --     * @deny@
    --
    --
    --     * @show@
    --
    --
    --     * @deploy@
    --
    --
    --     * @manage@
    --
    --
    --     * @iam_only@
    --
    --
    -- For more information on the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
    level :: Core.Maybe Types.String,
    -- | A stack ID.
    stackId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Permission' value with any optional fields omitted.
mkPermission ::
  Permission
mkPermission =
  Permission'
    { allowSsh = Core.Nothing,
      allowSudo = Core.Nothing,
      iamUserArn = Core.Nothing,
      level = Core.Nothing,
      stackId = Core.Nothing
    }

-- | Whether the user can use SSH.
--
-- /Note:/ Consider using 'allowSsh' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowSsh :: Lens.Lens' Permission (Core.Maybe Core.Bool)
pAllowSsh = Lens.field @"allowSsh"
{-# DEPRECATED pAllowSsh "Use generic-lens or generic-optics with 'allowSsh' instead." #-}

-- | Whether the user can use __sudo__ .
--
-- /Note:/ Consider using 'allowSudo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowSudo :: Lens.Lens' Permission (Core.Maybe Core.Bool)
pAllowSudo = Lens.field @"allowSudo"
{-# DEPRECATED pAllowSudo "Use generic-lens or generic-optics with 'allowSudo' instead." #-}

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIamUserArn :: Lens.Lens' Permission (Core.Maybe Types.String)
pIamUserArn = Lens.field @"iamUserArn"
{-# DEPRECATED pIamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead." #-}

-- | The user's permission level, which must be the following:
--
--
--     * @deny@
--
--
--     * @show@
--
--
--     * @deploy@
--
--
--     * @manage@
--
--
--     * @iam_only@
--
--
-- For more information on the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLevel :: Lens.Lens' Permission (Core.Maybe Types.String)
pLevel = Lens.field @"level"
{-# DEPRECATED pLevel "Use generic-lens or generic-optics with 'level' instead." #-}

-- | A stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStackId :: Lens.Lens' Permission (Core.Maybe Types.String)
pStackId = Lens.field @"stackId"
{-# DEPRECATED pStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON Permission where
  parseJSON =
    Core.withObject "Permission" Core.$
      \x ->
        Permission'
          Core.<$> (x Core..:? "AllowSsh")
          Core.<*> (x Core..:? "AllowSudo")
          Core.<*> (x Core..:? "IamUserArn")
          Core.<*> (x Core..:? "Level")
          Core.<*> (x Core..:? "StackId")
