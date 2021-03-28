{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Permission
  ( Permission (..)
  -- * Smart constructor
  , mkPermission
  -- * Lenses
  , pAllowSsh
  , pAllowSudo
  , pIamUserArn
  , pLevel
  , pStackId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes stack or user permissions.
--
-- /See:/ 'mkPermission' smart constructor.
data Permission = Permission'
  { allowSsh :: Core.Maybe Core.Bool
    -- ^ Whether the user can use SSH.
  , allowSudo :: Core.Maybe Core.Bool
    -- ^ Whether the user can use __sudo__ .
  , iamUserArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
  , level :: Core.Maybe Core.Text
    -- ^ The user's permission level, which must be the following:
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
  , stackId :: Core.Maybe Core.Text
    -- ^ A stack ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Permission' value with any optional fields omitted.
mkPermission
    :: Permission
mkPermission
  = Permission'{allowSsh = Core.Nothing, allowSudo = Core.Nothing,
                iamUserArn = Core.Nothing, level = Core.Nothing,
                stackId = Core.Nothing}

-- | Whether the user can use SSH.
--
-- /Note:/ Consider using 'allowSsh' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowSsh :: Lens.Lens' Permission (Core.Maybe Core.Bool)
pAllowSsh = Lens.field @"allowSsh"
{-# INLINEABLE pAllowSsh #-}
{-# DEPRECATED allowSsh "Use generic-lens or generic-optics with 'allowSsh' instead"  #-}

-- | Whether the user can use __sudo__ .
--
-- /Note:/ Consider using 'allowSudo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAllowSudo :: Lens.Lens' Permission (Core.Maybe Core.Bool)
pAllowSudo = Lens.field @"allowSudo"
{-# INLINEABLE pAllowSudo #-}
{-# DEPRECATED allowSudo "Use generic-lens or generic-optics with 'allowSudo' instead"  #-}

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pIamUserArn :: Lens.Lens' Permission (Core.Maybe Core.Text)
pIamUserArn = Lens.field @"iamUserArn"
{-# INLINEABLE pIamUserArn #-}
{-# DEPRECATED iamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead"  #-}

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
pLevel :: Lens.Lens' Permission (Core.Maybe Core.Text)
pLevel = Lens.field @"level"
{-# INLINEABLE pLevel #-}
{-# DEPRECATED level "Use generic-lens or generic-optics with 'level' instead"  #-}

-- | A stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pStackId :: Lens.Lens' Permission (Core.Maybe Core.Text)
pStackId = Lens.field @"stackId"
{-# INLINEABLE pStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.FromJSON Permission where
        parseJSON
          = Core.withObject "Permission" Core.$
              \ x ->
                Permission' Core.<$>
                  (x Core..:? "AllowSsh") Core.<*> x Core..:? "AllowSudo" Core.<*>
                    x Core..:? "IamUserArn"
                    Core.<*> x Core..:? "Level"
                    Core.<*> x Core..:? "StackId"
