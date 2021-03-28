{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.AutoScalingGroup
  ( AutoScalingGroup (..)
  -- * Smart constructor
  , mkAutoScalingGroup
  -- * Lenses
  , asgHook
  , asgName
  ) where

import qualified Network.AWS.CodeDeploy.Types.Hook as Types
import qualified Network.AWS.CodeDeploy.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an Auto Scaling group.
--
-- /See:/ 'mkAutoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { hook :: Core.Maybe Types.Hook
    -- ^ An Auto Scaling lifecycle event hook name.
  , name :: Core.Maybe Types.Name
    -- ^ The Auto Scaling group name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingGroup' value with any optional fields omitted.
mkAutoScalingGroup
    :: AutoScalingGroup
mkAutoScalingGroup
  = AutoScalingGroup'{hook = Core.Nothing, name = Core.Nothing}

-- | An Auto Scaling lifecycle event hook name.
--
-- /Note:/ Consider using 'hook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgHook :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.Hook)
asgHook = Lens.field @"hook"
{-# INLINEABLE asgHook #-}
{-# DEPRECATED hook "Use generic-lens or generic-optics with 'hook' instead"  #-}

-- | The Auto Scaling group name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgName :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.Name)
asgName = Lens.field @"name"
{-# INLINEABLE asgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON AutoScalingGroup where
        parseJSON
          = Core.withObject "AutoScalingGroup" Core.$
              \ x ->
                AutoScalingGroup' Core.<$>
                  (x Core..:? "hook") Core.<*> x Core..:? "name"
