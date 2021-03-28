{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.RefreshPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.RefreshPreferences
  ( RefreshPreferences (..)
  -- * Smart constructor
  , mkRefreshPreferences
  -- * Lenses
  , rpInstanceWarmup
  , rpMinHealthyPercentage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes information used to start an instance refresh. 
--
-- /See:/ 'mkRefreshPreferences' smart constructor.
data RefreshPreferences = RefreshPreferences'
  { instanceWarmup :: Core.Maybe Core.Natural
    -- ^ The number of seconds until a newly launched instance is configured and ready to use. During this time, Amazon EC2 Auto Scaling does not immediately move on to the next replacement. The default is to use the value for the health check grace period defined for the group.
  , minHealthyPercentage :: Core.Maybe Core.Natural
    -- ^ The amount of capacity in the Auto Scaling group that must remain healthy during an instance refresh to allow the operation to continue, as a percentage of the desired capacity of the Auto Scaling group (rounded up to the nearest integer). The default is @90@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RefreshPreferences' value with any optional fields omitted.
mkRefreshPreferences
    :: RefreshPreferences
mkRefreshPreferences
  = RefreshPreferences'{instanceWarmup = Core.Nothing,
                        minHealthyPercentage = Core.Nothing}

-- | The number of seconds until a newly launched instance is configured and ready to use. During this time, Amazon EC2 Auto Scaling does not immediately move on to the next replacement. The default is to use the value for the health check grace period defined for the group.
--
-- /Note:/ Consider using 'instanceWarmup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpInstanceWarmup :: Lens.Lens' RefreshPreferences (Core.Maybe Core.Natural)
rpInstanceWarmup = Lens.field @"instanceWarmup"
{-# INLINEABLE rpInstanceWarmup #-}
{-# DEPRECATED instanceWarmup "Use generic-lens or generic-optics with 'instanceWarmup' instead"  #-}

-- | The amount of capacity in the Auto Scaling group that must remain healthy during an instance refresh to allow the operation to continue, as a percentage of the desired capacity of the Auto Scaling group (rounded up to the nearest integer). The default is @90@ . 
--
-- /Note:/ Consider using 'minHealthyPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpMinHealthyPercentage :: Lens.Lens' RefreshPreferences (Core.Maybe Core.Natural)
rpMinHealthyPercentage = Lens.field @"minHealthyPercentage"
{-# INLINEABLE rpMinHealthyPercentage #-}
{-# DEPRECATED minHealthyPercentage "Use generic-lens or generic-optics with 'minHealthyPercentage' instead"  #-}

instance Core.ToQuery RefreshPreferences where
        toQuery RefreshPreferences{..}
          = Core.maybe Core.mempty (Core.toQueryPair "InstanceWarmup")
              instanceWarmup
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MinHealthyPercentage")
                minHealthyPercentage
