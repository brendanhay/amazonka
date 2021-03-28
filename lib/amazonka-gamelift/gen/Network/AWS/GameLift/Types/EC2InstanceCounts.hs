{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EC2InstanceCounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.EC2InstanceCounts
  ( EC2InstanceCounts (..)
  -- * Smart constructor
  , mkEC2InstanceCounts
  -- * Lenses
  , ecicACTIVE
  , ecicDESIRED
  , ecicIDLE
  , ecicMAXIMUM
  , ecicMINIMUM
  , ecicPENDING
  , ecicTERMINATING
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Current status of fleet capacity. The number of active instances should match or be in the process of matching the number of desired instances. Pending and terminating counts are non-zero only if fleet capacity is adjusting to an 'UpdateFleetCapacity' request, or if access to resources is temporarily affected.
--
--
--     * 'CreateFleet' 
--
--
--     * 'ListFleets' 
--
--
--     * 'DeleteFleet' 
--
--
--     * 'DescribeFleetAttributes' 
--
--
--     * 'UpdateFleetAttributes' 
--
--
--     * 'StartFleetActions' or 'StopFleetActions' 
--
--
--
-- /See:/ 'mkEC2InstanceCounts' smart constructor.
data EC2InstanceCounts = EC2InstanceCounts'
  { active :: Core.Maybe Core.Natural
    -- ^ Actual number of active instances in the fleet.
  , desired :: Core.Maybe Core.Natural
    -- ^ Ideal number of active instances in the fleet.
  , idle :: Core.Maybe Core.Natural
    -- ^ Number of active instances in the fleet that are not currently hosting a game session.
  , maximum :: Core.Maybe Core.Natural
    -- ^ The maximum value allowed for the fleet's instance count.
  , minimum :: Core.Maybe Core.Natural
    -- ^ The minimum value allowed for the fleet's instance count.
  , pending :: Core.Maybe Core.Natural
    -- ^ Number of instances in the fleet that are starting but not yet active.
  , terminating :: Core.Maybe Core.Natural
    -- ^ Number of instances in the fleet that are no longer active but haven't yet been terminated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EC2InstanceCounts' value with any optional fields omitted.
mkEC2InstanceCounts
    :: EC2InstanceCounts
mkEC2InstanceCounts
  = EC2InstanceCounts'{active = Core.Nothing, desired = Core.Nothing,
                       idle = Core.Nothing, maximum = Core.Nothing,
                       minimum = Core.Nothing, pending = Core.Nothing,
                       terminating = Core.Nothing}

-- | Actual number of active instances in the fleet.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecicACTIVE :: Lens.Lens' EC2InstanceCounts (Core.Maybe Core.Natural)
ecicACTIVE = Lens.field @"active"
{-# INLINEABLE ecicACTIVE #-}
{-# DEPRECATED active "Use generic-lens or generic-optics with 'active' instead"  #-}

-- | Ideal number of active instances in the fleet.
--
-- /Note:/ Consider using 'desired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecicDESIRED :: Lens.Lens' EC2InstanceCounts (Core.Maybe Core.Natural)
ecicDESIRED = Lens.field @"desired"
{-# INLINEABLE ecicDESIRED #-}
{-# DEPRECATED desired "Use generic-lens or generic-optics with 'desired' instead"  #-}

-- | Number of active instances in the fleet that are not currently hosting a game session.
--
-- /Note:/ Consider using 'idle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecicIDLE :: Lens.Lens' EC2InstanceCounts (Core.Maybe Core.Natural)
ecicIDLE = Lens.field @"idle"
{-# INLINEABLE ecicIDLE #-}
{-# DEPRECATED idle "Use generic-lens or generic-optics with 'idle' instead"  #-}

-- | The maximum value allowed for the fleet's instance count.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecicMAXIMUM :: Lens.Lens' EC2InstanceCounts (Core.Maybe Core.Natural)
ecicMAXIMUM = Lens.field @"maximum"
{-# INLINEABLE ecicMAXIMUM #-}
{-# DEPRECATED maximum "Use generic-lens or generic-optics with 'maximum' instead"  #-}

-- | The minimum value allowed for the fleet's instance count.
--
-- /Note:/ Consider using 'minimum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecicMINIMUM :: Lens.Lens' EC2InstanceCounts (Core.Maybe Core.Natural)
ecicMINIMUM = Lens.field @"minimum"
{-# INLINEABLE ecicMINIMUM #-}
{-# DEPRECATED minimum "Use generic-lens or generic-optics with 'minimum' instead"  #-}

-- | Number of instances in the fleet that are starting but not yet active.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecicPENDING :: Lens.Lens' EC2InstanceCounts (Core.Maybe Core.Natural)
ecicPENDING = Lens.field @"pending"
{-# INLINEABLE ecicPENDING #-}
{-# DEPRECATED pending "Use generic-lens or generic-optics with 'pending' instead"  #-}

-- | Number of instances in the fleet that are no longer active but haven't yet been terminated.
--
-- /Note:/ Consider using 'terminating' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecicTERMINATING :: Lens.Lens' EC2InstanceCounts (Core.Maybe Core.Natural)
ecicTERMINATING = Lens.field @"terminating"
{-# INLINEABLE ecicTERMINATING #-}
{-# DEPRECATED terminating "Use generic-lens or generic-optics with 'terminating' instead"  #-}

instance Core.FromJSON EC2InstanceCounts where
        parseJSON
          = Core.withObject "EC2InstanceCounts" Core.$
              \ x ->
                EC2InstanceCounts' Core.<$>
                  (x Core..:? "ACTIVE") Core.<*> x Core..:? "DESIRED" Core.<*>
                    x Core..:? "IDLE"
                    Core.<*> x Core..:? "MAXIMUM"
                    Core.<*> x Core..:? "MINIMUM"
                    Core.<*> x Core..:? "PENDING"
                    Core.<*> x Core..:? "TERMINATING"
