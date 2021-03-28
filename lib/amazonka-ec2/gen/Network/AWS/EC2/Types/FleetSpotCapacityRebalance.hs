{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetSpotCapacityRebalance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetSpotCapacityRebalance
  ( FleetSpotCapacityRebalance (..)
  -- * Smart constructor
  , mkFleetSpotCapacityRebalance
  -- * Lenses
  , fscrReplacementStrategy
  ) where

import qualified Network.AWS.EC2.Types.FleetReplacementStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted.
--
-- /See:/ 'mkFleetSpotCapacityRebalance' smart constructor.
newtype FleetSpotCapacityRebalance = FleetSpotCapacityRebalance'
  { replacementStrategy :: Core.Maybe Types.FleetReplacementStrategy
    -- ^ To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . Only available for fleets of type @maintain@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FleetSpotCapacityRebalance' value with any optional fields omitted.
mkFleetSpotCapacityRebalance
    :: FleetSpotCapacityRebalance
mkFleetSpotCapacityRebalance
  = FleetSpotCapacityRebalance'{replacementStrategy = Core.Nothing}

-- | To allow EC2 Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for an existing Spot Instance in the fleet, specify @launch@ . Only available for fleets of type @maintain@ .
--
-- /Note:/ Consider using 'replacementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fscrReplacementStrategy :: Lens.Lens' FleetSpotCapacityRebalance (Core.Maybe Types.FleetReplacementStrategy)
fscrReplacementStrategy = Lens.field @"replacementStrategy"
{-# INLINEABLE fscrReplacementStrategy #-}
{-# DEPRECATED replacementStrategy "Use generic-lens or generic-optics with 'replacementStrategy' instead"  #-}

instance Core.FromXML FleetSpotCapacityRebalance where
        parseXML x
          = FleetSpotCapacityRebalance' Core.<$>
              (x Core..@? "replacementStrategy")
