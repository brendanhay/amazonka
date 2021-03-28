{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotCapacityRebalance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotCapacityRebalance
  ( SpotCapacityRebalance (..)
  -- * Smart constructor
  , mkSpotCapacityRebalance
  -- * Lenses
  , scrReplacementStrategy
  ) where

import qualified Network.AWS.EC2.Types.ReplacementStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Spot Instance replacement strategy to use when Amazon EC2 emits a signal that your Spot Instance is at an elevated risk of being interrupted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#spot-fleet-capacity-rebalance Capacity rebalancing> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /See:/ 'mkSpotCapacityRebalance' smart constructor.
newtype SpotCapacityRebalance = SpotCapacityRebalance'
  { replacementStrategy :: Core.Maybe Types.ReplacementStrategy
    -- ^ The replacement strategy to use. Only available for fleets of type @maintain@ . You must specify a value, otherwise you get an error.
--
-- To allow Spot Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for a Spot Instance in the fleet, specify @launch@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SpotCapacityRebalance' value with any optional fields omitted.
mkSpotCapacityRebalance
    :: SpotCapacityRebalance
mkSpotCapacityRebalance
  = SpotCapacityRebalance'{replacementStrategy = Core.Nothing}

-- | The replacement strategy to use. Only available for fleets of type @maintain@ . You must specify a value, otherwise you get an error.
--
-- To allow Spot Fleet to launch a replacement Spot Instance when an instance rebalance notification is emitted for a Spot Instance in the fleet, specify @launch@ .
--
-- /Note:/ Consider using 'replacementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrReplacementStrategy :: Lens.Lens' SpotCapacityRebalance (Core.Maybe Types.ReplacementStrategy)
scrReplacementStrategy = Lens.field @"replacementStrategy"
{-# INLINEABLE scrReplacementStrategy #-}
{-# DEPRECATED replacementStrategy "Use generic-lens or generic-optics with 'replacementStrategy' instead"  #-}

instance Core.ToQuery SpotCapacityRebalance where
        toQuery SpotCapacityRebalance{..}
          = Core.maybe Core.mempty (Core.toQueryPair "ReplacementStrategy")
              replacementStrategy

instance Core.FromXML SpotCapacityRebalance where
        parseXML x
          = SpotCapacityRebalance' Core.<$>
              (x Core..@? "replacementStrategy")
