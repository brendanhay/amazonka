{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CapacityReservationOptions
  ( CapacityReservationOptions (..)
  -- * Smart constructor
  , mkCapacityReservationOptions
  -- * Lenses
  , croUsageStrategy
  ) where

import qualified Network.AWS.EC2.Types.FleetCapacityReservationUsageStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the strategy for using unused Capacity Reservations for fulfilling On-Demand capacity.
--
-- For more information about Capacity Reservations, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-capacity-reservations.html On-Demand Capacity Reservations> in the /Amazon Elastic Compute Cloud User Guide/ . For examples of using Capacity Reservations in an EC2 Fleet, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-examples.html EC2 Fleet example configurations> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkCapacityReservationOptions' smart constructor.
newtype CapacityReservationOptions = CapacityReservationOptions'
  { usageStrategy :: Core.Maybe Types.FleetCapacityReservationUsageStrategy
    -- ^ Indicates whether to use unused Capacity Reservations for fulfilling On-Demand capacity.
--
-- If you specify @use-capacity-reservations-first@ , the fleet uses unused Capacity Reservations to fulfill On-Demand capacity up to the target On-Demand capacity. If multiple instance pools have unused Capacity Reservations, the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ) is applied. If the number of unused Capacity Reservations is less than the On-Demand target capacity, the remaining On-Demand target capacity is launched according to the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ).
-- If you do not specify a value, the fleet fulfils the On-Demand capacity according to the chosen On-Demand allocation strategy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CapacityReservationOptions' value with any optional fields omitted.
mkCapacityReservationOptions
    :: CapacityReservationOptions
mkCapacityReservationOptions
  = CapacityReservationOptions'{usageStrategy = Core.Nothing}

-- | Indicates whether to use unused Capacity Reservations for fulfilling On-Demand capacity.
--
-- If you specify @use-capacity-reservations-first@ , the fleet uses unused Capacity Reservations to fulfill On-Demand capacity up to the target On-Demand capacity. If multiple instance pools have unused Capacity Reservations, the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ) is applied. If the number of unused Capacity Reservations is less than the On-Demand target capacity, the remaining On-Demand target capacity is launched according to the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ).
-- If you do not specify a value, the fleet fulfils the On-Demand capacity according to the chosen On-Demand allocation strategy.
--
-- /Note:/ Consider using 'usageStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
croUsageStrategy :: Lens.Lens' CapacityReservationOptions (Core.Maybe Types.FleetCapacityReservationUsageStrategy)
croUsageStrategy = Lens.field @"usageStrategy"
{-# INLINEABLE croUsageStrategy #-}
{-# DEPRECATED usageStrategy "Use generic-lens or generic-optics with 'usageStrategy' instead"  #-}

instance Core.FromXML CapacityReservationOptions where
        parseXML x
          = CapacityReservationOptions' Core.<$> (x Core..@? "usageStrategy")
