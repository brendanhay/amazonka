{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
  ( FleetLaunchTemplateOverrides (..)
  -- * Smart constructor
  , mkFleetLaunchTemplateOverrides
  -- * Lenses
  , fltoAvailabilityZone
  , fltoInstanceType
  , fltoMaxPrice
  , fltoPlacement
  , fltoPriority
  , fltoSubnetId
  , fltoWeightedCapacity
  ) where

import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.PlacementResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes overrides for a launch template.
--
-- /See:/ 'mkFleetLaunchTemplateOverrides' smart constructor.
data FleetLaunchTemplateOverrides = FleetLaunchTemplateOverrides'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone in which to launch the instances.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The instance type.
  , maxPrice :: Core.Maybe Core.Text
    -- ^ The maximum price per unit hour that you are willing to pay for a Spot Instance.
  , placement :: Core.Maybe Types.PlacementResponse
    -- ^ The location where the instance launched, if applicable.
  , priority :: Core.Maybe Core.Double
    -- ^ The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the override has the lowest priority.
  , subnetId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet in which to launch the instances.
  , weightedCapacity :: Core.Maybe Core.Double
    -- ^ The number of units provided by the specified instance type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FleetLaunchTemplateOverrides' value with any optional fields omitted.
mkFleetLaunchTemplateOverrides
    :: FleetLaunchTemplateOverrides
mkFleetLaunchTemplateOverrides
  = FleetLaunchTemplateOverrides'{availabilityZone = Core.Nothing,
                                  instanceType = Core.Nothing, maxPrice = Core.Nothing,
                                  placement = Core.Nothing, priority = Core.Nothing,
                                  subnetId = Core.Nothing, weightedCapacity = Core.Nothing}

-- | The Availability Zone in which to launch the instances.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoAvailabilityZone :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Core.Text)
fltoAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE fltoAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoInstanceType :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Types.InstanceType)
fltoInstanceType = Lens.field @"instanceType"
{-# INLINEABLE fltoInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'maxPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoMaxPrice :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Core.Text)
fltoMaxPrice = Lens.field @"maxPrice"
{-# INLINEABLE fltoMaxPrice #-}
{-# DEPRECATED maxPrice "Use generic-lens or generic-optics with 'maxPrice' instead"  #-}

-- | The location where the instance launched, if applicable.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoPlacement :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Types.PlacementResponse)
fltoPlacement = Lens.field @"placement"
{-# INLINEABLE fltoPlacement #-}
{-# DEPRECATED placement "Use generic-lens or generic-optics with 'placement' instead"  #-}

-- | The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the override has the lowest priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoPriority :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Core.Double)
fltoPriority = Lens.field @"priority"
{-# INLINEABLE fltoPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | The ID of the subnet in which to launch the instances.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoSubnetId :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Core.Text)
fltoSubnetId = Lens.field @"subnetId"
{-# INLINEABLE fltoSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The number of units provided by the specified instance type.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoWeightedCapacity :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Core.Double)
fltoWeightedCapacity = Lens.field @"weightedCapacity"
{-# INLINEABLE fltoWeightedCapacity #-}
{-# DEPRECATED weightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead"  #-}

instance Core.FromXML FleetLaunchTemplateOverrides where
        parseXML x
          = FleetLaunchTemplateOverrides' Core.<$>
              (x Core..@? "availabilityZone") Core.<*> x Core..@? "instanceType"
                Core.<*> x Core..@? "maxPrice"
                Core.<*> x Core..@? "placement"
                Core.<*> x Core..@? "priority"
                Core.<*> x Core..@? "subnetId"
                Core.<*> x Core..@? "weightedCapacity"
