{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateOverrides
  ( LaunchTemplateOverrides (..)
  -- * Smart constructor
  , mkLaunchTemplateOverrides
  -- * Lenses
  , ltoAvailabilityZone
  , ltoInstanceType
  , ltoPriority
  , ltoSpotPrice
  , ltoSubnetId
  , ltoWeightedCapacity
  ) where

import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes overrides for a launch template.
--
-- /See:/ 'mkLaunchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone in which to launch the instances.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The instance type.
  , priority :: Core.Maybe Core.Double
    -- ^ The priority for the launch template override. If __OnDemandAllocationStrategy__ is set to @prioritized@ , Spot Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
  , spotPrice :: Core.Maybe Core.Text
    -- ^ The maximum price per unit hour that you are willing to pay for a Spot Instance.
  , subnetId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet in which to launch the instances.
  , weightedCapacity :: Core.Maybe Core.Double
    -- ^ The number of units provided by the specified instance type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateOverrides' value with any optional fields omitted.
mkLaunchTemplateOverrides
    :: LaunchTemplateOverrides
mkLaunchTemplateOverrides
  = LaunchTemplateOverrides'{availabilityZone = Core.Nothing,
                             instanceType = Core.Nothing, priority = Core.Nothing,
                             spotPrice = Core.Nothing, subnetId = Core.Nothing,
                             weightedCapacity = Core.Nothing}

-- | The Availability Zone in which to launch the instances.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoAvailabilityZone :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Text)
ltoAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE ltoAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoInstanceType :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Types.InstanceType)
ltoInstanceType = Lens.field @"instanceType"
{-# INLINEABLE ltoInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The priority for the launch template override. If __OnDemandAllocationStrategy__ is set to @prioritized@ , Spot Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoPriority :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Double)
ltoPriority = Lens.field @"priority"
{-# INLINEABLE ltoPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoSpotPrice :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Text)
ltoSpotPrice = Lens.field @"spotPrice"
{-# INLINEABLE ltoSpotPrice #-}
{-# DEPRECATED spotPrice "Use generic-lens or generic-optics with 'spotPrice' instead"  #-}

-- | The ID of the subnet in which to launch the instances.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoSubnetId :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Text)
ltoSubnetId = Lens.field @"subnetId"
{-# INLINEABLE ltoSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The number of units provided by the specified instance type.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoWeightedCapacity :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Double)
ltoWeightedCapacity = Lens.field @"weightedCapacity"
{-# INLINEABLE ltoWeightedCapacity #-}
{-# DEPRECATED weightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead"  #-}

instance Core.ToQuery LaunchTemplateOverrides where
        toQuery LaunchTemplateOverrides{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZone")
              availabilityZone
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceType")
                instanceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Priority") priority
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SpotPrice") spotPrice
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SubnetId") subnetId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "WeightedCapacity")
                weightedCapacity

instance Core.FromXML LaunchTemplateOverrides where
        parseXML x
          = LaunchTemplateOverrides' Core.<$>
              (x Core..@? "availabilityZone") Core.<*> x Core..@? "instanceType"
                Core.<*> x Core..@? "priority"
                Core.<*> x Core..@? "spotPrice"
                Core.<*> x Core..@? "subnetId"
                Core.<*> x Core..@? "weightedCapacity"
