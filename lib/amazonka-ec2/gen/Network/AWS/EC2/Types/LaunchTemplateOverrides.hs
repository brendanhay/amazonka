{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateOverrides
  ( LaunchTemplateOverrides (..),

    -- * Smart constructor
    mkLaunchTemplateOverrides,

    -- * Lenses
    ltoAvailabilityZone,
    ltoInstanceType,
    ltoPriority,
    ltoSpotPrice,
    ltoSubnetId,
    ltoWeightedCapacity,
  )
where

import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes overrides for a launch template.
--
-- /See:/ 'mkLaunchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Core.Maybe Types.String,
    -- | The instance type.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The priority for the launch template override. If __OnDemandAllocationStrategy__ is set to @prioritized@ , Spot Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
    priority :: Core.Maybe Core.Double,
    -- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
    spotPrice :: Core.Maybe Types.String,
    -- | The ID of the subnet in which to launch the instances.
    subnetId :: Core.Maybe Types.SubnetId,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateOverrides' value with any optional fields omitted.
mkLaunchTemplateOverrides ::
  LaunchTemplateOverrides
mkLaunchTemplateOverrides =
  LaunchTemplateOverrides'
    { availabilityZone = Core.Nothing,
      instanceType = Core.Nothing,
      priority = Core.Nothing,
      spotPrice = Core.Nothing,
      subnetId = Core.Nothing,
      weightedCapacity = Core.Nothing
    }

-- | The Availability Zone in which to launch the instances.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoAvailabilityZone :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Types.String)
ltoAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ltoAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoInstanceType :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Types.InstanceType)
ltoInstanceType = Lens.field @"instanceType"
{-# DEPRECATED ltoInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The priority for the launch template override. If __OnDemandAllocationStrategy__ is set to @prioritized@ , Spot Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoPriority :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Double)
ltoPriority = Lens.field @"priority"
{-# DEPRECATED ltoPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoSpotPrice :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Types.String)
ltoSpotPrice = Lens.field @"spotPrice"
{-# DEPRECATED ltoSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The ID of the subnet in which to launch the instances.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoSubnetId :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Types.SubnetId)
ltoSubnetId = Lens.field @"subnetId"
{-# DEPRECATED ltoSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The number of units provided by the specified instance type.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoWeightedCapacity :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Double)
ltoWeightedCapacity = Lens.field @"weightedCapacity"
{-# DEPRECATED ltoWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

instance Core.FromXML LaunchTemplateOverrides where
  parseXML x =
    LaunchTemplateOverrides'
      Core.<$> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "priority")
      Core.<*> (x Core..@? "spotPrice")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "weightedCapacity")
