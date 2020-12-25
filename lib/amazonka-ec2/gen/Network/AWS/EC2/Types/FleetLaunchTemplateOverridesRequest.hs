{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateOverridesRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateOverridesRequest
  ( FleetLaunchTemplateOverridesRequest (..),

    -- * Smart constructor
    mkFleetLaunchTemplateOverridesRequest,

    -- * Lenses
    fltorAvailabilityZone,
    fltorInstanceType,
    fltorMaxPrice,
    fltorPlacement,
    fltorPriority,
    fltorSubnetId,
    fltorWeightedCapacity,
  )
where

import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.Placement as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes overrides for a launch template.
--
-- /See:/ 'mkFleetLaunchTemplateOverridesRequest' smart constructor.
data FleetLaunchTemplateOverridesRequest = FleetLaunchTemplateOverridesRequest'
  { -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Core.Maybe Types.String,
    -- | The instance type.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
    maxPrice :: Core.Maybe Types.String,
    -- | The location where the instance launched, if applicable.
    placement :: Core.Maybe Types.Placement,
    -- | The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
    priority :: Core.Maybe Core.Double,
    -- | The IDs of the subnets in which to launch the instances. Separate multiple subnet IDs using commas (for example, @subnet-1234abcdeexample1, subnet-0987cdef6example2@ ). A request of type @instant@ can have only one subnet ID.
    subnetId :: Core.Maybe Types.SubnetId,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FleetLaunchTemplateOverridesRequest' value with any optional fields omitted.
mkFleetLaunchTemplateOverridesRequest ::
  FleetLaunchTemplateOverridesRequest
mkFleetLaunchTemplateOverridesRequest =
  FleetLaunchTemplateOverridesRequest'
    { availabilityZone =
        Core.Nothing,
      instanceType = Core.Nothing,
      maxPrice = Core.Nothing,
      placement = Core.Nothing,
      priority = Core.Nothing,
      subnetId = Core.Nothing,
      weightedCapacity = Core.Nothing
    }

-- | The Availability Zone in which to launch the instances.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorAvailabilityZone :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Types.String)
fltorAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED fltorAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorInstanceType :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Types.InstanceType)
fltorInstanceType = Lens.field @"instanceType"
{-# DEPRECATED fltorInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'maxPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorMaxPrice :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Types.String)
fltorMaxPrice = Lens.field @"maxPrice"
{-# DEPRECATED fltorMaxPrice "Use generic-lens or generic-optics with 'maxPrice' instead." #-}

-- | The location where the instance launched, if applicable.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorPlacement :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Types.Placement)
fltorPlacement = Lens.field @"placement"
{-# DEPRECATED fltorPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorPriority :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Core.Double)
fltorPriority = Lens.field @"priority"
{-# DEPRECATED fltorPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The IDs of the subnets in which to launch the instances. Separate multiple subnet IDs using commas (for example, @subnet-1234abcdeexample1, subnet-0987cdef6example2@ ). A request of type @instant@ can have only one subnet ID.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorSubnetId :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Types.SubnetId)
fltorSubnetId = Lens.field @"subnetId"
{-# DEPRECATED fltorSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The number of units provided by the specified instance type.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorWeightedCapacity :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Core.Double)
fltorWeightedCapacity = Lens.field @"weightedCapacity"
{-# DEPRECATED fltorWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}
