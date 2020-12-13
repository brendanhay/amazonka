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
    fltorPriority,
    fltorWeightedCapacity,
    fltorSubnetId,
    fltorInstanceType,
    fltorAvailabilityZone,
    fltorPlacement,
    fltorMaxPrice,
  )
where

import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.Placement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes overrides for a launch template.
--
-- /See:/ 'mkFleetLaunchTemplateOverridesRequest' smart constructor.
data FleetLaunchTemplateOverridesRequest = FleetLaunchTemplateOverridesRequest'
  { -- | The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
    priority :: Lude.Maybe Lude.Double,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Lude.Maybe Lude.Double,
    -- | The IDs of the subnets in which to launch the instances. Separate multiple subnet IDs using commas (for example, @subnet-1234abcdeexample1, subnet-0987cdef6example2@ ). A request of type @instant@ can have only one subnet ID.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The instance type.
    instanceType :: Lude.Maybe InstanceType,
    -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The location where the instance launched, if applicable.
    placement :: Lude.Maybe Placement,
    -- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
    maxPrice :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetLaunchTemplateOverridesRequest' with the minimum fields required to make a request.
--
-- * 'priority' - The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
-- * 'weightedCapacity' - The number of units provided by the specified instance type.
-- * 'subnetId' - The IDs of the subnets in which to launch the instances. Separate multiple subnet IDs using commas (for example, @subnet-1234abcdeexample1, subnet-0987cdef6example2@ ). A request of type @instant@ can have only one subnet ID.
-- * 'instanceType' - The instance type.
-- * 'availabilityZone' - The Availability Zone in which to launch the instances.
-- * 'placement' - The location where the instance launched, if applicable.
-- * 'maxPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance.
mkFleetLaunchTemplateOverridesRequest ::
  FleetLaunchTemplateOverridesRequest
mkFleetLaunchTemplateOverridesRequest =
  FleetLaunchTemplateOverridesRequest'
    { priority = Lude.Nothing,
      weightedCapacity = Lude.Nothing,
      subnetId = Lude.Nothing,
      instanceType = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      placement = Lude.Nothing,
      maxPrice = Lude.Nothing
    }

-- | The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorPriority :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Lude.Maybe Lude.Double)
fltorPriority = Lens.lens (priority :: FleetLaunchTemplateOverridesRequest -> Lude.Maybe Lude.Double) (\s a -> s {priority = a} :: FleetLaunchTemplateOverridesRequest)
{-# DEPRECATED fltorPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The number of units provided by the specified instance type.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorWeightedCapacity :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Lude.Maybe Lude.Double)
fltorWeightedCapacity = Lens.lens (weightedCapacity :: FleetLaunchTemplateOverridesRequest -> Lude.Maybe Lude.Double) (\s a -> s {weightedCapacity = a} :: FleetLaunchTemplateOverridesRequest)
{-# DEPRECATED fltorWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

-- | The IDs of the subnets in which to launch the instances. Separate multiple subnet IDs using commas (for example, @subnet-1234abcdeexample1, subnet-0987cdef6example2@ ). A request of type @instant@ can have only one subnet ID.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorSubnetId :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Lude.Maybe Lude.Text)
fltorSubnetId = Lens.lens (subnetId :: FleetLaunchTemplateOverridesRequest -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: FleetLaunchTemplateOverridesRequest)
{-# DEPRECATED fltorSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorInstanceType :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Lude.Maybe InstanceType)
fltorInstanceType = Lens.lens (instanceType :: FleetLaunchTemplateOverridesRequest -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: FleetLaunchTemplateOverridesRequest)
{-# DEPRECATED fltorInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Availability Zone in which to launch the instances.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorAvailabilityZone :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Lude.Maybe Lude.Text)
fltorAvailabilityZone = Lens.lens (availabilityZone :: FleetLaunchTemplateOverridesRequest -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: FleetLaunchTemplateOverridesRequest)
{-# DEPRECATED fltorAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The location where the instance launched, if applicable.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorPlacement :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Lude.Maybe Placement)
fltorPlacement = Lens.lens (placement :: FleetLaunchTemplateOverridesRequest -> Lude.Maybe Placement) (\s a -> s {placement = a} :: FleetLaunchTemplateOverridesRequest)
{-# DEPRECATED fltorPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'maxPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltorMaxPrice :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Lude.Maybe Lude.Text)
fltorMaxPrice = Lens.lens (maxPrice :: FleetLaunchTemplateOverridesRequest -> Lude.Maybe Lude.Text) (\s a -> s {maxPrice = a} :: FleetLaunchTemplateOverridesRequest)
{-# DEPRECATED fltorMaxPrice "Use generic-lens or generic-optics with 'maxPrice' instead." #-}

instance Lude.ToQuery FleetLaunchTemplateOverridesRequest where
  toQuery FleetLaunchTemplateOverridesRequest' {..} =
    Lude.mconcat
      [ "Priority" Lude.=: priority,
        "WeightedCapacity" Lude.=: weightedCapacity,
        "SubnetId" Lude.=: subnetId,
        "InstanceType" Lude.=: instanceType,
        "AvailabilityZone" Lude.=: availabilityZone,
        "Placement" Lude.=: placement,
        "MaxPrice" Lude.=: maxPrice
      ]
