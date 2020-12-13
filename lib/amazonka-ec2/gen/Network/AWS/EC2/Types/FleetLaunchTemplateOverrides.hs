{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
  ( FleetLaunchTemplateOverrides (..),

    -- * Smart constructor
    mkFleetLaunchTemplateOverrides,

    -- * Lenses
    fltoPriority,
    fltoWeightedCapacity,
    fltoSubnetId,
    fltoInstanceType,
    fltoAvailabilityZone,
    fltoPlacement,
    fltoMaxPrice,
  )
where

import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.PlacementResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes overrides for a launch template.
--
-- /See:/ 'mkFleetLaunchTemplateOverrides' smart constructor.
data FleetLaunchTemplateOverrides = FleetLaunchTemplateOverrides'
  { -- | The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the override has the lowest priority.
    priority :: Lude.Maybe Lude.Double,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Lude.Maybe Lude.Double,
    -- | The ID of the subnet in which to launch the instances.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The instance type.
    instanceType :: Lude.Maybe InstanceType,
    -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The location where the instance launched, if applicable.
    placement :: Lude.Maybe PlacementResponse,
    -- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
    maxPrice :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetLaunchTemplateOverrides' with the minimum fields required to make a request.
--
-- * 'priority' - The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the override has the lowest priority.
-- * 'weightedCapacity' - The number of units provided by the specified instance type.
-- * 'subnetId' - The ID of the subnet in which to launch the instances.
-- * 'instanceType' - The instance type.
-- * 'availabilityZone' - The Availability Zone in which to launch the instances.
-- * 'placement' - The location where the instance launched, if applicable.
-- * 'maxPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance.
mkFleetLaunchTemplateOverrides ::
  FleetLaunchTemplateOverrides
mkFleetLaunchTemplateOverrides =
  FleetLaunchTemplateOverrides'
    { priority = Lude.Nothing,
      weightedCapacity = Lude.Nothing,
      subnetId = Lude.Nothing,
      instanceType = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      placement = Lude.Nothing,
      maxPrice = Lude.Nothing
    }

-- | The priority for the launch template override. If __AllocationStrategy__ is set to @prioritized@ , EC2 Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the override has the lowest priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoPriority :: Lens.Lens' FleetLaunchTemplateOverrides (Lude.Maybe Lude.Double)
fltoPriority = Lens.lens (priority :: FleetLaunchTemplateOverrides -> Lude.Maybe Lude.Double) (\s a -> s {priority = a} :: FleetLaunchTemplateOverrides)
{-# DEPRECATED fltoPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The number of units provided by the specified instance type.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoWeightedCapacity :: Lens.Lens' FleetLaunchTemplateOverrides (Lude.Maybe Lude.Double)
fltoWeightedCapacity = Lens.lens (weightedCapacity :: FleetLaunchTemplateOverrides -> Lude.Maybe Lude.Double) (\s a -> s {weightedCapacity = a} :: FleetLaunchTemplateOverrides)
{-# DEPRECATED fltoWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

-- | The ID of the subnet in which to launch the instances.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoSubnetId :: Lens.Lens' FleetLaunchTemplateOverrides (Lude.Maybe Lude.Text)
fltoSubnetId = Lens.lens (subnetId :: FleetLaunchTemplateOverrides -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: FleetLaunchTemplateOverrides)
{-# DEPRECATED fltoSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoInstanceType :: Lens.Lens' FleetLaunchTemplateOverrides (Lude.Maybe InstanceType)
fltoInstanceType = Lens.lens (instanceType :: FleetLaunchTemplateOverrides -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: FleetLaunchTemplateOverrides)
{-# DEPRECATED fltoInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Availability Zone in which to launch the instances.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoAvailabilityZone :: Lens.Lens' FleetLaunchTemplateOverrides (Lude.Maybe Lude.Text)
fltoAvailabilityZone = Lens.lens (availabilityZone :: FleetLaunchTemplateOverrides -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: FleetLaunchTemplateOverrides)
{-# DEPRECATED fltoAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The location where the instance launched, if applicable.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoPlacement :: Lens.Lens' FleetLaunchTemplateOverrides (Lude.Maybe PlacementResponse)
fltoPlacement = Lens.lens (placement :: FleetLaunchTemplateOverrides -> Lude.Maybe PlacementResponse) (\s a -> s {placement = a} :: FleetLaunchTemplateOverrides)
{-# DEPRECATED fltoPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'maxPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltoMaxPrice :: Lens.Lens' FleetLaunchTemplateOverrides (Lude.Maybe Lude.Text)
fltoMaxPrice = Lens.lens (maxPrice :: FleetLaunchTemplateOverrides -> Lude.Maybe Lude.Text) (\s a -> s {maxPrice = a} :: FleetLaunchTemplateOverrides)
{-# DEPRECATED fltoMaxPrice "Use generic-lens or generic-optics with 'maxPrice' instead." #-}

instance Lude.FromXML FleetLaunchTemplateOverrides where
  parseXML x =
    FleetLaunchTemplateOverrides'
      Lude.<$> (x Lude..@? "priority")
      Lude.<*> (x Lude..@? "weightedCapacity")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "placement")
      Lude.<*> (x Lude..@? "maxPrice")
