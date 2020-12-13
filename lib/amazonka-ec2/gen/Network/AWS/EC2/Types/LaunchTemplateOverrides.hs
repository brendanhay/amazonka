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
    ltoPriority,
    ltoSpotPrice,
    ltoWeightedCapacity,
    ltoSubnetId,
    ltoInstanceType,
    ltoAvailabilityZone,
  )
where

import Network.AWS.EC2.Types.InstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes overrides for a launch template.
--
-- /See:/ 'mkLaunchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { -- | The priority for the launch template override. If __OnDemandAllocationStrategy__ is set to @prioritized@ , Spot Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
    priority :: Lude.Maybe Lude.Double,
    -- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
    spotPrice :: Lude.Maybe Lude.Text,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Lude.Maybe Lude.Double,
    -- | The ID of the subnet in which to launch the instances.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The instance type.
    instanceType :: Lude.Maybe InstanceType,
    -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateOverrides' with the minimum fields required to make a request.
--
-- * 'priority' - The priority for the launch template override. If __OnDemandAllocationStrategy__ is set to @prioritized@ , Spot Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
-- * 'spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance.
-- * 'weightedCapacity' - The number of units provided by the specified instance type.
-- * 'subnetId' - The ID of the subnet in which to launch the instances.
-- * 'instanceType' - The instance type.
-- * 'availabilityZone' - The Availability Zone in which to launch the instances.
mkLaunchTemplateOverrides ::
  LaunchTemplateOverrides
mkLaunchTemplateOverrides =
  LaunchTemplateOverrides'
    { priority = Lude.Nothing,
      spotPrice = Lude.Nothing,
      weightedCapacity = Lude.Nothing,
      subnetId = Lude.Nothing,
      instanceType = Lude.Nothing,
      availabilityZone = Lude.Nothing
    }

-- | The priority for the launch template override. If __OnDemandAllocationStrategy__ is set to @prioritized@ , Spot Fleet uses priority to determine which launch template override to use first in fulfilling On-Demand capacity. The highest priority is launched first. Valid values are whole numbers starting at @0@ . The lower the number, the higher the priority. If no number is set, the launch template override has the lowest priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoPriority :: Lens.Lens' LaunchTemplateOverrides (Lude.Maybe Lude.Double)
ltoPriority = Lens.lens (priority :: LaunchTemplateOverrides -> Lude.Maybe Lude.Double) (\s a -> s {priority = a} :: LaunchTemplateOverrides)
{-# DEPRECATED ltoPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoSpotPrice :: Lens.Lens' LaunchTemplateOverrides (Lude.Maybe Lude.Text)
ltoSpotPrice = Lens.lens (spotPrice :: LaunchTemplateOverrides -> Lude.Maybe Lude.Text) (\s a -> s {spotPrice = a} :: LaunchTemplateOverrides)
{-# DEPRECATED ltoSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The number of units provided by the specified instance type.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoWeightedCapacity :: Lens.Lens' LaunchTemplateOverrides (Lude.Maybe Lude.Double)
ltoWeightedCapacity = Lens.lens (weightedCapacity :: LaunchTemplateOverrides -> Lude.Maybe Lude.Double) (\s a -> s {weightedCapacity = a} :: LaunchTemplateOverrides)
{-# DEPRECATED ltoWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

-- | The ID of the subnet in which to launch the instances.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoSubnetId :: Lens.Lens' LaunchTemplateOverrides (Lude.Maybe Lude.Text)
ltoSubnetId = Lens.lens (subnetId :: LaunchTemplateOverrides -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: LaunchTemplateOverrides)
{-# DEPRECATED ltoSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoInstanceType :: Lens.Lens' LaunchTemplateOverrides (Lude.Maybe InstanceType)
ltoInstanceType = Lens.lens (instanceType :: LaunchTemplateOverrides -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: LaunchTemplateOverrides)
{-# DEPRECATED ltoInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Availability Zone in which to launch the instances.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoAvailabilityZone :: Lens.Lens' LaunchTemplateOverrides (Lude.Maybe Lude.Text)
ltoAvailabilityZone = Lens.lens (availabilityZone :: LaunchTemplateOverrides -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: LaunchTemplateOverrides)
{-# DEPRECATED ltoAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

instance Lude.FromXML LaunchTemplateOverrides where
  parseXML x =
    LaunchTemplateOverrides'
      Lude.<$> (x Lude..@? "priority")
      Lude.<*> (x Lude..@? "spotPrice")
      Lude.<*> (x Lude..@? "weightedCapacity")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "availabilityZone")

instance Lude.ToQuery LaunchTemplateOverrides where
  toQuery LaunchTemplateOverrides' {..} =
    Lude.mconcat
      [ "Priority" Lude.=: priority,
        "SpotPrice" Lude.=: spotPrice,
        "WeightedCapacity" Lude.=: weightedCapacity,
        "SubnetId" Lude.=: subnetId,
        "InstanceType" Lude.=: instanceType,
        "AvailabilityZone" Lude.=: availabilityZone
      ]
