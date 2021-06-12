{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateOverrides where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.PlacementResponse
import qualified Network.AWS.Lens as Lens

-- | Describes overrides for a launch template.
--
-- /See:/ 'newFleetLaunchTemplateOverrides' smart constructor.
data FleetLaunchTemplateOverrides = FleetLaunchTemplateOverrides'
  { -- | The instance type.
    instanceType :: Core.Maybe InstanceType,
    -- | The location where the instance launched, if applicable.
    placement :: Core.Maybe PlacementResponse,
    -- | The priority for the launch template override. If __AllocationStrategy__
    -- is set to @prioritized@, EC2 Fleet uses priority to determine which
    -- launch template override to use first in fulfilling On-Demand capacity.
    -- The highest priority is launched first. Valid values are whole numbers
    -- starting at @0@. The lower the number, the higher the priority. If no
    -- number is set, the override has the lowest priority.
    priority :: Core.Maybe Core.Double,
    -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance.
    maxPrice :: Core.Maybe Core.Text,
    -- | The ID of the subnet in which to launch the instances.
    subnetId :: Core.Maybe Core.Text,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FleetLaunchTemplateOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'fleetLaunchTemplateOverrides_instanceType' - The instance type.
--
-- 'placement', 'fleetLaunchTemplateOverrides_placement' - The location where the instance launched, if applicable.
--
-- 'priority', 'fleetLaunchTemplateOverrides_priority' - The priority for the launch template override. If __AllocationStrategy__
-- is set to @prioritized@, EC2 Fleet uses priority to determine which
-- launch template override to use first in fulfilling On-Demand capacity.
-- The highest priority is launched first. Valid values are whole numbers
-- starting at @0@. The lower the number, the higher the priority. If no
-- number is set, the override has the lowest priority.
--
-- 'availabilityZone', 'fleetLaunchTemplateOverrides_availabilityZone' - The Availability Zone in which to launch the instances.
--
-- 'maxPrice', 'fleetLaunchTemplateOverrides_maxPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
--
-- 'subnetId', 'fleetLaunchTemplateOverrides_subnetId' - The ID of the subnet in which to launch the instances.
--
-- 'weightedCapacity', 'fleetLaunchTemplateOverrides_weightedCapacity' - The number of units provided by the specified instance type.
newFleetLaunchTemplateOverrides ::
  FleetLaunchTemplateOverrides
newFleetLaunchTemplateOverrides =
  FleetLaunchTemplateOverrides'
    { instanceType =
        Core.Nothing,
      placement = Core.Nothing,
      priority = Core.Nothing,
      availabilityZone = Core.Nothing,
      maxPrice = Core.Nothing,
      subnetId = Core.Nothing,
      weightedCapacity = Core.Nothing
    }

-- | The instance type.
fleetLaunchTemplateOverrides_instanceType :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe InstanceType)
fleetLaunchTemplateOverrides_instanceType = Lens.lens (\FleetLaunchTemplateOverrides' {instanceType} -> instanceType) (\s@FleetLaunchTemplateOverrides' {} a -> s {instanceType = a} :: FleetLaunchTemplateOverrides)

-- | The location where the instance launched, if applicable.
fleetLaunchTemplateOverrides_placement :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe PlacementResponse)
fleetLaunchTemplateOverrides_placement = Lens.lens (\FleetLaunchTemplateOverrides' {placement} -> placement) (\s@FleetLaunchTemplateOverrides' {} a -> s {placement = a} :: FleetLaunchTemplateOverrides)

-- | The priority for the launch template override. If __AllocationStrategy__
-- is set to @prioritized@, EC2 Fleet uses priority to determine which
-- launch template override to use first in fulfilling On-Demand capacity.
-- The highest priority is launched first. Valid values are whole numbers
-- starting at @0@. The lower the number, the higher the priority. If no
-- number is set, the override has the lowest priority.
fleetLaunchTemplateOverrides_priority :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Core.Double)
fleetLaunchTemplateOverrides_priority = Lens.lens (\FleetLaunchTemplateOverrides' {priority} -> priority) (\s@FleetLaunchTemplateOverrides' {} a -> s {priority = a} :: FleetLaunchTemplateOverrides)

-- | The Availability Zone in which to launch the instances.
fleetLaunchTemplateOverrides_availabilityZone :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Core.Text)
fleetLaunchTemplateOverrides_availabilityZone = Lens.lens (\FleetLaunchTemplateOverrides' {availabilityZone} -> availabilityZone) (\s@FleetLaunchTemplateOverrides' {} a -> s {availabilityZone = a} :: FleetLaunchTemplateOverrides)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
fleetLaunchTemplateOverrides_maxPrice :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Core.Text)
fleetLaunchTemplateOverrides_maxPrice = Lens.lens (\FleetLaunchTemplateOverrides' {maxPrice} -> maxPrice) (\s@FleetLaunchTemplateOverrides' {} a -> s {maxPrice = a} :: FleetLaunchTemplateOverrides)

-- | The ID of the subnet in which to launch the instances.
fleetLaunchTemplateOverrides_subnetId :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Core.Text)
fleetLaunchTemplateOverrides_subnetId = Lens.lens (\FleetLaunchTemplateOverrides' {subnetId} -> subnetId) (\s@FleetLaunchTemplateOverrides' {} a -> s {subnetId = a} :: FleetLaunchTemplateOverrides)

-- | The number of units provided by the specified instance type.
fleetLaunchTemplateOverrides_weightedCapacity :: Lens.Lens' FleetLaunchTemplateOverrides (Core.Maybe Core.Double)
fleetLaunchTemplateOverrides_weightedCapacity = Lens.lens (\FleetLaunchTemplateOverrides' {weightedCapacity} -> weightedCapacity) (\s@FleetLaunchTemplateOverrides' {} a -> s {weightedCapacity = a} :: FleetLaunchTemplateOverrides)

instance Core.FromXML FleetLaunchTemplateOverrides where
  parseXML x =
    FleetLaunchTemplateOverrides'
      Core.<$> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "placement")
      Core.<*> (x Core..@? "priority")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "maxPrice")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "weightedCapacity")

instance Core.Hashable FleetLaunchTemplateOverrides

instance Core.NFData FleetLaunchTemplateOverrides
