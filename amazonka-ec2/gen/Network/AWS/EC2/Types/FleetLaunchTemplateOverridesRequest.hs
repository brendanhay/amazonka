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
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateOverridesRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateOverridesRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.Placement
import qualified Network.AWS.Lens as Lens

-- | Describes overrides for a launch template.
--
-- /See:/ 'newFleetLaunchTemplateOverridesRequest' smart constructor.
data FleetLaunchTemplateOverridesRequest = FleetLaunchTemplateOverridesRequest'
  { -- | The instance type.
    instanceType :: Core.Maybe InstanceType,
    -- | The location where the instance launched, if applicable.
    placement :: Core.Maybe Placement,
    -- | The priority for the launch template override. If __AllocationStrategy__
    -- is set to @prioritized@, EC2 Fleet uses priority to determine which
    -- launch template override to use first in fulfilling On-Demand capacity.
    -- The highest priority is launched first. Valid values are whole numbers
    -- starting at @0@. The lower the number, the higher the priority. If no
    -- number is set, the launch template override has the lowest priority.
    priority :: Core.Maybe Core.Double,
    -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance.
    maxPrice :: Core.Maybe Core.Text,
    -- | The IDs of the subnets in which to launch the instances. Separate
    -- multiple subnet IDs using commas (for example,
    -- @subnet-1234abcdeexample1, subnet-0987cdef6example2@). A request of type
    -- @instant@ can have only one subnet ID.
    subnetId :: Core.Maybe Core.Text,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FleetLaunchTemplateOverridesRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'fleetLaunchTemplateOverridesRequest_instanceType' - The instance type.
--
-- 'placement', 'fleetLaunchTemplateOverridesRequest_placement' - The location where the instance launched, if applicable.
--
-- 'priority', 'fleetLaunchTemplateOverridesRequest_priority' - The priority for the launch template override. If __AllocationStrategy__
-- is set to @prioritized@, EC2 Fleet uses priority to determine which
-- launch template override to use first in fulfilling On-Demand capacity.
-- The highest priority is launched first. Valid values are whole numbers
-- starting at @0@. The lower the number, the higher the priority. If no
-- number is set, the launch template override has the lowest priority.
--
-- 'availabilityZone', 'fleetLaunchTemplateOverridesRequest_availabilityZone' - The Availability Zone in which to launch the instances.
--
-- 'maxPrice', 'fleetLaunchTemplateOverridesRequest_maxPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
--
-- 'subnetId', 'fleetLaunchTemplateOverridesRequest_subnetId' - The IDs of the subnets in which to launch the instances. Separate
-- multiple subnet IDs using commas (for example,
-- @subnet-1234abcdeexample1, subnet-0987cdef6example2@). A request of type
-- @instant@ can have only one subnet ID.
--
-- 'weightedCapacity', 'fleetLaunchTemplateOverridesRequest_weightedCapacity' - The number of units provided by the specified instance type.
newFleetLaunchTemplateOverridesRequest ::
  FleetLaunchTemplateOverridesRequest
newFleetLaunchTemplateOverridesRequest =
  FleetLaunchTemplateOverridesRequest'
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
fleetLaunchTemplateOverridesRequest_instanceType :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe InstanceType)
fleetLaunchTemplateOverridesRequest_instanceType = Lens.lens (\FleetLaunchTemplateOverridesRequest' {instanceType} -> instanceType) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {instanceType = a} :: FleetLaunchTemplateOverridesRequest)

-- | The location where the instance launched, if applicable.
fleetLaunchTemplateOverridesRequest_placement :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Placement)
fleetLaunchTemplateOverridesRequest_placement = Lens.lens (\FleetLaunchTemplateOverridesRequest' {placement} -> placement) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {placement = a} :: FleetLaunchTemplateOverridesRequest)

-- | The priority for the launch template override. If __AllocationStrategy__
-- is set to @prioritized@, EC2 Fleet uses priority to determine which
-- launch template override to use first in fulfilling On-Demand capacity.
-- The highest priority is launched first. Valid values are whole numbers
-- starting at @0@. The lower the number, the higher the priority. If no
-- number is set, the launch template override has the lowest priority.
fleetLaunchTemplateOverridesRequest_priority :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Core.Double)
fleetLaunchTemplateOverridesRequest_priority = Lens.lens (\FleetLaunchTemplateOverridesRequest' {priority} -> priority) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {priority = a} :: FleetLaunchTemplateOverridesRequest)

-- | The Availability Zone in which to launch the instances.
fleetLaunchTemplateOverridesRequest_availabilityZone :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Core.Text)
fleetLaunchTemplateOverridesRequest_availabilityZone = Lens.lens (\FleetLaunchTemplateOverridesRequest' {availabilityZone} -> availabilityZone) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {availabilityZone = a} :: FleetLaunchTemplateOverridesRequest)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
fleetLaunchTemplateOverridesRequest_maxPrice :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Core.Text)
fleetLaunchTemplateOverridesRequest_maxPrice = Lens.lens (\FleetLaunchTemplateOverridesRequest' {maxPrice} -> maxPrice) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {maxPrice = a} :: FleetLaunchTemplateOverridesRequest)

-- | The IDs of the subnets in which to launch the instances. Separate
-- multiple subnet IDs using commas (for example,
-- @subnet-1234abcdeexample1, subnet-0987cdef6example2@). A request of type
-- @instant@ can have only one subnet ID.
fleetLaunchTemplateOverridesRequest_subnetId :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Core.Text)
fleetLaunchTemplateOverridesRequest_subnetId = Lens.lens (\FleetLaunchTemplateOverridesRequest' {subnetId} -> subnetId) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {subnetId = a} :: FleetLaunchTemplateOverridesRequest)

-- | The number of units provided by the specified instance type.
fleetLaunchTemplateOverridesRequest_weightedCapacity :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Core.Maybe Core.Double)
fleetLaunchTemplateOverridesRequest_weightedCapacity = Lens.lens (\FleetLaunchTemplateOverridesRequest' {weightedCapacity} -> weightedCapacity) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {weightedCapacity = a} :: FleetLaunchTemplateOverridesRequest)

instance
  Core.Hashable
    FleetLaunchTemplateOverridesRequest

instance
  Core.NFData
    FleetLaunchTemplateOverridesRequest

instance
  Core.ToQuery
    FleetLaunchTemplateOverridesRequest
  where
  toQuery FleetLaunchTemplateOverridesRequest' {..} =
    Core.mconcat
      [ "InstanceType" Core.=: instanceType,
        "Placement" Core.=: placement,
        "Priority" Core.=: priority,
        "AvailabilityZone" Core.=: availabilityZone,
        "MaxPrice" Core.=: maxPrice,
        "SubnetId" Core.=: subnetId,
        "WeightedCapacity" Core.=: weightedCapacity
      ]
