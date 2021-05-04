{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.Placement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes overrides for a launch template.
--
-- /See:/ 'newFleetLaunchTemplateOverridesRequest' smart constructor.
data FleetLaunchTemplateOverridesRequest = FleetLaunchTemplateOverridesRequest'
  { -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The location where the instance launched, if applicable.
    placement :: Prelude.Maybe Placement,
    -- | The priority for the launch template override. If __AllocationStrategy__
    -- is set to @prioritized@, EC2 Fleet uses priority to determine which
    -- launch template override to use first in fulfilling On-Demand capacity.
    -- The highest priority is launched first. Valid values are whole numbers
    -- starting at @0@. The lower the number, the higher the priority. If no
    -- number is set, the launch template override has the lowest priority.
    priority :: Prelude.Maybe Prelude.Double,
    -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance.
    maxPrice :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the subnets in which to launch the instances. Separate
    -- multiple subnet IDs using commas (for example,
    -- @subnet-1234abcdeexample1, subnet-0987cdef6example2@). A request of type
    -- @instant@ can have only one subnet ID.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      placement = Prelude.Nothing,
      priority = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      maxPrice = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing
    }

-- | The instance type.
fleetLaunchTemplateOverridesRequest_instanceType :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe InstanceType)
fleetLaunchTemplateOverridesRequest_instanceType = Lens.lens (\FleetLaunchTemplateOverridesRequest' {instanceType} -> instanceType) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {instanceType = a} :: FleetLaunchTemplateOverridesRequest)

-- | The location where the instance launched, if applicable.
fleetLaunchTemplateOverridesRequest_placement :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Placement)
fleetLaunchTemplateOverridesRequest_placement = Lens.lens (\FleetLaunchTemplateOverridesRequest' {placement} -> placement) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {placement = a} :: FleetLaunchTemplateOverridesRequest)

-- | The priority for the launch template override. If __AllocationStrategy__
-- is set to @prioritized@, EC2 Fleet uses priority to determine which
-- launch template override to use first in fulfilling On-Demand capacity.
-- The highest priority is launched first. Valid values are whole numbers
-- starting at @0@. The lower the number, the higher the priority. If no
-- number is set, the launch template override has the lowest priority.
fleetLaunchTemplateOverridesRequest_priority :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Double)
fleetLaunchTemplateOverridesRequest_priority = Lens.lens (\FleetLaunchTemplateOverridesRequest' {priority} -> priority) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {priority = a} :: FleetLaunchTemplateOverridesRequest)

-- | The Availability Zone in which to launch the instances.
fleetLaunchTemplateOverridesRequest_availabilityZone :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateOverridesRequest_availabilityZone = Lens.lens (\FleetLaunchTemplateOverridesRequest' {availabilityZone} -> availabilityZone) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {availabilityZone = a} :: FleetLaunchTemplateOverridesRequest)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
fleetLaunchTemplateOverridesRequest_maxPrice :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateOverridesRequest_maxPrice = Lens.lens (\FleetLaunchTemplateOverridesRequest' {maxPrice} -> maxPrice) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {maxPrice = a} :: FleetLaunchTemplateOverridesRequest)

-- | The IDs of the subnets in which to launch the instances. Separate
-- multiple subnet IDs using commas (for example,
-- @subnet-1234abcdeexample1, subnet-0987cdef6example2@). A request of type
-- @instant@ can have only one subnet ID.
fleetLaunchTemplateOverridesRequest_subnetId :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateOverridesRequest_subnetId = Lens.lens (\FleetLaunchTemplateOverridesRequest' {subnetId} -> subnetId) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {subnetId = a} :: FleetLaunchTemplateOverridesRequest)

-- | The number of units provided by the specified instance type.
fleetLaunchTemplateOverridesRequest_weightedCapacity :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Double)
fleetLaunchTemplateOverridesRequest_weightedCapacity = Lens.lens (\FleetLaunchTemplateOverridesRequest' {weightedCapacity} -> weightedCapacity) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {weightedCapacity = a} :: FleetLaunchTemplateOverridesRequest)

instance
  Prelude.Hashable
    FleetLaunchTemplateOverridesRequest

instance
  Prelude.NFData
    FleetLaunchTemplateOverridesRequest

instance
  Prelude.ToQuery
    FleetLaunchTemplateOverridesRequest
  where
  toQuery FleetLaunchTemplateOverridesRequest' {..} =
    Prelude.mconcat
      [ "InstanceType" Prelude.=: instanceType,
        "Placement" Prelude.=: placement,
        "Priority" Prelude.=: priority,
        "AvailabilityZone" Prelude.=: availabilityZone,
        "MaxPrice" Prelude.=: maxPrice,
        "SubnetId" Prelude.=: subnetId,
        "WeightedCapacity" Prelude.=: weightedCapacity
      ]
