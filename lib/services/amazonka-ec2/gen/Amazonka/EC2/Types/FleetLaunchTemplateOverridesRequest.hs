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
-- Module      : Amazonka.EC2.Types.FleetLaunchTemplateOverridesRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetLaunchTemplateOverridesRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceRequirementsRequest
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.Placement
import qualified Amazonka.Prelude as Prelude

-- | Describes overrides for a launch template.
--
-- /See:/ 'newFleetLaunchTemplateOverridesRequest' smart constructor.
data FleetLaunchTemplateOverridesRequest = FleetLaunchTemplateOverridesRequest'
  { -- | The location where the instance launched, if applicable.
    placement :: Prelude.Maybe Placement,
    -- | The attributes for the instance types. When you specify instance
    -- attributes, Amazon EC2 will identify instance types with those
    -- attributes.
    --
    -- If you specify @InstanceRequirements@, you can\'t specify
    -- @InstanceType@.
    instanceRequirements :: Prelude.Maybe InstanceRequirementsRequest,
    -- | The IDs of the subnets in which to launch the instances. Separate
    -- multiple subnet IDs using commas (for example,
    -- @subnet-1234abcdeexample1, subnet-0987cdef6example2@). A request of type
    -- @instant@ can have only one subnet ID.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. We do not recommend using this parameter because it can lead
    -- to increased interruptions. If you do not specify this parameter, you
    -- will pay the current Spot price.
    --
    -- If you specify a maximum price, your instances will be interrupted more
    -- frequently than if you do not specify this parameter.
    maxPrice :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
    --
    -- If you specify @InstanceType@, you can\'t specify
    -- @InstanceRequirements@.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The priority for the launch template override. The highest priority is
    -- launched first.
    --
    -- If the On-Demand @AllocationStrategy@ is set to @prioritized@, EC2 Fleet
    -- uses priority to determine which launch template override to use first
    -- in fulfilling On-Demand capacity.
    --
    -- If the Spot @AllocationStrategy@ is set to
    -- @capacity-optimized-prioritized@, EC2 Fleet uses priority on a
    -- best-effort basis to determine which launch template override to use in
    -- fulfilling Spot capacity, but optimizes for capacity first.
    --
    -- Valid values are whole numbers starting at @0@. The lower the number,
    -- the higher the priority. If no number is set, the launch template
    -- override has the lowest priority. You can set the same priority for
    -- different launch template overrides.
    priority :: Prelude.Maybe Prelude.Double,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Prelude.Maybe Prelude.Double,
    -- | The ID of the AMI. An AMI is required to launch an instance. The AMI ID
    -- must be specified here or in the launch template.
    imageId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetLaunchTemplateOverridesRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placement', 'fleetLaunchTemplateOverridesRequest_placement' - The location where the instance launched, if applicable.
--
-- 'instanceRequirements', 'fleetLaunchTemplateOverridesRequest_instanceRequirements' - The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with those
-- attributes.
--
-- If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
--
-- 'subnetId', 'fleetLaunchTemplateOverridesRequest_subnetId' - The IDs of the subnets in which to launch the instances. Separate
-- multiple subnet IDs using commas (for example,
-- @subnet-1234abcdeexample1, subnet-0987cdef6example2@). A request of type
-- @instant@ can have only one subnet ID.
--
-- 'maxPrice', 'fleetLaunchTemplateOverridesRequest_maxPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
--
-- 'availabilityZone', 'fleetLaunchTemplateOverridesRequest_availabilityZone' - The Availability Zone in which to launch the instances.
--
-- 'instanceType', 'fleetLaunchTemplateOverridesRequest_instanceType' - The instance type.
--
-- If you specify @InstanceType@, you can\'t specify
-- @InstanceRequirements@.
--
-- 'priority', 'fleetLaunchTemplateOverridesRequest_priority' - The priority for the launch template override. The highest priority is
-- launched first.
--
-- If the On-Demand @AllocationStrategy@ is set to @prioritized@, EC2 Fleet
-- uses priority to determine which launch template override to use first
-- in fulfilling On-Demand capacity.
--
-- If the Spot @AllocationStrategy@ is set to
-- @capacity-optimized-prioritized@, EC2 Fleet uses priority on a
-- best-effort basis to determine which launch template override to use in
-- fulfilling Spot capacity, but optimizes for capacity first.
--
-- Valid values are whole numbers starting at @0@. The lower the number,
-- the higher the priority. If no number is set, the launch template
-- override has the lowest priority. You can set the same priority for
-- different launch template overrides.
--
-- 'weightedCapacity', 'fleetLaunchTemplateOverridesRequest_weightedCapacity' - The number of units provided by the specified instance type.
--
-- 'imageId', 'fleetLaunchTemplateOverridesRequest_imageId' - The ID of the AMI. An AMI is required to launch an instance. The AMI ID
-- must be specified here or in the launch template.
newFleetLaunchTemplateOverridesRequest ::
  FleetLaunchTemplateOverridesRequest
newFleetLaunchTemplateOverridesRequest =
  FleetLaunchTemplateOverridesRequest'
    { placement =
        Prelude.Nothing,
      instanceRequirements = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      maxPrice = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      priority = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing,
      imageId = Prelude.Nothing
    }

-- | The location where the instance launched, if applicable.
fleetLaunchTemplateOverridesRequest_placement :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Placement)
fleetLaunchTemplateOverridesRequest_placement = Lens.lens (\FleetLaunchTemplateOverridesRequest' {placement} -> placement) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {placement = a} :: FleetLaunchTemplateOverridesRequest)

-- | The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with those
-- attributes.
--
-- If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
fleetLaunchTemplateOverridesRequest_instanceRequirements :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe InstanceRequirementsRequest)
fleetLaunchTemplateOverridesRequest_instanceRequirements = Lens.lens (\FleetLaunchTemplateOverridesRequest' {instanceRequirements} -> instanceRequirements) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {instanceRequirements = a} :: FleetLaunchTemplateOverridesRequest)

-- | The IDs of the subnets in which to launch the instances. Separate
-- multiple subnet IDs using commas (for example,
-- @subnet-1234abcdeexample1, subnet-0987cdef6example2@). A request of type
-- @instant@ can have only one subnet ID.
fleetLaunchTemplateOverridesRequest_subnetId :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateOverridesRequest_subnetId = Lens.lens (\FleetLaunchTemplateOverridesRequest' {subnetId} -> subnetId) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {subnetId = a} :: FleetLaunchTemplateOverridesRequest)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
fleetLaunchTemplateOverridesRequest_maxPrice :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateOverridesRequest_maxPrice = Lens.lens (\FleetLaunchTemplateOverridesRequest' {maxPrice} -> maxPrice) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {maxPrice = a} :: FleetLaunchTemplateOverridesRequest)

-- | The Availability Zone in which to launch the instances.
fleetLaunchTemplateOverridesRequest_availabilityZone :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateOverridesRequest_availabilityZone = Lens.lens (\FleetLaunchTemplateOverridesRequest' {availabilityZone} -> availabilityZone) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {availabilityZone = a} :: FleetLaunchTemplateOverridesRequest)

-- | The instance type.
--
-- If you specify @InstanceType@, you can\'t specify
-- @InstanceRequirements@.
fleetLaunchTemplateOverridesRequest_instanceType :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe InstanceType)
fleetLaunchTemplateOverridesRequest_instanceType = Lens.lens (\FleetLaunchTemplateOverridesRequest' {instanceType} -> instanceType) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {instanceType = a} :: FleetLaunchTemplateOverridesRequest)

-- | The priority for the launch template override. The highest priority is
-- launched first.
--
-- If the On-Demand @AllocationStrategy@ is set to @prioritized@, EC2 Fleet
-- uses priority to determine which launch template override to use first
-- in fulfilling On-Demand capacity.
--
-- If the Spot @AllocationStrategy@ is set to
-- @capacity-optimized-prioritized@, EC2 Fleet uses priority on a
-- best-effort basis to determine which launch template override to use in
-- fulfilling Spot capacity, but optimizes for capacity first.
--
-- Valid values are whole numbers starting at @0@. The lower the number,
-- the higher the priority. If no number is set, the launch template
-- override has the lowest priority. You can set the same priority for
-- different launch template overrides.
fleetLaunchTemplateOverridesRequest_priority :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Double)
fleetLaunchTemplateOverridesRequest_priority = Lens.lens (\FleetLaunchTemplateOverridesRequest' {priority} -> priority) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {priority = a} :: FleetLaunchTemplateOverridesRequest)

-- | The number of units provided by the specified instance type.
fleetLaunchTemplateOverridesRequest_weightedCapacity :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Double)
fleetLaunchTemplateOverridesRequest_weightedCapacity = Lens.lens (\FleetLaunchTemplateOverridesRequest' {weightedCapacity} -> weightedCapacity) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {weightedCapacity = a} :: FleetLaunchTemplateOverridesRequest)

-- | The ID of the AMI. An AMI is required to launch an instance. The AMI ID
-- must be specified here or in the launch template.
fleetLaunchTemplateOverridesRequest_imageId :: Lens.Lens' FleetLaunchTemplateOverridesRequest (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateOverridesRequest_imageId = Lens.lens (\FleetLaunchTemplateOverridesRequest' {imageId} -> imageId) (\s@FleetLaunchTemplateOverridesRequest' {} a -> s {imageId = a} :: FleetLaunchTemplateOverridesRequest)

instance
  Prelude.Hashable
    FleetLaunchTemplateOverridesRequest
  where
  hashWithSalt
    _salt
    FleetLaunchTemplateOverridesRequest' {..} =
      _salt `Prelude.hashWithSalt` placement
        `Prelude.hashWithSalt` instanceRequirements
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` maxPrice
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` weightedCapacity
        `Prelude.hashWithSalt` imageId

instance
  Prelude.NFData
    FleetLaunchTemplateOverridesRequest
  where
  rnf FleetLaunchTemplateOverridesRequest' {..} =
    Prelude.rnf placement
      `Prelude.seq` Prelude.rnf instanceRequirements
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf maxPrice
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf weightedCapacity
      `Prelude.seq` Prelude.rnf imageId

instance
  Core.ToQuery
    FleetLaunchTemplateOverridesRequest
  where
  toQuery FleetLaunchTemplateOverridesRequest' {..} =
    Prelude.mconcat
      [ "Placement" Core.=: placement,
        "InstanceRequirements" Core.=: instanceRequirements,
        "SubnetId" Core.=: subnetId,
        "MaxPrice" Core.=: maxPrice,
        "AvailabilityZone" Core.=: availabilityZone,
        "InstanceType" Core.=: instanceType,
        "Priority" Core.=: priority,
        "WeightedCapacity" Core.=: weightedCapacity,
        "ImageId" Core.=: imageId
      ]
