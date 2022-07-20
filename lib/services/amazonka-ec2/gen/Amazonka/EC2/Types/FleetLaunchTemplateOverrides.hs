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
-- Module      : Amazonka.EC2.Types.FleetLaunchTemplateOverrides
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetLaunchTemplateOverrides where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.PlacementResponse
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes overrides for a launch template.
--
-- /See:/ 'newFleetLaunchTemplateOverrides' smart constructor.
data FleetLaunchTemplateOverrides = FleetLaunchTemplateOverrides'
  { -- | The location where the instance launched, if applicable.
    placement :: Prelude.Maybe PlacementResponse,
    -- | The ID of the subnet in which to launch the instances.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance.
    maxPrice :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
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
    -- the higher the priority. If no number is set, the override has the
    -- lowest priority. You can set the same priority for different launch
    -- template overrides.
    priority :: Prelude.Maybe Prelude.Double,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetLaunchTemplateOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placement', 'fleetLaunchTemplateOverrides_placement' - The location where the instance launched, if applicable.
--
-- 'subnetId', 'fleetLaunchTemplateOverrides_subnetId' - The ID of the subnet in which to launch the instances.
--
-- 'maxPrice', 'fleetLaunchTemplateOverrides_maxPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
--
-- 'availabilityZone', 'fleetLaunchTemplateOverrides_availabilityZone' - The Availability Zone in which to launch the instances.
--
-- 'instanceType', 'fleetLaunchTemplateOverrides_instanceType' - The instance type.
--
-- 'priority', 'fleetLaunchTemplateOverrides_priority' - The priority for the launch template override. The highest priority is
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
-- the higher the priority. If no number is set, the override has the
-- lowest priority. You can set the same priority for different launch
-- template overrides.
--
-- 'weightedCapacity', 'fleetLaunchTemplateOverrides_weightedCapacity' - The number of units provided by the specified instance type.
newFleetLaunchTemplateOverrides ::
  FleetLaunchTemplateOverrides
newFleetLaunchTemplateOverrides =
  FleetLaunchTemplateOverrides'
    { placement =
        Prelude.Nothing,
      subnetId = Prelude.Nothing,
      maxPrice = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      priority = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing
    }

-- | The location where the instance launched, if applicable.
fleetLaunchTemplateOverrides_placement :: Lens.Lens' FleetLaunchTemplateOverrides (Prelude.Maybe PlacementResponse)
fleetLaunchTemplateOverrides_placement = Lens.lens (\FleetLaunchTemplateOverrides' {placement} -> placement) (\s@FleetLaunchTemplateOverrides' {} a -> s {placement = a} :: FleetLaunchTemplateOverrides)

-- | The ID of the subnet in which to launch the instances.
fleetLaunchTemplateOverrides_subnetId :: Lens.Lens' FleetLaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateOverrides_subnetId = Lens.lens (\FleetLaunchTemplateOverrides' {subnetId} -> subnetId) (\s@FleetLaunchTemplateOverrides' {} a -> s {subnetId = a} :: FleetLaunchTemplateOverrides)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
fleetLaunchTemplateOverrides_maxPrice :: Lens.Lens' FleetLaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateOverrides_maxPrice = Lens.lens (\FleetLaunchTemplateOverrides' {maxPrice} -> maxPrice) (\s@FleetLaunchTemplateOverrides' {} a -> s {maxPrice = a} :: FleetLaunchTemplateOverrides)

-- | The Availability Zone in which to launch the instances.
fleetLaunchTemplateOverrides_availabilityZone :: Lens.Lens' FleetLaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
fleetLaunchTemplateOverrides_availabilityZone = Lens.lens (\FleetLaunchTemplateOverrides' {availabilityZone} -> availabilityZone) (\s@FleetLaunchTemplateOverrides' {} a -> s {availabilityZone = a} :: FleetLaunchTemplateOverrides)

-- | The instance type.
fleetLaunchTemplateOverrides_instanceType :: Lens.Lens' FleetLaunchTemplateOverrides (Prelude.Maybe InstanceType)
fleetLaunchTemplateOverrides_instanceType = Lens.lens (\FleetLaunchTemplateOverrides' {instanceType} -> instanceType) (\s@FleetLaunchTemplateOverrides' {} a -> s {instanceType = a} :: FleetLaunchTemplateOverrides)

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
-- the higher the priority. If no number is set, the override has the
-- lowest priority. You can set the same priority for different launch
-- template overrides.
fleetLaunchTemplateOverrides_priority :: Lens.Lens' FleetLaunchTemplateOverrides (Prelude.Maybe Prelude.Double)
fleetLaunchTemplateOverrides_priority = Lens.lens (\FleetLaunchTemplateOverrides' {priority} -> priority) (\s@FleetLaunchTemplateOverrides' {} a -> s {priority = a} :: FleetLaunchTemplateOverrides)

-- | The number of units provided by the specified instance type.
fleetLaunchTemplateOverrides_weightedCapacity :: Lens.Lens' FleetLaunchTemplateOverrides (Prelude.Maybe Prelude.Double)
fleetLaunchTemplateOverrides_weightedCapacity = Lens.lens (\FleetLaunchTemplateOverrides' {weightedCapacity} -> weightedCapacity) (\s@FleetLaunchTemplateOverrides' {} a -> s {weightedCapacity = a} :: FleetLaunchTemplateOverrides)

instance Core.FromXML FleetLaunchTemplateOverrides where
  parseXML x =
    FleetLaunchTemplateOverrides'
      Prelude.<$> (x Core..@? "placement")
      Prelude.<*> (x Core..@? "subnetId")
      Prelude.<*> (x Core..@? "maxPrice")
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "instanceType")
      Prelude.<*> (x Core..@? "priority")
      Prelude.<*> (x Core..@? "weightedCapacity")

instance
  Prelude.Hashable
    FleetLaunchTemplateOverrides
  where
  hashWithSalt _salt FleetLaunchTemplateOverrides' {..} =
    _salt `Prelude.hashWithSalt` placement
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` maxPrice
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` weightedCapacity

instance Prelude.NFData FleetLaunchTemplateOverrides where
  rnf FleetLaunchTemplateOverrides' {..} =
    Prelude.rnf placement
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf maxPrice
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf weightedCapacity
