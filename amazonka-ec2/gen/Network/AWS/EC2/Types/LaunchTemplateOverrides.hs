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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateOverrides
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateOverrides where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import qualified Network.AWS.Lens as Lens

-- | Describes overrides for a launch template.
--
-- /See:/ 'newLaunchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { -- | The instance type.
    instanceType :: Core.Maybe InstanceType,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance.
    spotPrice :: Core.Maybe Core.Text,
    -- | The priority for the launch template override. If
    -- __OnDemandAllocationStrategy__ is set to @prioritized@, Spot Fleet uses
    -- priority to determine which launch template override to use first in
    -- fulfilling On-Demand capacity. The highest priority is launched first.
    -- Valid values are whole numbers starting at @0@. The lower the number,
    -- the higher the priority. If no number is set, the launch template
    -- override has the lowest priority.
    priority :: Core.Maybe Core.Double,
    -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The ID of the subnet in which to launch the instances.
    subnetId :: Core.Maybe Core.Text,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'launchTemplateOverrides_instanceType' - The instance type.
--
-- 'spotPrice', 'launchTemplateOverrides_spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
--
-- 'priority', 'launchTemplateOverrides_priority' - The priority for the launch template override. If
-- __OnDemandAllocationStrategy__ is set to @prioritized@, Spot Fleet uses
-- priority to determine which launch template override to use first in
-- fulfilling On-Demand capacity. The highest priority is launched first.
-- Valid values are whole numbers starting at @0@. The lower the number,
-- the higher the priority. If no number is set, the launch template
-- override has the lowest priority.
--
-- 'availabilityZone', 'launchTemplateOverrides_availabilityZone' - The Availability Zone in which to launch the instances.
--
-- 'subnetId', 'launchTemplateOverrides_subnetId' - The ID of the subnet in which to launch the instances.
--
-- 'weightedCapacity', 'launchTemplateOverrides_weightedCapacity' - The number of units provided by the specified instance type.
newLaunchTemplateOverrides ::
  LaunchTemplateOverrides
newLaunchTemplateOverrides =
  LaunchTemplateOverrides'
    { instanceType =
        Core.Nothing,
      spotPrice = Core.Nothing,
      priority = Core.Nothing,
      availabilityZone = Core.Nothing,
      subnetId = Core.Nothing,
      weightedCapacity = Core.Nothing
    }

-- | The instance type.
launchTemplateOverrides_instanceType :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe InstanceType)
launchTemplateOverrides_instanceType = Lens.lens (\LaunchTemplateOverrides' {instanceType} -> instanceType) (\s@LaunchTemplateOverrides' {} a -> s {instanceType = a} :: LaunchTemplateOverrides)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
launchTemplateOverrides_spotPrice :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Text)
launchTemplateOverrides_spotPrice = Lens.lens (\LaunchTemplateOverrides' {spotPrice} -> spotPrice) (\s@LaunchTemplateOverrides' {} a -> s {spotPrice = a} :: LaunchTemplateOverrides)

-- | The priority for the launch template override. If
-- __OnDemandAllocationStrategy__ is set to @prioritized@, Spot Fleet uses
-- priority to determine which launch template override to use first in
-- fulfilling On-Demand capacity. The highest priority is launched first.
-- Valid values are whole numbers starting at @0@. The lower the number,
-- the higher the priority. If no number is set, the launch template
-- override has the lowest priority.
launchTemplateOverrides_priority :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Double)
launchTemplateOverrides_priority = Lens.lens (\LaunchTemplateOverrides' {priority} -> priority) (\s@LaunchTemplateOverrides' {} a -> s {priority = a} :: LaunchTemplateOverrides)

-- | The Availability Zone in which to launch the instances.
launchTemplateOverrides_availabilityZone :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Text)
launchTemplateOverrides_availabilityZone = Lens.lens (\LaunchTemplateOverrides' {availabilityZone} -> availabilityZone) (\s@LaunchTemplateOverrides' {} a -> s {availabilityZone = a} :: LaunchTemplateOverrides)

-- | The ID of the subnet in which to launch the instances.
launchTemplateOverrides_subnetId :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Text)
launchTemplateOverrides_subnetId = Lens.lens (\LaunchTemplateOverrides' {subnetId} -> subnetId) (\s@LaunchTemplateOverrides' {} a -> s {subnetId = a} :: LaunchTemplateOverrides)

-- | The number of units provided by the specified instance type.
launchTemplateOverrides_weightedCapacity :: Lens.Lens' LaunchTemplateOverrides (Core.Maybe Core.Double)
launchTemplateOverrides_weightedCapacity = Lens.lens (\LaunchTemplateOverrides' {weightedCapacity} -> weightedCapacity) (\s@LaunchTemplateOverrides' {} a -> s {weightedCapacity = a} :: LaunchTemplateOverrides)

instance Core.FromXML LaunchTemplateOverrides where
  parseXML x =
    LaunchTemplateOverrides'
      Core.<$> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "spotPrice")
      Core.<*> (x Core..@? "priority")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "weightedCapacity")

instance Core.Hashable LaunchTemplateOverrides

instance Core.NFData LaunchTemplateOverrides

instance Core.ToQuery LaunchTemplateOverrides where
  toQuery LaunchTemplateOverrides' {..} =
    Core.mconcat
      [ "InstanceType" Core.=: instanceType,
        "SpotPrice" Core.=: spotPrice,
        "Priority" Core.=: priority,
        "AvailabilityZone" Core.=: availabilityZone,
        "SubnetId" Core.=: subnetId,
        "WeightedCapacity" Core.=: weightedCapacity
      ]
