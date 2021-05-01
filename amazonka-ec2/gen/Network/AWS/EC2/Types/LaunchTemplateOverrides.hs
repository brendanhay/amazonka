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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateOverrides
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateOverrides where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes overrides for a launch template.
--
-- /See:/ 'newLaunchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The priority for the launch template override. If
    -- __OnDemandAllocationStrategy__ is set to @prioritized@, Spot Fleet uses
    -- priority to determine which launch template override to use first in
    -- fulfilling On-Demand capacity. The highest priority is launched first.
    -- Valid values are whole numbers starting at @0@. The lower the number,
    -- the higher the priority. If no number is set, the launch template
    -- override has the lowest priority.
    priority :: Prelude.Maybe Prelude.Double,
    -- | The Availability Zone in which to launch the instances.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet in which to launch the instances.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The number of units provided by the specified instance type.
    weightedCapacity :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      priority = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing
    }

-- | The instance type.
launchTemplateOverrides_instanceType :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe InstanceType)
launchTemplateOverrides_instanceType = Lens.lens (\LaunchTemplateOverrides' {instanceType} -> instanceType) (\s@LaunchTemplateOverrides' {} a -> s {instanceType = a} :: LaunchTemplateOverrides)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance.
launchTemplateOverrides_spotPrice :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
launchTemplateOverrides_spotPrice = Lens.lens (\LaunchTemplateOverrides' {spotPrice} -> spotPrice) (\s@LaunchTemplateOverrides' {} a -> s {spotPrice = a} :: LaunchTemplateOverrides)

-- | The priority for the launch template override. If
-- __OnDemandAllocationStrategy__ is set to @prioritized@, Spot Fleet uses
-- priority to determine which launch template override to use first in
-- fulfilling On-Demand capacity. The highest priority is launched first.
-- Valid values are whole numbers starting at @0@. The lower the number,
-- the higher the priority. If no number is set, the launch template
-- override has the lowest priority.
launchTemplateOverrides_priority :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe Prelude.Double)
launchTemplateOverrides_priority = Lens.lens (\LaunchTemplateOverrides' {priority} -> priority) (\s@LaunchTemplateOverrides' {} a -> s {priority = a} :: LaunchTemplateOverrides)

-- | The Availability Zone in which to launch the instances.
launchTemplateOverrides_availabilityZone :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
launchTemplateOverrides_availabilityZone = Lens.lens (\LaunchTemplateOverrides' {availabilityZone} -> availabilityZone) (\s@LaunchTemplateOverrides' {} a -> s {availabilityZone = a} :: LaunchTemplateOverrides)

-- | The ID of the subnet in which to launch the instances.
launchTemplateOverrides_subnetId :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
launchTemplateOverrides_subnetId = Lens.lens (\LaunchTemplateOverrides' {subnetId} -> subnetId) (\s@LaunchTemplateOverrides' {} a -> s {subnetId = a} :: LaunchTemplateOverrides)

-- | The number of units provided by the specified instance type.
launchTemplateOverrides_weightedCapacity :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe Prelude.Double)
launchTemplateOverrides_weightedCapacity = Lens.lens (\LaunchTemplateOverrides' {weightedCapacity} -> weightedCapacity) (\s@LaunchTemplateOverrides' {} a -> s {weightedCapacity = a} :: LaunchTemplateOverrides)

instance Prelude.FromXML LaunchTemplateOverrides where
  parseXML x =
    LaunchTemplateOverrides'
      Prelude.<$> (x Prelude..@? "instanceType")
      Prelude.<*> (x Prelude..@? "spotPrice")
      Prelude.<*> (x Prelude..@? "priority")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "subnetId")
      Prelude.<*> (x Prelude..@? "weightedCapacity")

instance Prelude.Hashable LaunchTemplateOverrides

instance Prelude.NFData LaunchTemplateOverrides

instance Prelude.ToQuery LaunchTemplateOverrides where
  toQuery LaunchTemplateOverrides' {..} =
    Prelude.mconcat
      [ "InstanceType" Prelude.=: instanceType,
        "SpotPrice" Prelude.=: spotPrice,
        "Priority" Prelude.=: priority,
        "AvailabilityZone" Prelude.=: availabilityZone,
        "SubnetId" Prelude.=: subnetId,
        "WeightedCapacity" Prelude.=: weightedCapacity
      ]
