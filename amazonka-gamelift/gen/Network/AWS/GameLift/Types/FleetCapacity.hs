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
-- Module      : Network.AWS.GameLift.Types.FleetCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetCapacity where

import Network.AWS.GameLift.Types.EC2InstanceCounts
import Network.AWS.GameLift.Types.EC2InstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the fleet\'s capacity. Fleet capacity is measured in
-- EC2 instances. By default, new fleets have a capacity of one instance,
-- but can be updated as needed. The maximum number of instances for a
-- fleet is determined by the fleet\'s instance type.
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   DescribeFleetAttributes
--
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
--
-- /See:/ 'newFleetCapacity' smart constructor.
data FleetCapacity = FleetCapacity'
  { -- | Name of an EC2 instance type that is supported in Amazon GameLift. A
    -- fleet instance type determines the computing resources of each instance
    -- in the fleet, including CPU, memory, storage, and networking capacity.
    -- Amazon GameLift supports the following EC2 instance types. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
    -- for detailed descriptions.
    instanceType :: Prelude.Maybe EC2InstanceType,
    -- | A unique identifier for a fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | Current status of fleet capacity.
    instanceCounts :: Prelude.Maybe EC2InstanceCounts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FleetCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'fleetCapacity_instanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A
-- fleet instance type determines the computing resources of each instance
-- in the fleet, including CPU, memory, storage, and networking capacity.
-- Amazon GameLift supports the following EC2 instance types. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
--
-- 'fleetId', 'fleetCapacity_fleetId' - A unique identifier for a fleet.
--
-- 'instanceCounts', 'fleetCapacity_instanceCounts' - Current status of fleet capacity.
newFleetCapacity ::
  FleetCapacity
newFleetCapacity =
  FleetCapacity'
    { instanceType = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      instanceCounts = Prelude.Nothing
    }

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A
-- fleet instance type determines the computing resources of each instance
-- in the fleet, including CPU, memory, storage, and networking capacity.
-- Amazon GameLift supports the following EC2 instance types. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
fleetCapacity_instanceType :: Lens.Lens' FleetCapacity (Prelude.Maybe EC2InstanceType)
fleetCapacity_instanceType = Lens.lens (\FleetCapacity' {instanceType} -> instanceType) (\s@FleetCapacity' {} a -> s {instanceType = a} :: FleetCapacity)

-- | A unique identifier for a fleet.
fleetCapacity_fleetId :: Lens.Lens' FleetCapacity (Prelude.Maybe Prelude.Text)
fleetCapacity_fleetId = Lens.lens (\FleetCapacity' {fleetId} -> fleetId) (\s@FleetCapacity' {} a -> s {fleetId = a} :: FleetCapacity)

-- | Current status of fleet capacity.
fleetCapacity_instanceCounts :: Lens.Lens' FleetCapacity (Prelude.Maybe EC2InstanceCounts)
fleetCapacity_instanceCounts = Lens.lens (\FleetCapacity' {instanceCounts} -> instanceCounts) (\s@FleetCapacity' {} a -> s {instanceCounts = a} :: FleetCapacity)

instance Prelude.FromJSON FleetCapacity where
  parseJSON =
    Prelude.withObject
      "FleetCapacity"
      ( \x ->
          FleetCapacity'
            Prelude.<$> (x Prelude..:? "InstanceType")
            Prelude.<*> (x Prelude..:? "FleetId")
            Prelude.<*> (x Prelude..:? "InstanceCounts")
      )

instance Prelude.Hashable FleetCapacity

instance Prelude.NFData FleetCapacity
