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
-- Module      : Amazonka.EMR.Types.InstanceFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceFleet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.InstanceFleetProvisioningSpecifications
import Amazonka.EMR.Types.InstanceFleetStatus
import Amazonka.EMR.Types.InstanceFleetType
import Amazonka.EMR.Types.InstanceTypeSpecification
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance fleet, which is a group of EC2 instances that host
-- a particular node type (master, core, or task) in an Amazon EMR cluster.
-- Instance fleets can consist of a mix of instance types and On-Demand and
-- Spot Instances, which are provisioned to meet a defined target capacity.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceFleet' smart constructor.
data InstanceFleet = InstanceFleet'
  { -- | The unique identifier of the instance fleet.
    id :: Prelude.Maybe Prelude.Text,
    -- | The node type that the instance fleet hosts. Valid values are MASTER,
    -- CORE, or TASK.
    instanceFleetType :: Prelude.Maybe InstanceFleetType,
    -- | An array of specifications for the instance types that comprise an
    -- instance fleet.
    instanceTypeSpecifications :: Prelude.Maybe [InstanceTypeSpecification],
    -- | Describes the launch specification for an instance fleet.
    launchSpecifications :: Prelude.Maybe InstanceFleetProvisioningSpecifications,
    -- | A friendly name for the instance fleet.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of On-Demand units that have been provisioned for the
    -- instance fleet to fulfill @TargetOnDemandCapacity@. This provisioned
    -- capacity might be less than or greater than @TargetOnDemandCapacity@.
    provisionedOnDemandCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The number of Spot units that have been provisioned for this instance
    -- fleet to fulfill @TargetSpotCapacity@. This provisioned capacity might
    -- be less than or greater than @TargetSpotCapacity@.
    provisionedSpotCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The current status of the instance fleet.
    status :: Prelude.Maybe InstanceFleetStatus,
    -- | The target capacity of On-Demand units for the instance fleet, which
    -- determines how many On-Demand Instances to provision. When the instance
    -- fleet launches, Amazon EMR tries to provision On-Demand Instances as
    -- specified by InstanceTypeConfig. Each instance configuration has a
    -- specified @WeightedCapacity@. When an On-Demand Instance is provisioned,
    -- the @WeightedCapacity@ units count toward the target capacity. Amazon
    -- EMR provisions instances until the target capacity is totally fulfilled,
    -- even if this results in an overage. For example, if there are 2 units
    -- remaining to fulfill capacity, and Amazon EMR can only provision an
    -- instance with a @WeightedCapacity@ of 5 units, the instance is
    -- provisioned, and the target capacity is exceeded by 3 units. You can use
    -- InstanceFleet$ProvisionedOnDemandCapacity to determine the Spot capacity
    -- units that have been provisioned for the instance fleet.
    --
    -- If not specified or set to 0, only Spot Instances are provisioned for
    -- the instance fleet using @TargetSpotCapacity@. At least one of
    -- @TargetSpotCapacity@ and @TargetOnDemandCapacity@ should be greater than
    -- 0. For a master instance fleet, only one of @TargetSpotCapacity@ and
    -- @TargetOnDemandCapacity@ can be specified, and its value must be 1.
    targetOnDemandCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The target capacity of Spot units for the instance fleet, which
    -- determines how many Spot Instances to provision. When the instance fleet
    -- launches, Amazon EMR tries to provision Spot Instances as specified by
    -- InstanceTypeConfig. Each instance configuration has a specified
    -- @WeightedCapacity@. When a Spot instance is provisioned, the
    -- @WeightedCapacity@ units count toward the target capacity. Amazon EMR
    -- provisions instances until the target capacity is totally fulfilled,
    -- even if this results in an overage. For example, if there are 2 units
    -- remaining to fulfill capacity, and Amazon EMR can only provision an
    -- instance with a @WeightedCapacity@ of 5 units, the instance is
    -- provisioned, and the target capacity is exceeded by 3 units. You can use
    -- InstanceFleet$ProvisionedSpotCapacity to determine the Spot capacity
    -- units that have been provisioned for the instance fleet.
    --
    -- If not specified or set to 0, only On-Demand Instances are provisioned
    -- for the instance fleet. At least one of @TargetSpotCapacity@ and
    -- @TargetOnDemandCapacity@ should be greater than 0. For a master instance
    -- fleet, only one of @TargetSpotCapacity@ and @TargetOnDemandCapacity@ can
    -- be specified, and its value must be 1.
    targetSpotCapacity :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'instanceFleet_id' - The unique identifier of the instance fleet.
--
-- 'instanceFleetType', 'instanceFleet_instanceFleetType' - The node type that the instance fleet hosts. Valid values are MASTER,
-- CORE, or TASK.
--
-- 'instanceTypeSpecifications', 'instanceFleet_instanceTypeSpecifications' - An array of specifications for the instance types that comprise an
-- instance fleet.
--
-- 'launchSpecifications', 'instanceFleet_launchSpecifications' - Describes the launch specification for an instance fleet.
--
-- 'name', 'instanceFleet_name' - A friendly name for the instance fleet.
--
-- 'provisionedOnDemandCapacity', 'instanceFleet_provisionedOnDemandCapacity' - The number of On-Demand units that have been provisioned for the
-- instance fleet to fulfill @TargetOnDemandCapacity@. This provisioned
-- capacity might be less than or greater than @TargetOnDemandCapacity@.
--
-- 'provisionedSpotCapacity', 'instanceFleet_provisionedSpotCapacity' - The number of Spot units that have been provisioned for this instance
-- fleet to fulfill @TargetSpotCapacity@. This provisioned capacity might
-- be less than or greater than @TargetSpotCapacity@.
--
-- 'status', 'instanceFleet_status' - The current status of the instance fleet.
--
-- 'targetOnDemandCapacity', 'instanceFleet_targetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet, which
-- determines how many On-Demand Instances to provision. When the instance
-- fleet launches, Amazon EMR tries to provision On-Demand Instances as
-- specified by InstanceTypeConfig. Each instance configuration has a
-- specified @WeightedCapacity@. When an On-Demand Instance is provisioned,
-- the @WeightedCapacity@ units count toward the target capacity. Amazon
-- EMR provisions instances until the target capacity is totally fulfilled,
-- even if this results in an overage. For example, if there are 2 units
-- remaining to fulfill capacity, and Amazon EMR can only provision an
-- instance with a @WeightedCapacity@ of 5 units, the instance is
-- provisioned, and the target capacity is exceeded by 3 units. You can use
-- InstanceFleet$ProvisionedOnDemandCapacity to determine the Spot capacity
-- units that have been provisioned for the instance fleet.
--
-- If not specified or set to 0, only Spot Instances are provisioned for
-- the instance fleet using @TargetSpotCapacity@. At least one of
-- @TargetSpotCapacity@ and @TargetOnDemandCapacity@ should be greater than
-- 0. For a master instance fleet, only one of @TargetSpotCapacity@ and
-- @TargetOnDemandCapacity@ can be specified, and its value must be 1.
--
-- 'targetSpotCapacity', 'instanceFleet_targetSpotCapacity' - The target capacity of Spot units for the instance fleet, which
-- determines how many Spot Instances to provision. When the instance fleet
-- launches, Amazon EMR tries to provision Spot Instances as specified by
-- InstanceTypeConfig. Each instance configuration has a specified
-- @WeightedCapacity@. When a Spot instance is provisioned, the
-- @WeightedCapacity@ units count toward the target capacity. Amazon EMR
-- provisions instances until the target capacity is totally fulfilled,
-- even if this results in an overage. For example, if there are 2 units
-- remaining to fulfill capacity, and Amazon EMR can only provision an
-- instance with a @WeightedCapacity@ of 5 units, the instance is
-- provisioned, and the target capacity is exceeded by 3 units. You can use
-- InstanceFleet$ProvisionedSpotCapacity to determine the Spot capacity
-- units that have been provisioned for the instance fleet.
--
-- If not specified or set to 0, only On-Demand Instances are provisioned
-- for the instance fleet. At least one of @TargetSpotCapacity@ and
-- @TargetOnDemandCapacity@ should be greater than 0. For a master instance
-- fleet, only one of @TargetSpotCapacity@ and @TargetOnDemandCapacity@ can
-- be specified, and its value must be 1.
newInstanceFleet ::
  InstanceFleet
newInstanceFleet =
  InstanceFleet'
    { id = Prelude.Nothing,
      instanceFleetType = Prelude.Nothing,
      instanceTypeSpecifications = Prelude.Nothing,
      launchSpecifications = Prelude.Nothing,
      name = Prelude.Nothing,
      provisionedOnDemandCapacity = Prelude.Nothing,
      provisionedSpotCapacity = Prelude.Nothing,
      status = Prelude.Nothing,
      targetOnDemandCapacity = Prelude.Nothing,
      targetSpotCapacity = Prelude.Nothing
    }

-- | The unique identifier of the instance fleet.
instanceFleet_id :: Lens.Lens' InstanceFleet (Prelude.Maybe Prelude.Text)
instanceFleet_id = Lens.lens (\InstanceFleet' {id} -> id) (\s@InstanceFleet' {} a -> s {id = a} :: InstanceFleet)

-- | The node type that the instance fleet hosts. Valid values are MASTER,
-- CORE, or TASK.
instanceFleet_instanceFleetType :: Lens.Lens' InstanceFleet (Prelude.Maybe InstanceFleetType)
instanceFleet_instanceFleetType = Lens.lens (\InstanceFleet' {instanceFleetType} -> instanceFleetType) (\s@InstanceFleet' {} a -> s {instanceFleetType = a} :: InstanceFleet)

-- | An array of specifications for the instance types that comprise an
-- instance fleet.
instanceFleet_instanceTypeSpecifications :: Lens.Lens' InstanceFleet (Prelude.Maybe [InstanceTypeSpecification])
instanceFleet_instanceTypeSpecifications = Lens.lens (\InstanceFleet' {instanceTypeSpecifications} -> instanceTypeSpecifications) (\s@InstanceFleet' {} a -> s {instanceTypeSpecifications = a} :: InstanceFleet) Prelude.. Lens.mapping Lens.coerced

-- | Describes the launch specification for an instance fleet.
instanceFleet_launchSpecifications :: Lens.Lens' InstanceFleet (Prelude.Maybe InstanceFleetProvisioningSpecifications)
instanceFleet_launchSpecifications = Lens.lens (\InstanceFleet' {launchSpecifications} -> launchSpecifications) (\s@InstanceFleet' {} a -> s {launchSpecifications = a} :: InstanceFleet)

-- | A friendly name for the instance fleet.
instanceFleet_name :: Lens.Lens' InstanceFleet (Prelude.Maybe Prelude.Text)
instanceFleet_name = Lens.lens (\InstanceFleet' {name} -> name) (\s@InstanceFleet' {} a -> s {name = a} :: InstanceFleet)

-- | The number of On-Demand units that have been provisioned for the
-- instance fleet to fulfill @TargetOnDemandCapacity@. This provisioned
-- capacity might be less than or greater than @TargetOnDemandCapacity@.
instanceFleet_provisionedOnDemandCapacity :: Lens.Lens' InstanceFleet (Prelude.Maybe Prelude.Natural)
instanceFleet_provisionedOnDemandCapacity = Lens.lens (\InstanceFleet' {provisionedOnDemandCapacity} -> provisionedOnDemandCapacity) (\s@InstanceFleet' {} a -> s {provisionedOnDemandCapacity = a} :: InstanceFleet)

-- | The number of Spot units that have been provisioned for this instance
-- fleet to fulfill @TargetSpotCapacity@. This provisioned capacity might
-- be less than or greater than @TargetSpotCapacity@.
instanceFleet_provisionedSpotCapacity :: Lens.Lens' InstanceFleet (Prelude.Maybe Prelude.Natural)
instanceFleet_provisionedSpotCapacity = Lens.lens (\InstanceFleet' {provisionedSpotCapacity} -> provisionedSpotCapacity) (\s@InstanceFleet' {} a -> s {provisionedSpotCapacity = a} :: InstanceFleet)

-- | The current status of the instance fleet.
instanceFleet_status :: Lens.Lens' InstanceFleet (Prelude.Maybe InstanceFleetStatus)
instanceFleet_status = Lens.lens (\InstanceFleet' {status} -> status) (\s@InstanceFleet' {} a -> s {status = a} :: InstanceFleet)

-- | The target capacity of On-Demand units for the instance fleet, which
-- determines how many On-Demand Instances to provision. When the instance
-- fleet launches, Amazon EMR tries to provision On-Demand Instances as
-- specified by InstanceTypeConfig. Each instance configuration has a
-- specified @WeightedCapacity@. When an On-Demand Instance is provisioned,
-- the @WeightedCapacity@ units count toward the target capacity. Amazon
-- EMR provisions instances until the target capacity is totally fulfilled,
-- even if this results in an overage. For example, if there are 2 units
-- remaining to fulfill capacity, and Amazon EMR can only provision an
-- instance with a @WeightedCapacity@ of 5 units, the instance is
-- provisioned, and the target capacity is exceeded by 3 units. You can use
-- InstanceFleet$ProvisionedOnDemandCapacity to determine the Spot capacity
-- units that have been provisioned for the instance fleet.
--
-- If not specified or set to 0, only Spot Instances are provisioned for
-- the instance fleet using @TargetSpotCapacity@. At least one of
-- @TargetSpotCapacity@ and @TargetOnDemandCapacity@ should be greater than
-- 0. For a master instance fleet, only one of @TargetSpotCapacity@ and
-- @TargetOnDemandCapacity@ can be specified, and its value must be 1.
instanceFleet_targetOnDemandCapacity :: Lens.Lens' InstanceFleet (Prelude.Maybe Prelude.Natural)
instanceFleet_targetOnDemandCapacity = Lens.lens (\InstanceFleet' {targetOnDemandCapacity} -> targetOnDemandCapacity) (\s@InstanceFleet' {} a -> s {targetOnDemandCapacity = a} :: InstanceFleet)

-- | The target capacity of Spot units for the instance fleet, which
-- determines how many Spot Instances to provision. When the instance fleet
-- launches, Amazon EMR tries to provision Spot Instances as specified by
-- InstanceTypeConfig. Each instance configuration has a specified
-- @WeightedCapacity@. When a Spot instance is provisioned, the
-- @WeightedCapacity@ units count toward the target capacity. Amazon EMR
-- provisions instances until the target capacity is totally fulfilled,
-- even if this results in an overage. For example, if there are 2 units
-- remaining to fulfill capacity, and Amazon EMR can only provision an
-- instance with a @WeightedCapacity@ of 5 units, the instance is
-- provisioned, and the target capacity is exceeded by 3 units. You can use
-- InstanceFleet$ProvisionedSpotCapacity to determine the Spot capacity
-- units that have been provisioned for the instance fleet.
--
-- If not specified or set to 0, only On-Demand Instances are provisioned
-- for the instance fleet. At least one of @TargetSpotCapacity@ and
-- @TargetOnDemandCapacity@ should be greater than 0. For a master instance
-- fleet, only one of @TargetSpotCapacity@ and @TargetOnDemandCapacity@ can
-- be specified, and its value must be 1.
instanceFleet_targetSpotCapacity :: Lens.Lens' InstanceFleet (Prelude.Maybe Prelude.Natural)
instanceFleet_targetSpotCapacity = Lens.lens (\InstanceFleet' {targetSpotCapacity} -> targetSpotCapacity) (\s@InstanceFleet' {} a -> s {targetSpotCapacity = a} :: InstanceFleet)

instance Data.FromJSON InstanceFleet where
  parseJSON =
    Data.withObject
      "InstanceFleet"
      ( \x ->
          InstanceFleet'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InstanceFleetType")
            Prelude.<*> ( x Data..:? "InstanceTypeSpecifications"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LaunchSpecifications")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ProvisionedOnDemandCapacity")
            Prelude.<*> (x Data..:? "ProvisionedSpotCapacity")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TargetOnDemandCapacity")
            Prelude.<*> (x Data..:? "TargetSpotCapacity")
      )

instance Prelude.Hashable InstanceFleet where
  hashWithSalt _salt InstanceFleet' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` instanceFleetType
      `Prelude.hashWithSalt` instanceTypeSpecifications
      `Prelude.hashWithSalt` launchSpecifications
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` provisionedOnDemandCapacity
      `Prelude.hashWithSalt` provisionedSpotCapacity
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetOnDemandCapacity
      `Prelude.hashWithSalt` targetSpotCapacity

instance Prelude.NFData InstanceFleet where
  rnf InstanceFleet' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf instanceFleetType
      `Prelude.seq` Prelude.rnf instanceTypeSpecifications
      `Prelude.seq` Prelude.rnf launchSpecifications
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf provisionedOnDemandCapacity
      `Prelude.seq` Prelude.rnf provisionedSpotCapacity
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetOnDemandCapacity
      `Prelude.seq` Prelude.rnf targetSpotCapacity
