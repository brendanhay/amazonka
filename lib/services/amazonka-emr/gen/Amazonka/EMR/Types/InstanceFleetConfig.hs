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
-- Module      : Amazonka.EMR.Types.InstanceFleetConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceFleetConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.InstanceFleetProvisioningSpecifications
import Amazonka.EMR.Types.InstanceFleetType
import Amazonka.EMR.Types.InstanceTypeConfig
import qualified Amazonka.Prelude as Prelude

-- | The configuration that defines an instance fleet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceFleetConfig' smart constructor.
data InstanceFleetConfig = InstanceFleetConfig'
  { -- | The instance type configurations that define the EC2 instances in the
    -- instance fleet.
    instanceTypeConfigs :: Prelude.Maybe [InstanceTypeConfig],
    -- | The launch specification for the instance fleet.
    launchSpecifications :: Prelude.Maybe InstanceFleetProvisioningSpecifications,
    -- | The friendly name of the instance fleet.
    name :: Prelude.Maybe Prelude.Text,
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
    -- provisioned, and the target capacity is exceeded by 3 units.
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
    -- @WeightedCapacity@. When a Spot Instance is provisioned, the
    -- @WeightedCapacity@ units count toward the target capacity. Amazon EMR
    -- provisions instances until the target capacity is totally fulfilled,
    -- even if this results in an overage. For example, if there are 2 units
    -- remaining to fulfill capacity, and Amazon EMR can only provision an
    -- instance with a @WeightedCapacity@ of 5 units, the instance is
    -- provisioned, and the target capacity is exceeded by 3 units.
    --
    -- If not specified or set to 0, only On-Demand Instances are provisioned
    -- for the instance fleet. At least one of @TargetSpotCapacity@ and
    -- @TargetOnDemandCapacity@ should be greater than 0. For a master instance
    -- fleet, only one of @TargetSpotCapacity@ and @TargetOnDemandCapacity@ can
    -- be specified, and its value must be 1.
    targetSpotCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The node type that the instance fleet hosts. Valid values are MASTER,
    -- CORE, and TASK.
    instanceFleetType :: InstanceFleetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceFleetConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTypeConfigs', 'instanceFleetConfig_instanceTypeConfigs' - The instance type configurations that define the EC2 instances in the
-- instance fleet.
--
-- 'launchSpecifications', 'instanceFleetConfig_launchSpecifications' - The launch specification for the instance fleet.
--
-- 'name', 'instanceFleetConfig_name' - The friendly name of the instance fleet.
--
-- 'targetOnDemandCapacity', 'instanceFleetConfig_targetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet, which
-- determines how many On-Demand Instances to provision. When the instance
-- fleet launches, Amazon EMR tries to provision On-Demand Instances as
-- specified by InstanceTypeConfig. Each instance configuration has a
-- specified @WeightedCapacity@. When an On-Demand Instance is provisioned,
-- the @WeightedCapacity@ units count toward the target capacity. Amazon
-- EMR provisions instances until the target capacity is totally fulfilled,
-- even if this results in an overage. For example, if there are 2 units
-- remaining to fulfill capacity, and Amazon EMR can only provision an
-- instance with a @WeightedCapacity@ of 5 units, the instance is
-- provisioned, and the target capacity is exceeded by 3 units.
--
-- If not specified or set to 0, only Spot Instances are provisioned for
-- the instance fleet using @TargetSpotCapacity@. At least one of
-- @TargetSpotCapacity@ and @TargetOnDemandCapacity@ should be greater than
-- 0. For a master instance fleet, only one of @TargetSpotCapacity@ and
-- @TargetOnDemandCapacity@ can be specified, and its value must be 1.
--
-- 'targetSpotCapacity', 'instanceFleetConfig_targetSpotCapacity' - The target capacity of Spot units for the instance fleet, which
-- determines how many Spot Instances to provision. When the instance fleet
-- launches, Amazon EMR tries to provision Spot Instances as specified by
-- InstanceTypeConfig. Each instance configuration has a specified
-- @WeightedCapacity@. When a Spot Instance is provisioned, the
-- @WeightedCapacity@ units count toward the target capacity. Amazon EMR
-- provisions instances until the target capacity is totally fulfilled,
-- even if this results in an overage. For example, if there are 2 units
-- remaining to fulfill capacity, and Amazon EMR can only provision an
-- instance with a @WeightedCapacity@ of 5 units, the instance is
-- provisioned, and the target capacity is exceeded by 3 units.
--
-- If not specified or set to 0, only On-Demand Instances are provisioned
-- for the instance fleet. At least one of @TargetSpotCapacity@ and
-- @TargetOnDemandCapacity@ should be greater than 0. For a master instance
-- fleet, only one of @TargetSpotCapacity@ and @TargetOnDemandCapacity@ can
-- be specified, and its value must be 1.
--
-- 'instanceFleetType', 'instanceFleetConfig_instanceFleetType' - The node type that the instance fleet hosts. Valid values are MASTER,
-- CORE, and TASK.
newInstanceFleetConfig ::
  -- | 'instanceFleetType'
  InstanceFleetType ->
  InstanceFleetConfig
newInstanceFleetConfig pInstanceFleetType_ =
  InstanceFleetConfig'
    { instanceTypeConfigs =
        Prelude.Nothing,
      launchSpecifications = Prelude.Nothing,
      name = Prelude.Nothing,
      targetOnDemandCapacity = Prelude.Nothing,
      targetSpotCapacity = Prelude.Nothing,
      instanceFleetType = pInstanceFleetType_
    }

-- | The instance type configurations that define the EC2 instances in the
-- instance fleet.
instanceFleetConfig_instanceTypeConfigs :: Lens.Lens' InstanceFleetConfig (Prelude.Maybe [InstanceTypeConfig])
instanceFleetConfig_instanceTypeConfigs = Lens.lens (\InstanceFleetConfig' {instanceTypeConfigs} -> instanceTypeConfigs) (\s@InstanceFleetConfig' {} a -> s {instanceTypeConfigs = a} :: InstanceFleetConfig) Prelude.. Lens.mapping Lens.coerced

-- | The launch specification for the instance fleet.
instanceFleetConfig_launchSpecifications :: Lens.Lens' InstanceFleetConfig (Prelude.Maybe InstanceFleetProvisioningSpecifications)
instanceFleetConfig_launchSpecifications = Lens.lens (\InstanceFleetConfig' {launchSpecifications} -> launchSpecifications) (\s@InstanceFleetConfig' {} a -> s {launchSpecifications = a} :: InstanceFleetConfig)

-- | The friendly name of the instance fleet.
instanceFleetConfig_name :: Lens.Lens' InstanceFleetConfig (Prelude.Maybe Prelude.Text)
instanceFleetConfig_name = Lens.lens (\InstanceFleetConfig' {name} -> name) (\s@InstanceFleetConfig' {} a -> s {name = a} :: InstanceFleetConfig)

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
-- provisioned, and the target capacity is exceeded by 3 units.
--
-- If not specified or set to 0, only Spot Instances are provisioned for
-- the instance fleet using @TargetSpotCapacity@. At least one of
-- @TargetSpotCapacity@ and @TargetOnDemandCapacity@ should be greater than
-- 0. For a master instance fleet, only one of @TargetSpotCapacity@ and
-- @TargetOnDemandCapacity@ can be specified, and its value must be 1.
instanceFleetConfig_targetOnDemandCapacity :: Lens.Lens' InstanceFleetConfig (Prelude.Maybe Prelude.Natural)
instanceFleetConfig_targetOnDemandCapacity = Lens.lens (\InstanceFleetConfig' {targetOnDemandCapacity} -> targetOnDemandCapacity) (\s@InstanceFleetConfig' {} a -> s {targetOnDemandCapacity = a} :: InstanceFleetConfig)

-- | The target capacity of Spot units for the instance fleet, which
-- determines how many Spot Instances to provision. When the instance fleet
-- launches, Amazon EMR tries to provision Spot Instances as specified by
-- InstanceTypeConfig. Each instance configuration has a specified
-- @WeightedCapacity@. When a Spot Instance is provisioned, the
-- @WeightedCapacity@ units count toward the target capacity. Amazon EMR
-- provisions instances until the target capacity is totally fulfilled,
-- even if this results in an overage. For example, if there are 2 units
-- remaining to fulfill capacity, and Amazon EMR can only provision an
-- instance with a @WeightedCapacity@ of 5 units, the instance is
-- provisioned, and the target capacity is exceeded by 3 units.
--
-- If not specified or set to 0, only On-Demand Instances are provisioned
-- for the instance fleet. At least one of @TargetSpotCapacity@ and
-- @TargetOnDemandCapacity@ should be greater than 0. For a master instance
-- fleet, only one of @TargetSpotCapacity@ and @TargetOnDemandCapacity@ can
-- be specified, and its value must be 1.
instanceFleetConfig_targetSpotCapacity :: Lens.Lens' InstanceFleetConfig (Prelude.Maybe Prelude.Natural)
instanceFleetConfig_targetSpotCapacity = Lens.lens (\InstanceFleetConfig' {targetSpotCapacity} -> targetSpotCapacity) (\s@InstanceFleetConfig' {} a -> s {targetSpotCapacity = a} :: InstanceFleetConfig)

-- | The node type that the instance fleet hosts. Valid values are MASTER,
-- CORE, and TASK.
instanceFleetConfig_instanceFleetType :: Lens.Lens' InstanceFleetConfig InstanceFleetType
instanceFleetConfig_instanceFleetType = Lens.lens (\InstanceFleetConfig' {instanceFleetType} -> instanceFleetType) (\s@InstanceFleetConfig' {} a -> s {instanceFleetType = a} :: InstanceFleetConfig)

instance Prelude.Hashable InstanceFleetConfig where
  hashWithSalt _salt InstanceFleetConfig' {..} =
    _salt
      `Prelude.hashWithSalt` instanceTypeConfigs
      `Prelude.hashWithSalt` launchSpecifications
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` targetOnDemandCapacity
      `Prelude.hashWithSalt` targetSpotCapacity
      `Prelude.hashWithSalt` instanceFleetType

instance Prelude.NFData InstanceFleetConfig where
  rnf InstanceFleetConfig' {..} =
    Prelude.rnf instanceTypeConfigs
      `Prelude.seq` Prelude.rnf launchSpecifications
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf targetOnDemandCapacity
      `Prelude.seq` Prelude.rnf targetSpotCapacity
      `Prelude.seq` Prelude.rnf instanceFleetType

instance Data.ToJSON InstanceFleetConfig where
  toJSON InstanceFleetConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceTypeConfigs" Data..=)
              Prelude.<$> instanceTypeConfigs,
            ("LaunchSpecifications" Data..=)
              Prelude.<$> launchSpecifications,
            ("Name" Data..=) Prelude.<$> name,
            ("TargetOnDemandCapacity" Data..=)
              Prelude.<$> targetOnDemandCapacity,
            ("TargetSpotCapacity" Data..=)
              Prelude.<$> targetSpotCapacity,
            Prelude.Just
              ("InstanceFleetType" Data..= instanceFleetType)
          ]
      )
