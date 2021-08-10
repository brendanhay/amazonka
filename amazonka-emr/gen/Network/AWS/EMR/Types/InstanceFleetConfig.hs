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
-- Module      : Network.AWS.EMR.Types.InstanceFleetConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
import Network.AWS.EMR.Types.InstanceFleetType
import Network.AWS.EMR.Types.InstanceTypeConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration that defines an instance fleet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceFleetConfig' smart constructor.
data InstanceFleetConfig = InstanceFleetConfig'
  { -- | The target capacity of On-Demand units for the instance fleet, which
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
    -- | The friendly name of the instance fleet.
    name :: Prelude.Maybe Prelude.Text,
    -- | The launch specification for the instance fleet.
    launchSpecifications :: Prelude.Maybe InstanceFleetProvisioningSpecifications,
    -- | The instance type configurations that define the EC2 instances in the
    -- instance fleet.
    instanceTypeConfigs :: Prelude.Maybe [InstanceTypeConfig],
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
-- 'name', 'instanceFleetConfig_name' - The friendly name of the instance fleet.
--
-- 'launchSpecifications', 'instanceFleetConfig_launchSpecifications' - The launch specification for the instance fleet.
--
-- 'instanceTypeConfigs', 'instanceFleetConfig_instanceTypeConfigs' - The instance type configurations that define the EC2 instances in the
-- instance fleet.
--
-- 'instanceFleetType', 'instanceFleetConfig_instanceFleetType' - The node type that the instance fleet hosts. Valid values are MASTER,
-- CORE, and TASK.
newInstanceFleetConfig ::
  -- | 'instanceFleetType'
  InstanceFleetType ->
  InstanceFleetConfig
newInstanceFleetConfig pInstanceFleetType_ =
  InstanceFleetConfig'
    { targetOnDemandCapacity =
        Prelude.Nothing,
      targetSpotCapacity = Prelude.Nothing,
      name = Prelude.Nothing,
      launchSpecifications = Prelude.Nothing,
      instanceTypeConfigs = Prelude.Nothing,
      instanceFleetType = pInstanceFleetType_
    }

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

-- | The friendly name of the instance fleet.
instanceFleetConfig_name :: Lens.Lens' InstanceFleetConfig (Prelude.Maybe Prelude.Text)
instanceFleetConfig_name = Lens.lens (\InstanceFleetConfig' {name} -> name) (\s@InstanceFleetConfig' {} a -> s {name = a} :: InstanceFleetConfig)

-- | The launch specification for the instance fleet.
instanceFleetConfig_launchSpecifications :: Lens.Lens' InstanceFleetConfig (Prelude.Maybe InstanceFleetProvisioningSpecifications)
instanceFleetConfig_launchSpecifications = Lens.lens (\InstanceFleetConfig' {launchSpecifications} -> launchSpecifications) (\s@InstanceFleetConfig' {} a -> s {launchSpecifications = a} :: InstanceFleetConfig)

-- | The instance type configurations that define the EC2 instances in the
-- instance fleet.
instanceFleetConfig_instanceTypeConfigs :: Lens.Lens' InstanceFleetConfig (Prelude.Maybe [InstanceTypeConfig])
instanceFleetConfig_instanceTypeConfigs = Lens.lens (\InstanceFleetConfig' {instanceTypeConfigs} -> instanceTypeConfigs) (\s@InstanceFleetConfig' {} a -> s {instanceTypeConfigs = a} :: InstanceFleetConfig) Prelude.. Lens.mapping Lens._Coerce

-- | The node type that the instance fleet hosts. Valid values are MASTER,
-- CORE, and TASK.
instanceFleetConfig_instanceFleetType :: Lens.Lens' InstanceFleetConfig InstanceFleetType
instanceFleetConfig_instanceFleetType = Lens.lens (\InstanceFleetConfig' {instanceFleetType} -> instanceFleetType) (\s@InstanceFleetConfig' {} a -> s {instanceFleetType = a} :: InstanceFleetConfig)

instance Prelude.Hashable InstanceFleetConfig

instance Prelude.NFData InstanceFleetConfig

instance Core.ToJSON InstanceFleetConfig where
  toJSON InstanceFleetConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TargetOnDemandCapacity" Core..=)
              Prelude.<$> targetOnDemandCapacity,
            ("TargetSpotCapacity" Core..=)
              Prelude.<$> targetSpotCapacity,
            ("Name" Core..=) Prelude.<$> name,
            ("LaunchSpecifications" Core..=)
              Prelude.<$> launchSpecifications,
            ("InstanceTypeConfigs" Core..=)
              Prelude.<$> instanceTypeConfigs,
            Prelude.Just
              ("InstanceFleetType" Core..= instanceFleetType)
          ]
      )
