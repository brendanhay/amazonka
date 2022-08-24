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
-- Module      : Amazonka.AutoScaling.Types.LaunchTemplateOverrides
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.LaunchTemplateOverrides where

import Amazonka.AutoScaling.Types.InstanceRequirements
import Amazonka.AutoScaling.Types.LaunchTemplateSpecification
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an override for a launch template. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-configuring-overrides.html Configuring overrides>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newLaunchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { -- | The instance requirements. When you specify instance requirements,
    -- Amazon EC2 Auto Scaling finds instance types that satisfy your
    -- requirements, and then uses your On-Demand and Spot allocation
    -- strategies to launch instances from these instance types, in the same
    -- way as when you specify a list of specific instance types.
    instanceRequirements :: Prelude.Maybe InstanceRequirements,
    -- | Provides a launch template for the specified instance type or instance
    -- requirements. For example, some instance types might require a launch
    -- template with a different AMI. If not provided, Amazon EC2 Auto Scaling
    -- uses the launch template that\'s defined for your mixed instances
    -- policy. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-launch-template-overrides.html Specifying a different launch template for an instance type>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    launchTemplateSpecification :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The instance type, such as @m3.xlarge@. You must use an instance type
    -- that is supported in your requested Region and Availability Zones. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The number of capacity units provided by the instance type specified in
    -- @InstanceType@ in terms of virtual CPUs, memory, storage, throughput, or
    -- other relative performance characteristic. When a Spot or On-Demand
    -- Instance is launched, the capacity units count toward the desired
    -- capacity. Amazon EC2 Auto Scaling launches instances until the desired
    -- capacity is totally fulfilled, even if this results in an overage. For
    -- example, if there are two units remaining to fulfill capacity, and
    -- Amazon EC2 Auto Scaling can only launch an instance with a
    -- @WeightedCapacity@ of five units, the instance is launched, and the
    -- desired capacity is exceeded by three units. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-instance-weighting.html Configuring instance weighting for Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/. Value must be in the range
    -- of 1–999.
    weightedCapacity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceRequirements', 'launchTemplateOverrides_instanceRequirements' - The instance requirements. When you specify instance requirements,
-- Amazon EC2 Auto Scaling finds instance types that satisfy your
-- requirements, and then uses your On-Demand and Spot allocation
-- strategies to launch instances from these instance types, in the same
-- way as when you specify a list of specific instance types.
--
-- 'launchTemplateSpecification', 'launchTemplateOverrides_launchTemplateSpecification' - Provides a launch template for the specified instance type or instance
-- requirements. For example, some instance types might require a launch
-- template with a different AMI. If not provided, Amazon EC2 Auto Scaling
-- uses the launch template that\'s defined for your mixed instances
-- policy. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-launch-template-overrides.html Specifying a different launch template for an instance type>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'instanceType', 'launchTemplateOverrides_instanceType' - The instance type, such as @m3.xlarge@. You must use an instance type
-- that is supported in your requested Region and Availability Zones. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'weightedCapacity', 'launchTemplateOverrides_weightedCapacity' - The number of capacity units provided by the instance type specified in
-- @InstanceType@ in terms of virtual CPUs, memory, storage, throughput, or
-- other relative performance characteristic. When a Spot or On-Demand
-- Instance is launched, the capacity units count toward the desired
-- capacity. Amazon EC2 Auto Scaling launches instances until the desired
-- capacity is totally fulfilled, even if this results in an overage. For
-- example, if there are two units remaining to fulfill capacity, and
-- Amazon EC2 Auto Scaling can only launch an instance with a
-- @WeightedCapacity@ of five units, the instance is launched, and the
-- desired capacity is exceeded by three units. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-instance-weighting.html Configuring instance weighting for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/. Value must be in the range
-- of 1–999.
newLaunchTemplateOverrides ::
  LaunchTemplateOverrides
newLaunchTemplateOverrides =
  LaunchTemplateOverrides'
    { instanceRequirements =
        Prelude.Nothing,
      launchTemplateSpecification = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing
    }

-- | The instance requirements. When you specify instance requirements,
-- Amazon EC2 Auto Scaling finds instance types that satisfy your
-- requirements, and then uses your On-Demand and Spot allocation
-- strategies to launch instances from these instance types, in the same
-- way as when you specify a list of specific instance types.
launchTemplateOverrides_instanceRequirements :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe InstanceRequirements)
launchTemplateOverrides_instanceRequirements = Lens.lens (\LaunchTemplateOverrides' {instanceRequirements} -> instanceRequirements) (\s@LaunchTemplateOverrides' {} a -> s {instanceRequirements = a} :: LaunchTemplateOverrides)

-- | Provides a launch template for the specified instance type or instance
-- requirements. For example, some instance types might require a launch
-- template with a different AMI. If not provided, Amazon EC2 Auto Scaling
-- uses the launch template that\'s defined for your mixed instances
-- policy. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-launch-template-overrides.html Specifying a different launch template for an instance type>
-- in the /Amazon EC2 Auto Scaling User Guide/.
launchTemplateOverrides_launchTemplateSpecification :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe LaunchTemplateSpecification)
launchTemplateOverrides_launchTemplateSpecification = Lens.lens (\LaunchTemplateOverrides' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@LaunchTemplateOverrides' {} a -> s {launchTemplateSpecification = a} :: LaunchTemplateOverrides)

-- | The instance type, such as @m3.xlarge@. You must use an instance type
-- that is supported in your requested Region and Availability Zones. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
launchTemplateOverrides_instanceType :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
launchTemplateOverrides_instanceType = Lens.lens (\LaunchTemplateOverrides' {instanceType} -> instanceType) (\s@LaunchTemplateOverrides' {} a -> s {instanceType = a} :: LaunchTemplateOverrides)

-- | The number of capacity units provided by the instance type specified in
-- @InstanceType@ in terms of virtual CPUs, memory, storage, throughput, or
-- other relative performance characteristic. When a Spot or On-Demand
-- Instance is launched, the capacity units count toward the desired
-- capacity. Amazon EC2 Auto Scaling launches instances until the desired
-- capacity is totally fulfilled, even if this results in an overage. For
-- example, if there are two units remaining to fulfill capacity, and
-- Amazon EC2 Auto Scaling can only launch an instance with a
-- @WeightedCapacity@ of five units, the instance is launched, and the
-- desired capacity is exceeded by three units. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-instance-weighting.html Configuring instance weighting for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/. Value must be in the range
-- of 1–999.
launchTemplateOverrides_weightedCapacity :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
launchTemplateOverrides_weightedCapacity = Lens.lens (\LaunchTemplateOverrides' {weightedCapacity} -> weightedCapacity) (\s@LaunchTemplateOverrides' {} a -> s {weightedCapacity = a} :: LaunchTemplateOverrides)

instance Core.FromXML LaunchTemplateOverrides where
  parseXML x =
    LaunchTemplateOverrides'
      Prelude.<$> (x Core..@? "InstanceRequirements")
      Prelude.<*> (x Core..@? "LaunchTemplateSpecification")
      Prelude.<*> (x Core..@? "InstanceType")
      Prelude.<*> (x Core..@? "WeightedCapacity")

instance Prelude.Hashable LaunchTemplateOverrides where
  hashWithSalt _salt LaunchTemplateOverrides' {..} =
    _salt `Prelude.hashWithSalt` instanceRequirements
      `Prelude.hashWithSalt` launchTemplateSpecification
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` weightedCapacity

instance Prelude.NFData LaunchTemplateOverrides where
  rnf LaunchTemplateOverrides' {..} =
    Prelude.rnf instanceRequirements
      `Prelude.seq` Prelude.rnf launchTemplateSpecification
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf weightedCapacity

instance Core.ToQuery LaunchTemplateOverrides where
  toQuery LaunchTemplateOverrides' {..} =
    Prelude.mconcat
      [ "InstanceRequirements" Core.=: instanceRequirements,
        "LaunchTemplateSpecification"
          Core.=: launchTemplateSpecification,
        "InstanceType" Core.=: instanceType,
        "WeightedCapacity" Core.=: weightedCapacity
      ]
