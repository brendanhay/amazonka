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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.LaunchTemplateOverrides where

import Amazonka.AutoScaling.Types.InstanceRequirements
import Amazonka.AutoScaling.Types.LaunchTemplateSpecification
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Use this structure to let Amazon EC2 Auto Scaling do the following when
-- the Auto Scaling group has a mixed instances policy:
--
-- -   Override the instance type that is specified in the launch template.
--
-- -   Use multiple instance types.
--
-- Specify the instance types that you want, or define your instance
-- requirements instead and let Amazon EC2 Auto Scaling provision the
-- available instance types that meet your requirements. This can provide
-- Amazon EC2 Auto Scaling with a larger selection of instance types to
-- choose from when fulfilling Spot and On-Demand capacities. You can view
-- which instance types are matched before you apply the instance
-- requirements to your Auto Scaling group.
--
-- After you define your instance requirements, you don\'t have to keep
-- updating these settings to get new EC2 instance types automatically.
-- Amazon EC2 Auto Scaling uses the instance requirements of the Auto
-- Scaling group to determine whether a new EC2 instance type can be used.
--
-- /See:/ 'newLaunchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { -- | The instance requirements. Amazon EC2 Auto Scaling uses your specified
    -- requirements to identify instance types. Then, it uses your On-Demand
    -- and Spot allocation strategies to launch instances from these instance
    -- types.
    --
    -- You can specify up to four separate sets of instance requirements per
    -- Auto Scaling group. This is useful for provisioning instances from
    -- different Amazon Machine Images (AMIs) in the same Auto Scaling group.
    -- To do this, create the AMIs and create a new launch template for each
    -- AMI. Then, create a compatible set of instance requirements for each
    -- launch template.
    --
    -- If you specify @InstanceRequirements@, you can\'t specify
    -- @InstanceType@.
    instanceRequirements :: Prelude.Maybe InstanceRequirements,
    -- | Provides a launch template for the specified instance type or set of
    -- instance requirements. For example, some instance types might require a
    -- launch template with a different AMI. If not provided, Amazon EC2 Auto
    -- Scaling uses the launch template that\'s specified in the
    -- @LaunchTemplate@ definition. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-launch-template-overrides.html Specifying a different launch template for an instance type>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- You can specify up to 20 launch templates per Auto Scaling group. The
    -- launch templates specified in the overrides and in the @LaunchTemplate@
    -- definition count towards this limit.
    launchTemplateSpecification :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The instance type, such as @m3.xlarge@. You must specify an instance
    -- type that is supported in your requested Region and Availability Zones.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- You can specify up to 40 instance types per Auto Scaling group.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | If you provide a list of instance types to use, you can specify the
    -- number of capacity units provided by each instance type in terms of
    -- virtual CPUs, memory, storage, throughput, or other relative performance
    -- characteristic. When a Spot or On-Demand Instance is launched, the
    -- capacity units count toward the desired capacity. Amazon EC2 Auto
    -- Scaling launches instances until the desired capacity is totally
    -- fulfilled, even if this results in an overage. For example, if there are
    -- two units remaining to fulfill capacity, and Amazon EC2 Auto Scaling can
    -- only launch an instance with a @WeightedCapacity@ of five units, the
    -- instance is launched, and the desired capacity is exceeded by three
    -- units. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-instance-weighting.html Configuring instance weighting for Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/. Value must be in the range
    -- of 1–999.
    --
    -- If you specify a value for @WeightedCapacity@ for one instance type, you
    -- must specify a value for @WeightedCapacity@ for all of them.
    --
    -- Every Auto Scaling group has three size parameters (@DesiredCapacity@,
    -- @MaxSize@, and @MinSize@). Usually, you set these sizes based on a
    -- specific number of instances. However, if you configure a mixed
    -- instances policy that defines weights for the instance types, you must
    -- specify these sizes with the same units that you use for weighting
    -- instances.
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
-- 'instanceRequirements', 'launchTemplateOverrides_instanceRequirements' - The instance requirements. Amazon EC2 Auto Scaling uses your specified
-- requirements to identify instance types. Then, it uses your On-Demand
-- and Spot allocation strategies to launch instances from these instance
-- types.
--
-- You can specify up to four separate sets of instance requirements per
-- Auto Scaling group. This is useful for provisioning instances from
-- different Amazon Machine Images (AMIs) in the same Auto Scaling group.
-- To do this, create the AMIs and create a new launch template for each
-- AMI. Then, create a compatible set of instance requirements for each
-- launch template.
--
-- If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
--
-- 'launchTemplateSpecification', 'launchTemplateOverrides_launchTemplateSpecification' - Provides a launch template for the specified instance type or set of
-- instance requirements. For example, some instance types might require a
-- launch template with a different AMI. If not provided, Amazon EC2 Auto
-- Scaling uses the launch template that\'s specified in the
-- @LaunchTemplate@ definition. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-launch-template-overrides.html Specifying a different launch template for an instance type>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- You can specify up to 20 launch templates per Auto Scaling group. The
-- launch templates specified in the overrides and in the @LaunchTemplate@
-- definition count towards this limit.
--
-- 'instanceType', 'launchTemplateOverrides_instanceType' - The instance type, such as @m3.xlarge@. You must specify an instance
-- type that is supported in your requested Region and Availability Zones.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You can specify up to 40 instance types per Auto Scaling group.
--
-- 'weightedCapacity', 'launchTemplateOverrides_weightedCapacity' - If you provide a list of instance types to use, you can specify the
-- number of capacity units provided by each instance type in terms of
-- virtual CPUs, memory, storage, throughput, or other relative performance
-- characteristic. When a Spot or On-Demand Instance is launched, the
-- capacity units count toward the desired capacity. Amazon EC2 Auto
-- Scaling launches instances until the desired capacity is totally
-- fulfilled, even if this results in an overage. For example, if there are
-- two units remaining to fulfill capacity, and Amazon EC2 Auto Scaling can
-- only launch an instance with a @WeightedCapacity@ of five units, the
-- instance is launched, and the desired capacity is exceeded by three
-- units. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-instance-weighting.html Configuring instance weighting for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/. Value must be in the range
-- of 1–999.
--
-- If you specify a value for @WeightedCapacity@ for one instance type, you
-- must specify a value for @WeightedCapacity@ for all of them.
--
-- Every Auto Scaling group has three size parameters (@DesiredCapacity@,
-- @MaxSize@, and @MinSize@). Usually, you set these sizes based on a
-- specific number of instances. However, if you configure a mixed
-- instances policy that defines weights for the instance types, you must
-- specify these sizes with the same units that you use for weighting
-- instances.
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

-- | The instance requirements. Amazon EC2 Auto Scaling uses your specified
-- requirements to identify instance types. Then, it uses your On-Demand
-- and Spot allocation strategies to launch instances from these instance
-- types.
--
-- You can specify up to four separate sets of instance requirements per
-- Auto Scaling group. This is useful for provisioning instances from
-- different Amazon Machine Images (AMIs) in the same Auto Scaling group.
-- To do this, create the AMIs and create a new launch template for each
-- AMI. Then, create a compatible set of instance requirements for each
-- launch template.
--
-- If you specify @InstanceRequirements@, you can\'t specify
-- @InstanceType@.
launchTemplateOverrides_instanceRequirements :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe InstanceRequirements)
launchTemplateOverrides_instanceRequirements = Lens.lens (\LaunchTemplateOverrides' {instanceRequirements} -> instanceRequirements) (\s@LaunchTemplateOverrides' {} a -> s {instanceRequirements = a} :: LaunchTemplateOverrides)

-- | Provides a launch template for the specified instance type or set of
-- instance requirements. For example, some instance types might require a
-- launch template with a different AMI. If not provided, Amazon EC2 Auto
-- Scaling uses the launch template that\'s specified in the
-- @LaunchTemplate@ definition. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-launch-template-overrides.html Specifying a different launch template for an instance type>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- You can specify up to 20 launch templates per Auto Scaling group. The
-- launch templates specified in the overrides and in the @LaunchTemplate@
-- definition count towards this limit.
launchTemplateOverrides_launchTemplateSpecification :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe LaunchTemplateSpecification)
launchTemplateOverrides_launchTemplateSpecification = Lens.lens (\LaunchTemplateOverrides' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@LaunchTemplateOverrides' {} a -> s {launchTemplateSpecification = a} :: LaunchTemplateOverrides)

-- | The instance type, such as @m3.xlarge@. You must specify an instance
-- type that is supported in your requested Region and Availability Zones.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You can specify up to 40 instance types per Auto Scaling group.
launchTemplateOverrides_instanceType :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
launchTemplateOverrides_instanceType = Lens.lens (\LaunchTemplateOverrides' {instanceType} -> instanceType) (\s@LaunchTemplateOverrides' {} a -> s {instanceType = a} :: LaunchTemplateOverrides)

-- | If you provide a list of instance types to use, you can specify the
-- number of capacity units provided by each instance type in terms of
-- virtual CPUs, memory, storage, throughput, or other relative performance
-- characteristic. When a Spot or On-Demand Instance is launched, the
-- capacity units count toward the desired capacity. Amazon EC2 Auto
-- Scaling launches instances until the desired capacity is totally
-- fulfilled, even if this results in an overage. For example, if there are
-- two units remaining to fulfill capacity, and Amazon EC2 Auto Scaling can
-- only launch an instance with a @WeightedCapacity@ of five units, the
-- instance is launched, and the desired capacity is exceeded by three
-- units. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups-instance-weighting.html Configuring instance weighting for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/. Value must be in the range
-- of 1–999.
--
-- If you specify a value for @WeightedCapacity@ for one instance type, you
-- must specify a value for @WeightedCapacity@ for all of them.
--
-- Every Auto Scaling group has three size parameters (@DesiredCapacity@,
-- @MaxSize@, and @MinSize@). Usually, you set these sizes based on a
-- specific number of instances. However, if you configure a mixed
-- instances policy that defines weights for the instance types, you must
-- specify these sizes with the same units that you use for weighting
-- instances.
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
