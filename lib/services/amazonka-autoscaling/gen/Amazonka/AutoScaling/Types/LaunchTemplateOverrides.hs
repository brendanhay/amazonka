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

import Amazonka.AutoScaling.Types.LaunchTemplateSpecification
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an override for a launch template. The maximum number of
-- instance types that can be associated with an Auto Scaling group is 40.
-- The maximum number of distinct launch templates you can define for an
-- Auto Scaling group is 20. For more information about configuring
-- overrides, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-override-options.html Configuring overrides>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newLaunchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { -- | The number of capacity units provided by the specified instance type in
    -- terms of virtual CPUs, memory, storage, throughput, or other relative
    -- performance characteristic. When a Spot or On-Demand Instance is
    -- provisioned, the capacity units count toward the desired capacity.
    -- Amazon EC2 Auto Scaling provisions instances until the desired capacity
    -- is totally fulfilled, even if this results in an overage. For example,
    -- if there are 2 units remaining to fulfill capacity, and Amazon EC2 Auto
    -- Scaling can only provision an instance with a @WeightedCapacity@ of 5
    -- units, the instance is provisioned, and the desired capacity is exceeded
    -- by 3 units. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance weighting for Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/. Value must be in the range
    -- of 1 to 999.
    weightedCapacity :: Prelude.Maybe Prelude.Text,
    -- | The instance type, such as @m3.xlarge@. You must use an instance type
    -- that is supported in your requested Region and Availability Zones. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Provides the launch template to be used when launching the instance
    -- type. For example, some instance types might require a launch template
    -- with a different AMI. If not provided, Amazon EC2 Auto Scaling uses the
    -- launch template that\'s defined for your mixed instances policy. For
    -- more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-template-overrides.html Specifying a different launch template for an instance type>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    launchTemplateSpecification :: Prelude.Maybe LaunchTemplateSpecification
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
-- 'weightedCapacity', 'launchTemplateOverrides_weightedCapacity' - The number of capacity units provided by the specified instance type in
-- terms of virtual CPUs, memory, storage, throughput, or other relative
-- performance characteristic. When a Spot or On-Demand Instance is
-- provisioned, the capacity units count toward the desired capacity.
-- Amazon EC2 Auto Scaling provisions instances until the desired capacity
-- is totally fulfilled, even if this results in an overage. For example,
-- if there are 2 units remaining to fulfill capacity, and Amazon EC2 Auto
-- Scaling can only provision an instance with a @WeightedCapacity@ of 5
-- units, the instance is provisioned, and the desired capacity is exceeded
-- by 3 units. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance weighting for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/. Value must be in the range
-- of 1 to 999.
--
-- 'instanceType', 'launchTemplateOverrides_instanceType' - The instance type, such as @m3.xlarge@. You must use an instance type
-- that is supported in your requested Region and Availability Zones. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'launchTemplateSpecification', 'launchTemplateOverrides_launchTemplateSpecification' - Provides the launch template to be used when launching the instance
-- type. For example, some instance types might require a launch template
-- with a different AMI. If not provided, Amazon EC2 Auto Scaling uses the
-- launch template that\'s defined for your mixed instances policy. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-template-overrides.html Specifying a different launch template for an instance type>
-- in the /Amazon EC2 Auto Scaling User Guide/.
newLaunchTemplateOverrides ::
  LaunchTemplateOverrides
newLaunchTemplateOverrides =
  LaunchTemplateOverrides'
    { weightedCapacity =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      launchTemplateSpecification = Prelude.Nothing
    }

-- | The number of capacity units provided by the specified instance type in
-- terms of virtual CPUs, memory, storage, throughput, or other relative
-- performance characteristic. When a Spot or On-Demand Instance is
-- provisioned, the capacity units count toward the desired capacity.
-- Amazon EC2 Auto Scaling provisions instances until the desired capacity
-- is totally fulfilled, even if this results in an overage. For example,
-- if there are 2 units remaining to fulfill capacity, and Amazon EC2 Auto
-- Scaling can only provision an instance with a @WeightedCapacity@ of 5
-- units, the instance is provisioned, and the desired capacity is exceeded
-- by 3 units. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance weighting for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/. Value must be in the range
-- of 1 to 999.
launchTemplateOverrides_weightedCapacity :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
launchTemplateOverrides_weightedCapacity = Lens.lens (\LaunchTemplateOverrides' {weightedCapacity} -> weightedCapacity) (\s@LaunchTemplateOverrides' {} a -> s {weightedCapacity = a} :: LaunchTemplateOverrides)

-- | The instance type, such as @m3.xlarge@. You must use an instance type
-- that is supported in your requested Region and Availability Zones. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
launchTemplateOverrides_instanceType :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe Prelude.Text)
launchTemplateOverrides_instanceType = Lens.lens (\LaunchTemplateOverrides' {instanceType} -> instanceType) (\s@LaunchTemplateOverrides' {} a -> s {instanceType = a} :: LaunchTemplateOverrides)

-- | Provides the launch template to be used when launching the instance
-- type. For example, some instance types might require a launch template
-- with a different AMI. If not provided, Amazon EC2 Auto Scaling uses the
-- launch template that\'s defined for your mixed instances policy. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-template-overrides.html Specifying a different launch template for an instance type>
-- in the /Amazon EC2 Auto Scaling User Guide/.
launchTemplateOverrides_launchTemplateSpecification :: Lens.Lens' LaunchTemplateOverrides (Prelude.Maybe LaunchTemplateSpecification)
launchTemplateOverrides_launchTemplateSpecification = Lens.lens (\LaunchTemplateOverrides' {launchTemplateSpecification} -> launchTemplateSpecification) (\s@LaunchTemplateOverrides' {} a -> s {launchTemplateSpecification = a} :: LaunchTemplateOverrides)

instance Core.FromXML LaunchTemplateOverrides where
  parseXML x =
    LaunchTemplateOverrides'
      Prelude.<$> (x Core..@? "WeightedCapacity")
      Prelude.<*> (x Core..@? "InstanceType")
      Prelude.<*> (x Core..@? "LaunchTemplateSpecification")

instance Prelude.Hashable LaunchTemplateOverrides where
  hashWithSalt _salt LaunchTemplateOverrides' {..} =
    _salt `Prelude.hashWithSalt` weightedCapacity
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` launchTemplateSpecification

instance Prelude.NFData LaunchTemplateOverrides where
  rnf LaunchTemplateOverrides' {..} =
    Prelude.rnf weightedCapacity
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf launchTemplateSpecification

instance Core.ToQuery LaunchTemplateOverrides where
  toQuery LaunchTemplateOverrides' {..} =
    Prelude.mconcat
      [ "WeightedCapacity" Core.=: weightedCapacity,
        "InstanceType" Core.=: instanceType,
        "LaunchTemplateSpecification"
          Core.=: launchTemplateSpecification
      ]
