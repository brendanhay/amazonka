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
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LaunchTemplateOverrides where

import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | The instance type, such as @m3.xlarge@. You must use an instance type
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
    launchTemplateSpecification :: Prelude.Maybe LaunchTemplateSpecification,
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
    weightedCapacity :: Prelude.Maybe Prelude.Text
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
newLaunchTemplateOverrides ::
  LaunchTemplateOverrides
newLaunchTemplateOverrides =
  LaunchTemplateOverrides'
    { instanceType =
        Prelude.Nothing,
      launchTemplateSpecification = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing
    }

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

instance Prelude.FromXML LaunchTemplateOverrides where
  parseXML x =
    LaunchTemplateOverrides'
      Prelude.<$> (x Prelude..@? "InstanceType")
      Prelude.<*> (x Prelude..@? "LaunchTemplateSpecification")
      Prelude.<*> (x Prelude..@? "WeightedCapacity")

instance Prelude.Hashable LaunchTemplateOverrides

instance Prelude.NFData LaunchTemplateOverrides

instance Prelude.ToQuery LaunchTemplateOverrides where
  toQuery LaunchTemplateOverrides' {..} =
    Prelude.mconcat
      [ "InstanceType" Prelude.=: instanceType,
        "LaunchTemplateSpecification"
          Prelude.=: launchTemplateSpecification,
        "WeightedCapacity" Prelude.=: weightedCapacity
      ]
