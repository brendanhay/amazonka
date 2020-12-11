-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
  ( LaunchTemplateOverrides (..),

    -- * Smart constructor
    mkLaunchTemplateOverrides,

    -- * Lenses
    ltoWeightedCapacity,
    ltoInstanceType,
    ltoLaunchTemplateSpecification,
  )
where

import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an override for a launch template. The maximum number of instance types that can be associated with an Auto Scaling group is 20. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-override-options.html Configuring overrides> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkLaunchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { weightedCapacity ::
      Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    launchTemplateSpecification ::
      Lude.Maybe LaunchTemplateSpecification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateOverrides' with the minimum fields required to make a request.
--
-- * 'instanceType' - The instance type, such as @m3.xlarge@ . You must use an instance type that is supported in your requested Region and Availability Zones. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'launchTemplateSpecification' - Provides the launch template to be used when launching the instance type. For example, some instance types might require a launch template with a different AMI. If not provided, Amazon EC2 Auto Scaling uses the launch template that's defined for your mixed instances policy. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-template-overrides.html Specifying a different launch template for an instance type> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'weightedCapacity' - The number of capacity units provided by the specified instance type in terms of virtual CPUs, memory, storage, throughput, or other relative performance characteristic. When a Spot or On-Demand Instance is provisioned, the capacity units count toward the desired capacity. Amazon EC2 Auto Scaling provisions instances until the desired capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EC2 Auto Scaling can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the desired capacity is exceeded by 3 units. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance weighting for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ . Value must be in the range of 1 to 999.
mkLaunchTemplateOverrides ::
  LaunchTemplateOverrides
mkLaunchTemplateOverrides =
  LaunchTemplateOverrides'
    { weightedCapacity = Lude.Nothing,
      instanceType = Lude.Nothing,
      launchTemplateSpecification = Lude.Nothing
    }

-- | The number of capacity units provided by the specified instance type in terms of virtual CPUs, memory, storage, throughput, or other relative performance characteristic. When a Spot or On-Demand Instance is provisioned, the capacity units count toward the desired capacity. Amazon EC2 Auto Scaling provisions instances until the desired capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EC2 Auto Scaling can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the desired capacity is exceeded by 3 units. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance weighting for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ . Value must be in the range of 1 to 999.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoWeightedCapacity :: Lens.Lens' LaunchTemplateOverrides (Lude.Maybe Lude.Text)
ltoWeightedCapacity = Lens.lens (weightedCapacity :: LaunchTemplateOverrides -> Lude.Maybe Lude.Text) (\s a -> s {weightedCapacity = a} :: LaunchTemplateOverrides)
{-# DEPRECATED ltoWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

-- | The instance type, such as @m3.xlarge@ . You must use an instance type that is supported in your requested Region and Availability Zones. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoInstanceType :: Lens.Lens' LaunchTemplateOverrides (Lude.Maybe Lude.Text)
ltoInstanceType = Lens.lens (instanceType :: LaunchTemplateOverrides -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: LaunchTemplateOverrides)
{-# DEPRECATED ltoInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Provides the launch template to be used when launching the instance type. For example, some instance types might require a launch template with a different AMI. If not provided, Amazon EC2 Auto Scaling uses the launch template that's defined for your mixed instances policy. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-template-overrides.html Specifying a different launch template for an instance type> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltoLaunchTemplateSpecification :: Lens.Lens' LaunchTemplateOverrides (Lude.Maybe LaunchTemplateSpecification)
ltoLaunchTemplateSpecification = Lens.lens (launchTemplateSpecification :: LaunchTemplateOverrides -> Lude.Maybe LaunchTemplateSpecification) (\s a -> s {launchTemplateSpecification = a} :: LaunchTemplateOverrides)
{-# DEPRECATED ltoLaunchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead." #-}

instance Lude.FromXML LaunchTemplateOverrides where
  parseXML x =
    LaunchTemplateOverrides'
      Lude.<$> (x Lude..@? "WeightedCapacity")
      Lude.<*> (x Lude..@? "InstanceType")
      Lude.<*> (x Lude..@? "LaunchTemplateSpecification")

instance Lude.ToQuery LaunchTemplateOverrides where
  toQuery LaunchTemplateOverrides' {..} =
    Lude.mconcat
      [ "WeightedCapacity" Lude.=: weightedCapacity,
        "InstanceType" Lude.=: instanceType,
        "LaunchTemplateSpecification" Lude.=: launchTemplateSpecification
      ]
