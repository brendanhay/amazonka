{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LaunchTemplateOverrides where

import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an override for a launch template. The maximum number of instance types that can be associated with an Auto Scaling group is 20. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-override-options.html Configuring overrides> in the /Amazon EC2 Auto Scaling User Guide/ .
--
--
--
-- /See:/ 'launchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'
  { _ltoWeightedCapacity ::
      !(Maybe Text),
    _ltoInstanceType :: !(Maybe Text),
    _ltoLaunchTemplateSpecification ::
      !(Maybe LaunchTemplateSpecification)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateOverrides' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltoWeightedCapacity' - The number of capacity units provided by the specified instance type in terms of virtual CPUs, memory, storage, throughput, or other relative performance characteristic. When a Spot or On-Demand Instance is provisioned, the capacity units count toward the desired capacity. Amazon EC2 Auto Scaling provisions instances until the desired capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EC2 Auto Scaling can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the desired capacity is exceeded by 3 units. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance weighting for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ . Value must be in the range of 1 to 999.
--
-- * 'ltoInstanceType' - The instance type, such as @m3.xlarge@ . You must use an instance type that is supported in your requested Region and Availability Zones. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ltoLaunchTemplateSpecification' - Provides the launch template to be used when launching the instance type. For example, some instance types might require a launch template with a different AMI. If not provided, Amazon EC2 Auto Scaling uses the launch template that's defined for your mixed instances policy. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-template-overrides.html Specifying a different launch template for an instance type> in the /Amazon EC2 Auto Scaling User Guide/ .
launchTemplateOverrides ::
  LaunchTemplateOverrides
launchTemplateOverrides =
  LaunchTemplateOverrides'
    { _ltoWeightedCapacity = Nothing,
      _ltoInstanceType = Nothing,
      _ltoLaunchTemplateSpecification = Nothing
    }

-- | The number of capacity units provided by the specified instance type in terms of virtual CPUs, memory, storage, throughput, or other relative performance characteristic. When a Spot or On-Demand Instance is provisioned, the capacity units count toward the desired capacity. Amazon EC2 Auto Scaling provisions instances until the desired capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EC2 Auto Scaling can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the desired capacity is exceeded by 3 units. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance weighting for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ . Value must be in the range of 1 to 999.
ltoWeightedCapacity :: Lens' LaunchTemplateOverrides (Maybe Text)
ltoWeightedCapacity = lens _ltoWeightedCapacity (\s a -> s {_ltoWeightedCapacity = a})

-- | The instance type, such as @m3.xlarge@ . You must use an instance type that is supported in your requested Region and Availability Zones. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> in the /Amazon Elastic Compute Cloud User Guide/ .
ltoInstanceType :: Lens' LaunchTemplateOverrides (Maybe Text)
ltoInstanceType = lens _ltoInstanceType (\s a -> s {_ltoInstanceType = a})

-- | Provides the launch template to be used when launching the instance type. For example, some instance types might require a launch template with a different AMI. If not provided, Amazon EC2 Auto Scaling uses the launch template that's defined for your mixed instances policy. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-template-overrides.html Specifying a different launch template for an instance type> in the /Amazon EC2 Auto Scaling User Guide/ .
ltoLaunchTemplateSpecification :: Lens' LaunchTemplateOverrides (Maybe LaunchTemplateSpecification)
ltoLaunchTemplateSpecification = lens _ltoLaunchTemplateSpecification (\s a -> s {_ltoLaunchTemplateSpecification = a})

instance FromXML LaunchTemplateOverrides where
  parseXML x =
    LaunchTemplateOverrides'
      <$> (x .@? "WeightedCapacity")
      <*> (x .@? "InstanceType")
      <*> (x .@? "LaunchTemplateSpecification")

instance Hashable LaunchTemplateOverrides

instance NFData LaunchTemplateOverrides

instance ToQuery LaunchTemplateOverrides where
  toQuery LaunchTemplateOverrides' {..} =
    mconcat
      [ "WeightedCapacity" =: _ltoWeightedCapacity,
        "InstanceType" =: _ltoInstanceType,
        "LaunchTemplateSpecification" =: _ltoLaunchTemplateSpecification
      ]
