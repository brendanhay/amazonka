{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ManagedScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ManagedScaling where

import Network.AWS.ECS.Types.ManagedScalingStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The managed scaling settings for the Auto Scaling group capacity provider.
--
--
-- When managed scaling is enabled, Amazon ECS manages the scale-in and scale-out actions of the Auto Scaling group. Amazon ECS manages a target tracking scaling policy using an Amazon ECS-managed CloudWatch metric with the specified @targetCapacity@ value as the target value for the metric. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/asg-capacity-providers.html#asg-capacity-providers-managed-scaling Using Managed Scaling> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If managed scaling is disabled, the user must manage the scaling of the Auto Scaling group.
--
--
-- /See:/ 'managedScaling' smart constructor.
data ManagedScaling = ManagedScaling'
  { _msStatus ::
      !(Maybe ManagedScalingStatus),
    _msMaximumScalingStepSize :: !(Maybe Nat),
    _msTargetCapacity :: !(Maybe Nat),
    _msMinimumScalingStepSize :: !(Maybe Nat),
    _msInstanceWarmupPeriod :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ManagedScaling' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msStatus' - Whether or not to enable managed scaling for the capacity provider.
--
-- * 'msMaximumScalingStepSize' - The maximum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @10000@ is used.
--
-- * 'msTargetCapacity' - The target capacity value for the capacity provider. The specified value must be greater than @0@ and less than or equal to @100@ . A value of @100@ will result in the Amazon EC2 instances in your Auto Scaling group being completely utilized.
--
-- * 'msMinimumScalingStepSize' - The minimum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @1@ is used.
--
-- * 'msInstanceWarmupPeriod' - The period of time, in seconds, after a newly launched Amazon EC2 instance can contribute to CloudWatch metrics for Auto Scaling group. If this parameter is omitted, the default value of @300@ seconds is used.
managedScaling ::
  ManagedScaling
managedScaling =
  ManagedScaling'
    { _msStatus = Nothing,
      _msMaximumScalingStepSize = Nothing,
      _msTargetCapacity = Nothing,
      _msMinimumScalingStepSize = Nothing,
      _msInstanceWarmupPeriod = Nothing
    }

-- | Whether or not to enable managed scaling for the capacity provider.
msStatus :: Lens' ManagedScaling (Maybe ManagedScalingStatus)
msStatus = lens _msStatus (\s a -> s {_msStatus = a})

-- | The maximum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @10000@ is used.
msMaximumScalingStepSize :: Lens' ManagedScaling (Maybe Natural)
msMaximumScalingStepSize = lens _msMaximumScalingStepSize (\s a -> s {_msMaximumScalingStepSize = a}) . mapping _Nat

-- | The target capacity value for the capacity provider. The specified value must be greater than @0@ and less than or equal to @100@ . A value of @100@ will result in the Amazon EC2 instances in your Auto Scaling group being completely utilized.
msTargetCapacity :: Lens' ManagedScaling (Maybe Natural)
msTargetCapacity = lens _msTargetCapacity (\s a -> s {_msTargetCapacity = a}) . mapping _Nat

-- | The minimum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @1@ is used.
msMinimumScalingStepSize :: Lens' ManagedScaling (Maybe Natural)
msMinimumScalingStepSize = lens _msMinimumScalingStepSize (\s a -> s {_msMinimumScalingStepSize = a}) . mapping _Nat

-- | The period of time, in seconds, after a newly launched Amazon EC2 instance can contribute to CloudWatch metrics for Auto Scaling group. If this parameter is omitted, the default value of @300@ seconds is used.
msInstanceWarmupPeriod :: Lens' ManagedScaling (Maybe Natural)
msInstanceWarmupPeriod = lens _msInstanceWarmupPeriod (\s a -> s {_msInstanceWarmupPeriod = a}) . mapping _Nat

instance FromJSON ManagedScaling where
  parseJSON =
    withObject
      "ManagedScaling"
      ( \x ->
          ManagedScaling'
            <$> (x .:? "status")
            <*> (x .:? "maximumScalingStepSize")
            <*> (x .:? "targetCapacity")
            <*> (x .:? "minimumScalingStepSize")
            <*> (x .:? "instanceWarmupPeriod")
      )

instance Hashable ManagedScaling

instance NFData ManagedScaling

instance ToJSON ManagedScaling where
  toJSON ManagedScaling' {..} =
    object
      ( catMaybes
          [ ("status" .=) <$> _msStatus,
            ("maximumScalingStepSize" .=) <$> _msMaximumScalingStepSize,
            ("targetCapacity" .=) <$> _msTargetCapacity,
            ("minimumScalingStepSize" .=) <$> _msMinimumScalingStepSize,
            ("instanceWarmupPeriod" .=) <$> _msInstanceWarmupPeriod
          ]
      )
