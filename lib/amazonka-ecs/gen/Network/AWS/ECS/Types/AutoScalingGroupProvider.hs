{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AutoScalingGroupProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AutoScalingGroupProvider where

import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.ManagedTerminationProtection
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of the Auto Scaling group for the capacity provider.
--
--
--
-- /See:/ 'autoScalingGroupProvider' smart constructor.
data AutoScalingGroupProvider = AutoScalingGroupProvider'
  { _asgpManagedScaling ::
      !(Maybe ManagedScaling),
    _asgpManagedTerminationProtection ::
      !(Maybe ManagedTerminationProtection),
    _asgpAutoScalingGroupARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingGroupProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgpManagedScaling' - The managed scaling settings for the Auto Scaling group capacity provider.
--
-- * 'asgpManagedTerminationProtection' - The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection. /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work. When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ . When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
--
-- * 'asgpAutoScalingGroupARN' - The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
autoScalingGroupProvider ::
  -- | 'asgpAutoScalingGroupARN'
  Text ->
  AutoScalingGroupProvider
autoScalingGroupProvider pAutoScalingGroupARN_ =
  AutoScalingGroupProvider'
    { _asgpManagedScaling = Nothing,
      _asgpManagedTerminationProtection = Nothing,
      _asgpAutoScalingGroupARN = pAutoScalingGroupARN_
    }

-- | The managed scaling settings for the Auto Scaling group capacity provider.
asgpManagedScaling :: Lens' AutoScalingGroupProvider (Maybe ManagedScaling)
asgpManagedScaling = lens _asgpManagedScaling (\s a -> s {_asgpManagedScaling = a})

-- | The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection. /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work. When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ . When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
asgpManagedTerminationProtection :: Lens' AutoScalingGroupProvider (Maybe ManagedTerminationProtection)
asgpManagedTerminationProtection = lens _asgpManagedTerminationProtection (\s a -> s {_asgpManagedTerminationProtection = a})

-- | The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
asgpAutoScalingGroupARN :: Lens' AutoScalingGroupProvider Text
asgpAutoScalingGroupARN = lens _asgpAutoScalingGroupARN (\s a -> s {_asgpAutoScalingGroupARN = a})

instance FromJSON AutoScalingGroupProvider where
  parseJSON =
    withObject
      "AutoScalingGroupProvider"
      ( \x ->
          AutoScalingGroupProvider'
            <$> (x .:? "managedScaling")
            <*> (x .:? "managedTerminationProtection")
            <*> (x .: "autoScalingGroupArn")
      )

instance Hashable AutoScalingGroupProvider

instance NFData AutoScalingGroupProvider

instance ToJSON AutoScalingGroupProvider where
  toJSON AutoScalingGroupProvider' {..} =
    object
      ( catMaybes
          [ ("managedScaling" .=) <$> _asgpManagedScaling,
            ("managedTerminationProtection" .=)
              <$> _asgpManagedTerminationProtection,
            Just ("autoScalingGroupArn" .= _asgpAutoScalingGroupARN)
          ]
      )
