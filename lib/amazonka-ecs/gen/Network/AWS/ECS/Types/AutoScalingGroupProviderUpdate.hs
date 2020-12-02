{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate where

import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.ManagedTerminationProtection
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of the Auto Scaling group capacity provider to update.
--
--
--
-- /See:/ 'autoScalingGroupProviderUpdate' smart constructor.
data AutoScalingGroupProviderUpdate = AutoScalingGroupProviderUpdate'
  { _asgpuManagedScaling ::
      !(Maybe ManagedScaling),
    _asgpuManagedTerminationProtection ::
      !( Maybe
           ManagedTerminationProtection
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingGroupProviderUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgpuManagedScaling' - Undocumented member.
--
-- * 'asgpuManagedTerminationProtection' - The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection. /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work. When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ . When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
autoScalingGroupProviderUpdate ::
  AutoScalingGroupProviderUpdate
autoScalingGroupProviderUpdate =
  AutoScalingGroupProviderUpdate'
    { _asgpuManagedScaling = Nothing,
      _asgpuManagedTerminationProtection = Nothing
    }

-- | Undocumented member.
asgpuManagedScaling :: Lens' AutoScalingGroupProviderUpdate (Maybe ManagedScaling)
asgpuManagedScaling = lens _asgpuManagedScaling (\s a -> s {_asgpuManagedScaling = a})

-- | The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection. /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work. When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ . When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
asgpuManagedTerminationProtection :: Lens' AutoScalingGroupProviderUpdate (Maybe ManagedTerminationProtection)
asgpuManagedTerminationProtection = lens _asgpuManagedTerminationProtection (\s a -> s {_asgpuManagedTerminationProtection = a})

instance Hashable AutoScalingGroupProviderUpdate

instance NFData AutoScalingGroupProviderUpdate

instance ToJSON AutoScalingGroupProviderUpdate where
  toJSON AutoScalingGroupProviderUpdate' {..} =
    object
      ( catMaybes
          [ ("managedScaling" .=) <$> _asgpuManagedScaling,
            ("managedTerminationProtection" .=)
              <$> _asgpuManagedTerminationProtection
          ]
      )
