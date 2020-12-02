{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the settings of a target tracking scaling policy that will be modified.
--
--
--
-- /See:/ 'autoScalingTargetTrackingScalingPolicyConfigurationUpdate' smart constructor.
data AutoScalingTargetTrackingScalingPolicyConfigurationUpdate = AutoScalingTargetTrackingScalingPolicyConfigurationUpdate'
  { _asttspcuScaleInCooldown ::
      !( Maybe
           Int
       ),
    _asttspcuDisableScaleIn ::
      !( Maybe
           Bool
       ),
    _asttspcuScaleOutCooldown ::
      !( Maybe
           Int
       ),
    _asttspcuTargetValue ::
      !Double
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asttspcuScaleInCooldown' - The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application auto scaling scales out your scalable target immediately.
--
-- * 'asttspcuDisableScaleIn' - Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
--
-- * 'asttspcuScaleOutCooldown' - The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
--
-- * 'asttspcuTargetValue' - The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
autoScalingTargetTrackingScalingPolicyConfigurationUpdate ::
  -- | 'asttspcuTargetValue'
  Double ->
  AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
autoScalingTargetTrackingScalingPolicyConfigurationUpdate
  pTargetValue_ =
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate'
      { _asttspcuScaleInCooldown =
          Nothing,
        _asttspcuDisableScaleIn = Nothing,
        _asttspcuScaleOutCooldown = Nothing,
        _asttspcuTargetValue = pTargetValue_
      }

-- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application auto scaling scales out your scalable target immediately.
asttspcuScaleInCooldown :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (Maybe Int)
asttspcuScaleInCooldown = lens _asttspcuScaleInCooldown (\s a -> s {_asttspcuScaleInCooldown = a})

-- | Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
asttspcuDisableScaleIn :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (Maybe Bool)
asttspcuDisableScaleIn = lens _asttspcuDisableScaleIn (\s a -> s {_asttspcuDisableScaleIn = a})

-- | The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
asttspcuScaleOutCooldown :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate (Maybe Int)
asttspcuScaleOutCooldown = lens _asttspcuScaleOutCooldown (\s a -> s {_asttspcuScaleOutCooldown = a})

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
asttspcuTargetValue :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationUpdate Double
asttspcuTargetValue = lens _asttspcuTargetValue (\s a -> s {_asttspcuTargetValue = a})

instance
  Hashable
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate

instance
  NFData
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate

instance
  ToJSON
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
  where
  toJSON
    AutoScalingTargetTrackingScalingPolicyConfigurationUpdate' {..} =
      object
        ( catMaybes
            [ ("ScaleInCooldown" .=) <$> _asttspcuScaleInCooldown,
              ("DisableScaleIn" .=) <$> _asttspcuDisableScaleIn,
              ("ScaleOutCooldown" .=) <$> _asttspcuScaleOutCooldown,
              Just ("TargetValue" .= _asttspcuTargetValue)
            ]
        )
