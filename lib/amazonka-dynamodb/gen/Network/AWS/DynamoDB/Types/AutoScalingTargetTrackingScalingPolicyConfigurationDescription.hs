{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of a target tracking scaling policy.
--
--
--
-- /See:/ 'autoScalingTargetTrackingScalingPolicyConfigurationDescription' smart constructor.
data AutoScalingTargetTrackingScalingPolicyConfigurationDescription = AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
  { _asttspcdScaleInCooldown ::
      !( Maybe
           Int
       ),
    _asttspcdDisableScaleIn ::
      !( Maybe
           Bool
       ),
    _asttspcdScaleOutCooldown ::
      !( Maybe
           Int
       ),
    _asttspcdTargetValue ::
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

-- | Creates a value of 'AutoScalingTargetTrackingScalingPolicyConfigurationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asttspcdScaleInCooldown' - The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application auto scaling scales out your scalable target immediately.
--
-- * 'asttspcdDisableScaleIn' - Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
--
-- * 'asttspcdScaleOutCooldown' - The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
--
-- * 'asttspcdTargetValue' - The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
autoScalingTargetTrackingScalingPolicyConfigurationDescription ::
  -- | 'asttspcdTargetValue'
  Double ->
  AutoScalingTargetTrackingScalingPolicyConfigurationDescription
autoScalingTargetTrackingScalingPolicyConfigurationDescription
  pTargetValue_ =
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
      { _asttspcdScaleInCooldown =
          Nothing,
        _asttspcdDisableScaleIn =
          Nothing,
        _asttspcdScaleOutCooldown =
          Nothing,
        _asttspcdTargetValue =
          pTargetValue_
      }

-- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale in requests until it has expired. You should scale in conservatively to protect your application's availability. However, if another alarm triggers a scale out policy during the cooldown period after a scale-in, application auto scaling scales out your scalable target immediately.
asttspcdScaleInCooldown :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Maybe Int)
asttspcdScaleInCooldown = lens _asttspcdScaleInCooldown (\s a -> s {_asttspcdScaleInCooldown = a})

-- | Indicates whether scale in by the target tracking policy is disabled. If the value is true, scale in is disabled and the target tracking policy won't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking policy can remove capacity from the scalable resource. The default value is false.
asttspcdDisableScaleIn :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Maybe Bool)
asttspcdDisableScaleIn = lens _asttspcdDisableScaleIn (\s a -> s {_asttspcdDisableScaleIn = a})

-- | The amount of time, in seconds, after a scale out activity completes before another scale out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. You should continuously (but not excessively) scale out.
asttspcdScaleOutCooldown :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription (Maybe Int)
asttspcdScaleOutCooldown = lens _asttspcdScaleOutCooldown (\s a -> s {_asttspcdScaleOutCooldown = a})

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
asttspcdTargetValue :: Lens' AutoScalingTargetTrackingScalingPolicyConfigurationDescription Double
asttspcdTargetValue = lens _asttspcdTargetValue (\s a -> s {_asttspcdTargetValue = a})

instance
  FromJSON
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription
  where
  parseJSON =
    withObject
      "AutoScalingTargetTrackingScalingPolicyConfigurationDescription"
      ( \x ->
          AutoScalingTargetTrackingScalingPolicyConfigurationDescription'
            <$> (x .:? "ScaleInCooldown")
            <*> (x .:? "DisableScaleIn")
            <*> (x .:? "ScaleOutCooldown")
            <*> (x .: "TargetValue")
      )

instance
  Hashable
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription

instance
  NFData
    AutoScalingTargetTrackingScalingPolicyConfigurationDescription
