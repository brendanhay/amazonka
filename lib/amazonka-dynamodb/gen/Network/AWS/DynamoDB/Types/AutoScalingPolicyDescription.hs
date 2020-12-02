{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription where

import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the properties of the scaling policy.
--
--
--
-- /See:/ 'autoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { _aspdPolicyName ::
      !(Maybe Text),
    _aspdTargetTrackingScalingPolicyConfiguration ::
      !( Maybe
           AutoScalingTargetTrackingScalingPolicyConfigurationDescription
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingPolicyDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspdPolicyName' - The name of the scaling policy.
--
-- * 'aspdTargetTrackingScalingPolicyConfiguration' - Represents a target tracking scaling policy configuration.
autoScalingPolicyDescription ::
  AutoScalingPolicyDescription
autoScalingPolicyDescription =
  AutoScalingPolicyDescription'
    { _aspdPolicyName = Nothing,
      _aspdTargetTrackingScalingPolicyConfiguration = Nothing
    }

-- | The name of the scaling policy.
aspdPolicyName :: Lens' AutoScalingPolicyDescription (Maybe Text)
aspdPolicyName = lens _aspdPolicyName (\s a -> s {_aspdPolicyName = a})

-- | Represents a target tracking scaling policy configuration.
aspdTargetTrackingScalingPolicyConfiguration :: Lens' AutoScalingPolicyDescription (Maybe AutoScalingTargetTrackingScalingPolicyConfigurationDescription)
aspdTargetTrackingScalingPolicyConfiguration = lens _aspdTargetTrackingScalingPolicyConfiguration (\s a -> s {_aspdTargetTrackingScalingPolicyConfiguration = a})

instance FromJSON AutoScalingPolicyDescription where
  parseJSON =
    withObject
      "AutoScalingPolicyDescription"
      ( \x ->
          AutoScalingPolicyDescription'
            <$> (x .:? "PolicyName")
            <*> (x .:? "TargetTrackingScalingPolicyConfiguration")
      )

instance Hashable AutoScalingPolicyDescription

instance NFData AutoScalingPolicyDescription
