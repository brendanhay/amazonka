{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the auto scaling policy to be modified.
--
--
--
-- /See:/ 'autoScalingPolicyUpdate' smart constructor.
data AutoScalingPolicyUpdate = AutoScalingPolicyUpdate'
  { _aspuPolicyName ::
      !(Maybe Text),
    _aspuTargetTrackingScalingPolicyConfiguration ::
      !AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingPolicyUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspuPolicyName' - The name of the scaling policy.
--
-- * 'aspuTargetTrackingScalingPolicyConfiguration' - Represents a target tracking scaling policy configuration.
autoScalingPolicyUpdate ::
  -- | 'aspuTargetTrackingScalingPolicyConfiguration'
  AutoScalingTargetTrackingScalingPolicyConfigurationUpdate ->
  AutoScalingPolicyUpdate
autoScalingPolicyUpdate pTargetTrackingScalingPolicyConfiguration_ =
  AutoScalingPolicyUpdate'
    { _aspuPolicyName = Nothing,
      _aspuTargetTrackingScalingPolicyConfiguration =
        pTargetTrackingScalingPolicyConfiguration_
    }

-- | The name of the scaling policy.
aspuPolicyName :: Lens' AutoScalingPolicyUpdate (Maybe Text)
aspuPolicyName = lens _aspuPolicyName (\s a -> s {_aspuPolicyName = a})

-- | Represents a target tracking scaling policy configuration.
aspuTargetTrackingScalingPolicyConfiguration :: Lens' AutoScalingPolicyUpdate AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
aspuTargetTrackingScalingPolicyConfiguration = lens _aspuTargetTrackingScalingPolicyConfiguration (\s a -> s {_aspuTargetTrackingScalingPolicyConfiguration = a})

instance Hashable AutoScalingPolicyUpdate

instance NFData AutoScalingPolicyUpdate

instance ToJSON AutoScalingPolicyUpdate where
  toJSON AutoScalingPolicyUpdate' {..} =
    object
      ( catMaybes
          [ ("PolicyName" .=) <$> _aspuPolicyName,
            Just
              ( "TargetTrackingScalingPolicyConfiguration"
                  .= _aspuTargetTrackingScalingPolicyConfiguration
              )
          ]
      )
