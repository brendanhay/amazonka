{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription where

import Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the auto scaling settings for a global table or global secondary index.
--
--
--
-- /See:/ 'autoScalingSettingsDescription' smart constructor.
data AutoScalingSettingsDescription = AutoScalingSettingsDescription'
  { _assdAutoScalingDisabled ::
      !(Maybe Bool),
    _assdMinimumUnits ::
      !(Maybe Nat),
    _assdMaximumUnits ::
      !(Maybe Nat),
    _assdScalingPolicies ::
      !( Maybe
           [AutoScalingPolicyDescription]
       ),
    _assdAutoScalingRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assdAutoScalingDisabled' - Disabled auto scaling for this global table or global secondary index.
--
-- * 'assdMinimumUnits' - The minimum capacity units that a global table or global secondary index should be scaled down to.
--
-- * 'assdMaximumUnits' - The maximum capacity units that a global table or global secondary index should be scaled up to.
--
-- * 'assdScalingPolicies' - Information about the scaling policies.
--
-- * 'assdAutoScalingRoleARN' - Role ARN used for configuring the auto scaling policy.
autoScalingSettingsDescription ::
  AutoScalingSettingsDescription
autoScalingSettingsDescription =
  AutoScalingSettingsDescription'
    { _assdAutoScalingDisabled =
        Nothing,
      _assdMinimumUnits = Nothing,
      _assdMaximumUnits = Nothing,
      _assdScalingPolicies = Nothing,
      _assdAutoScalingRoleARN = Nothing
    }

-- | Disabled auto scaling for this global table or global secondary index.
assdAutoScalingDisabled :: Lens' AutoScalingSettingsDescription (Maybe Bool)
assdAutoScalingDisabled = lens _assdAutoScalingDisabled (\s a -> s {_assdAutoScalingDisabled = a})

-- | The minimum capacity units that a global table or global secondary index should be scaled down to.
assdMinimumUnits :: Lens' AutoScalingSettingsDescription (Maybe Natural)
assdMinimumUnits = lens _assdMinimumUnits (\s a -> s {_assdMinimumUnits = a}) . mapping _Nat

-- | The maximum capacity units that a global table or global secondary index should be scaled up to.
assdMaximumUnits :: Lens' AutoScalingSettingsDescription (Maybe Natural)
assdMaximumUnits = lens _assdMaximumUnits (\s a -> s {_assdMaximumUnits = a}) . mapping _Nat

-- | Information about the scaling policies.
assdScalingPolicies :: Lens' AutoScalingSettingsDescription [AutoScalingPolicyDescription]
assdScalingPolicies = lens _assdScalingPolicies (\s a -> s {_assdScalingPolicies = a}) . _Default . _Coerce

-- | Role ARN used for configuring the auto scaling policy.
assdAutoScalingRoleARN :: Lens' AutoScalingSettingsDescription (Maybe Text)
assdAutoScalingRoleARN = lens _assdAutoScalingRoleARN (\s a -> s {_assdAutoScalingRoleARN = a})

instance FromJSON AutoScalingSettingsDescription where
  parseJSON =
    withObject
      "AutoScalingSettingsDescription"
      ( \x ->
          AutoScalingSettingsDescription'
            <$> (x .:? "AutoScalingDisabled")
            <*> (x .:? "MinimumUnits")
            <*> (x .:? "MaximumUnits")
            <*> (x .:? "ScalingPolicies" .!= mempty)
            <*> (x .:? "AutoScalingRoleArn")
      )

instance Hashable AutoScalingSettingsDescription

instance NFData AutoScalingSettingsDescription
