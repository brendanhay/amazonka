{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate where

import Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the auto scaling settings to be modified for a global table or global secondary index.
--
--
--
-- /See:/ 'autoScalingSettingsUpdate' smart constructor.
data AutoScalingSettingsUpdate = AutoScalingSettingsUpdate'
  { _assuAutoScalingDisabled ::
      !(Maybe Bool),
    _assuMinimumUnits :: !(Maybe Nat),
    _assuScalingPolicyUpdate ::
      !(Maybe AutoScalingPolicyUpdate),
    _assuMaximumUnits :: !(Maybe Nat),
    _assuAutoScalingRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingSettingsUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assuAutoScalingDisabled' - Disabled auto scaling for this global table or global secondary index.
--
-- * 'assuMinimumUnits' - The minimum capacity units that a global table or global secondary index should be scaled down to.
--
-- * 'assuScalingPolicyUpdate' - The scaling policy to apply for scaling target global table or global secondary index capacity units.
--
-- * 'assuMaximumUnits' - The maximum capacity units that a global table or global secondary index should be scaled up to.
--
-- * 'assuAutoScalingRoleARN' - Role ARN used for configuring auto scaling policy.
autoScalingSettingsUpdate ::
  AutoScalingSettingsUpdate
autoScalingSettingsUpdate =
  AutoScalingSettingsUpdate'
    { _assuAutoScalingDisabled = Nothing,
      _assuMinimumUnits = Nothing,
      _assuScalingPolicyUpdate = Nothing,
      _assuMaximumUnits = Nothing,
      _assuAutoScalingRoleARN = Nothing
    }

-- | Disabled auto scaling for this global table or global secondary index.
assuAutoScalingDisabled :: Lens' AutoScalingSettingsUpdate (Maybe Bool)
assuAutoScalingDisabled = lens _assuAutoScalingDisabled (\s a -> s {_assuAutoScalingDisabled = a})

-- | The minimum capacity units that a global table or global secondary index should be scaled down to.
assuMinimumUnits :: Lens' AutoScalingSettingsUpdate (Maybe Natural)
assuMinimumUnits = lens _assuMinimumUnits (\s a -> s {_assuMinimumUnits = a}) . mapping _Nat

-- | The scaling policy to apply for scaling target global table or global secondary index capacity units.
assuScalingPolicyUpdate :: Lens' AutoScalingSettingsUpdate (Maybe AutoScalingPolicyUpdate)
assuScalingPolicyUpdate = lens _assuScalingPolicyUpdate (\s a -> s {_assuScalingPolicyUpdate = a})

-- | The maximum capacity units that a global table or global secondary index should be scaled up to.
assuMaximumUnits :: Lens' AutoScalingSettingsUpdate (Maybe Natural)
assuMaximumUnits = lens _assuMaximumUnits (\s a -> s {_assuMaximumUnits = a}) . mapping _Nat

-- | Role ARN used for configuring auto scaling policy.
assuAutoScalingRoleARN :: Lens' AutoScalingSettingsUpdate (Maybe Text)
assuAutoScalingRoleARN = lens _assuAutoScalingRoleARN (\s a -> s {_assuAutoScalingRoleARN = a})

instance Hashable AutoScalingSettingsUpdate

instance NFData AutoScalingSettingsUpdate

instance ToJSON AutoScalingSettingsUpdate where
  toJSON AutoScalingSettingsUpdate' {..} =
    object
      ( catMaybes
          [ ("AutoScalingDisabled" .=) <$> _assuAutoScalingDisabled,
            ("MinimumUnits" .=) <$> _assuMinimumUnits,
            ("ScalingPolicyUpdate" .=) <$> _assuScalingPolicyUpdate,
            ("MaximumUnits" .=) <$> _assuMaximumUnits,
            ("AutoScalingRoleArn" .=) <$> _assuAutoScalingRoleARN
          ]
      )
