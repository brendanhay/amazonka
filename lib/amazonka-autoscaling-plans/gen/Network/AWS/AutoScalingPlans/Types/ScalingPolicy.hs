{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingPolicy where

import Network.AWS.AutoScalingPlans.Types.PolicyType
import Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a scaling policy.
--
--
--
-- /See:/ 'scalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { _spTargetTrackingConfiguration ::
      !(Maybe TargetTrackingConfiguration),
    _spPolicyName :: !Text,
    _spPolicyType :: !PolicyType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spTargetTrackingConfiguration' - The target tracking scaling policy. Includes support for predefined or customized metrics.
--
-- * 'spPolicyName' - The name of the scaling policy.
--
-- * 'spPolicyType' - The type of scaling policy.
scalingPolicy ::
  -- | 'spPolicyName'
  Text ->
  -- | 'spPolicyType'
  PolicyType ->
  ScalingPolicy
scalingPolicy pPolicyName_ pPolicyType_ =
  ScalingPolicy'
    { _spTargetTrackingConfiguration = Nothing,
      _spPolicyName = pPolicyName_,
      _spPolicyType = pPolicyType_
    }

-- | The target tracking scaling policy. Includes support for predefined or customized metrics.
spTargetTrackingConfiguration :: Lens' ScalingPolicy (Maybe TargetTrackingConfiguration)
spTargetTrackingConfiguration = lens _spTargetTrackingConfiguration (\s a -> s {_spTargetTrackingConfiguration = a})

-- | The name of the scaling policy.
spPolicyName :: Lens' ScalingPolicy Text
spPolicyName = lens _spPolicyName (\s a -> s {_spPolicyName = a})

-- | The type of scaling policy.
spPolicyType :: Lens' ScalingPolicy PolicyType
spPolicyType = lens _spPolicyType (\s a -> s {_spPolicyType = a})

instance FromJSON ScalingPolicy where
  parseJSON =
    withObject
      "ScalingPolicy"
      ( \x ->
          ScalingPolicy'
            <$> (x .:? "TargetTrackingConfiguration")
            <*> (x .: "PolicyName")
            <*> (x .: "PolicyType")
      )

instance Hashable ScalingPolicy

instance NFData ScalingPolicy
