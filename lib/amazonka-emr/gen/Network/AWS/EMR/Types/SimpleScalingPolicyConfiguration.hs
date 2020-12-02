{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration where

import Network.AWS.EMR.Types.AdjustmentType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An automatic scaling configuration, which describes how the policy adds or removes instances, the cooldown period, and the number of EC2 instances that will be added each time the CloudWatch metric alarm condition is satisfied.
--
--
--
-- /See:/ 'simpleScalingPolicyConfiguration' smart constructor.
data SimpleScalingPolicyConfiguration = SimpleScalingPolicyConfiguration'
  { _sspcAdjustmentType ::
      !(Maybe AdjustmentType),
    _sspcCoolDown ::
      !(Maybe Int),
    _sspcScalingAdjustment ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SimpleScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sspcAdjustmentType' - The way in which EC2 instances are added (if @ScalingAdjustment@ is a positive number) or terminated (if @ScalingAdjustment@ is a negative number) each time the scaling activity is triggered. @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that the EC2 instance count increments or decrements by @ScalingAdjustment@ , which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@ indicates the instance count increments or decrements by the percentage specified by @ScalingAdjustment@ , which should be expressed as an integer. For example, 20 indicates an increase in 20% increments of cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity results in an instance group with the number of EC2 instances specified by @ScalingAdjustment@ , which should be expressed as a positive integer.
--
-- * 'sspcCoolDown' - The amount of time, in seconds, after a scaling activity completes before any further trigger-related scaling activities can start. The default value is 0.
--
-- * 'sspcScalingAdjustment' - The amount by which to scale in or scale out, based on the specified @AdjustmentType@ . A positive value adds to the instance group's EC2 instance count while a negative number removes instances. If @AdjustmentType@ is set to @EXACT_CAPACITY@ , the number should only be a positive integer. If @AdjustmentType@ is set to @PERCENT_CHANGE_IN_CAPACITY@ , the value should express the percentage as an integer. For example, -20 indicates a decrease in 20% increments of cluster capacity.
simpleScalingPolicyConfiguration ::
  -- | 'sspcScalingAdjustment'
  Int ->
  SimpleScalingPolicyConfiguration
simpleScalingPolicyConfiguration pScalingAdjustment_ =
  SimpleScalingPolicyConfiguration'
    { _sspcAdjustmentType = Nothing,
      _sspcCoolDown = Nothing,
      _sspcScalingAdjustment = pScalingAdjustment_
    }

-- | The way in which EC2 instances are added (if @ScalingAdjustment@ is a positive number) or terminated (if @ScalingAdjustment@ is a negative number) each time the scaling activity is triggered. @CHANGE_IN_CAPACITY@ is the default. @CHANGE_IN_CAPACITY@ indicates that the EC2 instance count increments or decrements by @ScalingAdjustment@ , which should be expressed as an integer. @PERCENT_CHANGE_IN_CAPACITY@ indicates the instance count increments or decrements by the percentage specified by @ScalingAdjustment@ , which should be expressed as an integer. For example, 20 indicates an increase in 20% increments of cluster capacity. @EXACT_CAPACITY@ indicates the scaling activity results in an instance group with the number of EC2 instances specified by @ScalingAdjustment@ , which should be expressed as a positive integer.
sspcAdjustmentType :: Lens' SimpleScalingPolicyConfiguration (Maybe AdjustmentType)
sspcAdjustmentType = lens _sspcAdjustmentType (\s a -> s {_sspcAdjustmentType = a})

-- | The amount of time, in seconds, after a scaling activity completes before any further trigger-related scaling activities can start. The default value is 0.
sspcCoolDown :: Lens' SimpleScalingPolicyConfiguration (Maybe Int)
sspcCoolDown = lens _sspcCoolDown (\s a -> s {_sspcCoolDown = a})

-- | The amount by which to scale in or scale out, based on the specified @AdjustmentType@ . A positive value adds to the instance group's EC2 instance count while a negative number removes instances. If @AdjustmentType@ is set to @EXACT_CAPACITY@ , the number should only be a positive integer. If @AdjustmentType@ is set to @PERCENT_CHANGE_IN_CAPACITY@ , the value should express the percentage as an integer. For example, -20 indicates a decrease in 20% increments of cluster capacity.
sspcScalingAdjustment :: Lens' SimpleScalingPolicyConfiguration Int
sspcScalingAdjustment = lens _sspcScalingAdjustment (\s a -> s {_sspcScalingAdjustment = a})

instance FromJSON SimpleScalingPolicyConfiguration where
  parseJSON =
    withObject
      "SimpleScalingPolicyConfiguration"
      ( \x ->
          SimpleScalingPolicyConfiguration'
            <$> (x .:? "AdjustmentType")
            <*> (x .:? "CoolDown")
            <*> (x .: "ScalingAdjustment")
      )

instance Hashable SimpleScalingPolicyConfiguration

instance NFData SimpleScalingPolicyConfiguration

instance ToJSON SimpleScalingPolicyConfiguration where
  toJSON SimpleScalingPolicyConfiguration' {..} =
    object
      ( catMaybes
          [ ("AdjustmentType" .=) <$> _sspcAdjustmentType,
            ("CoolDown" .=) <$> _sspcCoolDown,
            Just ("ScalingAdjustment" .= _sspcScalingAdjustment)
          ]
      )
