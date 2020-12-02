{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScalingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingRule where

import Network.AWS.EMR.Types.ScalingAction
import Network.AWS.EMR.Types.ScalingTrigger
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A scale-in or scale-out rule that defines scaling activity, including the CloudWatch metric alarm that triggers activity, how EC2 instances are added or removed, and the periodicity of adjustments. The automatic scaling policy for an instance group can comprise one or more automatic scaling rules.
--
--
--
-- /See:/ 'scalingRule' smart constructor.
data ScalingRule = ScalingRule'
  { _srDescription :: !(Maybe Text),
    _srName :: !Text,
    _srAction :: !ScalingAction,
    _srTrigger :: !ScalingTrigger
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srDescription' - A friendly, more verbose description of the automatic scaling rule.
--
-- * 'srName' - The name used to identify an automatic scaling rule. Rule names must be unique within a scaling policy.
--
-- * 'srAction' - The conditions that trigger an automatic scaling activity.
--
-- * 'srTrigger' - The CloudWatch alarm definition that determines when automatic scaling activity is triggered.
scalingRule ::
  -- | 'srName'
  Text ->
  -- | 'srAction'
  ScalingAction ->
  -- | 'srTrigger'
  ScalingTrigger ->
  ScalingRule
scalingRule pName_ pAction_ pTrigger_ =
  ScalingRule'
    { _srDescription = Nothing,
      _srName = pName_,
      _srAction = pAction_,
      _srTrigger = pTrigger_
    }

-- | A friendly, more verbose description of the automatic scaling rule.
srDescription :: Lens' ScalingRule (Maybe Text)
srDescription = lens _srDescription (\s a -> s {_srDescription = a})

-- | The name used to identify an automatic scaling rule. Rule names must be unique within a scaling policy.
srName :: Lens' ScalingRule Text
srName = lens _srName (\s a -> s {_srName = a})

-- | The conditions that trigger an automatic scaling activity.
srAction :: Lens' ScalingRule ScalingAction
srAction = lens _srAction (\s a -> s {_srAction = a})

-- | The CloudWatch alarm definition that determines when automatic scaling activity is triggered.
srTrigger :: Lens' ScalingRule ScalingTrigger
srTrigger = lens _srTrigger (\s a -> s {_srTrigger = a})

instance FromJSON ScalingRule where
  parseJSON =
    withObject
      "ScalingRule"
      ( \x ->
          ScalingRule'
            <$> (x .:? "Description")
            <*> (x .: "Name")
            <*> (x .: "Action")
            <*> (x .: "Trigger")
      )

instance Hashable ScalingRule

instance NFData ScalingRule

instance ToJSON ScalingRule where
  toJSON ScalingRule' {..} =
    object
      ( catMaybes
          [ ("Description" .=) <$> _srDescription,
            Just ("Name" .= _srName),
            Just ("Action" .= _srAction),
            Just ("Trigger" .= _srTrigger)
          ]
      )
