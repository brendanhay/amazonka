{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScalingTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingTrigger where

import Network.AWS.EMR.Types.CloudWatchAlarmDefinition
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The conditions that trigger an automatic scaling activity.
--
--
--
-- /See:/ 'scalingTrigger' smart constructor.
newtype ScalingTrigger = ScalingTrigger'
  { _stCloudWatchAlarmDefinition ::
      CloudWatchAlarmDefinition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stCloudWatchAlarmDefinition' - The definition of a CloudWatch metric alarm. When the defined alarm conditions are met along with other trigger parameters, scaling activity begins.
scalingTrigger ::
  -- | 'stCloudWatchAlarmDefinition'
  CloudWatchAlarmDefinition ->
  ScalingTrigger
scalingTrigger pCloudWatchAlarmDefinition_ =
  ScalingTrigger'
    { _stCloudWatchAlarmDefinition =
        pCloudWatchAlarmDefinition_
    }

-- | The definition of a CloudWatch metric alarm. When the defined alarm conditions are met along with other trigger parameters, scaling activity begins.
stCloudWatchAlarmDefinition :: Lens' ScalingTrigger CloudWatchAlarmDefinition
stCloudWatchAlarmDefinition = lens _stCloudWatchAlarmDefinition (\s a -> s {_stCloudWatchAlarmDefinition = a})

instance FromJSON ScalingTrigger where
  parseJSON =
    withObject
      "ScalingTrigger"
      (\x -> ScalingTrigger' <$> (x .: "CloudWatchAlarmDefinition"))

instance Hashable ScalingTrigger

instance NFData ScalingTrigger

instance ToJSON ScalingTrigger where
  toJSON ScalingTrigger' {..} =
    object
      ( catMaybes
          [ Just
              ("CloudWatchAlarmDefinition" .= _stCloudWatchAlarmDefinition)
          ]
      )
