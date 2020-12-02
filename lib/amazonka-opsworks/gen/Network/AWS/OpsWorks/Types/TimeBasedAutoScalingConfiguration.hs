{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
import Network.AWS.Prelude

-- | Describes an instance's time-based auto scaling configuration.
--
--
--
-- /See:/ 'timeBasedAutoScalingConfiguration' smart constructor.
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration'
  { _tbascInstanceId ::
      !(Maybe Text),
    _tbascAutoScalingSchedule ::
      !( Maybe
           WeeklyAutoScalingSchedule
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimeBasedAutoScalingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbascInstanceId' - The instance ID.
--
-- * 'tbascAutoScalingSchedule' - A @WeeklyAutoScalingSchedule@ object with the instance schedule.
timeBasedAutoScalingConfiguration ::
  TimeBasedAutoScalingConfiguration
timeBasedAutoScalingConfiguration =
  TimeBasedAutoScalingConfiguration'
    { _tbascInstanceId = Nothing,
      _tbascAutoScalingSchedule = Nothing
    }

-- | The instance ID.
tbascInstanceId :: Lens' TimeBasedAutoScalingConfiguration (Maybe Text)
tbascInstanceId = lens _tbascInstanceId (\s a -> s {_tbascInstanceId = a})

-- | A @WeeklyAutoScalingSchedule@ object with the instance schedule.
tbascAutoScalingSchedule :: Lens' TimeBasedAutoScalingConfiguration (Maybe WeeklyAutoScalingSchedule)
tbascAutoScalingSchedule = lens _tbascAutoScalingSchedule (\s a -> s {_tbascAutoScalingSchedule = a})

instance FromJSON TimeBasedAutoScalingConfiguration where
  parseJSON =
    withObject
      "TimeBasedAutoScalingConfiguration"
      ( \x ->
          TimeBasedAutoScalingConfiguration'
            <$> (x .:? "InstanceId") <*> (x .:? "AutoScalingSchedule")
      )

instance Hashable TimeBasedAutoScalingConfiguration

instance NFData TimeBasedAutoScalingConfiguration
