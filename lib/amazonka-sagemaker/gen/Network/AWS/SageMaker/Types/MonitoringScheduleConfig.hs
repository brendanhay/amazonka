{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringScheduleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringScheduleConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.MonitoringJobDefinition
import Network.AWS.SageMaker.Types.ScheduleConfig

-- | Configures the monitoring schedule and defines the monitoring job.
--
--
--
-- /See:/ 'monitoringScheduleConfig' smart constructor.
data MonitoringScheduleConfig = MonitoringScheduleConfig'
  { _mscScheduleConfig ::
      !(Maybe ScheduleConfig),
    _mscMonitoringJobDefinition ::
      !MonitoringJobDefinition
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringScheduleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mscScheduleConfig' - Configures the monitoring schedule.
--
-- * 'mscMonitoringJobDefinition' - Defines the monitoring job.
monitoringScheduleConfig ::
  -- | 'mscMonitoringJobDefinition'
  MonitoringJobDefinition ->
  MonitoringScheduleConfig
monitoringScheduleConfig pMonitoringJobDefinition_ =
  MonitoringScheduleConfig'
    { _mscScheduleConfig = Nothing,
      _mscMonitoringJobDefinition = pMonitoringJobDefinition_
    }

-- | Configures the monitoring schedule.
mscScheduleConfig :: Lens' MonitoringScheduleConfig (Maybe ScheduleConfig)
mscScheduleConfig = lens _mscScheduleConfig (\s a -> s {_mscScheduleConfig = a})

-- | Defines the monitoring job.
mscMonitoringJobDefinition :: Lens' MonitoringScheduleConfig MonitoringJobDefinition
mscMonitoringJobDefinition = lens _mscMonitoringJobDefinition (\s a -> s {_mscMonitoringJobDefinition = a})

instance FromJSON MonitoringScheduleConfig where
  parseJSON =
    withObject
      "MonitoringScheduleConfig"
      ( \x ->
          MonitoringScheduleConfig'
            <$> (x .:? "ScheduleConfig") <*> (x .: "MonitoringJobDefinition")
      )

instance Hashable MonitoringScheduleConfig

instance NFData MonitoringScheduleConfig

instance ToJSON MonitoringScheduleConfig where
  toJSON MonitoringScheduleConfig' {..} =
    object
      ( catMaybes
          [ ("ScheduleConfig" .=) <$> _mscScheduleConfig,
            Just ("MonitoringJobDefinition" .= _mscMonitoringJobDefinition)
          ]
      )
