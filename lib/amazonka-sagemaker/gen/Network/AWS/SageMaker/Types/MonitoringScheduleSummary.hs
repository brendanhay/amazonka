{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringScheduleSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringScheduleSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ScheduleStatus

-- | Summarizes the monitoring schedule.
--
--
--
-- /See:/ 'monitoringScheduleSummary' smart constructor.
data MonitoringScheduleSummary = MonitoringScheduleSummary'
  { _mssEndpointName ::
      !(Maybe Text),
    _mssMonitoringScheduleName :: !Text,
    _mssMonitoringScheduleARN :: !Text,
    _mssCreationTime :: !POSIX,
    _mssLastModifiedTime :: !POSIX,
    _mssMonitoringScheduleStatus ::
      !ScheduleStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringScheduleSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mssEndpointName' - The name of the endpoint using the monitoring schedule.
--
-- * 'mssMonitoringScheduleName' - The name of the monitoring schedule.
--
-- * 'mssMonitoringScheduleARN' - The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- * 'mssCreationTime' - The creation time of the monitoring schedule.
--
-- * 'mssLastModifiedTime' - The last time the monitoring schedule was modified.
--
-- * 'mssMonitoringScheduleStatus' - The status of the monitoring schedule.
monitoringScheduleSummary ::
  -- | 'mssMonitoringScheduleName'
  Text ->
  -- | 'mssMonitoringScheduleARN'
  Text ->
  -- | 'mssCreationTime'
  UTCTime ->
  -- | 'mssLastModifiedTime'
  UTCTime ->
  -- | 'mssMonitoringScheduleStatus'
  ScheduleStatus ->
  MonitoringScheduleSummary
monitoringScheduleSummary
  pMonitoringScheduleName_
  pMonitoringScheduleARN_
  pCreationTime_
  pLastModifiedTime_
  pMonitoringScheduleStatus_ =
    MonitoringScheduleSummary'
      { _mssEndpointName = Nothing,
        _mssMonitoringScheduleName = pMonitoringScheduleName_,
        _mssMonitoringScheduleARN = pMonitoringScheduleARN_,
        _mssCreationTime = _Time # pCreationTime_,
        _mssLastModifiedTime = _Time # pLastModifiedTime_,
        _mssMonitoringScheduleStatus = pMonitoringScheduleStatus_
      }

-- | The name of the endpoint using the monitoring schedule.
mssEndpointName :: Lens' MonitoringScheduleSummary (Maybe Text)
mssEndpointName = lens _mssEndpointName (\s a -> s {_mssEndpointName = a})

-- | The name of the monitoring schedule.
mssMonitoringScheduleName :: Lens' MonitoringScheduleSummary Text
mssMonitoringScheduleName = lens _mssMonitoringScheduleName (\s a -> s {_mssMonitoringScheduleName = a})

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
mssMonitoringScheduleARN :: Lens' MonitoringScheduleSummary Text
mssMonitoringScheduleARN = lens _mssMonitoringScheduleARN (\s a -> s {_mssMonitoringScheduleARN = a})

-- | The creation time of the monitoring schedule.
mssCreationTime :: Lens' MonitoringScheduleSummary UTCTime
mssCreationTime = lens _mssCreationTime (\s a -> s {_mssCreationTime = a}) . _Time

-- | The last time the monitoring schedule was modified.
mssLastModifiedTime :: Lens' MonitoringScheduleSummary UTCTime
mssLastModifiedTime = lens _mssLastModifiedTime (\s a -> s {_mssLastModifiedTime = a}) . _Time

-- | The status of the monitoring schedule.
mssMonitoringScheduleStatus :: Lens' MonitoringScheduleSummary ScheduleStatus
mssMonitoringScheduleStatus = lens _mssMonitoringScheduleStatus (\s a -> s {_mssMonitoringScheduleStatus = a})

instance FromJSON MonitoringScheduleSummary where
  parseJSON =
    withObject
      "MonitoringScheduleSummary"
      ( \x ->
          MonitoringScheduleSummary'
            <$> (x .:? "EndpointName")
            <*> (x .: "MonitoringScheduleName")
            <*> (x .: "MonitoringScheduleArn")
            <*> (x .: "CreationTime")
            <*> (x .: "LastModifiedTime")
            <*> (x .: "MonitoringScheduleStatus")
      )

instance Hashable MonitoringScheduleSummary

instance NFData MonitoringScheduleSummary
