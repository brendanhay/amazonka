{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringExecutionSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ExecutionStatus

-- | Summary of information about the last monitoring job to run.
--
--
--
-- /See:/ 'monitoringExecutionSummary' smart constructor.
data MonitoringExecutionSummary = MonitoringExecutionSummary'
  { _mesFailureReason ::
      !(Maybe Text),
    _mesEndpointName :: !(Maybe Text),
    _mesProcessingJobARN :: !(Maybe Text),
    _mesMonitoringScheduleName :: !Text,
    _mesScheduledTime :: !POSIX,
    _mesCreationTime :: !POSIX,
    _mesLastModifiedTime :: !POSIX,
    _mesMonitoringExecutionStatus ::
      !ExecutionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringExecutionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mesFailureReason' - Contains the reason a monitoring job failed, if it failed.
--
-- * 'mesEndpointName' - The name of teh endpoint used to run the monitoring job.
--
-- * 'mesProcessingJobARN' - The Amazon Resource Name (ARN) of the monitoring job.
--
-- * 'mesMonitoringScheduleName' - The name of the monitoring schedule.
--
-- * 'mesScheduledTime' - The time the monitoring job was scheduled.
--
-- * 'mesCreationTime' - The time at which the monitoring job was created.
--
-- * 'mesLastModifiedTime' - A timestamp that indicates the last time the monitoring job was modified.
--
-- * 'mesMonitoringExecutionStatus' - The status of the monitoring job.
monitoringExecutionSummary ::
  -- | 'mesMonitoringScheduleName'
  Text ->
  -- | 'mesScheduledTime'
  UTCTime ->
  -- | 'mesCreationTime'
  UTCTime ->
  -- | 'mesLastModifiedTime'
  UTCTime ->
  -- | 'mesMonitoringExecutionStatus'
  ExecutionStatus ->
  MonitoringExecutionSummary
monitoringExecutionSummary
  pMonitoringScheduleName_
  pScheduledTime_
  pCreationTime_
  pLastModifiedTime_
  pMonitoringExecutionStatus_ =
    MonitoringExecutionSummary'
      { _mesFailureReason = Nothing,
        _mesEndpointName = Nothing,
        _mesProcessingJobARN = Nothing,
        _mesMonitoringScheduleName = pMonitoringScheduleName_,
        _mesScheduledTime = _Time # pScheduledTime_,
        _mesCreationTime = _Time # pCreationTime_,
        _mesLastModifiedTime = _Time # pLastModifiedTime_,
        _mesMonitoringExecutionStatus = pMonitoringExecutionStatus_
      }

-- | Contains the reason a monitoring job failed, if it failed.
mesFailureReason :: Lens' MonitoringExecutionSummary (Maybe Text)
mesFailureReason = lens _mesFailureReason (\s a -> s {_mesFailureReason = a})

-- | The name of teh endpoint used to run the monitoring job.
mesEndpointName :: Lens' MonitoringExecutionSummary (Maybe Text)
mesEndpointName = lens _mesEndpointName (\s a -> s {_mesEndpointName = a})

-- | The Amazon Resource Name (ARN) of the monitoring job.
mesProcessingJobARN :: Lens' MonitoringExecutionSummary (Maybe Text)
mesProcessingJobARN = lens _mesProcessingJobARN (\s a -> s {_mesProcessingJobARN = a})

-- | The name of the monitoring schedule.
mesMonitoringScheduleName :: Lens' MonitoringExecutionSummary Text
mesMonitoringScheduleName = lens _mesMonitoringScheduleName (\s a -> s {_mesMonitoringScheduleName = a})

-- | The time the monitoring job was scheduled.
mesScheduledTime :: Lens' MonitoringExecutionSummary UTCTime
mesScheduledTime = lens _mesScheduledTime (\s a -> s {_mesScheduledTime = a}) . _Time

-- | The time at which the monitoring job was created.
mesCreationTime :: Lens' MonitoringExecutionSummary UTCTime
mesCreationTime = lens _mesCreationTime (\s a -> s {_mesCreationTime = a}) . _Time

-- | A timestamp that indicates the last time the monitoring job was modified.
mesLastModifiedTime :: Lens' MonitoringExecutionSummary UTCTime
mesLastModifiedTime = lens _mesLastModifiedTime (\s a -> s {_mesLastModifiedTime = a}) . _Time

-- | The status of the monitoring job.
mesMonitoringExecutionStatus :: Lens' MonitoringExecutionSummary ExecutionStatus
mesMonitoringExecutionStatus = lens _mesMonitoringExecutionStatus (\s a -> s {_mesMonitoringExecutionStatus = a})

instance FromJSON MonitoringExecutionSummary where
  parseJSON =
    withObject
      "MonitoringExecutionSummary"
      ( \x ->
          MonitoringExecutionSummary'
            <$> (x .:? "FailureReason")
            <*> (x .:? "EndpointName")
            <*> (x .:? "ProcessingJobArn")
            <*> (x .: "MonitoringScheduleName")
            <*> (x .: "ScheduledTime")
            <*> (x .: "CreationTime")
            <*> (x .: "LastModifiedTime")
            <*> (x .: "MonitoringExecutionStatus")
      )

instance Hashable MonitoringExecutionSummary

instance NFData MonitoringExecutionSummary
