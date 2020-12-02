{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeMonitoringSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the schedule for a monitoring job.
module Network.AWS.SageMaker.DescribeMonitoringSchedule
  ( -- * Creating a Request
    describeMonitoringSchedule,
    DescribeMonitoringSchedule,

    -- * Request Lenses
    dmsMonitoringScheduleName,

    -- * Destructuring the Response
    describeMonitoringScheduleResponse,
    DescribeMonitoringScheduleResponse,

    -- * Response Lenses
    dmsrsFailureReason,
    dmsrsEndpointName,
    dmsrsLastMonitoringExecutionSummary,
    dmsrsResponseStatus,
    dmsrsMonitoringScheduleARN,
    dmsrsMonitoringScheduleName,
    dmsrsMonitoringScheduleStatus,
    dmsrsCreationTime,
    dmsrsLastModifiedTime,
    dmsrsMonitoringScheduleConfig,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeMonitoringSchedule' smart constructor.
newtype DescribeMonitoringSchedule = DescribeMonitoringSchedule'
  { _dmsMonitoringScheduleName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMonitoringSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmsMonitoringScheduleName' - Name of a previously created monitoring schedule.
describeMonitoringSchedule ::
  -- | 'dmsMonitoringScheduleName'
  Text ->
  DescribeMonitoringSchedule
describeMonitoringSchedule pMonitoringScheduleName_ =
  DescribeMonitoringSchedule'
    { _dmsMonitoringScheduleName =
        pMonitoringScheduleName_
    }

-- | Name of a previously created monitoring schedule.
dmsMonitoringScheduleName :: Lens' DescribeMonitoringSchedule Text
dmsMonitoringScheduleName = lens _dmsMonitoringScheduleName (\s a -> s {_dmsMonitoringScheduleName = a})

instance AWSRequest DescribeMonitoringSchedule where
  type
    Rs DescribeMonitoringSchedule =
      DescribeMonitoringScheduleResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeMonitoringScheduleResponse'
            <$> (x .?> "FailureReason")
            <*> (x .?> "EndpointName")
            <*> (x .?> "LastMonitoringExecutionSummary")
            <*> (pure (fromEnum s))
            <*> (x .:> "MonitoringScheduleArn")
            <*> (x .:> "MonitoringScheduleName")
            <*> (x .:> "MonitoringScheduleStatus")
            <*> (x .:> "CreationTime")
            <*> (x .:> "LastModifiedTime")
            <*> (x .:> "MonitoringScheduleConfig")
      )

instance Hashable DescribeMonitoringSchedule

instance NFData DescribeMonitoringSchedule

instance ToHeaders DescribeMonitoringSchedule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DescribeMonitoringSchedule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeMonitoringSchedule where
  toJSON DescribeMonitoringSchedule' {..} =
    object
      ( catMaybes
          [Just ("MonitoringScheduleName" .= _dmsMonitoringScheduleName)]
      )

instance ToPath DescribeMonitoringSchedule where
  toPath = const "/"

instance ToQuery DescribeMonitoringSchedule where
  toQuery = const mempty

-- | /See:/ 'describeMonitoringScheduleResponse' smart constructor.
data DescribeMonitoringScheduleResponse = DescribeMonitoringScheduleResponse'
  { _dmsrsFailureReason ::
      !(Maybe Text),
    _dmsrsEndpointName ::
      !(Maybe Text),
    _dmsrsLastMonitoringExecutionSummary ::
      !( Maybe
           MonitoringExecutionSummary
       ),
    _dmsrsResponseStatus ::
      !Int,
    _dmsrsMonitoringScheduleARN ::
      !Text,
    _dmsrsMonitoringScheduleName ::
      !Text,
    _dmsrsMonitoringScheduleStatus ::
      !ScheduleStatus,
    _dmsrsCreationTime ::
      !POSIX,
    _dmsrsLastModifiedTime ::
      !POSIX,
    _dmsrsMonitoringScheduleConfig ::
      !MonitoringScheduleConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMonitoringScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmsrsFailureReason' - A string, up to one KB in size, that contains the reason a monitoring job failed, if it failed.
--
-- * 'dmsrsEndpointName' - The name of the endpoint for the monitoring job.
--
-- * 'dmsrsLastMonitoringExecutionSummary' - Describes metadata on the last execution to run, if there was one.
--
-- * 'dmsrsResponseStatus' - -- | The response status code.
--
-- * 'dmsrsMonitoringScheduleARN' - The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- * 'dmsrsMonitoringScheduleName' - Name of the monitoring schedule.
--
-- * 'dmsrsMonitoringScheduleStatus' - The status of an monitoring job.
--
-- * 'dmsrsCreationTime' - The time at which the monitoring job was created.
--
-- * 'dmsrsLastModifiedTime' - The time at which the monitoring job was last modified.
--
-- * 'dmsrsMonitoringScheduleConfig' - The configuration object that specifies the monitoring schedule and defines the monitoring job.
describeMonitoringScheduleResponse ::
  -- | 'dmsrsResponseStatus'
  Int ->
  -- | 'dmsrsMonitoringScheduleARN'
  Text ->
  -- | 'dmsrsMonitoringScheduleName'
  Text ->
  -- | 'dmsrsMonitoringScheduleStatus'
  ScheduleStatus ->
  -- | 'dmsrsCreationTime'
  UTCTime ->
  -- | 'dmsrsLastModifiedTime'
  UTCTime ->
  -- | 'dmsrsMonitoringScheduleConfig'
  MonitoringScheduleConfig ->
  DescribeMonitoringScheduleResponse
describeMonitoringScheduleResponse
  pResponseStatus_
  pMonitoringScheduleARN_
  pMonitoringScheduleName_
  pMonitoringScheduleStatus_
  pCreationTime_
  pLastModifiedTime_
  pMonitoringScheduleConfig_ =
    DescribeMonitoringScheduleResponse'
      { _dmsrsFailureReason =
          Nothing,
        _dmsrsEndpointName = Nothing,
        _dmsrsLastMonitoringExecutionSummary = Nothing,
        _dmsrsResponseStatus = pResponseStatus_,
        _dmsrsMonitoringScheduleARN = pMonitoringScheduleARN_,
        _dmsrsMonitoringScheduleName = pMonitoringScheduleName_,
        _dmsrsMonitoringScheduleStatus = pMonitoringScheduleStatus_,
        _dmsrsCreationTime = _Time # pCreationTime_,
        _dmsrsLastModifiedTime = _Time # pLastModifiedTime_,
        _dmsrsMonitoringScheduleConfig = pMonitoringScheduleConfig_
      }

-- | A string, up to one KB in size, that contains the reason a monitoring job failed, if it failed.
dmsrsFailureReason :: Lens' DescribeMonitoringScheduleResponse (Maybe Text)
dmsrsFailureReason = lens _dmsrsFailureReason (\s a -> s {_dmsrsFailureReason = a})

-- | The name of the endpoint for the monitoring job.
dmsrsEndpointName :: Lens' DescribeMonitoringScheduleResponse (Maybe Text)
dmsrsEndpointName = lens _dmsrsEndpointName (\s a -> s {_dmsrsEndpointName = a})

-- | Describes metadata on the last execution to run, if there was one.
dmsrsLastMonitoringExecutionSummary :: Lens' DescribeMonitoringScheduleResponse (Maybe MonitoringExecutionSummary)
dmsrsLastMonitoringExecutionSummary = lens _dmsrsLastMonitoringExecutionSummary (\s a -> s {_dmsrsLastMonitoringExecutionSummary = a})

-- | -- | The response status code.
dmsrsResponseStatus :: Lens' DescribeMonitoringScheduleResponse Int
dmsrsResponseStatus = lens _dmsrsResponseStatus (\s a -> s {_dmsrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
dmsrsMonitoringScheduleARN :: Lens' DescribeMonitoringScheduleResponse Text
dmsrsMonitoringScheduleARN = lens _dmsrsMonitoringScheduleARN (\s a -> s {_dmsrsMonitoringScheduleARN = a})

-- | Name of the monitoring schedule.
dmsrsMonitoringScheduleName :: Lens' DescribeMonitoringScheduleResponse Text
dmsrsMonitoringScheduleName = lens _dmsrsMonitoringScheduleName (\s a -> s {_dmsrsMonitoringScheduleName = a})

-- | The status of an monitoring job.
dmsrsMonitoringScheduleStatus :: Lens' DescribeMonitoringScheduleResponse ScheduleStatus
dmsrsMonitoringScheduleStatus = lens _dmsrsMonitoringScheduleStatus (\s a -> s {_dmsrsMonitoringScheduleStatus = a})

-- | The time at which the monitoring job was created.
dmsrsCreationTime :: Lens' DescribeMonitoringScheduleResponse UTCTime
dmsrsCreationTime = lens _dmsrsCreationTime (\s a -> s {_dmsrsCreationTime = a}) . _Time

-- | The time at which the monitoring job was last modified.
dmsrsLastModifiedTime :: Lens' DescribeMonitoringScheduleResponse UTCTime
dmsrsLastModifiedTime = lens _dmsrsLastModifiedTime (\s a -> s {_dmsrsLastModifiedTime = a}) . _Time

-- | The configuration object that specifies the monitoring schedule and defines the monitoring job.
dmsrsMonitoringScheduleConfig :: Lens' DescribeMonitoringScheduleResponse MonitoringScheduleConfig
dmsrsMonitoringScheduleConfig = lens _dmsrsMonitoringScheduleConfig (\s a -> s {_dmsrsMonitoringScheduleConfig = a})

instance NFData DescribeMonitoringScheduleResponse
