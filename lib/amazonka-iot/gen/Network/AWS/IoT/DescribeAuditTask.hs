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
-- Module      : Network.AWS.IoT.DescribeAuditTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender audit.
module Network.AWS.IoT.DescribeAuditTask
  ( -- * Creating a Request
    describeAuditTask,
    DescribeAuditTask,

    -- * Request Lenses
    datTaskId,

    -- * Destructuring the Response
    describeAuditTaskResponse,
    DescribeAuditTaskResponse,

    -- * Response Lenses
    datrsAuditDetails,
    datrsTaskType,
    datrsTaskStartTime,
    datrsTaskStatistics,
    datrsScheduledAuditName,
    datrsTaskStatus,
    datrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAuditTask' smart constructor.
newtype DescribeAuditTask = DescribeAuditTask' {_datTaskId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAuditTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datTaskId' - The ID of the audit whose information you want to get.
describeAuditTask ::
  -- | 'datTaskId'
  Text ->
  DescribeAuditTask
describeAuditTask pTaskId_ =
  DescribeAuditTask' {_datTaskId = pTaskId_}

-- | The ID of the audit whose information you want to get.
datTaskId :: Lens' DescribeAuditTask Text
datTaskId = lens _datTaskId (\s a -> s {_datTaskId = a})

instance AWSRequest DescribeAuditTask where
  type Rs DescribeAuditTask = DescribeAuditTaskResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeAuditTaskResponse'
            <$> (x .?> "auditDetails" .!@ mempty)
            <*> (x .?> "taskType")
            <*> (x .?> "taskStartTime")
            <*> (x .?> "taskStatistics")
            <*> (x .?> "scheduledAuditName")
            <*> (x .?> "taskStatus")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAuditTask

instance NFData DescribeAuditTask

instance ToHeaders DescribeAuditTask where
  toHeaders = const mempty

instance ToPath DescribeAuditTask where
  toPath DescribeAuditTask' {..} =
    mconcat ["/audit/tasks/", toBS _datTaskId]

instance ToQuery DescribeAuditTask where
  toQuery = const mempty

-- | /See:/ 'describeAuditTaskResponse' smart constructor.
data DescribeAuditTaskResponse = DescribeAuditTaskResponse'
  { _datrsAuditDetails ::
      !(Maybe (Map Text (AuditCheckDetails))),
    _datrsTaskType ::
      !(Maybe AuditTaskType),
    _datrsTaskStartTime :: !(Maybe POSIX),
    _datrsTaskStatistics ::
      !(Maybe TaskStatistics),
    _datrsScheduledAuditName ::
      !(Maybe Text),
    _datrsTaskStatus ::
      !(Maybe AuditTaskStatus),
    _datrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAuditTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datrsAuditDetails' - Detailed information about each check performed during this audit.
--
-- * 'datrsTaskType' - The type of audit: "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
--
-- * 'datrsTaskStartTime' - The time the audit started.
--
-- * 'datrsTaskStatistics' - Statistical information about the audit.
--
-- * 'datrsScheduledAuditName' - The name of the scheduled audit (only if the audit was a scheduled audit).
--
-- * 'datrsTaskStatus' - The status of the audit: one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
--
-- * 'datrsResponseStatus' - -- | The response status code.
describeAuditTaskResponse ::
  -- | 'datrsResponseStatus'
  Int ->
  DescribeAuditTaskResponse
describeAuditTaskResponse pResponseStatus_ =
  DescribeAuditTaskResponse'
    { _datrsAuditDetails = Nothing,
      _datrsTaskType = Nothing,
      _datrsTaskStartTime = Nothing,
      _datrsTaskStatistics = Nothing,
      _datrsScheduledAuditName = Nothing,
      _datrsTaskStatus = Nothing,
      _datrsResponseStatus = pResponseStatus_
    }

-- | Detailed information about each check performed during this audit.
datrsAuditDetails :: Lens' DescribeAuditTaskResponse (HashMap Text (AuditCheckDetails))
datrsAuditDetails = lens _datrsAuditDetails (\s a -> s {_datrsAuditDetails = a}) . _Default . _Map

-- | The type of audit: "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
datrsTaskType :: Lens' DescribeAuditTaskResponse (Maybe AuditTaskType)
datrsTaskType = lens _datrsTaskType (\s a -> s {_datrsTaskType = a})

-- | The time the audit started.
datrsTaskStartTime :: Lens' DescribeAuditTaskResponse (Maybe UTCTime)
datrsTaskStartTime = lens _datrsTaskStartTime (\s a -> s {_datrsTaskStartTime = a}) . mapping _Time

-- | Statistical information about the audit.
datrsTaskStatistics :: Lens' DescribeAuditTaskResponse (Maybe TaskStatistics)
datrsTaskStatistics = lens _datrsTaskStatistics (\s a -> s {_datrsTaskStatistics = a})

-- | The name of the scheduled audit (only if the audit was a scheduled audit).
datrsScheduledAuditName :: Lens' DescribeAuditTaskResponse (Maybe Text)
datrsScheduledAuditName = lens _datrsScheduledAuditName (\s a -> s {_datrsScheduledAuditName = a})

-- | The status of the audit: one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
datrsTaskStatus :: Lens' DescribeAuditTaskResponse (Maybe AuditTaskStatus)
datrsTaskStatus = lens _datrsTaskStatus (\s a -> s {_datrsTaskStatus = a})

-- | -- | The response status code.
datrsResponseStatus :: Lens' DescribeAuditTaskResponse Int
datrsResponseStatus = lens _datrsResponseStatus (\s a -> s {_datrsResponseStatus = a})

instance NFData DescribeAuditTaskResponse
