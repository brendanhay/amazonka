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
-- Module      : Network.AWS.IoT.DescribeAuditMitigationActionsTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an audit mitigation task that is used to apply mitigation actions to a set of audit findings. Properties include the actions being applied, the audit checks to which they're being applied, the task status, and aggregated task statistics.
module Network.AWS.IoT.DescribeAuditMitigationActionsTask
  ( -- * Creating a Request
    describeAuditMitigationActionsTask,
    DescribeAuditMitigationActionsTask,

    -- * Request Lenses
    damatTaskId,

    -- * Destructuring the Response
    describeAuditMitigationActionsTaskResponse,
    DescribeAuditMitigationActionsTaskResponse,

    -- * Response Lenses
    damatrsStartTime,
    damatrsTaskStatistics,
    damatrsActionsDefinition,
    damatrsAuditCheckToActionsMapping,
    damatrsEndTime,
    damatrsTarget,
    damatrsTaskStatus,
    damatrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAuditMitigationActionsTask' smart constructor.
newtype DescribeAuditMitigationActionsTask = DescribeAuditMitigationActionsTask'
  { _damatTaskId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAuditMitigationActionsTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'damatTaskId' - The unique identifier for the audit mitigation task.
describeAuditMitigationActionsTask ::
  -- | 'damatTaskId'
  Text ->
  DescribeAuditMitigationActionsTask
describeAuditMitigationActionsTask pTaskId_ =
  DescribeAuditMitigationActionsTask' {_damatTaskId = pTaskId_}

-- | The unique identifier for the audit mitigation task.
damatTaskId :: Lens' DescribeAuditMitigationActionsTask Text
damatTaskId = lens _damatTaskId (\s a -> s {_damatTaskId = a})

instance AWSRequest DescribeAuditMitigationActionsTask where
  type
    Rs DescribeAuditMitigationActionsTask =
      DescribeAuditMitigationActionsTaskResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeAuditMitigationActionsTaskResponse'
            <$> (x .?> "startTime")
            <*> (x .?> "taskStatistics" .!@ mempty)
            <*> (x .?> "actionsDefinition" .!@ mempty)
            <*> (x .?> "auditCheckToActionsMapping" .!@ mempty)
            <*> (x .?> "endTime")
            <*> (x .?> "target")
            <*> (x .?> "taskStatus")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAuditMitigationActionsTask

instance NFData DescribeAuditMitigationActionsTask

instance ToHeaders DescribeAuditMitigationActionsTask where
  toHeaders = const mempty

instance ToPath DescribeAuditMitigationActionsTask where
  toPath DescribeAuditMitigationActionsTask' {..} =
    mconcat ["/audit/mitigationactions/tasks/", toBS _damatTaskId]

instance ToQuery DescribeAuditMitigationActionsTask where
  toQuery = const mempty

-- | /See:/ 'describeAuditMitigationActionsTaskResponse' smart constructor.
data DescribeAuditMitigationActionsTaskResponse = DescribeAuditMitigationActionsTaskResponse'
  { _damatrsStartTime ::
      !( Maybe
           POSIX
       ),
    _damatrsTaskStatistics ::
      !( Maybe
           ( Map
               Text
               (TaskStatisticsForAuditCheck)
           )
       ),
    _damatrsActionsDefinition ::
      !( Maybe
           [MitigationAction]
       ),
    _damatrsAuditCheckToActionsMapping ::
      !( Maybe
           ( Map
               Text
               ( List1
                   Text
               )
           )
       ),
    _damatrsEndTime ::
      !( Maybe
           POSIX
       ),
    _damatrsTarget ::
      !( Maybe
           AuditMitigationActionsTaskTarget
       ),
    _damatrsTaskStatus ::
      !( Maybe
           AuditMitigationActionsTaskStatus
       ),
    _damatrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeAuditMitigationActionsTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'damatrsStartTime' - The date and time when the task was started.
--
-- * 'damatrsTaskStatistics' - Aggregate counts of the results when the mitigation tasks were applied to the findings for this audit mitigation actions task.
--
-- * 'damatrsActionsDefinition' - Specifies the mitigation actions and their parameters that are applied as part of this task.
--
-- * 'damatrsAuditCheckToActionsMapping' - Specifies the mitigation actions that should be applied to specific audit checks.
--
-- * 'damatrsEndTime' - The date and time when the task was completed or canceled.
--
-- * 'damatrsTarget' - Identifies the findings to which the mitigation actions are applied. This can be by audit checks, by audit task, or a set of findings.
--
-- * 'damatrsTaskStatus' - The current status of the task.
--
-- * 'damatrsResponseStatus' - -- | The response status code.
describeAuditMitigationActionsTaskResponse ::
  -- | 'damatrsResponseStatus'
  Int ->
  DescribeAuditMitigationActionsTaskResponse
describeAuditMitigationActionsTaskResponse pResponseStatus_ =
  DescribeAuditMitigationActionsTaskResponse'
    { _damatrsStartTime =
        Nothing,
      _damatrsTaskStatistics = Nothing,
      _damatrsActionsDefinition = Nothing,
      _damatrsAuditCheckToActionsMapping = Nothing,
      _damatrsEndTime = Nothing,
      _damatrsTarget = Nothing,
      _damatrsTaskStatus = Nothing,
      _damatrsResponseStatus = pResponseStatus_
    }

-- | The date and time when the task was started.
damatrsStartTime :: Lens' DescribeAuditMitigationActionsTaskResponse (Maybe UTCTime)
damatrsStartTime = lens _damatrsStartTime (\s a -> s {_damatrsStartTime = a}) . mapping _Time

-- | Aggregate counts of the results when the mitigation tasks were applied to the findings for this audit mitigation actions task.
damatrsTaskStatistics :: Lens' DescribeAuditMitigationActionsTaskResponse (HashMap Text (TaskStatisticsForAuditCheck))
damatrsTaskStatistics = lens _damatrsTaskStatistics (\s a -> s {_damatrsTaskStatistics = a}) . _Default . _Map

-- | Specifies the mitigation actions and their parameters that are applied as part of this task.
damatrsActionsDefinition :: Lens' DescribeAuditMitigationActionsTaskResponse [MitigationAction]
damatrsActionsDefinition = lens _damatrsActionsDefinition (\s a -> s {_damatrsActionsDefinition = a}) . _Default . _Coerce

-- | Specifies the mitigation actions that should be applied to specific audit checks.
damatrsAuditCheckToActionsMapping :: Lens' DescribeAuditMitigationActionsTaskResponse (HashMap Text (NonEmpty Text))
damatrsAuditCheckToActionsMapping = lens _damatrsAuditCheckToActionsMapping (\s a -> s {_damatrsAuditCheckToActionsMapping = a}) . _Default . _Map

-- | The date and time when the task was completed or canceled.
damatrsEndTime :: Lens' DescribeAuditMitigationActionsTaskResponse (Maybe UTCTime)
damatrsEndTime = lens _damatrsEndTime (\s a -> s {_damatrsEndTime = a}) . mapping _Time

-- | Identifies the findings to which the mitigation actions are applied. This can be by audit checks, by audit task, or a set of findings.
damatrsTarget :: Lens' DescribeAuditMitigationActionsTaskResponse (Maybe AuditMitigationActionsTaskTarget)
damatrsTarget = lens _damatrsTarget (\s a -> s {_damatrsTarget = a})

-- | The current status of the task.
damatrsTaskStatus :: Lens' DescribeAuditMitigationActionsTaskResponse (Maybe AuditMitigationActionsTaskStatus)
damatrsTaskStatus = lens _damatrsTaskStatus (\s a -> s {_damatrsTaskStatus = a})

-- | -- | The response status code.
damatrsResponseStatus :: Lens' DescribeAuditMitigationActionsTaskResponse Int
damatrsResponseStatus = lens _damatrsResponseStatus (\s a -> s {_damatrsResponseStatus = a})

instance NFData DescribeAuditMitigationActionsTaskResponse
