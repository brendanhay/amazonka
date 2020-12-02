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
-- Module      : Network.AWS.IoT.ListAuditTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender audits that have been performed during a given time period.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditTasks
  ( -- * Creating a Request
    listAuditTasks,
    ListAuditTasks,

    -- * Request Lenses
    latTaskType,
    latNextToken,
    latMaxResults,
    latTaskStatus,
    latStartTime,
    latEndTime,

    -- * Destructuring the Response
    listAuditTasksResponse,
    ListAuditTasksResponse,

    -- * Response Lenses
    latrsTasks,
    latrsNextToken,
    latrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAuditTasks' smart constructor.
data ListAuditTasks = ListAuditTasks'
  { _latTaskType ::
      !(Maybe AuditTaskType),
    _latNextToken :: !(Maybe Text),
    _latMaxResults :: !(Maybe Nat),
    _latTaskStatus :: !(Maybe AuditTaskStatus),
    _latStartTime :: !POSIX,
    _latEndTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAuditTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'latTaskType' - A filter to limit the output to the specified type of audit: can be one of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED__AUDIT_TASK".
--
-- * 'latNextToken' - The token for the next set of results.
--
-- * 'latMaxResults' - The maximum number of results to return at one time. The default is 25.
--
-- * 'latTaskStatus' - A filter to limit the output to audits with the specified completion status: can be one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
--
-- * 'latStartTime' - The beginning of the time period. Audit information is retained for a limited time (90 days). Requesting a start time prior to what is retained results in an "InvalidRequestException".
--
-- * 'latEndTime' - The end of the time period.
listAuditTasks ::
  -- | 'latStartTime'
  UTCTime ->
  -- | 'latEndTime'
  UTCTime ->
  ListAuditTasks
listAuditTasks pStartTime_ pEndTime_ =
  ListAuditTasks'
    { _latTaskType = Nothing,
      _latNextToken = Nothing,
      _latMaxResults = Nothing,
      _latTaskStatus = Nothing,
      _latStartTime = _Time # pStartTime_,
      _latEndTime = _Time # pEndTime_
    }

-- | A filter to limit the output to the specified type of audit: can be one of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED__AUDIT_TASK".
latTaskType :: Lens' ListAuditTasks (Maybe AuditTaskType)
latTaskType = lens _latTaskType (\s a -> s {_latTaskType = a})

-- | The token for the next set of results.
latNextToken :: Lens' ListAuditTasks (Maybe Text)
latNextToken = lens _latNextToken (\s a -> s {_latNextToken = a})

-- | The maximum number of results to return at one time. The default is 25.
latMaxResults :: Lens' ListAuditTasks (Maybe Natural)
latMaxResults = lens _latMaxResults (\s a -> s {_latMaxResults = a}) . mapping _Nat

-- | A filter to limit the output to audits with the specified completion status: can be one of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
latTaskStatus :: Lens' ListAuditTasks (Maybe AuditTaskStatus)
latTaskStatus = lens _latTaskStatus (\s a -> s {_latTaskStatus = a})

-- | The beginning of the time period. Audit information is retained for a limited time (90 days). Requesting a start time prior to what is retained results in an "InvalidRequestException".
latStartTime :: Lens' ListAuditTasks UTCTime
latStartTime = lens _latStartTime (\s a -> s {_latStartTime = a}) . _Time

-- | The end of the time period.
latEndTime :: Lens' ListAuditTasks UTCTime
latEndTime = lens _latEndTime (\s a -> s {_latEndTime = a}) . _Time

instance AWSPager ListAuditTasks where
  page rq rs
    | stop (rs ^. latrsNextToken) = Nothing
    | stop (rs ^. latrsTasks) = Nothing
    | otherwise = Just $ rq & latNextToken .~ rs ^. latrsNextToken

instance AWSRequest ListAuditTasks where
  type Rs ListAuditTasks = ListAuditTasksResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListAuditTasksResponse'
            <$> (x .?> "tasks" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListAuditTasks

instance NFData ListAuditTasks

instance ToHeaders ListAuditTasks where
  toHeaders = const mempty

instance ToPath ListAuditTasks where
  toPath = const "/audit/tasks"

instance ToQuery ListAuditTasks where
  toQuery ListAuditTasks' {..} =
    mconcat
      [ "taskType" =: _latTaskType,
        "nextToken" =: _latNextToken,
        "maxResults" =: _latMaxResults,
        "taskStatus" =: _latTaskStatus,
        "startTime" =: _latStartTime,
        "endTime" =: _latEndTime
      ]

-- | /See:/ 'listAuditTasksResponse' smart constructor.
data ListAuditTasksResponse = ListAuditTasksResponse'
  { _latrsTasks ::
      !(Maybe [AuditTaskMetadata]),
    _latrsNextToken :: !(Maybe Text),
    _latrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAuditTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'latrsTasks' - The audits that were performed during the specified time period.
--
-- * 'latrsNextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- * 'latrsResponseStatus' - -- | The response status code.
listAuditTasksResponse ::
  -- | 'latrsResponseStatus'
  Int ->
  ListAuditTasksResponse
listAuditTasksResponse pResponseStatus_ =
  ListAuditTasksResponse'
    { _latrsTasks = Nothing,
      _latrsNextToken = Nothing,
      _latrsResponseStatus = pResponseStatus_
    }

-- | The audits that were performed during the specified time period.
latrsTasks :: Lens' ListAuditTasksResponse [AuditTaskMetadata]
latrsTasks = lens _latrsTasks (\s a -> s {_latrsTasks = a}) . _Default . _Coerce

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
latrsNextToken :: Lens' ListAuditTasksResponse (Maybe Text)
latrsNextToken = lens _latrsNextToken (\s a -> s {_latrsNextToken = a})

-- | -- | The response status code.
latrsResponseStatus :: Lens' ListAuditTasksResponse Int
latrsResponseStatus = lens _latrsResponseStatus (\s a -> s {_latrsResponseStatus = a})

instance NFData ListAuditTasksResponse
