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
-- Module      : Network.AWS.IoT.ListAuditMitigationActionsTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of audit mitigation action tasks that match the specified filters.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditMitigationActionsTasks
  ( -- * Creating a Request
    listAuditMitigationActionsTasks,
    ListAuditMitigationActionsTasks,

    -- * Request Lenses
    lamatAuditTaskId,
    lamatNextToken,
    lamatFindingId,
    lamatMaxResults,
    lamatTaskStatus,
    lamatStartTime,
    lamatEndTime,

    -- * Destructuring the Response
    listAuditMitigationActionsTasksResponse,
    ListAuditMitigationActionsTasksResponse,

    -- * Response Lenses
    lamatrsTasks,
    lamatrsNextToken,
    lamatrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAuditMitigationActionsTasks' smart constructor.
data ListAuditMitigationActionsTasks = ListAuditMitigationActionsTasks'
  { _lamatAuditTaskId ::
      !(Maybe Text),
    _lamatNextToken ::
      !(Maybe Text),
    _lamatFindingId ::
      !(Maybe Text),
    _lamatMaxResults ::
      !(Maybe Nat),
    _lamatTaskStatus ::
      !( Maybe
           AuditMitigationActionsTaskStatus
       ),
    _lamatStartTime :: !POSIX,
    _lamatEndTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAuditMitigationActionsTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lamatAuditTaskId' - Specify this filter to limit results to tasks that were applied to results for a specific audit.
--
-- * 'lamatNextToken' - The token for the next set of results.
--
-- * 'lamatFindingId' - Specify this filter to limit results to tasks that were applied to a specific audit finding.
--
-- * 'lamatMaxResults' - The maximum number of results to return at one time. The default is 25.
--
-- * 'lamatTaskStatus' - Specify this filter to limit results to tasks that are in a specific state.
--
-- * 'lamatStartTime' - Specify this filter to limit results to tasks that began on or after a specific date and time.
--
-- * 'lamatEndTime' - Specify this filter to limit results to tasks that were completed or canceled on or before a specific date and time.
listAuditMitigationActionsTasks ::
  -- | 'lamatStartTime'
  UTCTime ->
  -- | 'lamatEndTime'
  UTCTime ->
  ListAuditMitigationActionsTasks
listAuditMitigationActionsTasks pStartTime_ pEndTime_ =
  ListAuditMitigationActionsTasks'
    { _lamatAuditTaskId = Nothing,
      _lamatNextToken = Nothing,
      _lamatFindingId = Nothing,
      _lamatMaxResults = Nothing,
      _lamatTaskStatus = Nothing,
      _lamatStartTime = _Time # pStartTime_,
      _lamatEndTime = _Time # pEndTime_
    }

-- | Specify this filter to limit results to tasks that were applied to results for a specific audit.
lamatAuditTaskId :: Lens' ListAuditMitigationActionsTasks (Maybe Text)
lamatAuditTaskId = lens _lamatAuditTaskId (\s a -> s {_lamatAuditTaskId = a})

-- | The token for the next set of results.
lamatNextToken :: Lens' ListAuditMitigationActionsTasks (Maybe Text)
lamatNextToken = lens _lamatNextToken (\s a -> s {_lamatNextToken = a})

-- | Specify this filter to limit results to tasks that were applied to a specific audit finding.
lamatFindingId :: Lens' ListAuditMitigationActionsTasks (Maybe Text)
lamatFindingId = lens _lamatFindingId (\s a -> s {_lamatFindingId = a})

-- | The maximum number of results to return at one time. The default is 25.
lamatMaxResults :: Lens' ListAuditMitigationActionsTasks (Maybe Natural)
lamatMaxResults = lens _lamatMaxResults (\s a -> s {_lamatMaxResults = a}) . mapping _Nat

-- | Specify this filter to limit results to tasks that are in a specific state.
lamatTaskStatus :: Lens' ListAuditMitigationActionsTasks (Maybe AuditMitigationActionsTaskStatus)
lamatTaskStatus = lens _lamatTaskStatus (\s a -> s {_lamatTaskStatus = a})

-- | Specify this filter to limit results to tasks that began on or after a specific date and time.
lamatStartTime :: Lens' ListAuditMitigationActionsTasks UTCTime
lamatStartTime = lens _lamatStartTime (\s a -> s {_lamatStartTime = a}) . _Time

-- | Specify this filter to limit results to tasks that were completed or canceled on or before a specific date and time.
lamatEndTime :: Lens' ListAuditMitigationActionsTasks UTCTime
lamatEndTime = lens _lamatEndTime (\s a -> s {_lamatEndTime = a}) . _Time

instance AWSPager ListAuditMitigationActionsTasks where
  page rq rs
    | stop (rs ^. lamatrsNextToken) = Nothing
    | stop (rs ^. lamatrsTasks) = Nothing
    | otherwise = Just $ rq & lamatNextToken .~ rs ^. lamatrsNextToken

instance AWSRequest ListAuditMitigationActionsTasks where
  type
    Rs ListAuditMitigationActionsTasks =
      ListAuditMitigationActionsTasksResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListAuditMitigationActionsTasksResponse'
            <$> (x .?> "tasks" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListAuditMitigationActionsTasks

instance NFData ListAuditMitigationActionsTasks

instance ToHeaders ListAuditMitigationActionsTasks where
  toHeaders = const mempty

instance ToPath ListAuditMitigationActionsTasks where
  toPath = const "/audit/mitigationactions/tasks"

instance ToQuery ListAuditMitigationActionsTasks where
  toQuery ListAuditMitigationActionsTasks' {..} =
    mconcat
      [ "auditTaskId" =: _lamatAuditTaskId,
        "nextToken" =: _lamatNextToken,
        "findingId" =: _lamatFindingId,
        "maxResults" =: _lamatMaxResults,
        "taskStatus" =: _lamatTaskStatus,
        "startTime" =: _lamatStartTime,
        "endTime" =: _lamatEndTime
      ]

-- | /See:/ 'listAuditMitigationActionsTasksResponse' smart constructor.
data ListAuditMitigationActionsTasksResponse = ListAuditMitigationActionsTasksResponse'
  { _lamatrsTasks ::
      !( Maybe
           [AuditMitigationActionsTaskMetadata]
       ),
    _lamatrsNextToken ::
      !( Maybe
           Text
       ),
    _lamatrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAuditMitigationActionsTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lamatrsTasks' - The collection of audit mitigation tasks that matched the filter criteria.
--
-- * 'lamatrsNextToken' - The token for the next set of results.
--
-- * 'lamatrsResponseStatus' - -- | The response status code.
listAuditMitigationActionsTasksResponse ::
  -- | 'lamatrsResponseStatus'
  Int ->
  ListAuditMitigationActionsTasksResponse
listAuditMitigationActionsTasksResponse pResponseStatus_ =
  ListAuditMitigationActionsTasksResponse'
    { _lamatrsTasks = Nothing,
      _lamatrsNextToken = Nothing,
      _lamatrsResponseStatus = pResponseStatus_
    }

-- | The collection of audit mitigation tasks that matched the filter criteria.
lamatrsTasks :: Lens' ListAuditMitigationActionsTasksResponse [AuditMitigationActionsTaskMetadata]
lamatrsTasks = lens _lamatrsTasks (\s a -> s {_lamatrsTasks = a}) . _Default . _Coerce

-- | The token for the next set of results.
lamatrsNextToken :: Lens' ListAuditMitigationActionsTasksResponse (Maybe Text)
lamatrsNextToken = lens _lamatrsNextToken (\s a -> s {_lamatrsNextToken = a})

-- | -- | The response status code.
lamatrsResponseStatus :: Lens' ListAuditMitigationActionsTasksResponse Int
lamatrsResponseStatus = lens _lamatrsResponseStatus (\s a -> s {_lamatrsResponseStatus = a})

instance NFData ListAuditMitigationActionsTasksResponse
