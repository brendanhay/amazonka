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
-- Module      : Network.AWS.SageMaker.ListMonitoringExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of all monitoring job executions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListMonitoringExecutions
  ( -- * Creating a Request
    listMonitoringExecutions,
    ListMonitoringExecutions,

    -- * Request Lenses
    lmeEndpointName,
    lmeLastModifiedTimeBefore,
    lmeScheduledTimeAfter,
    lmeCreationTimeAfter,
    lmeNextToken,
    lmeSortOrder,
    lmeLastModifiedTimeAfter,
    lmeCreationTimeBefore,
    lmeScheduledTimeBefore,
    lmeStatusEquals,
    lmeMonitoringScheduleName,
    lmeMaxResults,
    lmeSortBy,

    -- * Destructuring the Response
    listMonitoringExecutionsResponse,
    ListMonitoringExecutionsResponse,

    -- * Response Lenses
    lmersNextToken,
    lmersResponseStatus,
    lmersMonitoringExecutionSummaries,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listMonitoringExecutions' smart constructor.
data ListMonitoringExecutions = ListMonitoringExecutions'
  { _lmeEndpointName ::
      !(Maybe Text),
    _lmeLastModifiedTimeBefore ::
      !(Maybe POSIX),
    _lmeScheduledTimeAfter :: !(Maybe POSIX),
    _lmeCreationTimeAfter :: !(Maybe POSIX),
    _lmeNextToken :: !(Maybe Text),
    _lmeSortOrder :: !(Maybe SortOrder),
    _lmeLastModifiedTimeAfter ::
      !(Maybe POSIX),
    _lmeCreationTimeBefore :: !(Maybe POSIX),
    _lmeScheduledTimeBefore :: !(Maybe POSIX),
    _lmeStatusEquals ::
      !(Maybe ExecutionStatus),
    _lmeMonitoringScheduleName ::
      !(Maybe Text),
    _lmeMaxResults :: !(Maybe Nat),
    _lmeSortBy ::
      !(Maybe MonitoringExecutionSortKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMonitoringExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmeEndpointName' - Name of a specific endpoint to fetch jobs for.
--
-- * 'lmeLastModifiedTimeBefore' - A filter that returns only jobs modified after a specified time.
--
-- * 'lmeScheduledTimeAfter' - Filter for jobs scheduled after a specified time.
--
-- * 'lmeCreationTimeAfter' - A filter that returns only jobs created after a specified time.
--
-- * 'lmeNextToken' - The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
--
-- * 'lmeSortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
--
-- * 'lmeLastModifiedTimeAfter' - A filter that returns only jobs modified before a specified time.
--
-- * 'lmeCreationTimeBefore' - A filter that returns only jobs created before a specified time.
--
-- * 'lmeScheduledTimeBefore' - Filter for jobs scheduled before a specified time.
--
-- * 'lmeStatusEquals' - A filter that retrieves only jobs with a specific status.
--
-- * 'lmeMonitoringScheduleName' - Name of a specific schedule to fetch jobs for.
--
-- * 'lmeMaxResults' - The maximum number of jobs to return in the response. The default value is 10.
--
-- * 'lmeSortBy' - Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
listMonitoringExecutions ::
  ListMonitoringExecutions
listMonitoringExecutions =
  ListMonitoringExecutions'
    { _lmeEndpointName = Nothing,
      _lmeLastModifiedTimeBefore = Nothing,
      _lmeScheduledTimeAfter = Nothing,
      _lmeCreationTimeAfter = Nothing,
      _lmeNextToken = Nothing,
      _lmeSortOrder = Nothing,
      _lmeLastModifiedTimeAfter = Nothing,
      _lmeCreationTimeBefore = Nothing,
      _lmeScheduledTimeBefore = Nothing,
      _lmeStatusEquals = Nothing,
      _lmeMonitoringScheduleName = Nothing,
      _lmeMaxResults = Nothing,
      _lmeSortBy = Nothing
    }

-- | Name of a specific endpoint to fetch jobs for.
lmeEndpointName :: Lens' ListMonitoringExecutions (Maybe Text)
lmeEndpointName = lens _lmeEndpointName (\s a -> s {_lmeEndpointName = a})

-- | A filter that returns only jobs modified after a specified time.
lmeLastModifiedTimeBefore :: Lens' ListMonitoringExecutions (Maybe UTCTime)
lmeLastModifiedTimeBefore = lens _lmeLastModifiedTimeBefore (\s a -> s {_lmeLastModifiedTimeBefore = a}) . mapping _Time

-- | Filter for jobs scheduled after a specified time.
lmeScheduledTimeAfter :: Lens' ListMonitoringExecutions (Maybe UTCTime)
lmeScheduledTimeAfter = lens _lmeScheduledTimeAfter (\s a -> s {_lmeScheduledTimeAfter = a}) . mapping _Time

-- | A filter that returns only jobs created after a specified time.
lmeCreationTimeAfter :: Lens' ListMonitoringExecutions (Maybe UTCTime)
lmeCreationTimeAfter = lens _lmeCreationTimeAfter (\s a -> s {_lmeCreationTimeAfter = a}) . mapping _Time

-- | The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
lmeNextToken :: Lens' ListMonitoringExecutions (Maybe Text)
lmeNextToken = lens _lmeNextToken (\s a -> s {_lmeNextToken = a})

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
lmeSortOrder :: Lens' ListMonitoringExecutions (Maybe SortOrder)
lmeSortOrder = lens _lmeSortOrder (\s a -> s {_lmeSortOrder = a})

-- | A filter that returns only jobs modified before a specified time.
lmeLastModifiedTimeAfter :: Lens' ListMonitoringExecutions (Maybe UTCTime)
lmeLastModifiedTimeAfter = lens _lmeLastModifiedTimeAfter (\s a -> s {_lmeLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only jobs created before a specified time.
lmeCreationTimeBefore :: Lens' ListMonitoringExecutions (Maybe UTCTime)
lmeCreationTimeBefore = lens _lmeCreationTimeBefore (\s a -> s {_lmeCreationTimeBefore = a}) . mapping _Time

-- | Filter for jobs scheduled before a specified time.
lmeScheduledTimeBefore :: Lens' ListMonitoringExecutions (Maybe UTCTime)
lmeScheduledTimeBefore = lens _lmeScheduledTimeBefore (\s a -> s {_lmeScheduledTimeBefore = a}) . mapping _Time

-- | A filter that retrieves only jobs with a specific status.
lmeStatusEquals :: Lens' ListMonitoringExecutions (Maybe ExecutionStatus)
lmeStatusEquals = lens _lmeStatusEquals (\s a -> s {_lmeStatusEquals = a})

-- | Name of a specific schedule to fetch jobs for.
lmeMonitoringScheduleName :: Lens' ListMonitoringExecutions (Maybe Text)
lmeMonitoringScheduleName = lens _lmeMonitoringScheduleName (\s a -> s {_lmeMonitoringScheduleName = a})

-- | The maximum number of jobs to return in the response. The default value is 10.
lmeMaxResults :: Lens' ListMonitoringExecutions (Maybe Natural)
lmeMaxResults = lens _lmeMaxResults (\s a -> s {_lmeMaxResults = a}) . mapping _Nat

-- | Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
lmeSortBy :: Lens' ListMonitoringExecutions (Maybe MonitoringExecutionSortKey)
lmeSortBy = lens _lmeSortBy (\s a -> s {_lmeSortBy = a})

instance AWSPager ListMonitoringExecutions where
  page rq rs
    | stop (rs ^. lmersNextToken) = Nothing
    | stop (rs ^. lmersMonitoringExecutionSummaries) = Nothing
    | otherwise = Just $ rq & lmeNextToken .~ rs ^. lmersNextToken

instance AWSRequest ListMonitoringExecutions where
  type Rs ListMonitoringExecutions = ListMonitoringExecutionsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListMonitoringExecutionsResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "MonitoringExecutionSummaries" .!@ mempty)
      )

instance Hashable ListMonitoringExecutions

instance NFData ListMonitoringExecutions

instance ToHeaders ListMonitoringExecutions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.ListMonitoringExecutions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListMonitoringExecutions where
  toJSON ListMonitoringExecutions' {..} =
    object
      ( catMaybes
          [ ("EndpointName" .=) <$> _lmeEndpointName,
            ("LastModifiedTimeBefore" .=) <$> _lmeLastModifiedTimeBefore,
            ("ScheduledTimeAfter" .=) <$> _lmeScheduledTimeAfter,
            ("CreationTimeAfter" .=) <$> _lmeCreationTimeAfter,
            ("NextToken" .=) <$> _lmeNextToken,
            ("SortOrder" .=) <$> _lmeSortOrder,
            ("LastModifiedTimeAfter" .=) <$> _lmeLastModifiedTimeAfter,
            ("CreationTimeBefore" .=) <$> _lmeCreationTimeBefore,
            ("ScheduledTimeBefore" .=) <$> _lmeScheduledTimeBefore,
            ("StatusEquals" .=) <$> _lmeStatusEquals,
            ("MonitoringScheduleName" .=) <$> _lmeMonitoringScheduleName,
            ("MaxResults" .=) <$> _lmeMaxResults,
            ("SortBy" .=) <$> _lmeSortBy
          ]
      )

instance ToPath ListMonitoringExecutions where
  toPath = const "/"

instance ToQuery ListMonitoringExecutions where
  toQuery = const mempty

-- | /See:/ 'listMonitoringExecutionsResponse' smart constructor.
data ListMonitoringExecutionsResponse = ListMonitoringExecutionsResponse'
  { _lmersNextToken ::
      !(Maybe Text),
    _lmersResponseStatus ::
      !Int,
    _lmersMonitoringExecutionSummaries ::
      ![MonitoringExecutionSummary]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMonitoringExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmersNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
--
-- * 'lmersResponseStatus' - -- | The response status code.
--
-- * 'lmersMonitoringExecutionSummaries' - A JSON array in which each element is a summary for a monitoring execution.
listMonitoringExecutionsResponse ::
  -- | 'lmersResponseStatus'
  Int ->
  ListMonitoringExecutionsResponse
listMonitoringExecutionsResponse pResponseStatus_ =
  ListMonitoringExecutionsResponse'
    { _lmersNextToken = Nothing,
      _lmersResponseStatus = pResponseStatus_,
      _lmersMonitoringExecutionSummaries = mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
lmersNextToken :: Lens' ListMonitoringExecutionsResponse (Maybe Text)
lmersNextToken = lens _lmersNextToken (\s a -> s {_lmersNextToken = a})

-- | -- | The response status code.
lmersResponseStatus :: Lens' ListMonitoringExecutionsResponse Int
lmersResponseStatus = lens _lmersResponseStatus (\s a -> s {_lmersResponseStatus = a})

-- | A JSON array in which each element is a summary for a monitoring execution.
lmersMonitoringExecutionSummaries :: Lens' ListMonitoringExecutionsResponse [MonitoringExecutionSummary]
lmersMonitoringExecutionSummaries = lens _lmersMonitoringExecutionSummaries (\s a -> s {_lmersMonitoringExecutionSummaries = a}) . _Coerce

instance NFData ListMonitoringExecutionsResponse
