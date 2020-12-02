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
-- Module      : Network.AWS.SageMaker.ListMonitoringSchedules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of all monitoring schedules.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListMonitoringSchedules
  ( -- * Creating a Request
    listMonitoringSchedules,
    ListMonitoringSchedules,

    -- * Request Lenses
    lmsNameContains,
    lmsEndpointName,
    lmsLastModifiedTimeBefore,
    lmsCreationTimeAfter,
    lmsNextToken,
    lmsSortOrder,
    lmsLastModifiedTimeAfter,
    lmsCreationTimeBefore,
    lmsStatusEquals,
    lmsMaxResults,
    lmsSortBy,

    -- * Destructuring the Response
    listMonitoringSchedulesResponse,
    ListMonitoringSchedulesResponse,

    -- * Response Lenses
    lmsrsNextToken,
    lmsrsResponseStatus,
    lmsrsMonitoringScheduleSummaries,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listMonitoringSchedules' smart constructor.
data ListMonitoringSchedules = ListMonitoringSchedules'
  { _lmsNameContains ::
      !(Maybe Text),
    _lmsEndpointName :: !(Maybe Text),
    _lmsLastModifiedTimeBefore ::
      !(Maybe POSIX),
    _lmsCreationTimeAfter :: !(Maybe POSIX),
    _lmsNextToken :: !(Maybe Text),
    _lmsSortOrder :: !(Maybe SortOrder),
    _lmsLastModifiedTimeAfter :: !(Maybe POSIX),
    _lmsCreationTimeBefore :: !(Maybe POSIX),
    _lmsStatusEquals :: !(Maybe ScheduleStatus),
    _lmsMaxResults :: !(Maybe Nat),
    _lmsSortBy ::
      !(Maybe MonitoringScheduleSortKey)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMonitoringSchedules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmsNameContains' - Filter for monitoring schedules whose name contains a specified string.
--
-- * 'lmsEndpointName' - Name of a specific endpoint to fetch schedules for.
--
-- * 'lmsLastModifiedTimeBefore' - A filter that returns only monitoring schedules modified before a specified time.
--
-- * 'lmsCreationTimeAfter' - A filter that returns only monitoring schedules created after a specified time.
--
-- * 'lmsNextToken' - The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
--
-- * 'lmsSortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
--
-- * 'lmsLastModifiedTimeAfter' - A filter that returns only monitoring schedules modified after a specified time.
--
-- * 'lmsCreationTimeBefore' - A filter that returns only monitoring schedules created before a specified time.
--
-- * 'lmsStatusEquals' - A filter that returns only monitoring schedules modified before a specified time.
--
-- * 'lmsMaxResults' - The maximum number of jobs to return in the response. The default value is 10.
--
-- * 'lmsSortBy' - Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
listMonitoringSchedules ::
  ListMonitoringSchedules
listMonitoringSchedules =
  ListMonitoringSchedules'
    { _lmsNameContains = Nothing,
      _lmsEndpointName = Nothing,
      _lmsLastModifiedTimeBefore = Nothing,
      _lmsCreationTimeAfter = Nothing,
      _lmsNextToken = Nothing,
      _lmsSortOrder = Nothing,
      _lmsLastModifiedTimeAfter = Nothing,
      _lmsCreationTimeBefore = Nothing,
      _lmsStatusEquals = Nothing,
      _lmsMaxResults = Nothing,
      _lmsSortBy = Nothing
    }

-- | Filter for monitoring schedules whose name contains a specified string.
lmsNameContains :: Lens' ListMonitoringSchedules (Maybe Text)
lmsNameContains = lens _lmsNameContains (\s a -> s {_lmsNameContains = a})

-- | Name of a specific endpoint to fetch schedules for.
lmsEndpointName :: Lens' ListMonitoringSchedules (Maybe Text)
lmsEndpointName = lens _lmsEndpointName (\s a -> s {_lmsEndpointName = a})

-- | A filter that returns only monitoring schedules modified before a specified time.
lmsLastModifiedTimeBefore :: Lens' ListMonitoringSchedules (Maybe UTCTime)
lmsLastModifiedTimeBefore = lens _lmsLastModifiedTimeBefore (\s a -> s {_lmsLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only monitoring schedules created after a specified time.
lmsCreationTimeAfter :: Lens' ListMonitoringSchedules (Maybe UTCTime)
lmsCreationTimeAfter = lens _lmsCreationTimeAfter (\s a -> s {_lmsCreationTimeAfter = a}) . mapping _Time

-- | The token returned if the response is truncated. To retrieve the next set of job executions, use it in the next request.
lmsNextToken :: Lens' ListMonitoringSchedules (Maybe Text)
lmsNextToken = lens _lmsNextToken (\s a -> s {_lmsNextToken = a})

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The default is @Descending@ .
lmsSortOrder :: Lens' ListMonitoringSchedules (Maybe SortOrder)
lmsSortOrder = lens _lmsSortOrder (\s a -> s {_lmsSortOrder = a})

-- | A filter that returns only monitoring schedules modified after a specified time.
lmsLastModifiedTimeAfter :: Lens' ListMonitoringSchedules (Maybe UTCTime)
lmsLastModifiedTimeAfter = lens _lmsLastModifiedTimeAfter (\s a -> s {_lmsLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only monitoring schedules created before a specified time.
lmsCreationTimeBefore :: Lens' ListMonitoringSchedules (Maybe UTCTime)
lmsCreationTimeBefore = lens _lmsCreationTimeBefore (\s a -> s {_lmsCreationTimeBefore = a}) . mapping _Time

-- | A filter that returns only monitoring schedules modified before a specified time.
lmsStatusEquals :: Lens' ListMonitoringSchedules (Maybe ScheduleStatus)
lmsStatusEquals = lens _lmsStatusEquals (\s a -> s {_lmsStatusEquals = a})

-- | The maximum number of jobs to return in the response. The default value is 10.
lmsMaxResults :: Lens' ListMonitoringSchedules (Maybe Natural)
lmsMaxResults = lens _lmsMaxResults (\s a -> s {_lmsMaxResults = a}) . mapping _Nat

-- | Whether to sort results by @Status@ , @CreationTime@ , @ScheduledTime@ field. The default is @CreationTime@ .
lmsSortBy :: Lens' ListMonitoringSchedules (Maybe MonitoringScheduleSortKey)
lmsSortBy = lens _lmsSortBy (\s a -> s {_lmsSortBy = a})

instance AWSPager ListMonitoringSchedules where
  page rq rs
    | stop (rs ^. lmsrsNextToken) = Nothing
    | stop (rs ^. lmsrsMonitoringScheduleSummaries) = Nothing
    | otherwise = Just $ rq & lmsNextToken .~ rs ^. lmsrsNextToken

instance AWSRequest ListMonitoringSchedules where
  type Rs ListMonitoringSchedules = ListMonitoringSchedulesResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListMonitoringSchedulesResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "MonitoringScheduleSummaries" .!@ mempty)
      )

instance Hashable ListMonitoringSchedules

instance NFData ListMonitoringSchedules

instance ToHeaders ListMonitoringSchedules where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.ListMonitoringSchedules" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListMonitoringSchedules where
  toJSON ListMonitoringSchedules' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _lmsNameContains,
            ("EndpointName" .=) <$> _lmsEndpointName,
            ("LastModifiedTimeBefore" .=) <$> _lmsLastModifiedTimeBefore,
            ("CreationTimeAfter" .=) <$> _lmsCreationTimeAfter,
            ("NextToken" .=) <$> _lmsNextToken,
            ("SortOrder" .=) <$> _lmsSortOrder,
            ("LastModifiedTimeAfter" .=) <$> _lmsLastModifiedTimeAfter,
            ("CreationTimeBefore" .=) <$> _lmsCreationTimeBefore,
            ("StatusEquals" .=) <$> _lmsStatusEquals,
            ("MaxResults" .=) <$> _lmsMaxResults,
            ("SortBy" .=) <$> _lmsSortBy
          ]
      )

instance ToPath ListMonitoringSchedules where
  toPath = const "/"

instance ToQuery ListMonitoringSchedules where
  toQuery = const mempty

-- | /See:/ 'listMonitoringSchedulesResponse' smart constructor.
data ListMonitoringSchedulesResponse = ListMonitoringSchedulesResponse'
  { _lmsrsNextToken ::
      !(Maybe Text),
    _lmsrsResponseStatus ::
      !Int,
    _lmsrsMonitoringScheduleSummaries ::
      ![MonitoringScheduleSummary]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMonitoringSchedulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmsrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
--
-- * 'lmsrsResponseStatus' - -- | The response status code.
--
-- * 'lmsrsMonitoringScheduleSummaries' - A JSON array in which each element is a summary for a monitoring schedule.
listMonitoringSchedulesResponse ::
  -- | 'lmsrsResponseStatus'
  Int ->
  ListMonitoringSchedulesResponse
listMonitoringSchedulesResponse pResponseStatus_ =
  ListMonitoringSchedulesResponse'
    { _lmsrsNextToken = Nothing,
      _lmsrsResponseStatus = pResponseStatus_,
      _lmsrsMonitoringScheduleSummaries = mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of jobs, use it in the subsequent reques
lmsrsNextToken :: Lens' ListMonitoringSchedulesResponse (Maybe Text)
lmsrsNextToken = lens _lmsrsNextToken (\s a -> s {_lmsrsNextToken = a})

-- | -- | The response status code.
lmsrsResponseStatus :: Lens' ListMonitoringSchedulesResponse Int
lmsrsResponseStatus = lens _lmsrsResponseStatus (\s a -> s {_lmsrsResponseStatus = a})

-- | A JSON array in which each element is a summary for a monitoring schedule.
lmsrsMonitoringScheduleSummaries :: Lens' ListMonitoringSchedulesResponse [MonitoringScheduleSummary]
lmsrsMonitoringScheduleSummaries = lens _lmsrsMonitoringScheduleSummaries (\s a -> s {_lmsrsMonitoringScheduleSummaries = a}) . _Coerce

instance NFData ListMonitoringSchedulesResponse
