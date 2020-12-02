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
-- Module      : Network.AWS.SageMaker.ListAutoMLJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request a list of jobs.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAutoMLJobs
  ( -- * Creating a Request
    listAutoMLJobs,
    ListAutoMLJobs,

    -- * Request Lenses
    lamljNameContains,
    lamljLastModifiedTimeBefore,
    lamljCreationTimeAfter,
    lamljNextToken,
    lamljSortOrder,
    lamljLastModifiedTimeAfter,
    lamljCreationTimeBefore,
    lamljStatusEquals,
    lamljMaxResults,
    lamljSortBy,

    -- * Destructuring the Response
    listAutoMLJobsResponse,
    ListAutoMLJobsResponse,

    -- * Response Lenses
    lamljrsNextToken,
    lamljrsResponseStatus,
    lamljrsAutoMLJobSummaries,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listAutoMLJobs' smart constructor.
data ListAutoMLJobs = ListAutoMLJobs'
  { _lamljNameContains ::
      !(Maybe Text),
    _lamljLastModifiedTimeBefore :: !(Maybe POSIX),
    _lamljCreationTimeAfter :: !(Maybe POSIX),
    _lamljNextToken :: !(Maybe Text),
    _lamljSortOrder :: !(Maybe AutoMLSortOrder),
    _lamljLastModifiedTimeAfter :: !(Maybe POSIX),
    _lamljCreationTimeBefore :: !(Maybe POSIX),
    _lamljStatusEquals :: !(Maybe AutoMLJobStatus),
    _lamljMaxResults :: !(Maybe Nat),
    _lamljSortBy :: !(Maybe AutoMLSortBy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAutoMLJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lamljNameContains' - Request a list of jobs, using a search filter for name.
--
-- * 'lamljLastModifiedTimeBefore' - Request a list of jobs, using a filter for time.
--
-- * 'lamljCreationTimeAfter' - Request a list of jobs, using a filter for time.
--
-- * 'lamljNextToken' - If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- * 'lamljSortOrder' - The sort order for the results. The default is Descending.
--
-- * 'lamljLastModifiedTimeAfter' - Request a list of jobs, using a filter for time.
--
-- * 'lamljCreationTimeBefore' - Request a list of jobs, using a filter for time.
--
-- * 'lamljStatusEquals' - Request a list of jobs, using a filter for status.
--
-- * 'lamljMaxResults' - Request a list of jobs up to a specified limit.
--
-- * 'lamljSortBy' - The parameter by which to sort the results. The default is AutoMLJobName.
listAutoMLJobs ::
  ListAutoMLJobs
listAutoMLJobs =
  ListAutoMLJobs'
    { _lamljNameContains = Nothing,
      _lamljLastModifiedTimeBefore = Nothing,
      _lamljCreationTimeAfter = Nothing,
      _lamljNextToken = Nothing,
      _lamljSortOrder = Nothing,
      _lamljLastModifiedTimeAfter = Nothing,
      _lamljCreationTimeBefore = Nothing,
      _lamljStatusEquals = Nothing,
      _lamljMaxResults = Nothing,
      _lamljSortBy = Nothing
    }

-- | Request a list of jobs, using a search filter for name.
lamljNameContains :: Lens' ListAutoMLJobs (Maybe Text)
lamljNameContains = lens _lamljNameContains (\s a -> s {_lamljNameContains = a})

-- | Request a list of jobs, using a filter for time.
lamljLastModifiedTimeBefore :: Lens' ListAutoMLJobs (Maybe UTCTime)
lamljLastModifiedTimeBefore = lens _lamljLastModifiedTimeBefore (\s a -> s {_lamljLastModifiedTimeBefore = a}) . mapping _Time

-- | Request a list of jobs, using a filter for time.
lamljCreationTimeAfter :: Lens' ListAutoMLJobs (Maybe UTCTime)
lamljCreationTimeAfter = lens _lamljCreationTimeAfter (\s a -> s {_lamljCreationTimeAfter = a}) . mapping _Time

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
lamljNextToken :: Lens' ListAutoMLJobs (Maybe Text)
lamljNextToken = lens _lamljNextToken (\s a -> s {_lamljNextToken = a})

-- | The sort order for the results. The default is Descending.
lamljSortOrder :: Lens' ListAutoMLJobs (Maybe AutoMLSortOrder)
lamljSortOrder = lens _lamljSortOrder (\s a -> s {_lamljSortOrder = a})

-- | Request a list of jobs, using a filter for time.
lamljLastModifiedTimeAfter :: Lens' ListAutoMLJobs (Maybe UTCTime)
lamljLastModifiedTimeAfter = lens _lamljLastModifiedTimeAfter (\s a -> s {_lamljLastModifiedTimeAfter = a}) . mapping _Time

-- | Request a list of jobs, using a filter for time.
lamljCreationTimeBefore :: Lens' ListAutoMLJobs (Maybe UTCTime)
lamljCreationTimeBefore = lens _lamljCreationTimeBefore (\s a -> s {_lamljCreationTimeBefore = a}) . mapping _Time

-- | Request a list of jobs, using a filter for status.
lamljStatusEquals :: Lens' ListAutoMLJobs (Maybe AutoMLJobStatus)
lamljStatusEquals = lens _lamljStatusEquals (\s a -> s {_lamljStatusEquals = a})

-- | Request a list of jobs up to a specified limit.
lamljMaxResults :: Lens' ListAutoMLJobs (Maybe Natural)
lamljMaxResults = lens _lamljMaxResults (\s a -> s {_lamljMaxResults = a}) . mapping _Nat

-- | The parameter by which to sort the results. The default is AutoMLJobName.
lamljSortBy :: Lens' ListAutoMLJobs (Maybe AutoMLSortBy)
lamljSortBy = lens _lamljSortBy (\s a -> s {_lamljSortBy = a})

instance AWSPager ListAutoMLJobs where
  page rq rs
    | stop (rs ^. lamljrsNextToken) = Nothing
    | stop (rs ^. lamljrsAutoMLJobSummaries) = Nothing
    | otherwise = Just $ rq & lamljNextToken .~ rs ^. lamljrsNextToken

instance AWSRequest ListAutoMLJobs where
  type Rs ListAutoMLJobs = ListAutoMLJobsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListAutoMLJobsResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "AutoMLJobSummaries" .!@ mempty)
      )

instance Hashable ListAutoMLJobs

instance NFData ListAutoMLJobs

instance ToHeaders ListAutoMLJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListAutoMLJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListAutoMLJobs where
  toJSON ListAutoMLJobs' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _lamljNameContains,
            ("LastModifiedTimeBefore" .=) <$> _lamljLastModifiedTimeBefore,
            ("CreationTimeAfter" .=) <$> _lamljCreationTimeAfter,
            ("NextToken" .=) <$> _lamljNextToken,
            ("SortOrder" .=) <$> _lamljSortOrder,
            ("LastModifiedTimeAfter" .=) <$> _lamljLastModifiedTimeAfter,
            ("CreationTimeBefore" .=) <$> _lamljCreationTimeBefore,
            ("StatusEquals" .=) <$> _lamljStatusEquals,
            ("MaxResults" .=) <$> _lamljMaxResults,
            ("SortBy" .=) <$> _lamljSortBy
          ]
      )

instance ToPath ListAutoMLJobs where
  toPath = const "/"

instance ToQuery ListAutoMLJobs where
  toQuery = const mempty

-- | /See:/ 'listAutoMLJobsResponse' smart constructor.
data ListAutoMLJobsResponse = ListAutoMLJobsResponse'
  { _lamljrsNextToken ::
      !(Maybe Text),
    _lamljrsResponseStatus :: !Int,
    _lamljrsAutoMLJobSummaries ::
      ![AutoMLJobSummary]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAutoMLJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lamljrsNextToken' - If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
--
-- * 'lamljrsResponseStatus' - -- | The response status code.
--
-- * 'lamljrsAutoMLJobSummaries' - Returns a summary list of jobs.
listAutoMLJobsResponse ::
  -- | 'lamljrsResponseStatus'
  Int ->
  ListAutoMLJobsResponse
listAutoMLJobsResponse pResponseStatus_ =
  ListAutoMLJobsResponse'
    { _lamljrsNextToken = Nothing,
      _lamljrsResponseStatus = pResponseStatus_,
      _lamljrsAutoMLJobSummaries = mempty
    }

-- | If the previous response was truncated, you receive this token. Use it in your next request to receive the next set of results.
lamljrsNextToken :: Lens' ListAutoMLJobsResponse (Maybe Text)
lamljrsNextToken = lens _lamljrsNextToken (\s a -> s {_lamljrsNextToken = a})

-- | -- | The response status code.
lamljrsResponseStatus :: Lens' ListAutoMLJobsResponse Int
lamljrsResponseStatus = lens _lamljrsResponseStatus (\s a -> s {_lamljrsResponseStatus = a})

-- | Returns a summary list of jobs.
lamljrsAutoMLJobSummaries :: Lens' ListAutoMLJobsResponse [AutoMLJobSummary]
lamljrsAutoMLJobSummaries = lens _lamljrsAutoMLJobSummaries (\s a -> s {_lamljrsAutoMLJobSummaries = a}) . _Coerce

instance NFData ListAutoMLJobsResponse
