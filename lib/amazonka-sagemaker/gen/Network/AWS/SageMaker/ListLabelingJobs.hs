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
-- Module      : Network.AWS.SageMaker.ListLabelingJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of labeling jobs.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListLabelingJobs
  ( -- * Creating a Request
    listLabelingJobs,
    ListLabelingJobs,

    -- * Request Lenses
    lljNameContains,
    lljLastModifiedTimeBefore,
    lljCreationTimeAfter,
    lljNextToken,
    lljSortOrder,
    lljLastModifiedTimeAfter,
    lljCreationTimeBefore,
    lljStatusEquals,
    lljMaxResults,
    lljSortBy,

    -- * Destructuring the Response
    listLabelingJobsResponse,
    ListLabelingJobsResponse,

    -- * Response Lenses
    lljrsLabelingJobSummaryList,
    lljrsNextToken,
    lljrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listLabelingJobs' smart constructor.
data ListLabelingJobs = ListLabelingJobs'
  { _lljNameContains ::
      !(Maybe Text),
    _lljLastModifiedTimeBefore :: !(Maybe POSIX),
    _lljCreationTimeAfter :: !(Maybe POSIX),
    _lljNextToken :: !(Maybe Text),
    _lljSortOrder :: !(Maybe SortOrder),
    _lljLastModifiedTimeAfter :: !(Maybe POSIX),
    _lljCreationTimeBefore :: !(Maybe POSIX),
    _lljStatusEquals :: !(Maybe LabelingJobStatus),
    _lljMaxResults :: !(Maybe Nat),
    _lljSortBy :: !(Maybe SortBy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListLabelingJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lljNameContains' - A string in the labeling job name. This filter returns only labeling jobs whose name contains the specified string.
--
-- * 'lljLastModifiedTimeBefore' - A filter that returns only labeling jobs modified before the specified time (timestamp).
--
-- * 'lljCreationTimeAfter' - A filter that returns only labeling jobs created after the specified time (timestamp).
--
-- * 'lljNextToken' - If the result of the previous @ListLabelingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- * 'lljSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'lljLastModifiedTimeAfter' - A filter that returns only labeling jobs modified after the specified time (timestamp).
--
-- * 'lljCreationTimeBefore' - A filter that returns only labeling jobs created before the specified time (timestamp).
--
-- * 'lljStatusEquals' - A filter that retrieves only labeling jobs with a specific status.
--
-- * 'lljMaxResults' - The maximum number of labeling jobs to return in each page of the response.
--
-- * 'lljSortBy' - The field to sort results by. The default is @CreationTime@ .
listLabelingJobs ::
  ListLabelingJobs
listLabelingJobs =
  ListLabelingJobs'
    { _lljNameContains = Nothing,
      _lljLastModifiedTimeBefore = Nothing,
      _lljCreationTimeAfter = Nothing,
      _lljNextToken = Nothing,
      _lljSortOrder = Nothing,
      _lljLastModifiedTimeAfter = Nothing,
      _lljCreationTimeBefore = Nothing,
      _lljStatusEquals = Nothing,
      _lljMaxResults = Nothing,
      _lljSortBy = Nothing
    }

-- | A string in the labeling job name. This filter returns only labeling jobs whose name contains the specified string.
lljNameContains :: Lens' ListLabelingJobs (Maybe Text)
lljNameContains = lens _lljNameContains (\s a -> s {_lljNameContains = a})

-- | A filter that returns only labeling jobs modified before the specified time (timestamp).
lljLastModifiedTimeBefore :: Lens' ListLabelingJobs (Maybe UTCTime)
lljLastModifiedTimeBefore = lens _lljLastModifiedTimeBefore (\s a -> s {_lljLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only labeling jobs created after the specified time (timestamp).
lljCreationTimeAfter :: Lens' ListLabelingJobs (Maybe UTCTime)
lljCreationTimeAfter = lens _lljCreationTimeAfter (\s a -> s {_lljCreationTimeAfter = a}) . mapping _Time

-- | If the result of the previous @ListLabelingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
lljNextToken :: Lens' ListLabelingJobs (Maybe Text)
lljNextToken = lens _lljNextToken (\s a -> s {_lljNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
lljSortOrder :: Lens' ListLabelingJobs (Maybe SortOrder)
lljSortOrder = lens _lljSortOrder (\s a -> s {_lljSortOrder = a})

-- | A filter that returns only labeling jobs modified after the specified time (timestamp).
lljLastModifiedTimeAfter :: Lens' ListLabelingJobs (Maybe UTCTime)
lljLastModifiedTimeAfter = lens _lljLastModifiedTimeAfter (\s a -> s {_lljLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only labeling jobs created before the specified time (timestamp).
lljCreationTimeBefore :: Lens' ListLabelingJobs (Maybe UTCTime)
lljCreationTimeBefore = lens _lljCreationTimeBefore (\s a -> s {_lljCreationTimeBefore = a}) . mapping _Time

-- | A filter that retrieves only labeling jobs with a specific status.
lljStatusEquals :: Lens' ListLabelingJobs (Maybe LabelingJobStatus)
lljStatusEquals = lens _lljStatusEquals (\s a -> s {_lljStatusEquals = a})

-- | The maximum number of labeling jobs to return in each page of the response.
lljMaxResults :: Lens' ListLabelingJobs (Maybe Natural)
lljMaxResults = lens _lljMaxResults (\s a -> s {_lljMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @CreationTime@ .
lljSortBy :: Lens' ListLabelingJobs (Maybe SortBy)
lljSortBy = lens _lljSortBy (\s a -> s {_lljSortBy = a})

instance AWSPager ListLabelingJobs where
  page rq rs
    | stop (rs ^. lljrsNextToken) = Nothing
    | stop (rs ^. lljrsLabelingJobSummaryList) = Nothing
    | otherwise = Just $ rq & lljNextToken .~ rs ^. lljrsNextToken

instance AWSRequest ListLabelingJobs where
  type Rs ListLabelingJobs = ListLabelingJobsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListLabelingJobsResponse'
            <$> (x .?> "LabelingJobSummaryList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListLabelingJobs

instance NFData ListLabelingJobs

instance ToHeaders ListLabelingJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListLabelingJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListLabelingJobs where
  toJSON ListLabelingJobs' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _lljNameContains,
            ("LastModifiedTimeBefore" .=) <$> _lljLastModifiedTimeBefore,
            ("CreationTimeAfter" .=) <$> _lljCreationTimeAfter,
            ("NextToken" .=) <$> _lljNextToken,
            ("SortOrder" .=) <$> _lljSortOrder,
            ("LastModifiedTimeAfter" .=) <$> _lljLastModifiedTimeAfter,
            ("CreationTimeBefore" .=) <$> _lljCreationTimeBefore,
            ("StatusEquals" .=) <$> _lljStatusEquals,
            ("MaxResults" .=) <$> _lljMaxResults,
            ("SortBy" .=) <$> _lljSortBy
          ]
      )

instance ToPath ListLabelingJobs where
  toPath = const "/"

instance ToQuery ListLabelingJobs where
  toQuery = const mempty

-- | /See:/ 'listLabelingJobsResponse' smart constructor.
data ListLabelingJobsResponse = ListLabelingJobsResponse'
  { _lljrsLabelingJobSummaryList ::
      !(Maybe [LabelingJobSummary]),
    _lljrsNextToken :: !(Maybe Text),
    _lljrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListLabelingJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lljrsLabelingJobSummaryList' - An array of @LabelingJobSummary@ objects, each describing a labeling job.
--
-- * 'lljrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
--
-- * 'lljrsResponseStatus' - -- | The response status code.
listLabelingJobsResponse ::
  -- | 'lljrsResponseStatus'
  Int ->
  ListLabelingJobsResponse
listLabelingJobsResponse pResponseStatus_ =
  ListLabelingJobsResponse'
    { _lljrsLabelingJobSummaryList = Nothing,
      _lljrsNextToken = Nothing,
      _lljrsResponseStatus = pResponseStatus_
    }

-- | An array of @LabelingJobSummary@ objects, each describing a labeling job.
lljrsLabelingJobSummaryList :: Lens' ListLabelingJobsResponse [LabelingJobSummary]
lljrsLabelingJobSummaryList = lens _lljrsLabelingJobSummaryList (\s a -> s {_lljrsLabelingJobSummaryList = a}) . _Default . _Coerce

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of labeling jobs, use it in the subsequent request.
lljrsNextToken :: Lens' ListLabelingJobsResponse (Maybe Text)
lljrsNextToken = lens _lljrsNextToken (\s a -> s {_lljrsNextToken = a})

-- | -- | The response status code.
lljrsResponseStatus :: Lens' ListLabelingJobsResponse Int
lljrsResponseStatus = lens _lljrsResponseStatus (\s a -> s {_lljrsResponseStatus = a})

instance NFData ListLabelingJobsResponse
