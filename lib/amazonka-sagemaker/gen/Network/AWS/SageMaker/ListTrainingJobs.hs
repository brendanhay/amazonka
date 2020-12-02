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
-- Module      : Network.AWS.SageMaker.ListTrainingJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists training jobs.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrainingJobs
  ( -- * Creating a Request
    listTrainingJobs,
    ListTrainingJobs,

    -- * Request Lenses
    ltjsNameContains,
    ltjsLastModifiedTimeBefore,
    ltjsCreationTimeAfter,
    ltjsNextToken,
    ltjsSortOrder,
    ltjsLastModifiedTimeAfter,
    ltjsCreationTimeBefore,
    ltjsStatusEquals,
    ltjsMaxResults,
    ltjsSortBy,

    -- * Destructuring the Response
    listTrainingJobsResponse,
    ListTrainingJobsResponse,

    -- * Response Lenses
    ltjrsNextToken,
    ltjrsResponseStatus,
    ltjrsTrainingJobSummaries,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listTrainingJobs' smart constructor.
data ListTrainingJobs = ListTrainingJobs'
  { _ltjsNameContains ::
      !(Maybe Text),
    _ltjsLastModifiedTimeBefore :: !(Maybe POSIX),
    _ltjsCreationTimeAfter :: !(Maybe POSIX),
    _ltjsNextToken :: !(Maybe Text),
    _ltjsSortOrder :: !(Maybe SortOrder),
    _ltjsLastModifiedTimeAfter :: !(Maybe POSIX),
    _ltjsCreationTimeBefore :: !(Maybe POSIX),
    _ltjsStatusEquals :: !(Maybe TrainingJobStatus),
    _ltjsMaxResults :: !(Maybe Nat),
    _ltjsSortBy :: !(Maybe SortBy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTrainingJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltjsNameContains' - A string in the training job name. This filter returns only training jobs whose name contains the specified string.
--
-- * 'ltjsLastModifiedTimeBefore' - A filter that returns only training jobs modified before the specified time (timestamp).
--
-- * 'ltjsCreationTimeAfter' - A filter that returns only training jobs created after the specified time (timestamp).
--
-- * 'ltjsNextToken' - If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- * 'ltjsSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'ltjsLastModifiedTimeAfter' - A filter that returns only training jobs modified after the specified time (timestamp).
--
-- * 'ltjsCreationTimeBefore' - A filter that returns only training jobs created before the specified time (timestamp).
--
-- * 'ltjsStatusEquals' - A filter that retrieves only training jobs with a specific status.
--
-- * 'ltjsMaxResults' - The maximum number of training jobs to return in the response.
--
-- * 'ltjsSortBy' - The field to sort results by. The default is @CreationTime@ .
listTrainingJobs ::
  ListTrainingJobs
listTrainingJobs =
  ListTrainingJobs'
    { _ltjsNameContains = Nothing,
      _ltjsLastModifiedTimeBefore = Nothing,
      _ltjsCreationTimeAfter = Nothing,
      _ltjsNextToken = Nothing,
      _ltjsSortOrder = Nothing,
      _ltjsLastModifiedTimeAfter = Nothing,
      _ltjsCreationTimeBefore = Nothing,
      _ltjsStatusEquals = Nothing,
      _ltjsMaxResults = Nothing,
      _ltjsSortBy = Nothing
    }

-- | A string in the training job name. This filter returns only training jobs whose name contains the specified string.
ltjsNameContains :: Lens' ListTrainingJobs (Maybe Text)
ltjsNameContains = lens _ltjsNameContains (\s a -> s {_ltjsNameContains = a})

-- | A filter that returns only training jobs modified before the specified time (timestamp).
ltjsLastModifiedTimeBefore :: Lens' ListTrainingJobs (Maybe UTCTime)
ltjsLastModifiedTimeBefore = lens _ltjsLastModifiedTimeBefore (\s a -> s {_ltjsLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only training jobs created after the specified time (timestamp).
ltjsCreationTimeAfter :: Lens' ListTrainingJobs (Maybe UTCTime)
ltjsCreationTimeAfter = lens _ltjsCreationTimeAfter (\s a -> s {_ltjsCreationTimeAfter = a}) . mapping _Time

-- | If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
ltjsNextToken :: Lens' ListTrainingJobs (Maybe Text)
ltjsNextToken = lens _ltjsNextToken (\s a -> s {_ltjsNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
ltjsSortOrder :: Lens' ListTrainingJobs (Maybe SortOrder)
ltjsSortOrder = lens _ltjsSortOrder (\s a -> s {_ltjsSortOrder = a})

-- | A filter that returns only training jobs modified after the specified time (timestamp).
ltjsLastModifiedTimeAfter :: Lens' ListTrainingJobs (Maybe UTCTime)
ltjsLastModifiedTimeAfter = lens _ltjsLastModifiedTimeAfter (\s a -> s {_ltjsLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only training jobs created before the specified time (timestamp).
ltjsCreationTimeBefore :: Lens' ListTrainingJobs (Maybe UTCTime)
ltjsCreationTimeBefore = lens _ltjsCreationTimeBefore (\s a -> s {_ltjsCreationTimeBefore = a}) . mapping _Time

-- | A filter that retrieves only training jobs with a specific status.
ltjsStatusEquals :: Lens' ListTrainingJobs (Maybe TrainingJobStatus)
ltjsStatusEquals = lens _ltjsStatusEquals (\s a -> s {_ltjsStatusEquals = a})

-- | The maximum number of training jobs to return in the response.
ltjsMaxResults :: Lens' ListTrainingJobs (Maybe Natural)
ltjsMaxResults = lens _ltjsMaxResults (\s a -> s {_ltjsMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @CreationTime@ .
ltjsSortBy :: Lens' ListTrainingJobs (Maybe SortBy)
ltjsSortBy = lens _ltjsSortBy (\s a -> s {_ltjsSortBy = a})

instance AWSPager ListTrainingJobs where
  page rq rs
    | stop (rs ^. ltjrsNextToken) = Nothing
    | stop (rs ^. ltjrsTrainingJobSummaries) = Nothing
    | otherwise = Just $ rq & ltjsNextToken .~ rs ^. ltjrsNextToken

instance AWSRequest ListTrainingJobs where
  type Rs ListTrainingJobs = ListTrainingJobsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListTrainingJobsResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "TrainingJobSummaries" .!@ mempty)
      )

instance Hashable ListTrainingJobs

instance NFData ListTrainingJobs

instance ToHeaders ListTrainingJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListTrainingJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListTrainingJobs where
  toJSON ListTrainingJobs' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _ltjsNameContains,
            ("LastModifiedTimeBefore" .=) <$> _ltjsLastModifiedTimeBefore,
            ("CreationTimeAfter" .=) <$> _ltjsCreationTimeAfter,
            ("NextToken" .=) <$> _ltjsNextToken,
            ("SortOrder" .=) <$> _ltjsSortOrder,
            ("LastModifiedTimeAfter" .=) <$> _ltjsLastModifiedTimeAfter,
            ("CreationTimeBefore" .=) <$> _ltjsCreationTimeBefore,
            ("StatusEquals" .=) <$> _ltjsStatusEquals,
            ("MaxResults" .=) <$> _ltjsMaxResults,
            ("SortBy" .=) <$> _ltjsSortBy
          ]
      )

instance ToPath ListTrainingJobs where
  toPath = const "/"

instance ToQuery ListTrainingJobs where
  toQuery = const mempty

-- | /See:/ 'listTrainingJobsResponse' smart constructor.
data ListTrainingJobsResponse = ListTrainingJobsResponse'
  { _ltjrsNextToken ::
      !(Maybe Text),
    _ltjrsResponseStatus :: !Int,
    _ltjrsTrainingJobSummaries ::
      ![TrainingJobSummary]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTrainingJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltjrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
--
-- * 'ltjrsResponseStatus' - -- | The response status code.
--
-- * 'ltjrsTrainingJobSummaries' - An array of @TrainingJobSummary@ objects, each listing a training job.
listTrainingJobsResponse ::
  -- | 'ltjrsResponseStatus'
  Int ->
  ListTrainingJobsResponse
listTrainingJobsResponse pResponseStatus_ =
  ListTrainingJobsResponse'
    { _ltjrsNextToken = Nothing,
      _ltjrsResponseStatus = pResponseStatus_,
      _ltjrsTrainingJobSummaries = mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
ltjrsNextToken :: Lens' ListTrainingJobsResponse (Maybe Text)
ltjrsNextToken = lens _ltjrsNextToken (\s a -> s {_ltjrsNextToken = a})

-- | -- | The response status code.
ltjrsResponseStatus :: Lens' ListTrainingJobsResponse Int
ltjrsResponseStatus = lens _ltjrsResponseStatus (\s a -> s {_ltjrsResponseStatus = a})

-- | An array of @TrainingJobSummary@ objects, each listing a training job.
ltjrsTrainingJobSummaries :: Lens' ListTrainingJobsResponse [TrainingJobSummary]
ltjrsTrainingJobSummaries = lens _ltjrsTrainingJobSummaries (\s a -> s {_ltjrsTrainingJobSummaries = a}) . _Coerce

instance NFData ListTrainingJobsResponse
