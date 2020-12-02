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
-- Module      : Network.AWS.SageMaker.ListProcessingJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists processing jobs that satisfy various filters.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListProcessingJobs
  ( -- * Creating a Request
    listProcessingJobs,
    ListProcessingJobs,

    -- * Request Lenses
    lpjNameContains,
    lpjLastModifiedTimeBefore,
    lpjCreationTimeAfter,
    lpjNextToken,
    lpjSortOrder,
    lpjLastModifiedTimeAfter,
    lpjCreationTimeBefore,
    lpjStatusEquals,
    lpjMaxResults,
    lpjSortBy,

    -- * Destructuring the Response
    listProcessingJobsResponse,
    ListProcessingJobsResponse,

    -- * Response Lenses
    lpjrsNextToken,
    lpjrsResponseStatus,
    lpjrsProcessingJobSummaries,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'listProcessingJobs' smart constructor.
data ListProcessingJobs = ListProcessingJobs'
  { _lpjNameContains ::
      !(Maybe Text),
    _lpjLastModifiedTimeBefore :: !(Maybe POSIX),
    _lpjCreationTimeAfter :: !(Maybe POSIX),
    _lpjNextToken :: !(Maybe Text),
    _lpjSortOrder :: !(Maybe SortOrder),
    _lpjLastModifiedTimeAfter :: !(Maybe POSIX),
    _lpjCreationTimeBefore :: !(Maybe POSIX),
    _lpjStatusEquals :: !(Maybe ProcessingJobStatus),
    _lpjMaxResults :: !(Maybe Nat),
    _lpjSortBy :: !(Maybe SortBy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListProcessingJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpjNameContains' - A string in the processing job name. This filter returns only processing jobs whose name contains the specified string.
--
-- * 'lpjLastModifiedTimeBefore' - A filter that returns only processing jobs modified before the specified time.
--
-- * 'lpjCreationTimeAfter' - A filter that returns only processing jobs created after the specified time.
--
-- * 'lpjNextToken' - If the result of the previous @ListProcessingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of processing jobs, use the token in the next request.
--
-- * 'lpjSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'lpjLastModifiedTimeAfter' - A filter that returns only processing jobs modified after the specified time.
--
-- * 'lpjCreationTimeBefore' - A filter that returns only processing jobs created after the specified time.
--
-- * 'lpjStatusEquals' - A filter that retrieves only processing jobs with a specific status.
--
-- * 'lpjMaxResults' - The maximum number of processing jobs to return in the response.
--
-- * 'lpjSortBy' - The field to sort results by. The default is @CreationTime@ .
listProcessingJobs ::
  ListProcessingJobs
listProcessingJobs =
  ListProcessingJobs'
    { _lpjNameContains = Nothing,
      _lpjLastModifiedTimeBefore = Nothing,
      _lpjCreationTimeAfter = Nothing,
      _lpjNextToken = Nothing,
      _lpjSortOrder = Nothing,
      _lpjLastModifiedTimeAfter = Nothing,
      _lpjCreationTimeBefore = Nothing,
      _lpjStatusEquals = Nothing,
      _lpjMaxResults = Nothing,
      _lpjSortBy = Nothing
    }

-- | A string in the processing job name. This filter returns only processing jobs whose name contains the specified string.
lpjNameContains :: Lens' ListProcessingJobs (Maybe Text)
lpjNameContains = lens _lpjNameContains (\s a -> s {_lpjNameContains = a})

-- | A filter that returns only processing jobs modified before the specified time.
lpjLastModifiedTimeBefore :: Lens' ListProcessingJobs (Maybe UTCTime)
lpjLastModifiedTimeBefore = lens _lpjLastModifiedTimeBefore (\s a -> s {_lpjLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only processing jobs created after the specified time.
lpjCreationTimeAfter :: Lens' ListProcessingJobs (Maybe UTCTime)
lpjCreationTimeAfter = lens _lpjCreationTimeAfter (\s a -> s {_lpjCreationTimeAfter = a}) . mapping _Time

-- | If the result of the previous @ListProcessingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of processing jobs, use the token in the next request.
lpjNextToken :: Lens' ListProcessingJobs (Maybe Text)
lpjNextToken = lens _lpjNextToken (\s a -> s {_lpjNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
lpjSortOrder :: Lens' ListProcessingJobs (Maybe SortOrder)
lpjSortOrder = lens _lpjSortOrder (\s a -> s {_lpjSortOrder = a})

-- | A filter that returns only processing jobs modified after the specified time.
lpjLastModifiedTimeAfter :: Lens' ListProcessingJobs (Maybe UTCTime)
lpjLastModifiedTimeAfter = lens _lpjLastModifiedTimeAfter (\s a -> s {_lpjLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only processing jobs created after the specified time.
lpjCreationTimeBefore :: Lens' ListProcessingJobs (Maybe UTCTime)
lpjCreationTimeBefore = lens _lpjCreationTimeBefore (\s a -> s {_lpjCreationTimeBefore = a}) . mapping _Time

-- | A filter that retrieves only processing jobs with a specific status.
lpjStatusEquals :: Lens' ListProcessingJobs (Maybe ProcessingJobStatus)
lpjStatusEquals = lens _lpjStatusEquals (\s a -> s {_lpjStatusEquals = a})

-- | The maximum number of processing jobs to return in the response.
lpjMaxResults :: Lens' ListProcessingJobs (Maybe Natural)
lpjMaxResults = lens _lpjMaxResults (\s a -> s {_lpjMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @CreationTime@ .
lpjSortBy :: Lens' ListProcessingJobs (Maybe SortBy)
lpjSortBy = lens _lpjSortBy (\s a -> s {_lpjSortBy = a})

instance AWSPager ListProcessingJobs where
  page rq rs
    | stop (rs ^. lpjrsNextToken) = Nothing
    | stop (rs ^. lpjrsProcessingJobSummaries) = Nothing
    | otherwise = Just $ rq & lpjNextToken .~ rs ^. lpjrsNextToken

instance AWSRequest ListProcessingJobs where
  type Rs ListProcessingJobs = ListProcessingJobsResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          ListProcessingJobsResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "ProcessingJobSummaries" .!@ mempty)
      )

instance Hashable ListProcessingJobs

instance NFData ListProcessingJobs

instance ToHeaders ListProcessingJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.ListProcessingJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListProcessingJobs where
  toJSON ListProcessingJobs' {..} =
    object
      ( catMaybes
          [ ("NameContains" .=) <$> _lpjNameContains,
            ("LastModifiedTimeBefore" .=) <$> _lpjLastModifiedTimeBefore,
            ("CreationTimeAfter" .=) <$> _lpjCreationTimeAfter,
            ("NextToken" .=) <$> _lpjNextToken,
            ("SortOrder" .=) <$> _lpjSortOrder,
            ("LastModifiedTimeAfter" .=) <$> _lpjLastModifiedTimeAfter,
            ("CreationTimeBefore" .=) <$> _lpjCreationTimeBefore,
            ("StatusEquals" .=) <$> _lpjStatusEquals,
            ("MaxResults" .=) <$> _lpjMaxResults,
            ("SortBy" .=) <$> _lpjSortBy
          ]
      )

instance ToPath ListProcessingJobs where
  toPath = const "/"

instance ToQuery ListProcessingJobs where
  toQuery = const mempty

-- | /See:/ 'listProcessingJobsResponse' smart constructor.
data ListProcessingJobsResponse = ListProcessingJobsResponse'
  { _lpjrsNextToken ::
      !(Maybe Text),
    _lpjrsResponseStatus :: !Int,
    _lpjrsProcessingJobSummaries ::
      ![ProcessingJobSummary]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListProcessingJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpjrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of processing jobs, use it in the subsequent request.
--
-- * 'lpjrsResponseStatus' - -- | The response status code.
--
-- * 'lpjrsProcessingJobSummaries' - An array of @ProcessingJobSummary@ objects, each listing a processing job.
listProcessingJobsResponse ::
  -- | 'lpjrsResponseStatus'
  Int ->
  ListProcessingJobsResponse
listProcessingJobsResponse pResponseStatus_ =
  ListProcessingJobsResponse'
    { _lpjrsNextToken = Nothing,
      _lpjrsResponseStatus = pResponseStatus_,
      _lpjrsProcessingJobSummaries = mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of processing jobs, use it in the subsequent request.
lpjrsNextToken :: Lens' ListProcessingJobsResponse (Maybe Text)
lpjrsNextToken = lens _lpjrsNextToken (\s a -> s {_lpjrsNextToken = a})

-- | -- | The response status code.
lpjrsResponseStatus :: Lens' ListProcessingJobsResponse Int
lpjrsResponseStatus = lens _lpjrsResponseStatus (\s a -> s {_lpjrsResponseStatus = a})

-- | An array of @ProcessingJobSummary@ objects, each listing a processing job.
lpjrsProcessingJobSummaries :: Lens' ListProcessingJobsResponse [ProcessingJobSummary]
lpjrsProcessingJobSummaries = lens _lpjrsProcessingJobSummaries (\s a -> s {_lpjrsProcessingJobSummaries = a}) . _Coerce

instance NFData ListProcessingJobsResponse
