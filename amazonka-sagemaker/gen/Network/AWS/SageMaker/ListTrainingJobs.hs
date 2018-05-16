{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTrainingJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
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
    (
    -- * Creating a Request
      listTrainingJobs
    , ListTrainingJobs
    -- * Request Lenses
    , ltjNameContains
    , ltjLastModifiedTimeBefore
    , ltjCreationTimeAfter
    , ltjNextToken
    , ltjSortOrder
    , ltjLastModifiedTimeAfter
    , ltjCreationTimeBefore
    , ltjStatusEquals
    , ltjMaxResults
    , ltjSortBy

    -- * Destructuring the Response
    , listTrainingJobsResponse
    , ListTrainingJobsResponse
    -- * Response Lenses
    , ltjrsNextToken
    , ltjrsResponseStatus
    , ltjrsTrainingJobSummaries
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listTrainingJobs' smart constructor.
data ListTrainingJobs = ListTrainingJobs'
  { _ltjNameContains           :: !(Maybe Text)
  , _ltjLastModifiedTimeBefore :: !(Maybe POSIX)
  , _ltjCreationTimeAfter      :: !(Maybe POSIX)
  , _ltjNextToken              :: !(Maybe Text)
  , _ltjSortOrder              :: !(Maybe SortOrder)
  , _ltjLastModifiedTimeAfter  :: !(Maybe POSIX)
  , _ltjCreationTimeBefore     :: !(Maybe POSIX)
  , _ltjStatusEquals           :: !(Maybe TrainingJobStatus)
  , _ltjMaxResults             :: !(Maybe Nat)
  , _ltjSortBy                 :: !(Maybe SortBy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrainingJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltjNameContains' - A string in the training job name. This filter returns only models whose name contains the specified string.
--
-- * 'ltjLastModifiedTimeBefore' - A filter that returns only training jobs modified before the specified time (timestamp).
--
-- * 'ltjCreationTimeAfter' - A filter that only training jobs created after the specified time (timestamp).
--
-- * 'ltjNextToken' - If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- * 'ltjSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'ltjLastModifiedTimeAfter' - A filter that returns only training jobs modified after the specified time (timestamp).
--
-- * 'ltjCreationTimeBefore' - A filter that returns only training jobs created before the specified time (timestamp).
--
-- * 'ltjStatusEquals' - A filter that retrieves only training jobs with a specific status.
--
-- * 'ltjMaxResults' - The maximum number of training jobs to return in the response.
--
-- * 'ltjSortBy' - The field to sort results by. The default is @CreationTime@ .
listTrainingJobs
    :: ListTrainingJobs
listTrainingJobs =
  ListTrainingJobs'
    { _ltjNameContains = Nothing
    , _ltjLastModifiedTimeBefore = Nothing
    , _ltjCreationTimeAfter = Nothing
    , _ltjNextToken = Nothing
    , _ltjSortOrder = Nothing
    , _ltjLastModifiedTimeAfter = Nothing
    , _ltjCreationTimeBefore = Nothing
    , _ltjStatusEquals = Nothing
    , _ltjMaxResults = Nothing
    , _ltjSortBy = Nothing
    }


-- | A string in the training job name. This filter returns only models whose name contains the specified string.
ltjNameContains :: Lens' ListTrainingJobs (Maybe Text)
ltjNameContains = lens _ltjNameContains (\ s a -> s{_ltjNameContains = a})

-- | A filter that returns only training jobs modified before the specified time (timestamp).
ltjLastModifiedTimeBefore :: Lens' ListTrainingJobs (Maybe UTCTime)
ltjLastModifiedTimeBefore = lens _ltjLastModifiedTimeBefore (\ s a -> s{_ltjLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that only training jobs created after the specified time (timestamp).
ltjCreationTimeAfter :: Lens' ListTrainingJobs (Maybe UTCTime)
ltjCreationTimeAfter = lens _ltjCreationTimeAfter (\ s a -> s{_ltjCreationTimeAfter = a}) . mapping _Time

-- | If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
ltjNextToken :: Lens' ListTrainingJobs (Maybe Text)
ltjNextToken = lens _ltjNextToken (\ s a -> s{_ltjNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
ltjSortOrder :: Lens' ListTrainingJobs (Maybe SortOrder)
ltjSortOrder = lens _ltjSortOrder (\ s a -> s{_ltjSortOrder = a})

-- | A filter that returns only training jobs modified after the specified time (timestamp).
ltjLastModifiedTimeAfter :: Lens' ListTrainingJobs (Maybe UTCTime)
ltjLastModifiedTimeAfter = lens _ltjLastModifiedTimeAfter (\ s a -> s{_ltjLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only training jobs created before the specified time (timestamp).
ltjCreationTimeBefore :: Lens' ListTrainingJobs (Maybe UTCTime)
ltjCreationTimeBefore = lens _ltjCreationTimeBefore (\ s a -> s{_ltjCreationTimeBefore = a}) . mapping _Time

-- | A filter that retrieves only training jobs with a specific status.
ltjStatusEquals :: Lens' ListTrainingJobs (Maybe TrainingJobStatus)
ltjStatusEquals = lens _ltjStatusEquals (\ s a -> s{_ltjStatusEquals = a})

-- | The maximum number of training jobs to return in the response.
ltjMaxResults :: Lens' ListTrainingJobs (Maybe Natural)
ltjMaxResults = lens _ltjMaxResults (\ s a -> s{_ltjMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @CreationTime@ .
ltjSortBy :: Lens' ListTrainingJobs (Maybe SortBy)
ltjSortBy = lens _ltjSortBy (\ s a -> s{_ltjSortBy = a})

instance AWSPager ListTrainingJobs where
        page rq rs
          | stop (rs ^. ltjrsNextToken) = Nothing
          | stop (rs ^. ltjrsTrainingJobSummaries) = Nothing
          | otherwise =
            Just $ rq & ltjNextToken .~ rs ^. ltjrsNextToken

instance AWSRequest ListTrainingJobs where
        type Rs ListTrainingJobs = ListTrainingJobsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListTrainingJobsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "TrainingJobSummaries" .!@ mempty))

instance Hashable ListTrainingJobs where

instance NFData ListTrainingJobs where

instance ToHeaders ListTrainingJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListTrainingJobs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTrainingJobs where
        toJSON ListTrainingJobs'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _ltjNameContains,
                  ("LastModifiedTimeBefore" .=) <$>
                    _ltjLastModifiedTimeBefore,
                  ("CreationTimeAfter" .=) <$> _ltjCreationTimeAfter,
                  ("NextToken" .=) <$> _ltjNextToken,
                  ("SortOrder" .=) <$> _ltjSortOrder,
                  ("LastModifiedTimeAfter" .=) <$>
                    _ltjLastModifiedTimeAfter,
                  ("CreationTimeBefore" .=) <$> _ltjCreationTimeBefore,
                  ("StatusEquals" .=) <$> _ltjStatusEquals,
                  ("MaxResults" .=) <$> _ltjMaxResults,
                  ("SortBy" .=) <$> _ltjSortBy])

instance ToPath ListTrainingJobs where
        toPath = const "/"

instance ToQuery ListTrainingJobs where
        toQuery = const mempty

-- | /See:/ 'listTrainingJobsResponse' smart constructor.
data ListTrainingJobsResponse = ListTrainingJobsResponse'
  { _ltjrsNextToken            :: !(Maybe Text)
  , _ltjrsResponseStatus       :: !Int
  , _ltjrsTrainingJobSummaries :: ![TrainingJobSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrainingJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltjrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
--
-- * 'ltjrsResponseStatus' - -- | The response status code.
--
-- * 'ltjrsTrainingJobSummaries' - An array of @TrainingJobSummary@ objects, each listing a training job.
listTrainingJobsResponse
    :: Int -- ^ 'ltjrsResponseStatus'
    -> ListTrainingJobsResponse
listTrainingJobsResponse pResponseStatus_ =
  ListTrainingJobsResponse'
    { _ltjrsNextToken = Nothing
    , _ltjrsResponseStatus = pResponseStatus_
    , _ltjrsTrainingJobSummaries = mempty
    }


-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of training jobs, use it in the subsequent request.
ltjrsNextToken :: Lens' ListTrainingJobsResponse (Maybe Text)
ltjrsNextToken = lens _ltjrsNextToken (\ s a -> s{_ltjrsNextToken = a})

-- | -- | The response status code.
ltjrsResponseStatus :: Lens' ListTrainingJobsResponse Int
ltjrsResponseStatus = lens _ltjrsResponseStatus (\ s a -> s{_ltjrsResponseStatus = a})

-- | An array of @TrainingJobSummary@ objects, each listing a training job.
ltjrsTrainingJobSummaries :: Lens' ListTrainingJobsResponse [TrainingJobSummary]
ltjrsTrainingJobSummaries = lens _ltjrsTrainingJobSummaries (\ s a -> s{_ltjrsTrainingJobSummaries = a}) . _Coerce

instance NFData ListTrainingJobsResponse where
