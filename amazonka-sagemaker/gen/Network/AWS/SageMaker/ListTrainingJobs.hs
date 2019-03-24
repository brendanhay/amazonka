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
    , lNameContains
    , lLastModifiedTimeBefore
    , lCreationTimeAfter
    , lNextToken
    , lSortOrder
    , lLastModifiedTimeAfter
    , lCreationTimeBefore
    , lStatusEquals
    , lMaxResults
    , lSortBy

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
  { _lNameContains           :: !(Maybe Text)
  , _lLastModifiedTimeBefore :: !(Maybe POSIX)
  , _lCreationTimeAfter      :: !(Maybe POSIX)
  , _lNextToken              :: !(Maybe Text)
  , _lSortOrder              :: !(Maybe SortOrder)
  , _lLastModifiedTimeAfter  :: !(Maybe POSIX)
  , _lCreationTimeBefore     :: !(Maybe POSIX)
  , _lStatusEquals           :: !(Maybe TrainingJobStatus)
  , _lMaxResults             :: !(Maybe Nat)
  , _lSortBy                 :: !(Maybe SortBy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrainingJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNameContains' - A string in the training job name. This filter returns only training jobs whose name contains the specified string.
--
-- * 'lLastModifiedTimeBefore' - A filter that returns only training jobs modified before the specified time (timestamp).
--
-- * 'lCreationTimeAfter' - A filter that returns only training jobs created after the specified time (timestamp).
--
-- * 'lNextToken' - If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
--
-- * 'lSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'lLastModifiedTimeAfter' - A filter that returns only training jobs modified after the specified time (timestamp).
--
-- * 'lCreationTimeBefore' - A filter that returns only training jobs created before the specified time (timestamp).
--
-- * 'lStatusEquals' - A filter that retrieves only training jobs with a specific status.
--
-- * 'lMaxResults' - The maximum number of training jobs to return in the response.
--
-- * 'lSortBy' - The field to sort results by. The default is @CreationTime@ .
listTrainingJobs
    :: ListTrainingJobs
listTrainingJobs =
  ListTrainingJobs'
    { _lNameContains = Nothing
    , _lLastModifiedTimeBefore = Nothing
    , _lCreationTimeAfter = Nothing
    , _lNextToken = Nothing
    , _lSortOrder = Nothing
    , _lLastModifiedTimeAfter = Nothing
    , _lCreationTimeBefore = Nothing
    , _lStatusEquals = Nothing
    , _lMaxResults = Nothing
    , _lSortBy = Nothing
    }


-- | A string in the training job name. This filter returns only training jobs whose name contains the specified string.
lNameContains :: Lens' ListTrainingJobs (Maybe Text)
lNameContains = lens _lNameContains (\ s a -> s{_lNameContains = a})

-- | A filter that returns only training jobs modified before the specified time (timestamp).
lLastModifiedTimeBefore :: Lens' ListTrainingJobs (Maybe UTCTime)
lLastModifiedTimeBefore = lens _lLastModifiedTimeBefore (\ s a -> s{_lLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only training jobs created after the specified time (timestamp).
lCreationTimeAfter :: Lens' ListTrainingJobs (Maybe UTCTime)
lCreationTimeAfter = lens _lCreationTimeAfter (\ s a -> s{_lCreationTimeAfter = a}) . mapping _Time

-- | If the result of the previous @ListTrainingJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of training jobs, use the token in the next request.
lNextToken :: Lens' ListTrainingJobs (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
lSortOrder :: Lens' ListTrainingJobs (Maybe SortOrder)
lSortOrder = lens _lSortOrder (\ s a -> s{_lSortOrder = a})

-- | A filter that returns only training jobs modified after the specified time (timestamp).
lLastModifiedTimeAfter :: Lens' ListTrainingJobs (Maybe UTCTime)
lLastModifiedTimeAfter = lens _lLastModifiedTimeAfter (\ s a -> s{_lLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only training jobs created before the specified time (timestamp).
lCreationTimeBefore :: Lens' ListTrainingJobs (Maybe UTCTime)
lCreationTimeBefore = lens _lCreationTimeBefore (\ s a -> s{_lCreationTimeBefore = a}) . mapping _Time

-- | A filter that retrieves only training jobs with a specific status.
lStatusEquals :: Lens' ListTrainingJobs (Maybe TrainingJobStatus)
lStatusEquals = lens _lStatusEquals (\ s a -> s{_lStatusEquals = a})

-- | The maximum number of training jobs to return in the response.
lMaxResults :: Lens' ListTrainingJobs (Maybe Natural)
lMaxResults = lens _lMaxResults (\ s a -> s{_lMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @CreationTime@ .
lSortBy :: Lens' ListTrainingJobs (Maybe SortBy)
lSortBy = lens _lSortBy (\ s a -> s{_lSortBy = a})

instance AWSPager ListTrainingJobs where
        page rq rs
          | stop (rs ^. ltjrsNextToken) = Nothing
          | stop (rs ^. ltjrsTrainingJobSummaries) = Nothing
          | otherwise =
            Just $ rq & lNextToken .~ rs ^. ltjrsNextToken

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
                 [("NameContains" .=) <$> _lNameContains,
                  ("LastModifiedTimeBefore" .=) <$>
                    _lLastModifiedTimeBefore,
                  ("CreationTimeAfter" .=) <$> _lCreationTimeAfter,
                  ("NextToken" .=) <$> _lNextToken,
                  ("SortOrder" .=) <$> _lSortOrder,
                  ("LastModifiedTimeAfter" .=) <$>
                    _lLastModifiedTimeAfter,
                  ("CreationTimeBefore" .=) <$> _lCreationTimeBefore,
                  ("StatusEquals" .=) <$> _lStatusEquals,
                  ("MaxResults" .=) <$> _lMaxResults,
                  ("SortBy" .=) <$> _lSortBy])

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
