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
-- Module      : Network.AWS.SageMaker.ListHyperParameterTuningJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of 'HyperParameterTuningJobSummary' objects that describe the hyperparameter tuning jobs launched in your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListHyperParameterTuningJobs
    (
    -- * Creating a Request
      listHyperParameterTuningJobs
    , ListHyperParameterTuningJobs
    -- * Request Lenses
    , lhptjNameContains
    , lhptjLastModifiedTimeBefore
    , lhptjCreationTimeAfter
    , lhptjNextToken
    , lhptjSortOrder
    , lhptjLastModifiedTimeAfter
    , lhptjCreationTimeBefore
    , lhptjStatusEquals
    , lhptjMaxResults
    , lhptjSortBy

    -- * Destructuring the Response
    , listHyperParameterTuningJobsResponse
    , ListHyperParameterTuningJobsResponse
    -- * Response Lenses
    , lhptjrsNextToken
    , lhptjrsResponseStatus
    , lhptjrsHyperParameterTuningJobSummaries
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listHyperParameterTuningJobs' smart constructor.
data ListHyperParameterTuningJobs = ListHyperParameterTuningJobs'
  { _lhptjNameContains :: !(Maybe Text)
  , _lhptjLastModifiedTimeBefore :: !(Maybe POSIX)
  , _lhptjCreationTimeAfter :: !(Maybe POSIX)
  , _lhptjNextToken :: !(Maybe Text)
  , _lhptjSortOrder :: !(Maybe SortOrder)
  , _lhptjLastModifiedTimeAfter :: !(Maybe POSIX)
  , _lhptjCreationTimeBefore :: !(Maybe POSIX)
  , _lhptjStatusEquals :: !(Maybe HyperParameterTuningJobStatus)
  , _lhptjMaxResults :: !(Maybe Nat)
  , _lhptjSortBy :: !(Maybe HyperParameterTuningJobSortByOptions)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHyperParameterTuningJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhptjNameContains' - A string in the tuning job name. This filter returns only tuning jobs whose name contains the specified string.
--
-- * 'lhptjLastModifiedTimeBefore' - A filter that returns only tuning jobs that were modified before the specified time.
--
-- * 'lhptjCreationTimeAfter' - A filter that returns only tuning jobs that were created after the specified time.
--
-- * 'lhptjNextToken' - If the result of the previous @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
--
-- * 'lhptjSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'lhptjLastModifiedTimeAfter' - A filter that returns only tuning jobs that were modified after the specified time.
--
-- * 'lhptjCreationTimeBefore' - A filter that returns only tuning jobs that were created before the specified time.
--
-- * 'lhptjStatusEquals' - A filter that returns only tuning jobs with the specified status.
--
-- * 'lhptjMaxResults' - The maximum number of tuning jobs to return. The default value is 10.
--
-- * 'lhptjSortBy' - The field to sort results by. The default is @Name@ .
listHyperParameterTuningJobs
    :: ListHyperParameterTuningJobs
listHyperParameterTuningJobs =
  ListHyperParameterTuningJobs'
    { _lhptjNameContains = Nothing
    , _lhptjLastModifiedTimeBefore = Nothing
    , _lhptjCreationTimeAfter = Nothing
    , _lhptjNextToken = Nothing
    , _lhptjSortOrder = Nothing
    , _lhptjLastModifiedTimeAfter = Nothing
    , _lhptjCreationTimeBefore = Nothing
    , _lhptjStatusEquals = Nothing
    , _lhptjMaxResults = Nothing
    , _lhptjSortBy = Nothing
    }


-- | A string in the tuning job name. This filter returns only tuning jobs whose name contains the specified string.
lhptjNameContains :: Lens' ListHyperParameterTuningJobs (Maybe Text)
lhptjNameContains = lens _lhptjNameContains (\ s a -> s{_lhptjNameContains = a})

-- | A filter that returns only tuning jobs that were modified before the specified time.
lhptjLastModifiedTimeBefore :: Lens' ListHyperParameterTuningJobs (Maybe UTCTime)
lhptjLastModifiedTimeBefore = lens _lhptjLastModifiedTimeBefore (\ s a -> s{_lhptjLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only tuning jobs that were created after the specified time.
lhptjCreationTimeAfter :: Lens' ListHyperParameterTuningJobs (Maybe UTCTime)
lhptjCreationTimeAfter = lens _lhptjCreationTimeAfter (\ s a -> s{_lhptjCreationTimeAfter = a}) . mapping _Time

-- | If the result of the previous @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
lhptjNextToken :: Lens' ListHyperParameterTuningJobs (Maybe Text)
lhptjNextToken = lens _lhptjNextToken (\ s a -> s{_lhptjNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
lhptjSortOrder :: Lens' ListHyperParameterTuningJobs (Maybe SortOrder)
lhptjSortOrder = lens _lhptjSortOrder (\ s a -> s{_lhptjSortOrder = a})

-- | A filter that returns only tuning jobs that were modified after the specified time.
lhptjLastModifiedTimeAfter :: Lens' ListHyperParameterTuningJobs (Maybe UTCTime)
lhptjLastModifiedTimeAfter = lens _lhptjLastModifiedTimeAfter (\ s a -> s{_lhptjLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only tuning jobs that were created before the specified time.
lhptjCreationTimeBefore :: Lens' ListHyperParameterTuningJobs (Maybe UTCTime)
lhptjCreationTimeBefore = lens _lhptjCreationTimeBefore (\ s a -> s{_lhptjCreationTimeBefore = a}) . mapping _Time

-- | A filter that returns only tuning jobs with the specified status.
lhptjStatusEquals :: Lens' ListHyperParameterTuningJobs (Maybe HyperParameterTuningJobStatus)
lhptjStatusEquals = lens _lhptjStatusEquals (\ s a -> s{_lhptjStatusEquals = a})

-- | The maximum number of tuning jobs to return. The default value is 10.
lhptjMaxResults :: Lens' ListHyperParameterTuningJobs (Maybe Natural)
lhptjMaxResults = lens _lhptjMaxResults (\ s a -> s{_lhptjMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @Name@ .
lhptjSortBy :: Lens' ListHyperParameterTuningJobs (Maybe HyperParameterTuningJobSortByOptions)
lhptjSortBy = lens _lhptjSortBy (\ s a -> s{_lhptjSortBy = a})

instance AWSPager ListHyperParameterTuningJobs where
        page rq rs
          | stop (rs ^. lhptjrsNextToken) = Nothing
          | stop
              (rs ^. lhptjrsHyperParameterTuningJobSummaries)
            = Nothing
          | otherwise =
            Just $ rq & lhptjNextToken .~ rs ^. lhptjrsNextToken

instance AWSRequest ListHyperParameterTuningJobs
         where
        type Rs ListHyperParameterTuningJobs =
             ListHyperParameterTuningJobsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListHyperParameterTuningJobsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "HyperParameterTuningJobSummaries" .!@
                        mempty))

instance Hashable ListHyperParameterTuningJobs where

instance NFData ListHyperParameterTuningJobs where

instance ToHeaders ListHyperParameterTuningJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListHyperParameterTuningJobs" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListHyperParameterTuningJobs where
        toJSON ListHyperParameterTuningJobs'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _lhptjNameContains,
                  ("LastModifiedTimeBefore" .=) <$>
                    _lhptjLastModifiedTimeBefore,
                  ("CreationTimeAfter" .=) <$> _lhptjCreationTimeAfter,
                  ("NextToken" .=) <$> _lhptjNextToken,
                  ("SortOrder" .=) <$> _lhptjSortOrder,
                  ("LastModifiedTimeAfter" .=) <$>
                    _lhptjLastModifiedTimeAfter,
                  ("CreationTimeBefore" .=) <$>
                    _lhptjCreationTimeBefore,
                  ("StatusEquals" .=) <$> _lhptjStatusEquals,
                  ("MaxResults" .=) <$> _lhptjMaxResults,
                  ("SortBy" .=) <$> _lhptjSortBy])

instance ToPath ListHyperParameterTuningJobs where
        toPath = const "/"

instance ToQuery ListHyperParameterTuningJobs where
        toQuery = const mempty

-- | /See:/ 'listHyperParameterTuningJobsResponse' smart constructor.
data ListHyperParameterTuningJobsResponse = ListHyperParameterTuningJobsResponse'
  { _lhptjrsNextToken :: !(Maybe Text)
  , _lhptjrsResponseStatus :: !Int
  , _lhptjrsHyperParameterTuningJobSummaries :: ![HyperParameterTuningJobSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHyperParameterTuningJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhptjrsNextToken' - If the result of this @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
--
-- * 'lhptjrsResponseStatus' - -- | The response status code.
--
-- * 'lhptjrsHyperParameterTuningJobSummaries' - A list of 'HyperParameterTuningJobSummary' objects that describe the tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
listHyperParameterTuningJobsResponse
    :: Int -- ^ 'lhptjrsResponseStatus'
    -> ListHyperParameterTuningJobsResponse
listHyperParameterTuningJobsResponse pResponseStatus_ =
  ListHyperParameterTuningJobsResponse'
    { _lhptjrsNextToken = Nothing
    , _lhptjrsResponseStatus = pResponseStatus_
    , _lhptjrsHyperParameterTuningJobSummaries = mempty
    }


-- | If the result of this @ListHyperParameterTuningJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of tuning jobs, use the token in the next request.
lhptjrsNextToken :: Lens' ListHyperParameterTuningJobsResponse (Maybe Text)
lhptjrsNextToken = lens _lhptjrsNextToken (\ s a -> s{_lhptjrsNextToken = a})

-- | -- | The response status code.
lhptjrsResponseStatus :: Lens' ListHyperParameterTuningJobsResponse Int
lhptjrsResponseStatus = lens _lhptjrsResponseStatus (\ s a -> s{_lhptjrsResponseStatus = a})

-- | A list of 'HyperParameterTuningJobSummary' objects that describe the tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
lhptjrsHyperParameterTuningJobSummaries :: Lens' ListHyperParameterTuningJobsResponse [HyperParameterTuningJobSummary]
lhptjrsHyperParameterTuningJobSummaries = lens _lhptjrsHyperParameterTuningJobSummaries (\ s a -> s{_lhptjrsHyperParameterTuningJobSummaries = a}) . _Coerce

instance NFData ListHyperParameterTuningJobsResponse
         where
