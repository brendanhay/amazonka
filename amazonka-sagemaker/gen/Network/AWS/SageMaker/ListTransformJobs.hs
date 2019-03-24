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
-- Module      : Network.AWS.SageMaker.ListTransformJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists transform jobs.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTransformJobs
    (
    -- * Creating a Request
      listTransformJobs
    , ListTransformJobs
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
    , listTransformJobsResponse
    , ListTransformJobsResponse
    -- * Response Lenses
    , lrsNextToken
    , lrsResponseStatus
    , lrsTransformJobSummaries
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listTransformJobs' smart constructor.
data ListTransformJobs = ListTransformJobs'
  { _ltjNameContains           :: !(Maybe Text)
  , _ltjLastModifiedTimeBefore :: !(Maybe POSIX)
  , _ltjCreationTimeAfter      :: !(Maybe POSIX)
  , _ltjNextToken              :: !(Maybe Text)
  , _ltjSortOrder              :: !(Maybe SortOrder)
  , _ltjLastModifiedTimeAfter  :: !(Maybe POSIX)
  , _ltjCreationTimeBefore     :: !(Maybe POSIX)
  , _ltjStatusEquals           :: !(Maybe TransformJobStatus)
  , _ltjMaxResults             :: !(Maybe Nat)
  , _ltjSortBy                 :: !(Maybe SortBy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTransformJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltjNameContains' - A string in the transform job name. This filter returns only transform jobs whose name contains the specified string.
--
-- * 'ltjLastModifiedTimeBefore' - A filter that returns only transform jobs modified before the specified time.
--
-- * 'ltjCreationTimeAfter' - A filter that returns only transform jobs created after the specified time.
--
-- * 'ltjNextToken' - If the result of the previous @ListTransformJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of transform jobs, use the token in the next request.
--
-- * 'ltjSortOrder' - The sort order for results. The default is @Descending@ .
--
-- * 'ltjLastModifiedTimeAfter' - A filter that returns only transform jobs modified after the specified time.
--
-- * 'ltjCreationTimeBefore' - A filter that returns only transform jobs created before the specified time.
--
-- * 'ltjStatusEquals' - A filter that retrieves only transform jobs with a specific status.
--
-- * 'ltjMaxResults' - The maximum number of transform jobs to return in the response. The default value is @10@ .
--
-- * 'ltjSortBy' - The field to sort results by. The default is @CreationTime@ .
listTransformJobs
    :: ListTransformJobs
listTransformJobs =
  ListTransformJobs'
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


-- | A string in the transform job name. This filter returns only transform jobs whose name contains the specified string.
ltjNameContains :: Lens' ListTransformJobs (Maybe Text)
ltjNameContains = lens _ltjNameContains (\ s a -> s{_ltjNameContains = a})

-- | A filter that returns only transform jobs modified before the specified time.
ltjLastModifiedTimeBefore :: Lens' ListTransformJobs (Maybe UTCTime)
ltjLastModifiedTimeBefore = lens _ltjLastModifiedTimeBefore (\ s a -> s{_ltjLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns only transform jobs created after the specified time.
ltjCreationTimeAfter :: Lens' ListTransformJobs (Maybe UTCTime)
ltjCreationTimeAfter = lens _ltjCreationTimeAfter (\ s a -> s{_ltjCreationTimeAfter = a}) . mapping _Time

-- | If the result of the previous @ListTransformJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of transform jobs, use the token in the next request.
ltjNextToken :: Lens' ListTransformJobs (Maybe Text)
ltjNextToken = lens _ltjNextToken (\ s a -> s{_ltjNextToken = a})

-- | The sort order for results. The default is @Descending@ .
ltjSortOrder :: Lens' ListTransformJobs (Maybe SortOrder)
ltjSortOrder = lens _ltjSortOrder (\ s a -> s{_ltjSortOrder = a})

-- | A filter that returns only transform jobs modified after the specified time.
ltjLastModifiedTimeAfter :: Lens' ListTransformJobs (Maybe UTCTime)
ltjLastModifiedTimeAfter = lens _ltjLastModifiedTimeAfter (\ s a -> s{_ltjLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns only transform jobs created before the specified time.
ltjCreationTimeBefore :: Lens' ListTransformJobs (Maybe UTCTime)
ltjCreationTimeBefore = lens _ltjCreationTimeBefore (\ s a -> s{_ltjCreationTimeBefore = a}) . mapping _Time

-- | A filter that retrieves only transform jobs with a specific status.
ltjStatusEquals :: Lens' ListTransformJobs (Maybe TransformJobStatus)
ltjStatusEquals = lens _ltjStatusEquals (\ s a -> s{_ltjStatusEquals = a})

-- | The maximum number of transform jobs to return in the response. The default value is @10@ .
ltjMaxResults :: Lens' ListTransformJobs (Maybe Natural)
ltjMaxResults = lens _ltjMaxResults (\ s a -> s{_ltjMaxResults = a}) . mapping _Nat

-- | The field to sort results by. The default is @CreationTime@ .
ltjSortBy :: Lens' ListTransformJobs (Maybe SortBy)
ltjSortBy = lens _ltjSortBy (\ s a -> s{_ltjSortBy = a})

instance AWSPager ListTransformJobs where
        page rq rs
          | stop (rs ^. lrsNextToken) = Nothing
          | stop (rs ^. lrsTransformJobSummaries) = Nothing
          | otherwise =
            Just $ rq & ltjNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListTransformJobs where
        type Rs ListTransformJobs = ListTransformJobsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListTransformJobsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "TransformJobSummaries" .!@ mempty))

instance Hashable ListTransformJobs where

instance NFData ListTransformJobs where

instance ToHeaders ListTransformJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListTransformJobs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTransformJobs where
        toJSON ListTransformJobs'{..}
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

instance ToPath ListTransformJobs where
        toPath = const "/"

instance ToQuery ListTransformJobs where
        toQuery = const mempty

-- | /See:/ 'listTransformJobsResponse' smart constructor.
data ListTransformJobsResponse = ListTransformJobsResponse'
  { _lrsNextToken             :: !(Maybe Text)
  , _lrsResponseStatus        :: !Int
  , _lrsTransformJobSummaries :: ![TransformJobSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTransformJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of transform jobs, use it in the next request.
--
-- * 'lrsResponseStatus' - -- | The response status code.
--
-- * 'lrsTransformJobSummaries' - An array of @TransformJobSummary@ objects.
listTransformJobsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListTransformJobsResponse
listTransformJobsResponse pResponseStatus_ =
  ListTransformJobsResponse'
    { _lrsNextToken = Nothing
    , _lrsResponseStatus = pResponseStatus_
    , _lrsTransformJobSummaries = mempty
    }


-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of transform jobs, use it in the next request.
lrsNextToken :: Lens' ListTransformJobsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListTransformJobsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

-- | An array of @TransformJobSummary@ objects.
lrsTransformJobSummaries :: Lens' ListTransformJobsResponse [TransformJobSummary]
lrsTransformJobSummaries = lens _lrsTransformJobSummaries (\ s a -> s{_lrsTransformJobSummaries = a}) . _Coerce

instance NFData ListTransformJobsResponse where
