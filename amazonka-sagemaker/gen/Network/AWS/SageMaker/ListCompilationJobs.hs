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
-- Module      : Network.AWS.SageMaker.ListCompilationJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists model compilation jobs that satisfy various filters.
--
--
-- To create a model compilation job, use 'CreateCompilationJob' . To get information about a particular model compilation job you have created, use 'DescribeCompilationJob' .
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCompilationJobs
    (
    -- * Creating a Request
      listCompilationJobs
    , ListCompilationJobs
    -- * Request Lenses
    , lcjNameContains
    , lcjLastModifiedTimeBefore
    , lcjCreationTimeAfter
    , lcjNextToken
    , lcjSortOrder
    , lcjLastModifiedTimeAfter
    , lcjCreationTimeBefore
    , lcjStatusEquals
    , lcjMaxResults
    , lcjSortBy

    -- * Destructuring the Response
    , listCompilationJobsResponse
    , ListCompilationJobsResponse
    -- * Response Lenses
    , lcjrsNextToken
    , lcjrsResponseStatus
    , lcjrsCompilationJobSummaries
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listCompilationJobs' smart constructor.
data ListCompilationJobs = ListCompilationJobs'
  { _lcjNameContains           :: !(Maybe Text)
  , _lcjLastModifiedTimeBefore :: !(Maybe POSIX)
  , _lcjCreationTimeAfter      :: !(Maybe POSIX)
  , _lcjNextToken              :: !(Maybe Text)
  , _lcjSortOrder              :: !(Maybe SortOrder)
  , _lcjLastModifiedTimeAfter  :: !(Maybe POSIX)
  , _lcjCreationTimeBefore     :: !(Maybe POSIX)
  , _lcjStatusEquals           :: !(Maybe CompilationJobStatus)
  , _lcjMaxResults             :: !(Maybe Nat)
  , _lcjSortBy                 :: !(Maybe ListCompilationJobsSortBy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCompilationJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcjNameContains' - A filter that returns the model compilation jobs whose name contains a specified string.
--
-- * 'lcjLastModifiedTimeBefore' - A filter that returns the model compilation jobs that were modified before a specified time.
--
-- * 'lcjCreationTimeAfter' - A filter that returns the model compilation jobs that were created after a specified time.
--
-- * 'lcjNextToken' - If the result of the previous @ListCompilationJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model compilation jobs, use the token in the next request.
--
-- * 'lcjSortOrder' - The sort order for results. The default is @Ascending@ .
--
-- * 'lcjLastModifiedTimeAfter' - A filter that returns the model compilation jobs that were modified after a specified time.
--
-- * 'lcjCreationTimeBefore' - A filter that returns the model compilation jobs that were created before a specified time.
--
-- * 'lcjStatusEquals' - A filter that retrieves model compilation jobs with a specific 'DescribeCompilationJobResponse$CompilationJobStatus' status.
--
-- * 'lcjMaxResults' - The maximum number of model compilation jobs to return in the response.
--
-- * 'lcjSortBy' - The field by which to sort results. The default is @CreationTime@ .
listCompilationJobs
    :: ListCompilationJobs
listCompilationJobs =
  ListCompilationJobs'
    { _lcjNameContains = Nothing
    , _lcjLastModifiedTimeBefore = Nothing
    , _lcjCreationTimeAfter = Nothing
    , _lcjNextToken = Nothing
    , _lcjSortOrder = Nothing
    , _lcjLastModifiedTimeAfter = Nothing
    , _lcjCreationTimeBefore = Nothing
    , _lcjStatusEquals = Nothing
    , _lcjMaxResults = Nothing
    , _lcjSortBy = Nothing
    }


-- | A filter that returns the model compilation jobs whose name contains a specified string.
lcjNameContains :: Lens' ListCompilationJobs (Maybe Text)
lcjNameContains = lens _lcjNameContains (\ s a -> s{_lcjNameContains = a})

-- | A filter that returns the model compilation jobs that were modified before a specified time.
lcjLastModifiedTimeBefore :: Lens' ListCompilationJobs (Maybe UTCTime)
lcjLastModifiedTimeBefore = lens _lcjLastModifiedTimeBefore (\ s a -> s{_lcjLastModifiedTimeBefore = a}) . mapping _Time

-- | A filter that returns the model compilation jobs that were created after a specified time.
lcjCreationTimeAfter :: Lens' ListCompilationJobs (Maybe UTCTime)
lcjCreationTimeAfter = lens _lcjCreationTimeAfter (\ s a -> s{_lcjCreationTimeAfter = a}) . mapping _Time

-- | If the result of the previous @ListCompilationJobs@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of model compilation jobs, use the token in the next request.
lcjNextToken :: Lens' ListCompilationJobs (Maybe Text)
lcjNextToken = lens _lcjNextToken (\ s a -> s{_lcjNextToken = a})

-- | The sort order for results. The default is @Ascending@ .
lcjSortOrder :: Lens' ListCompilationJobs (Maybe SortOrder)
lcjSortOrder = lens _lcjSortOrder (\ s a -> s{_lcjSortOrder = a})

-- | A filter that returns the model compilation jobs that were modified after a specified time.
lcjLastModifiedTimeAfter :: Lens' ListCompilationJobs (Maybe UTCTime)
lcjLastModifiedTimeAfter = lens _lcjLastModifiedTimeAfter (\ s a -> s{_lcjLastModifiedTimeAfter = a}) . mapping _Time

-- | A filter that returns the model compilation jobs that were created before a specified time.
lcjCreationTimeBefore :: Lens' ListCompilationJobs (Maybe UTCTime)
lcjCreationTimeBefore = lens _lcjCreationTimeBefore (\ s a -> s{_lcjCreationTimeBefore = a}) . mapping _Time

-- | A filter that retrieves model compilation jobs with a specific 'DescribeCompilationJobResponse$CompilationJobStatus' status.
lcjStatusEquals :: Lens' ListCompilationJobs (Maybe CompilationJobStatus)
lcjStatusEquals = lens _lcjStatusEquals (\ s a -> s{_lcjStatusEquals = a})

-- | The maximum number of model compilation jobs to return in the response.
lcjMaxResults :: Lens' ListCompilationJobs (Maybe Natural)
lcjMaxResults = lens _lcjMaxResults (\ s a -> s{_lcjMaxResults = a}) . mapping _Nat

-- | The field by which to sort results. The default is @CreationTime@ .
lcjSortBy :: Lens' ListCompilationJobs (Maybe ListCompilationJobsSortBy)
lcjSortBy = lens _lcjSortBy (\ s a -> s{_lcjSortBy = a})

instance AWSPager ListCompilationJobs where
        page rq rs
          | stop (rs ^. lcjrsNextToken) = Nothing
          | stop (rs ^. lcjrsCompilationJobSummaries) = Nothing
          | otherwise =
            Just $ rq & lcjNextToken .~ rs ^. lcjrsNextToken

instance AWSRequest ListCompilationJobs where
        type Rs ListCompilationJobs =
             ListCompilationJobsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListCompilationJobsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "CompilationJobSummaries" .!@ mempty))

instance Hashable ListCompilationJobs where

instance NFData ListCompilationJobs where

instance ToHeaders ListCompilationJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListCompilationJobs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCompilationJobs where
        toJSON ListCompilationJobs'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _lcjNameContains,
                  ("LastModifiedTimeBefore" .=) <$>
                    _lcjLastModifiedTimeBefore,
                  ("CreationTimeAfter" .=) <$> _lcjCreationTimeAfter,
                  ("NextToken" .=) <$> _lcjNextToken,
                  ("SortOrder" .=) <$> _lcjSortOrder,
                  ("LastModifiedTimeAfter" .=) <$>
                    _lcjLastModifiedTimeAfter,
                  ("CreationTimeBefore" .=) <$> _lcjCreationTimeBefore,
                  ("StatusEquals" .=) <$> _lcjStatusEquals,
                  ("MaxResults" .=) <$> _lcjMaxResults,
                  ("SortBy" .=) <$> _lcjSortBy])

instance ToPath ListCompilationJobs where
        toPath = const "/"

instance ToQuery ListCompilationJobs where
        toQuery = const mempty

-- | /See:/ 'listCompilationJobsResponse' smart constructor.
data ListCompilationJobsResponse = ListCompilationJobsResponse'
  { _lcjrsNextToken               :: !(Maybe Text)
  , _lcjrsResponseStatus          :: !Int
  , _lcjrsCompilationJobSummaries :: ![CompilationJobSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCompilationJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcjrsNextToken' - If the response is truncated, Amazon SageMaker returns this @NextToken@ . To retrieve the next set of model compilation jobs, use this token in the next request.
--
-- * 'lcjrsResponseStatus' - -- | The response status code.
--
-- * 'lcjrsCompilationJobSummaries' - An array of 'CompilationJobSummary' objects, each describing a model compilation job.
listCompilationJobsResponse
    :: Int -- ^ 'lcjrsResponseStatus'
    -> ListCompilationJobsResponse
listCompilationJobsResponse pResponseStatus_ =
  ListCompilationJobsResponse'
    { _lcjrsNextToken = Nothing
    , _lcjrsResponseStatus = pResponseStatus_
    , _lcjrsCompilationJobSummaries = mempty
    }


-- | If the response is truncated, Amazon SageMaker returns this @NextToken@ . To retrieve the next set of model compilation jobs, use this token in the next request.
lcjrsNextToken :: Lens' ListCompilationJobsResponse (Maybe Text)
lcjrsNextToken = lens _lcjrsNextToken (\ s a -> s{_lcjrsNextToken = a})

-- | -- | The response status code.
lcjrsResponseStatus :: Lens' ListCompilationJobsResponse Int
lcjrsResponseStatus = lens _lcjrsResponseStatus (\ s a -> s{_lcjrsResponseStatus = a})

-- | An array of 'CompilationJobSummary' objects, each describing a model compilation job.
lcjrsCompilationJobSummaries :: Lens' ListCompilationJobsResponse [CompilationJobSummary]
lcjrsCompilationJobSummaries = lens _lcjrsCompilationJobSummaries (\ s a -> s{_lcjrsCompilationJobSummaries = a}) . _Coerce

instance NFData ListCompilationJobsResponse where
