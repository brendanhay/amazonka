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
-- Module      : Network.AWS.Batch.ListJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task jobs for a specified job queue. You can filter the results by job status with the @jobStatus@ parameter. If you do not specify a status, only @RUNNING@ jobs are returned.
--
--
module Network.AWS.Batch.ListJobs
    (
    -- * Creating a Request
      listJobs
    , ListJobs
    -- * Request Lenses
    , ljNextToken
    , ljJobStatus
    , ljArrayJobId
    , ljJobQueue
    , ljMaxResults

    -- * Destructuring the Response
    , listJobsResponse
    , ListJobsResponse
    -- * Response Lenses
    , ljrsNextToken
    , ljrsResponseStatus
    , ljrsJobSummaryList
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listJobs' smart constructor.
data ListJobs = ListJobs'
  { _ljNextToken  :: !(Maybe Text)
  , _ljJobStatus  :: !(Maybe JobStatus)
  , _ljArrayJobId :: !(Maybe Text)
  , _ljJobQueue   :: !(Maybe Text)
  , _ljMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljNextToken' - The @nextToken@ value returned from a previous paginated @ListJobs@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- * 'ljJobStatus' - The job status with which to filter jobs in the specified queue. If you do not specify a status, only @RUNNING@ jobs are returned.
--
-- * 'ljArrayJobId' - The job ID for an array job. Specifying an array job ID with this parameter lists all child jobs from within the specified array.
--
-- * 'ljJobQueue' - The name or full Amazon Resource Name (ARN) of the job queue with which to list jobs.
--
-- * 'ljMaxResults' - The maximum number of results returned by @ListJobs@ in paginated output. When this parameter is used, @ListJobs@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListJobs@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListJobs@ returns up to 100 results and a @nextToken@ value if applicable.
listJobs
    :: ListJobs
listJobs =
  ListJobs'
    { _ljNextToken = Nothing
    , _ljJobStatus = Nothing
    , _ljArrayJobId = Nothing
    , _ljJobQueue = Nothing
    , _ljMaxResults = Nothing
    }


-- | The @nextToken@ value returned from a previous paginated @ListJobs@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
ljNextToken :: Lens' ListJobs (Maybe Text)
ljNextToken = lens _ljNextToken (\ s a -> s{_ljNextToken = a})

-- | The job status with which to filter jobs in the specified queue. If you do not specify a status, only @RUNNING@ jobs are returned.
ljJobStatus :: Lens' ListJobs (Maybe JobStatus)
ljJobStatus = lens _ljJobStatus (\ s a -> s{_ljJobStatus = a})

-- | The job ID for an array job. Specifying an array job ID with this parameter lists all child jobs from within the specified array.
ljArrayJobId :: Lens' ListJobs (Maybe Text)
ljArrayJobId = lens _ljArrayJobId (\ s a -> s{_ljArrayJobId = a})

-- | The name or full Amazon Resource Name (ARN) of the job queue with which to list jobs.
ljJobQueue :: Lens' ListJobs (Maybe Text)
ljJobQueue = lens _ljJobQueue (\ s a -> s{_ljJobQueue = a})

-- | The maximum number of results returned by @ListJobs@ in paginated output. When this parameter is used, @ListJobs@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListJobs@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListJobs@ returns up to 100 results and a @nextToken@ value if applicable.
ljMaxResults :: Lens' ListJobs (Maybe Int)
ljMaxResults = lens _ljMaxResults (\ s a -> s{_ljMaxResults = a})

instance AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 ListJobsResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "jobSummaryList" .!@ mempty))

instance Hashable ListJobs where

instance NFData ListJobs where

instance ToHeaders ListJobs where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListJobs where
        toJSON ListJobs'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _ljNextToken,
                  ("jobStatus" .=) <$> _ljJobStatus,
                  ("arrayJobId" .=) <$> _ljArrayJobId,
                  ("jobQueue" .=) <$> _ljJobQueue,
                  ("maxResults" .=) <$> _ljMaxResults])

instance ToPath ListJobs where
        toPath = const "/v1/listjobs"

instance ToQuery ListJobs where
        toQuery = const mempty

-- | /See:/ 'listJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { _ljrsNextToken      :: !(Maybe Text)
  , _ljrsResponseStatus :: !Int
  , _ljrsJobSummaryList :: ![JobSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljrsNextToken' - The @nextToken@ value to include in a future @ListJobs@ request. When the results of a @ListJobs@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'ljrsResponseStatus' - -- | The response status code.
--
-- * 'ljrsJobSummaryList' - A list of job summaries that match the request.
listJobsResponse
    :: Int -- ^ 'ljrsResponseStatus'
    -> ListJobsResponse
listJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { _ljrsNextToken = Nothing
    , _ljrsResponseStatus = pResponseStatus_
    , _ljrsJobSummaryList = mempty
    }


-- | The @nextToken@ value to include in a future @ListJobs@ request. When the results of a @ListJobs@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
ljrsNextToken :: Lens' ListJobsResponse (Maybe Text)
ljrsNextToken = lens _ljrsNextToken (\ s a -> s{_ljrsNextToken = a})

-- | -- | The response status code.
ljrsResponseStatus :: Lens' ListJobsResponse Int
ljrsResponseStatus = lens _ljrsResponseStatus (\ s a -> s{_ljrsResponseStatus = a})

-- | A list of job summaries that match the request.
ljrsJobSummaryList :: Lens' ListJobsResponse [JobSummary]
ljrsJobSummaryList = lens _ljrsJobSummaryList (\ s a -> s{_ljrsJobSummaryList = a}) . _Coerce

instance NFData ListJobsResponse where
