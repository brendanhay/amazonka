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
-- Module      : Network.AWS.IoT.ListJobExecutionsForJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for a job.
--
--
module Network.AWS.IoT.ListJobExecutionsForJob
    (
    -- * Creating a Request
      listJobExecutionsForJob
    , ListJobExecutionsForJob
    -- * Request Lenses
    , ljefjStatus
    , ljefjNextToken
    , ljefjMaxResults
    , ljefjJobId

    -- * Destructuring the Response
    , listJobExecutionsForJobResponse
    , ListJobExecutionsForJobResponse
    -- * Response Lenses
    , ljefjrsExecutionSummaries
    , ljefjrsNextToken
    , ljefjrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listJobExecutionsForJob' smart constructor.
data ListJobExecutionsForJob = ListJobExecutionsForJob'
  { _ljefjStatus     :: !(Maybe JobExecutionStatus)
  , _ljefjNextToken  :: !(Maybe Text)
  , _ljefjMaxResults :: !(Maybe Nat)
  , _ljefjJobId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobExecutionsForJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljefjStatus' - The status of the job.
--
-- * 'ljefjNextToken' - The token to retrieve the next set of results.
--
-- * 'ljefjMaxResults' - The maximum number of results to be returned per request.
--
-- * 'ljefjJobId' - The unique identifier you assigned to this job when it was created.
listJobExecutionsForJob
    :: Text -- ^ 'ljefjJobId'
    -> ListJobExecutionsForJob
listJobExecutionsForJob pJobId_ =
  ListJobExecutionsForJob'
    { _ljefjStatus = Nothing
    , _ljefjNextToken = Nothing
    , _ljefjMaxResults = Nothing
    , _ljefjJobId = pJobId_
    }


-- | The status of the job.
ljefjStatus :: Lens' ListJobExecutionsForJob (Maybe JobExecutionStatus)
ljefjStatus = lens _ljefjStatus (\ s a -> s{_ljefjStatus = a})

-- | The token to retrieve the next set of results.
ljefjNextToken :: Lens' ListJobExecutionsForJob (Maybe Text)
ljefjNextToken = lens _ljefjNextToken (\ s a -> s{_ljefjNextToken = a})

-- | The maximum number of results to be returned per request.
ljefjMaxResults :: Lens' ListJobExecutionsForJob (Maybe Natural)
ljefjMaxResults = lens _ljefjMaxResults (\ s a -> s{_ljefjMaxResults = a}) . mapping _Nat

-- | The unique identifier you assigned to this job when it was created.
ljefjJobId :: Lens' ListJobExecutionsForJob Text
ljefjJobId = lens _ljefjJobId (\ s a -> s{_ljefjJobId = a})

instance AWSRequest ListJobExecutionsForJob where
        type Rs ListJobExecutionsForJob =
             ListJobExecutionsForJobResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListJobExecutionsForJobResponse' <$>
                   (x .?> "executionSummaries" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListJobExecutionsForJob where

instance NFData ListJobExecutionsForJob where

instance ToHeaders ListJobExecutionsForJob where
        toHeaders = const mempty

instance ToPath ListJobExecutionsForJob where
        toPath ListJobExecutionsForJob'{..}
          = mconcat ["/jobs/", toBS _ljefjJobId, "/things"]

instance ToQuery ListJobExecutionsForJob where
        toQuery ListJobExecutionsForJob'{..}
          = mconcat
              ["status" =: _ljefjStatus,
               "nextToken" =: _ljefjNextToken,
               "maxResults" =: _ljefjMaxResults]

-- | /See:/ 'listJobExecutionsForJobResponse' smart constructor.
data ListJobExecutionsForJobResponse = ListJobExecutionsForJobResponse'
  { _ljefjrsExecutionSummaries :: !(Maybe [JobExecutionSummaryForJob])
  , _ljefjrsNextToken          :: !(Maybe Text)
  , _ljefjrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobExecutionsForJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljefjrsExecutionSummaries' - A list of job execution summaries.
--
-- * 'ljefjrsNextToken' - The token for the next set of results, or __null__ if there are no additional results.
--
-- * 'ljefjrsResponseStatus' - -- | The response status code.
listJobExecutionsForJobResponse
    :: Int -- ^ 'ljefjrsResponseStatus'
    -> ListJobExecutionsForJobResponse
listJobExecutionsForJobResponse pResponseStatus_ =
  ListJobExecutionsForJobResponse'
    { _ljefjrsExecutionSummaries = Nothing
    , _ljefjrsNextToken = Nothing
    , _ljefjrsResponseStatus = pResponseStatus_
    }


-- | A list of job execution summaries.
ljefjrsExecutionSummaries :: Lens' ListJobExecutionsForJobResponse [JobExecutionSummaryForJob]
ljefjrsExecutionSummaries = lens _ljefjrsExecutionSummaries (\ s a -> s{_ljefjrsExecutionSummaries = a}) . _Default . _Coerce

-- | The token for the next set of results, or __null__ if there are no additional results.
ljefjrsNextToken :: Lens' ListJobExecutionsForJobResponse (Maybe Text)
ljefjrsNextToken = lens _ljefjrsNextToken (\ s a -> s{_ljefjrsNextToken = a})

-- | -- | The response status code.
ljefjrsResponseStatus :: Lens' ListJobExecutionsForJobResponse Int
ljefjrsResponseStatus = lens _ljefjrsResponseStatus (\ s a -> s{_ljefjrsResponseStatus = a})

instance NFData ListJobExecutionsForJobResponse where
