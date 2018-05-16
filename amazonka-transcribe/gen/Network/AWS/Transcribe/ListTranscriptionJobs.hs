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
-- Module      : Network.AWS.Transcribe.ListTranscriptionJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists transcription jobs with the specified status.
--
--
module Network.AWS.Transcribe.ListTranscriptionJobs
    (
    -- * Creating a Request
      listTranscriptionJobs
    , ListTranscriptionJobs
    -- * Request Lenses
    , ltjStatus
    , ltjNextToken
    , ltjJobNameContains
    , ltjMaxResults

    -- * Destructuring the Response
    , listTranscriptionJobsResponse
    , ListTranscriptionJobsResponse
    -- * Response Lenses
    , ltjrsStatus
    , ltjrsNextToken
    , ltjrsTranscriptionJobSummaries
    , ltjrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'listTranscriptionJobs' smart constructor.
data ListTranscriptionJobs = ListTranscriptionJobs'
  { _ltjStatus          :: !(Maybe TranscriptionJobStatus)
  , _ltjNextToken       :: !(Maybe Text)
  , _ltjJobNameContains :: !(Maybe Text)
  , _ltjMaxResults      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTranscriptionJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltjStatus' - When specified, returns only transcription jobs with the specified status.
--
-- * 'ltjNextToken' - If the result of the previous request to @ListTranscriptionJobs@ was truncated, include the @NextToken@ to fetch the next set of jobs.
--
-- * 'ltjJobNameContains' - When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
--
-- * 'ltjMaxResults' - The maximum number of jobs to return in the response. If there are fewer results in the list, this response contains only the actual results.
listTranscriptionJobs
    :: ListTranscriptionJobs
listTranscriptionJobs =
  ListTranscriptionJobs'
    { _ltjStatus = Nothing
    , _ltjNextToken = Nothing
    , _ltjJobNameContains = Nothing
    , _ltjMaxResults = Nothing
    }


-- | When specified, returns only transcription jobs with the specified status.
ltjStatus :: Lens' ListTranscriptionJobs (Maybe TranscriptionJobStatus)
ltjStatus = lens _ltjStatus (\ s a -> s{_ltjStatus = a})

-- | If the result of the previous request to @ListTranscriptionJobs@ was truncated, include the @NextToken@ to fetch the next set of jobs.
ltjNextToken :: Lens' ListTranscriptionJobs (Maybe Text)
ltjNextToken = lens _ltjNextToken (\ s a -> s{_ltjNextToken = a})

-- | When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
ltjJobNameContains :: Lens' ListTranscriptionJobs (Maybe Text)
ltjJobNameContains = lens _ltjJobNameContains (\ s a -> s{_ltjJobNameContains = a})

-- | The maximum number of jobs to return in the response. If there are fewer results in the list, this response contains only the actual results.
ltjMaxResults :: Lens' ListTranscriptionJobs (Maybe Natural)
ltjMaxResults = lens _ltjMaxResults (\ s a -> s{_ltjMaxResults = a}) . mapping _Nat

instance AWSRequest ListTranscriptionJobs where
        type Rs ListTranscriptionJobs =
             ListTranscriptionJobsResponse
        request = postJSON transcribe
        response
          = receiveJSON
              (\ s h x ->
                 ListTranscriptionJobsResponse' <$>
                   (x .?> "Status") <*> (x .?> "NextToken") <*>
                     (x .?> "TranscriptionJobSummaries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTranscriptionJobs where

instance NFData ListTranscriptionJobs where

instance ToHeaders ListTranscriptionJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.ListTranscriptionJobs" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTranscriptionJobs where
        toJSON ListTranscriptionJobs'{..}
          = object
              (catMaybes
                 [("Status" .=) <$> _ltjStatus,
                  ("NextToken" .=) <$> _ltjNextToken,
                  ("JobNameContains" .=) <$> _ltjJobNameContains,
                  ("MaxResults" .=) <$> _ltjMaxResults])

instance ToPath ListTranscriptionJobs where
        toPath = const "/"

instance ToQuery ListTranscriptionJobs where
        toQuery = const mempty

-- | /See:/ 'listTranscriptionJobsResponse' smart constructor.
data ListTranscriptionJobsResponse = ListTranscriptionJobsResponse'
  { _ltjrsStatus                    :: !(Maybe TranscriptionJobStatus)
  , _ltjrsNextToken                 :: !(Maybe Text)
  , _ltjrsTranscriptionJobSummaries :: !(Maybe [TranscriptionJobSummary])
  , _ltjrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTranscriptionJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltjrsStatus' - The requested status of the jobs returned.
--
-- * 'ltjrsNextToken' - The @ListTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListTranscriptionJobs@ operation to return in the next page of jobs.
--
-- * 'ltjrsTranscriptionJobSummaries' - A list of objects containing summary information for a transcription job.
--
-- * 'ltjrsResponseStatus' - -- | The response status code.
listTranscriptionJobsResponse
    :: Int -- ^ 'ltjrsResponseStatus'
    -> ListTranscriptionJobsResponse
listTranscriptionJobsResponse pResponseStatus_ =
  ListTranscriptionJobsResponse'
    { _ltjrsStatus = Nothing
    , _ltjrsNextToken = Nothing
    , _ltjrsTranscriptionJobSummaries = Nothing
    , _ltjrsResponseStatus = pResponseStatus_
    }


-- | The requested status of the jobs returned.
ltjrsStatus :: Lens' ListTranscriptionJobsResponse (Maybe TranscriptionJobStatus)
ltjrsStatus = lens _ltjrsStatus (\ s a -> s{_ltjrsStatus = a})

-- | The @ListTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListTranscriptionJobs@ operation to return in the next page of jobs.
ltjrsNextToken :: Lens' ListTranscriptionJobsResponse (Maybe Text)
ltjrsNextToken = lens _ltjrsNextToken (\ s a -> s{_ltjrsNextToken = a})

-- | A list of objects containing summary information for a transcription job.
ltjrsTranscriptionJobSummaries :: Lens' ListTranscriptionJobsResponse [TranscriptionJobSummary]
ltjrsTranscriptionJobSummaries = lens _ltjrsTranscriptionJobSummaries (\ s a -> s{_ltjrsTranscriptionJobSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
ltjrsResponseStatus :: Lens' ListTranscriptionJobsResponse Int
ltjrsResponseStatus = lens _ltjrsResponseStatus (\ s a -> s{_ltjrsResponseStatus = a})

instance NFData ListTranscriptionJobsResponse where
