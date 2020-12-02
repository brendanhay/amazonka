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
-- Module      : Network.AWS.Transcribe.ListMedicalTranscriptionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists medical transcription jobs with a specified status or substring that matches their names.
module Network.AWS.Transcribe.ListMedicalTranscriptionJobs
  ( -- * Creating a Request
    listMedicalTranscriptionJobs,
    ListMedicalTranscriptionJobs,

    -- * Request Lenses
    lmtjStatus,
    lmtjNextToken,
    lmtjJobNameContains,
    lmtjMaxResults,

    -- * Destructuring the Response
    listMedicalTranscriptionJobsResponse,
    ListMedicalTranscriptionJobsResponse,

    -- * Response Lenses
    lmtjrsStatus,
    lmtjrsNextToken,
    lmtjrsMedicalTranscriptionJobSummaries,
    lmtjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'listMedicalTranscriptionJobs' smart constructor.
data ListMedicalTranscriptionJobs = ListMedicalTranscriptionJobs'
  { _lmtjStatus ::
      !(Maybe TranscriptionJobStatus),
    _lmtjNextToken :: !(Maybe Text),
    _lmtjJobNameContains ::
      !(Maybe Text),
    _lmtjMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMedicalTranscriptionJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmtjStatus' - When specified, returns only medical transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don't specify a status, Amazon Transcribe Medical returns all transcription jobs ordered by creation date.
--
-- * 'lmtjNextToken' - If you a receive a truncated result in the previous request of @ListMedicalTranscriptionJobs@ , include @NextToken@ to fetch the next set of jobs.
--
-- * 'lmtjJobNameContains' - When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
--
-- * 'lmtjMaxResults' - The maximum number of medical transcription jobs to return in the response. IF there are fewer results in the list, this response contains only the actual results.
listMedicalTranscriptionJobs ::
  ListMedicalTranscriptionJobs
listMedicalTranscriptionJobs =
  ListMedicalTranscriptionJobs'
    { _lmtjStatus = Nothing,
      _lmtjNextToken = Nothing,
      _lmtjJobNameContains = Nothing,
      _lmtjMaxResults = Nothing
    }

-- | When specified, returns only medical transcription jobs with the specified status. Jobs are ordered by creation date, with the newest jobs returned first. If you don't specify a status, Amazon Transcribe Medical returns all transcription jobs ordered by creation date.
lmtjStatus :: Lens' ListMedicalTranscriptionJobs (Maybe TranscriptionJobStatus)
lmtjStatus = lens _lmtjStatus (\s a -> s {_lmtjStatus = a})

-- | If you a receive a truncated result in the previous request of @ListMedicalTranscriptionJobs@ , include @NextToken@ to fetch the next set of jobs.
lmtjNextToken :: Lens' ListMedicalTranscriptionJobs (Maybe Text)
lmtjNextToken = lens _lmtjNextToken (\s a -> s {_lmtjNextToken = a})

-- | When specified, the jobs returned in the list are limited to jobs whose name contains the specified string.
lmtjJobNameContains :: Lens' ListMedicalTranscriptionJobs (Maybe Text)
lmtjJobNameContains = lens _lmtjJobNameContains (\s a -> s {_lmtjJobNameContains = a})

-- | The maximum number of medical transcription jobs to return in the response. IF there are fewer results in the list, this response contains only the actual results.
lmtjMaxResults :: Lens' ListMedicalTranscriptionJobs (Maybe Natural)
lmtjMaxResults = lens _lmtjMaxResults (\s a -> s {_lmtjMaxResults = a}) . mapping _Nat

instance AWSRequest ListMedicalTranscriptionJobs where
  type
    Rs ListMedicalTranscriptionJobs =
      ListMedicalTranscriptionJobsResponse
  request = postJSON transcribe
  response =
    receiveJSON
      ( \s h x ->
          ListMedicalTranscriptionJobsResponse'
            <$> (x .?> "Status")
            <*> (x .?> "NextToken")
            <*> (x .?> "MedicalTranscriptionJobSummaries" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListMedicalTranscriptionJobs

instance NFData ListMedicalTranscriptionJobs

instance ToHeaders ListMedicalTranscriptionJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Transcribe.ListMedicalTranscriptionJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListMedicalTranscriptionJobs where
  toJSON ListMedicalTranscriptionJobs' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _lmtjStatus,
            ("NextToken" .=) <$> _lmtjNextToken,
            ("JobNameContains" .=) <$> _lmtjJobNameContains,
            ("MaxResults" .=) <$> _lmtjMaxResults
          ]
      )

instance ToPath ListMedicalTranscriptionJobs where
  toPath = const "/"

instance ToQuery ListMedicalTranscriptionJobs where
  toQuery = const mempty

-- | /See:/ 'listMedicalTranscriptionJobsResponse' smart constructor.
data ListMedicalTranscriptionJobsResponse = ListMedicalTranscriptionJobsResponse'
  { _lmtjrsStatus ::
      !( Maybe
           TranscriptionJobStatus
       ),
    _lmtjrsNextToken ::
      !(Maybe Text),
    _lmtjrsMedicalTranscriptionJobSummaries ::
      !( Maybe
           [MedicalTranscriptionJobSummary]
       ),
    _lmtjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMedicalTranscriptionJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmtjrsStatus' - The requested status of the medical transcription jobs returned.
--
-- * 'lmtjrsNextToken' - The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If the number of jobs exceeds what can fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. Include the token in the next request to the @ListMedicalTranscriptionJobs@ operation to return in the next page of jobs.
--
-- * 'lmtjrsMedicalTranscriptionJobSummaries' - A list of objects containing summary information for a transcription job.
--
-- * 'lmtjrsResponseStatus' - -- | The response status code.
listMedicalTranscriptionJobsResponse ::
  -- | 'lmtjrsResponseStatus'
  Int ->
  ListMedicalTranscriptionJobsResponse
listMedicalTranscriptionJobsResponse pResponseStatus_ =
  ListMedicalTranscriptionJobsResponse'
    { _lmtjrsStatus = Nothing,
      _lmtjrsNextToken = Nothing,
      _lmtjrsMedicalTranscriptionJobSummaries = Nothing,
      _lmtjrsResponseStatus = pResponseStatus_
    }

-- | The requested status of the medical transcription jobs returned.
lmtjrsStatus :: Lens' ListMedicalTranscriptionJobsResponse (Maybe TranscriptionJobStatus)
lmtjrsStatus = lens _lmtjrsStatus (\s a -> s {_lmtjrsStatus = a})

-- | The @ListMedicalTranscriptionJobs@ operation returns a page of jobs at a time. The maximum size of the page is set by the @MaxResults@ parameter. If the number of jobs exceeds what can fit on a page, Amazon Transcribe Medical returns the @NextPage@ token. Include the token in the next request to the @ListMedicalTranscriptionJobs@ operation to return in the next page of jobs.
lmtjrsNextToken :: Lens' ListMedicalTranscriptionJobsResponse (Maybe Text)
lmtjrsNextToken = lens _lmtjrsNextToken (\s a -> s {_lmtjrsNextToken = a})

-- | A list of objects containing summary information for a transcription job.
lmtjrsMedicalTranscriptionJobSummaries :: Lens' ListMedicalTranscriptionJobsResponse [MedicalTranscriptionJobSummary]
lmtjrsMedicalTranscriptionJobSummaries = lens _lmtjrsMedicalTranscriptionJobSummaries (\s a -> s {_lmtjrsMedicalTranscriptionJobSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lmtjrsResponseStatus :: Lens' ListMedicalTranscriptionJobsResponse Int
lmtjrsResponseStatus = lens _lmtjrsResponseStatus (\s a -> s {_lmtjrsResponseStatus = a})

instance NFData ListMedicalTranscriptionJobsResponse
