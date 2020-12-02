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
-- Module      : Network.AWS.Comprehend.ListEventsDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the events detection jobs that you have submitted.
module Network.AWS.Comprehend.ListEventsDetectionJobs
  ( -- * Creating a Request
    listEventsDetectionJobs,
    ListEventsDetectionJobs,

    -- * Request Lenses
    lNextToken,
    lFilter,
    lMaxResults,

    -- * Destructuring the Response
    listEventsDetectionJobsResponse,
    ListEventsDetectionJobsResponse,

    -- * Response Lenses
    lrsEventsDetectionJobPropertiesList,
    lrsNextToken,
    lrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listEventsDetectionJobs' smart constructor.
data ListEventsDetectionJobs = ListEventsDetectionJobs'
  { _lNextToken ::
      !(Maybe Text),
    _lFilter ::
      !(Maybe EventsDetectionJobFilter),
    _lMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListEventsDetectionJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - Identifies the next page of results to return.
--
-- * 'lFilter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- * 'lMaxResults' - The maximum number of results to return in each page.
listEventsDetectionJobs ::
  ListEventsDetectionJobs
listEventsDetectionJobs =
  ListEventsDetectionJobs'
    { _lNextToken = Nothing,
      _lFilter = Nothing,
      _lMaxResults = Nothing
    }

-- | Identifies the next page of results to return.
lNextToken :: Lens' ListEventsDetectionJobs (Maybe Text)
lNextToken = lens _lNextToken (\s a -> s {_lNextToken = a})

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
lFilter :: Lens' ListEventsDetectionJobs (Maybe EventsDetectionJobFilter)
lFilter = lens _lFilter (\s a -> s {_lFilter = a})

-- | The maximum number of results to return in each page.
lMaxResults :: Lens' ListEventsDetectionJobs (Maybe Natural)
lMaxResults = lens _lMaxResults (\s a -> s {_lMaxResults = a}) . mapping _Nat

instance AWSRequest ListEventsDetectionJobs where
  type Rs ListEventsDetectionJobs = ListEventsDetectionJobsResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          ListEventsDetectionJobsResponse'
            <$> (x .?> "EventsDetectionJobPropertiesList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListEventsDetectionJobs

instance NFData ListEventsDetectionJobs

instance ToHeaders ListEventsDetectionJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.ListEventsDetectionJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListEventsDetectionJobs where
  toJSON ListEventsDetectionJobs' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lNextToken,
            ("Filter" .=) <$> _lFilter,
            ("MaxResults" .=) <$> _lMaxResults
          ]
      )

instance ToPath ListEventsDetectionJobs where
  toPath = const "/"

instance ToQuery ListEventsDetectionJobs where
  toQuery = const mempty

-- | /See:/ 'listEventsDetectionJobsResponse' smart constructor.
data ListEventsDetectionJobsResponse = ListEventsDetectionJobsResponse'
  { _lrsEventsDetectionJobPropertiesList ::
      !( Maybe
           [EventsDetectionJobProperties]
       ),
    _lrsNextToken ::
      !(Maybe Text),
    _lrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListEventsDetectionJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsEventsDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- * 'lrsNextToken' - Identifies the next page of results to return.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listEventsDetectionJobsResponse ::
  -- | 'lrsResponseStatus'
  Int ->
  ListEventsDetectionJobsResponse
listEventsDetectionJobsResponse pResponseStatus_ =
  ListEventsDetectionJobsResponse'
    { _lrsEventsDetectionJobPropertiesList =
        Nothing,
      _lrsNextToken = Nothing,
      _lrsResponseStatus = pResponseStatus_
    }

-- | A list containing the properties of each job that is returned.
lrsEventsDetectionJobPropertiesList :: Lens' ListEventsDetectionJobsResponse [EventsDetectionJobProperties]
lrsEventsDetectionJobPropertiesList = lens _lrsEventsDetectionJobPropertiesList (\s a -> s {_lrsEventsDetectionJobPropertiesList = a}) . _Default . _Coerce

-- | Identifies the next page of results to return.
lrsNextToken :: Lens' ListEventsDetectionJobsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\s a -> s {_lrsNextToken = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListEventsDetectionJobsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\s a -> s {_lrsResponseStatus = a})

instance NFData ListEventsDetectionJobsResponse
