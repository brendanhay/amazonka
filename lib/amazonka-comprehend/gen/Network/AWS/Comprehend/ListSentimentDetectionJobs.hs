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
-- Module      : Network.AWS.Comprehend.ListSentimentDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of sentiment detection jobs that you have submitted.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListSentimentDetectionJobs
  ( -- * Creating a Request
    listSentimentDetectionJobs,
    ListSentimentDetectionJobs,

    -- * Request Lenses
    lsdjNextToken,
    lsdjFilter,
    lsdjMaxResults,

    -- * Destructuring the Response
    listSentimentDetectionJobsResponse,
    ListSentimentDetectionJobsResponse,

    -- * Response Lenses
    lsdjrsNextToken,
    lsdjrsSentimentDetectionJobPropertiesList,
    lsdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSentimentDetectionJobs' smart constructor.
data ListSentimentDetectionJobs = ListSentimentDetectionJobs'
  { _lsdjNextToken ::
      !(Maybe Text),
    _lsdjFilter ::
      !(Maybe SentimentDetectionJobFilter),
    _lsdjMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSentimentDetectionJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsdjNextToken' - Identifies the next page of results to return.
--
-- * 'lsdjFilter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- * 'lsdjMaxResults' - The maximum number of results to return in each page. The default is 100.
listSentimentDetectionJobs ::
  ListSentimentDetectionJobs
listSentimentDetectionJobs =
  ListSentimentDetectionJobs'
    { _lsdjNextToken = Nothing,
      _lsdjFilter = Nothing,
      _lsdjMaxResults = Nothing
    }

-- | Identifies the next page of results to return.
lsdjNextToken :: Lens' ListSentimentDetectionJobs (Maybe Text)
lsdjNextToken = lens _lsdjNextToken (\s a -> s {_lsdjNextToken = a})

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
lsdjFilter :: Lens' ListSentimentDetectionJobs (Maybe SentimentDetectionJobFilter)
lsdjFilter = lens _lsdjFilter (\s a -> s {_lsdjFilter = a})

-- | The maximum number of results to return in each page. The default is 100.
lsdjMaxResults :: Lens' ListSentimentDetectionJobs (Maybe Natural)
lsdjMaxResults = lens _lsdjMaxResults (\s a -> s {_lsdjMaxResults = a}) . mapping _Nat

instance AWSPager ListSentimentDetectionJobs where
  page rq rs
    | stop (rs ^. lsdjrsNextToken) = Nothing
    | stop (rs ^. lsdjrsSentimentDetectionJobPropertiesList) = Nothing
    | otherwise = Just $ rq & lsdjNextToken .~ rs ^. lsdjrsNextToken

instance AWSRequest ListSentimentDetectionJobs where
  type
    Rs ListSentimentDetectionJobs =
      ListSentimentDetectionJobsResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          ListSentimentDetectionJobsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "SentimentDetectionJobPropertiesList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListSentimentDetectionJobs

instance NFData ListSentimentDetectionJobs

instance ToHeaders ListSentimentDetectionJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.ListSentimentDetectionJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListSentimentDetectionJobs where
  toJSON ListSentimentDetectionJobs' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lsdjNextToken,
            ("Filter" .=) <$> _lsdjFilter,
            ("MaxResults" .=) <$> _lsdjMaxResults
          ]
      )

instance ToPath ListSentimentDetectionJobs where
  toPath = const "/"

instance ToQuery ListSentimentDetectionJobs where
  toQuery = const mempty

-- | /See:/ 'listSentimentDetectionJobsResponse' smart constructor.
data ListSentimentDetectionJobsResponse = ListSentimentDetectionJobsResponse'
  { _lsdjrsNextToken ::
      !(Maybe Text),
    _lsdjrsSentimentDetectionJobPropertiesList ::
      !( Maybe
           [SentimentDetectionJobProperties]
       ),
    _lsdjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSentimentDetectionJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsdjrsNextToken' - Identifies the next page of results to return.
--
-- * 'lsdjrsSentimentDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- * 'lsdjrsResponseStatus' - -- | The response status code.
listSentimentDetectionJobsResponse ::
  -- | 'lsdjrsResponseStatus'
  Int ->
  ListSentimentDetectionJobsResponse
listSentimentDetectionJobsResponse pResponseStatus_ =
  ListSentimentDetectionJobsResponse'
    { _lsdjrsNextToken = Nothing,
      _lsdjrsSentimentDetectionJobPropertiesList = Nothing,
      _lsdjrsResponseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
lsdjrsNextToken :: Lens' ListSentimentDetectionJobsResponse (Maybe Text)
lsdjrsNextToken = lens _lsdjrsNextToken (\s a -> s {_lsdjrsNextToken = a})

-- | A list containing the properties of each job that is returned.
lsdjrsSentimentDetectionJobPropertiesList :: Lens' ListSentimentDetectionJobsResponse [SentimentDetectionJobProperties]
lsdjrsSentimentDetectionJobPropertiesList = lens _lsdjrsSentimentDetectionJobPropertiesList (\s a -> s {_lsdjrsSentimentDetectionJobPropertiesList = a}) . _Default . _Coerce

-- | -- | The response status code.
lsdjrsResponseStatus :: Lens' ListSentimentDetectionJobsResponse Int
lsdjrsResponseStatus = lens _lsdjrsResponseStatus (\s a -> s {_lsdjrsResponseStatus = a})

instance NFData ListSentimentDetectionJobsResponse
