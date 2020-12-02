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
-- Module      : Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of key phrase detection jobs that you have submitted.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
  ( -- * Creating a Request
    listKeyPhrasesDetectionJobs,
    ListKeyPhrasesDetectionJobs,

    -- * Request Lenses
    lkpdjNextToken,
    lkpdjFilter,
    lkpdjMaxResults,

    -- * Destructuring the Response
    listKeyPhrasesDetectionJobsResponse,
    ListKeyPhrasesDetectionJobsResponse,

    -- * Response Lenses
    lkpdjrsKeyPhrasesDetectionJobPropertiesList,
    lkpdjrsNextToken,
    lkpdjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listKeyPhrasesDetectionJobs' smart constructor.
data ListKeyPhrasesDetectionJobs = ListKeyPhrasesDetectionJobs'
  { _lkpdjNextToken ::
      !(Maybe Text),
    _lkpdjFilter ::
      !( Maybe
           KeyPhrasesDetectionJobFilter
       ),
    _lkpdjMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListKeyPhrasesDetectionJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lkpdjNextToken' - Identifies the next page of results to return.
--
-- * 'lkpdjFilter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- * 'lkpdjMaxResults' - The maximum number of results to return in each page. The default is 100.
listKeyPhrasesDetectionJobs ::
  ListKeyPhrasesDetectionJobs
listKeyPhrasesDetectionJobs =
  ListKeyPhrasesDetectionJobs'
    { _lkpdjNextToken = Nothing,
      _lkpdjFilter = Nothing,
      _lkpdjMaxResults = Nothing
    }

-- | Identifies the next page of results to return.
lkpdjNextToken :: Lens' ListKeyPhrasesDetectionJobs (Maybe Text)
lkpdjNextToken = lens _lkpdjNextToken (\s a -> s {_lkpdjNextToken = a})

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
lkpdjFilter :: Lens' ListKeyPhrasesDetectionJobs (Maybe KeyPhrasesDetectionJobFilter)
lkpdjFilter = lens _lkpdjFilter (\s a -> s {_lkpdjFilter = a})

-- | The maximum number of results to return in each page. The default is 100.
lkpdjMaxResults :: Lens' ListKeyPhrasesDetectionJobs (Maybe Natural)
lkpdjMaxResults = lens _lkpdjMaxResults (\s a -> s {_lkpdjMaxResults = a}) . mapping _Nat

instance AWSPager ListKeyPhrasesDetectionJobs where
  page rq rs
    | stop (rs ^. lkpdjrsNextToken) = Nothing
    | stop (rs ^. lkpdjrsKeyPhrasesDetectionJobPropertiesList) =
      Nothing
    | otherwise = Just $ rq & lkpdjNextToken .~ rs ^. lkpdjrsNextToken

instance AWSRequest ListKeyPhrasesDetectionJobs where
  type
    Rs ListKeyPhrasesDetectionJobs =
      ListKeyPhrasesDetectionJobsResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          ListKeyPhrasesDetectionJobsResponse'
            <$> (x .?> "KeyPhrasesDetectionJobPropertiesList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListKeyPhrasesDetectionJobs

instance NFData ListKeyPhrasesDetectionJobs

instance ToHeaders ListKeyPhrasesDetectionJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.ListKeyPhrasesDetectionJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListKeyPhrasesDetectionJobs where
  toJSON ListKeyPhrasesDetectionJobs' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lkpdjNextToken,
            ("Filter" .=) <$> _lkpdjFilter,
            ("MaxResults" .=) <$> _lkpdjMaxResults
          ]
      )

instance ToPath ListKeyPhrasesDetectionJobs where
  toPath = const "/"

instance ToQuery ListKeyPhrasesDetectionJobs where
  toQuery = const mempty

-- | /See:/ 'listKeyPhrasesDetectionJobsResponse' smart constructor.
data ListKeyPhrasesDetectionJobsResponse = ListKeyPhrasesDetectionJobsResponse'
  { _lkpdjrsKeyPhrasesDetectionJobPropertiesList ::
      !( Maybe
           [KeyPhrasesDetectionJobProperties]
       ),
    _lkpdjrsNextToken ::
      !(Maybe Text),
    _lkpdjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListKeyPhrasesDetectionJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lkpdjrsKeyPhrasesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- * 'lkpdjrsNextToken' - Identifies the next page of results to return.
--
-- * 'lkpdjrsResponseStatus' - -- | The response status code.
listKeyPhrasesDetectionJobsResponse ::
  -- | 'lkpdjrsResponseStatus'
  Int ->
  ListKeyPhrasesDetectionJobsResponse
listKeyPhrasesDetectionJobsResponse pResponseStatus_ =
  ListKeyPhrasesDetectionJobsResponse'
    { _lkpdjrsKeyPhrasesDetectionJobPropertiesList =
        Nothing,
      _lkpdjrsNextToken = Nothing,
      _lkpdjrsResponseStatus = pResponseStatus_
    }

-- | A list containing the properties of each job that is returned.
lkpdjrsKeyPhrasesDetectionJobPropertiesList :: Lens' ListKeyPhrasesDetectionJobsResponse [KeyPhrasesDetectionJobProperties]
lkpdjrsKeyPhrasesDetectionJobPropertiesList = lens _lkpdjrsKeyPhrasesDetectionJobPropertiesList (\s a -> s {_lkpdjrsKeyPhrasesDetectionJobPropertiesList = a}) . _Default . _Coerce

-- | Identifies the next page of results to return.
lkpdjrsNextToken :: Lens' ListKeyPhrasesDetectionJobsResponse (Maybe Text)
lkpdjrsNextToken = lens _lkpdjrsNextToken (\s a -> s {_lkpdjrsNextToken = a})

-- | -- | The response status code.
lkpdjrsResponseStatus :: Lens' ListKeyPhrasesDetectionJobsResponse Int
lkpdjrsResponseStatus = lens _lkpdjrsResponseStatus (\s a -> s {_lkpdjrsResponseStatus = a})

instance NFData ListKeyPhrasesDetectionJobsResponse
