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
-- Module      : Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the PII entity detection jobs that you have submitted.
module Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs
  ( -- * Creating a Request
    listPiiEntitiesDetectionJobs,
    ListPiiEntitiesDetectionJobs,

    -- * Request Lenses
    lpedjNextToken,
    lpedjFilter,
    lpedjMaxResults,

    -- * Destructuring the Response
    listPiiEntitiesDetectionJobsResponse,
    ListPiiEntitiesDetectionJobsResponse,

    -- * Response Lenses
    lpedjrsNextToken,
    lpedjrsPiiEntitiesDetectionJobPropertiesList,
    lpedjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPiiEntitiesDetectionJobs' smart constructor.
data ListPiiEntitiesDetectionJobs = ListPiiEntitiesDetectionJobs'
  { _lpedjNextToken ::
      !(Maybe Text),
    _lpedjFilter ::
      !( Maybe
           PiiEntitiesDetectionJobFilter
       ),
    _lpedjMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPiiEntitiesDetectionJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpedjNextToken' - Identifies the next page of results to return.
--
-- * 'lpedjFilter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- * 'lpedjMaxResults' - The maximum number of results to return in each page.
listPiiEntitiesDetectionJobs ::
  ListPiiEntitiesDetectionJobs
listPiiEntitiesDetectionJobs =
  ListPiiEntitiesDetectionJobs'
    { _lpedjNextToken = Nothing,
      _lpedjFilter = Nothing,
      _lpedjMaxResults = Nothing
    }

-- | Identifies the next page of results to return.
lpedjNextToken :: Lens' ListPiiEntitiesDetectionJobs (Maybe Text)
lpedjNextToken = lens _lpedjNextToken (\s a -> s {_lpedjNextToken = a})

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
lpedjFilter :: Lens' ListPiiEntitiesDetectionJobs (Maybe PiiEntitiesDetectionJobFilter)
lpedjFilter = lens _lpedjFilter (\s a -> s {_lpedjFilter = a})

-- | The maximum number of results to return in each page.
lpedjMaxResults :: Lens' ListPiiEntitiesDetectionJobs (Maybe Natural)
lpedjMaxResults = lens _lpedjMaxResults (\s a -> s {_lpedjMaxResults = a}) . mapping _Nat

instance AWSRequest ListPiiEntitiesDetectionJobs where
  type
    Rs ListPiiEntitiesDetectionJobs =
      ListPiiEntitiesDetectionJobsResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          ListPiiEntitiesDetectionJobsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "PiiEntitiesDetectionJobPropertiesList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListPiiEntitiesDetectionJobs

instance NFData ListPiiEntitiesDetectionJobs

instance ToHeaders ListPiiEntitiesDetectionJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.ListPiiEntitiesDetectionJobs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListPiiEntitiesDetectionJobs where
  toJSON ListPiiEntitiesDetectionJobs' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lpedjNextToken,
            ("Filter" .=) <$> _lpedjFilter,
            ("MaxResults" .=) <$> _lpedjMaxResults
          ]
      )

instance ToPath ListPiiEntitiesDetectionJobs where
  toPath = const "/"

instance ToQuery ListPiiEntitiesDetectionJobs where
  toQuery = const mempty

-- | /See:/ 'listPiiEntitiesDetectionJobsResponse' smart constructor.
data ListPiiEntitiesDetectionJobsResponse = ListPiiEntitiesDetectionJobsResponse'
  { _lpedjrsNextToken ::
      !(Maybe Text),
    _lpedjrsPiiEntitiesDetectionJobPropertiesList ::
      !( Maybe
           [PiiEntitiesDetectionJobProperties]
       ),
    _lpedjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPiiEntitiesDetectionJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpedjrsNextToken' - Identifies the next page of results to return.
--
-- * 'lpedjrsPiiEntitiesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- * 'lpedjrsResponseStatus' - -- | The response status code.
listPiiEntitiesDetectionJobsResponse ::
  -- | 'lpedjrsResponseStatus'
  Int ->
  ListPiiEntitiesDetectionJobsResponse
listPiiEntitiesDetectionJobsResponse pResponseStatus_ =
  ListPiiEntitiesDetectionJobsResponse'
    { _lpedjrsNextToken =
        Nothing,
      _lpedjrsPiiEntitiesDetectionJobPropertiesList = Nothing,
      _lpedjrsResponseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
lpedjrsNextToken :: Lens' ListPiiEntitiesDetectionJobsResponse (Maybe Text)
lpedjrsNextToken = lens _lpedjrsNextToken (\s a -> s {_lpedjrsNextToken = a})

-- | A list containing the properties of each job that is returned.
lpedjrsPiiEntitiesDetectionJobPropertiesList :: Lens' ListPiiEntitiesDetectionJobsResponse [PiiEntitiesDetectionJobProperties]
lpedjrsPiiEntitiesDetectionJobPropertiesList = lens _lpedjrsPiiEntitiesDetectionJobPropertiesList (\s a -> s {_lpedjrsPiiEntitiesDetectionJobPropertiesList = a}) . _Default . _Coerce

-- | -- | The response status code.
lpedjrsResponseStatus :: Lens' ListPiiEntitiesDetectionJobsResponse Int
lpedjrsResponseStatus = lens _lpedjrsResponseStatus (\s a -> s {_lpedjrsResponseStatus = a})

instance NFData ListPiiEntitiesDetectionJobsResponse
