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
-- Module      : Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the dominant language detection jobs that you have submitted.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
  ( -- * Creating a Request
    listDominantLanguageDetectionJobs,
    ListDominantLanguageDetectionJobs,

    -- * Request Lenses
    ldldjNextToken,
    ldldjFilter,
    ldldjMaxResults,

    -- * Destructuring the Response
    listDominantLanguageDetectionJobsResponse,
    ListDominantLanguageDetectionJobsResponse,

    -- * Response Lenses
    ldldjrsNextToken,
    ldldjrsDominantLanguageDetectionJobPropertiesList,
    ldldjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDominantLanguageDetectionJobs' smart constructor.
data ListDominantLanguageDetectionJobs = ListDominantLanguageDetectionJobs'
  { _ldldjNextToken ::
      !(Maybe Text),
    _ldldjFilter ::
      !( Maybe
           DominantLanguageDetectionJobFilter
       ),
    _ldldjMaxResults ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDominantLanguageDetectionJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldldjNextToken' - Identifies the next page of results to return.
--
-- * 'ldldjFilter' - Filters that jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- * 'ldldjMaxResults' - The maximum number of results to return in each page. The default is 100.
listDominantLanguageDetectionJobs ::
  ListDominantLanguageDetectionJobs
listDominantLanguageDetectionJobs =
  ListDominantLanguageDetectionJobs'
    { _ldldjNextToken = Nothing,
      _ldldjFilter = Nothing,
      _ldldjMaxResults = Nothing
    }

-- | Identifies the next page of results to return.
ldldjNextToken :: Lens' ListDominantLanguageDetectionJobs (Maybe Text)
ldldjNextToken = lens _ldldjNextToken (\s a -> s {_ldldjNextToken = a})

-- | Filters that jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
ldldjFilter :: Lens' ListDominantLanguageDetectionJobs (Maybe DominantLanguageDetectionJobFilter)
ldldjFilter = lens _ldldjFilter (\s a -> s {_ldldjFilter = a})

-- | The maximum number of results to return in each page. The default is 100.
ldldjMaxResults :: Lens' ListDominantLanguageDetectionJobs (Maybe Natural)
ldldjMaxResults = lens _ldldjMaxResults (\s a -> s {_ldldjMaxResults = a}) . mapping _Nat

instance AWSPager ListDominantLanguageDetectionJobs where
  page rq rs
    | stop (rs ^. ldldjrsNextToken) = Nothing
    | stop (rs ^. ldldjrsDominantLanguageDetectionJobPropertiesList) =
      Nothing
    | otherwise = Just $ rq & ldldjNextToken .~ rs ^. ldldjrsNextToken

instance AWSRequest ListDominantLanguageDetectionJobs where
  type
    Rs ListDominantLanguageDetectionJobs =
      ListDominantLanguageDetectionJobsResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          ListDominantLanguageDetectionJobsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "DominantLanguageDetectionJobPropertiesList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListDominantLanguageDetectionJobs

instance NFData ListDominantLanguageDetectionJobs

instance ToHeaders ListDominantLanguageDetectionJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Comprehend_20171127.ListDominantLanguageDetectionJobs" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListDominantLanguageDetectionJobs where
  toJSON ListDominantLanguageDetectionJobs' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _ldldjNextToken,
            ("Filter" .=) <$> _ldldjFilter,
            ("MaxResults" .=) <$> _ldldjMaxResults
          ]
      )

instance ToPath ListDominantLanguageDetectionJobs where
  toPath = const "/"

instance ToQuery ListDominantLanguageDetectionJobs where
  toQuery = const mempty

-- | /See:/ 'listDominantLanguageDetectionJobsResponse' smart constructor.
data ListDominantLanguageDetectionJobsResponse = ListDominantLanguageDetectionJobsResponse'
  { _ldldjrsNextToken ::
      !( Maybe
           Text
       ),
    _ldldjrsDominantLanguageDetectionJobPropertiesList ::
      !( Maybe
           [DominantLanguageDetectionJobProperties]
       ),
    _ldldjrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListDominantLanguageDetectionJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldldjrsNextToken' - Identifies the next page of results to return.
--
-- * 'ldldjrsDominantLanguageDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- * 'ldldjrsResponseStatus' - -- | The response status code.
listDominantLanguageDetectionJobsResponse ::
  -- | 'ldldjrsResponseStatus'
  Int ->
  ListDominantLanguageDetectionJobsResponse
listDominantLanguageDetectionJobsResponse pResponseStatus_ =
  ListDominantLanguageDetectionJobsResponse'
    { _ldldjrsNextToken =
        Nothing,
      _ldldjrsDominantLanguageDetectionJobPropertiesList =
        Nothing,
      _ldldjrsResponseStatus = pResponseStatus_
    }

-- | Identifies the next page of results to return.
ldldjrsNextToken :: Lens' ListDominantLanguageDetectionJobsResponse (Maybe Text)
ldldjrsNextToken = lens _ldldjrsNextToken (\s a -> s {_ldldjrsNextToken = a})

-- | A list containing the properties of each job that is returned.
ldldjrsDominantLanguageDetectionJobPropertiesList :: Lens' ListDominantLanguageDetectionJobsResponse [DominantLanguageDetectionJobProperties]
ldldjrsDominantLanguageDetectionJobPropertiesList = lens _ldldjrsDominantLanguageDetectionJobPropertiesList (\s a -> s {_ldldjrsDominantLanguageDetectionJobPropertiesList = a}) . _Default . _Coerce

-- | -- | The response status code.
ldldjrsResponseStatus :: Lens' ListDominantLanguageDetectionJobsResponse Int
ldldjrsResponseStatus = lens _ldldjrsResponseStatus (\s a -> s {_ldldjrsResponseStatus = a})

instance NFData ListDominantLanguageDetectionJobsResponse
