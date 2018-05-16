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
-- Module      : Network.AWS.Comprehend.ListTopicsDetectionJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the topic detection jobs that you have submitted.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListTopicsDetectionJobs
    (
    -- * Creating a Request
      listTopicsDetectionJobs
    , ListTopicsDetectionJobs
    -- * Request Lenses
    , ltdjNextToken
    , ltdjFilter
    , ltdjMaxResults

    -- * Destructuring the Response
    , listTopicsDetectionJobsResponse
    , ListTopicsDetectionJobsResponse
    -- * Response Lenses
    , ltdjrsNextToken
    , ltdjrsTopicsDetectionJobPropertiesList
    , ltdjrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTopicsDetectionJobs' smart constructor.
data ListTopicsDetectionJobs = ListTopicsDetectionJobs'
  { _ltdjNextToken  :: !(Maybe Text)
  , _ltdjFilter     :: !(Maybe TopicsDetectionJobFilter)
  , _ltdjMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTopicsDetectionJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltdjNextToken' - Identifies the next page of results to return.
--
-- * 'ltdjFilter' - Filters the jobs that are returned. Jobs can be filtered on their name, status, or the date and time that they were submitted. You can set only one filter at a time.
--
-- * 'ltdjMaxResults' - The maximum number of results to return in each page.
listTopicsDetectionJobs
    :: ListTopicsDetectionJobs
listTopicsDetectionJobs =
  ListTopicsDetectionJobs'
    {_ltdjNextToken = Nothing, _ltdjFilter = Nothing, _ltdjMaxResults = Nothing}


-- | Identifies the next page of results to return.
ltdjNextToken :: Lens' ListTopicsDetectionJobs (Maybe Text)
ltdjNextToken = lens _ltdjNextToken (\ s a -> s{_ltdjNextToken = a})

-- | Filters the jobs that are returned. Jobs can be filtered on their name, status, or the date and time that they were submitted. You can set only one filter at a time.
ltdjFilter :: Lens' ListTopicsDetectionJobs (Maybe TopicsDetectionJobFilter)
ltdjFilter = lens _ltdjFilter (\ s a -> s{_ltdjFilter = a})

-- | The maximum number of results to return in each page.
ltdjMaxResults :: Lens' ListTopicsDetectionJobs (Maybe Natural)
ltdjMaxResults = lens _ltdjMaxResults (\ s a -> s{_ltdjMaxResults = a}) . mapping _Nat

instance AWSPager ListTopicsDetectionJobs where
        page rq rs
          | stop (rs ^. ltdjrsNextToken) = Nothing
          | stop (rs ^. ltdjrsTopicsDetectionJobPropertiesList)
            = Nothing
          | otherwise =
            Just $ rq & ltdjNextToken .~ rs ^. ltdjrsNextToken

instance AWSRequest ListTopicsDetectionJobs where
        type Rs ListTopicsDetectionJobs =
             ListTopicsDetectionJobsResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 ListTopicsDetectionJobsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "TopicsDetectionJobPropertiesList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTopicsDetectionJobs where

instance NFData ListTopicsDetectionJobs where

instance ToHeaders ListTopicsDetectionJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.ListTopicsDetectionJobs" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTopicsDetectionJobs where
        toJSON ListTopicsDetectionJobs'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltdjNextToken,
                  ("Filter" .=) <$> _ltdjFilter,
                  ("MaxResults" .=) <$> _ltdjMaxResults])

instance ToPath ListTopicsDetectionJobs where
        toPath = const "/"

instance ToQuery ListTopicsDetectionJobs where
        toQuery = const mempty

-- | /See:/ 'listTopicsDetectionJobsResponse' smart constructor.
data ListTopicsDetectionJobsResponse = ListTopicsDetectionJobsResponse'
  { _ltdjrsNextToken :: !(Maybe Text)
  , _ltdjrsTopicsDetectionJobPropertiesList :: !(Maybe [TopicsDetectionJobProperties])
  , _ltdjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTopicsDetectionJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltdjrsNextToken' - Identifies the next page of results to return.
--
-- * 'ltdjrsTopicsDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- * 'ltdjrsResponseStatus' - -- | The response status code.
listTopicsDetectionJobsResponse
    :: Int -- ^ 'ltdjrsResponseStatus'
    -> ListTopicsDetectionJobsResponse
listTopicsDetectionJobsResponse pResponseStatus_ =
  ListTopicsDetectionJobsResponse'
    { _ltdjrsNextToken = Nothing
    , _ltdjrsTopicsDetectionJobPropertiesList = Nothing
    , _ltdjrsResponseStatus = pResponseStatus_
    }


-- | Identifies the next page of results to return.
ltdjrsNextToken :: Lens' ListTopicsDetectionJobsResponse (Maybe Text)
ltdjrsNextToken = lens _ltdjrsNextToken (\ s a -> s{_ltdjrsNextToken = a})

-- | A list containing the properties of each job that is returned.
ltdjrsTopicsDetectionJobPropertiesList :: Lens' ListTopicsDetectionJobsResponse [TopicsDetectionJobProperties]
ltdjrsTopicsDetectionJobPropertiesList = lens _ltdjrsTopicsDetectionJobPropertiesList (\ s a -> s{_ltdjrsTopicsDetectionJobPropertiesList = a}) . _Default . _Coerce

-- | -- | The response status code.
ltdjrsResponseStatus :: Lens' ListTopicsDetectionJobsResponse Int
ltdjrsResponseStatus = lens _ltdjrsResponseStatus (\ s a -> s{_ltdjrsResponseStatus = a})

instance NFData ListTopicsDetectionJobsResponse where
