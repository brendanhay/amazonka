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
-- Module      : Network.AWS.Comprehend.ListEntitiesDetectionJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the entity detection jobs that you have submitted.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListEntitiesDetectionJobs
    (
    -- * Creating a Request
      listEntitiesDetectionJobs
    , ListEntitiesDetectionJobs
    -- * Request Lenses
    , ledjNextToken
    , ledjFilter
    , ledjMaxResults

    -- * Destructuring the Response
    , listEntitiesDetectionJobsResponse
    , ListEntitiesDetectionJobsResponse
    -- * Response Lenses
    , ledjrsEntitiesDetectionJobPropertiesList
    , ledjrsNextToken
    , ledjrsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listEntitiesDetectionJobs' smart constructor.
data ListEntitiesDetectionJobs = ListEntitiesDetectionJobs'
  { _ledjNextToken  :: !(Maybe Text)
  , _ledjFilter     :: !(Maybe EntitiesDetectionJobFilter)
  , _ledjMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEntitiesDetectionJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ledjNextToken' - Identifies the next page of results to return.
--
-- * 'ledjFilter' - Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- * 'ledjMaxResults' - The maximum number of results to return in each page. The default is 100.
listEntitiesDetectionJobs
    :: ListEntitiesDetectionJobs
listEntitiesDetectionJobs =
  ListEntitiesDetectionJobs'
    {_ledjNextToken = Nothing, _ledjFilter = Nothing, _ledjMaxResults = Nothing}


-- | Identifies the next page of results to return.
ledjNextToken :: Lens' ListEntitiesDetectionJobs (Maybe Text)
ledjNextToken = lens _ledjNextToken (\ s a -> s{_ledjNextToken = a})

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
ledjFilter :: Lens' ListEntitiesDetectionJobs (Maybe EntitiesDetectionJobFilter)
ledjFilter = lens _ledjFilter (\ s a -> s{_ledjFilter = a})

-- | The maximum number of results to return in each page. The default is 100.
ledjMaxResults :: Lens' ListEntitiesDetectionJobs (Maybe Natural)
ledjMaxResults = lens _ledjMaxResults (\ s a -> s{_ledjMaxResults = a}) . mapping _Nat

instance AWSPager ListEntitiesDetectionJobs where
        page rq rs
          | stop (rs ^. ledjrsNextToken) = Nothing
          | stop
              (rs ^. ledjrsEntitiesDetectionJobPropertiesList)
            = Nothing
          | otherwise =
            Just $ rq & ledjNextToken .~ rs ^. ledjrsNextToken

instance AWSRequest ListEntitiesDetectionJobs where
        type Rs ListEntitiesDetectionJobs =
             ListEntitiesDetectionJobsResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 ListEntitiesDetectionJobsResponse' <$>
                   (x .?> "EntitiesDetectionJobPropertiesList" .!@
                      mempty)
                     <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListEntitiesDetectionJobs where

instance NFData ListEntitiesDetectionJobs where

instance ToHeaders ListEntitiesDetectionJobs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.ListEntitiesDetectionJobs" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListEntitiesDetectionJobs where
        toJSON ListEntitiesDetectionJobs'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ledjNextToken,
                  ("Filter" .=) <$> _ledjFilter,
                  ("MaxResults" .=) <$> _ledjMaxResults])

instance ToPath ListEntitiesDetectionJobs where
        toPath = const "/"

instance ToQuery ListEntitiesDetectionJobs where
        toQuery = const mempty

-- | /See:/ 'listEntitiesDetectionJobsResponse' smart constructor.
data ListEntitiesDetectionJobsResponse = ListEntitiesDetectionJobsResponse'
  { _ledjrsEntitiesDetectionJobPropertiesList :: !(Maybe [EntitiesDetectionJobProperties])
  , _ledjrsNextToken :: !(Maybe Text)
  , _ledjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEntitiesDetectionJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ledjrsEntitiesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- * 'ledjrsNextToken' - Identifies the next page of results to return.
--
-- * 'ledjrsResponseStatus' - -- | The response status code.
listEntitiesDetectionJobsResponse
    :: Int -- ^ 'ledjrsResponseStatus'
    -> ListEntitiesDetectionJobsResponse
listEntitiesDetectionJobsResponse pResponseStatus_ =
  ListEntitiesDetectionJobsResponse'
    { _ledjrsEntitiesDetectionJobPropertiesList = Nothing
    , _ledjrsNextToken = Nothing
    , _ledjrsResponseStatus = pResponseStatus_
    }


-- | A list containing the properties of each job that is returned.
ledjrsEntitiesDetectionJobPropertiesList :: Lens' ListEntitiesDetectionJobsResponse [EntitiesDetectionJobProperties]
ledjrsEntitiesDetectionJobPropertiesList = lens _ledjrsEntitiesDetectionJobPropertiesList (\ s a -> s{_ledjrsEntitiesDetectionJobPropertiesList = a}) . _Default . _Coerce

-- | Identifies the next page of results to return.
ledjrsNextToken :: Lens' ListEntitiesDetectionJobsResponse (Maybe Text)
ledjrsNextToken = lens _ledjrsNextToken (\ s a -> s{_ledjrsNextToken = a})

-- | -- | The response status code.
ledjrsResponseStatus :: Lens' ListEntitiesDetectionJobsResponse Int
ledjrsResponseStatus = lens _ledjrsResponseStatus (\ s a -> s{_ledjrsResponseStatus = a})

instance NFData ListEntitiesDetectionJobsResponse
         where
