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
-- Module      : Network.AWS.IoT.ListJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists jobs.
--
--
module Network.AWS.IoT.ListJobs
    (
    -- * Creating a Request
      listJobs
    , ListJobs
    -- * Request Lenses
    , ljStatus
    , ljThingGroupId
    , ljNextToken
    , ljThingGroupName
    , ljMaxResults
    , ljTargetSelection

    -- * Destructuring the Response
    , listJobsResponse
    , ListJobsResponse
    -- * Response Lenses
    , ljrsJobs
    , ljrsNextToken
    , ljrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listJobs' smart constructor.
data ListJobs = ListJobs'
  { _ljStatus          :: !(Maybe JobStatus)
  , _ljThingGroupId    :: !(Maybe Text)
  , _ljNextToken       :: !(Maybe Text)
  , _ljThingGroupName  :: !(Maybe Text)
  , _ljMaxResults      :: !(Maybe Nat)
  , _ljTargetSelection :: !(Maybe TargetSelection)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljStatus' - An optional filter that lets you search for jobs that have the specified status.
--
-- * 'ljThingGroupId' - A filter that limits the returned jobs to those for the specified group.
--
-- * 'ljNextToken' - The token to retrieve the next set of results.
--
-- * 'ljThingGroupName' - A filter that limits the returned jobs to those for the specified group.
--
-- * 'ljMaxResults' - The maximum number of results to return per request.
--
-- * 'ljTargetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
listJobs
    :: ListJobs
listJobs =
  ListJobs'
    { _ljStatus = Nothing
    , _ljThingGroupId = Nothing
    , _ljNextToken = Nothing
    , _ljThingGroupName = Nothing
    , _ljMaxResults = Nothing
    , _ljTargetSelection = Nothing
    }


-- | An optional filter that lets you search for jobs that have the specified status.
ljStatus :: Lens' ListJobs (Maybe JobStatus)
ljStatus = lens _ljStatus (\ s a -> s{_ljStatus = a})

-- | A filter that limits the returned jobs to those for the specified group.
ljThingGroupId :: Lens' ListJobs (Maybe Text)
ljThingGroupId = lens _ljThingGroupId (\ s a -> s{_ljThingGroupId = a})

-- | The token to retrieve the next set of results.
ljNextToken :: Lens' ListJobs (Maybe Text)
ljNextToken = lens _ljNextToken (\ s a -> s{_ljNextToken = a})

-- | A filter that limits the returned jobs to those for the specified group.
ljThingGroupName :: Lens' ListJobs (Maybe Text)
ljThingGroupName = lens _ljThingGroupName (\ s a -> s{_ljThingGroupName = a})

-- | The maximum number of results to return per request.
ljMaxResults :: Lens' ListJobs (Maybe Natural)
ljMaxResults = lens _ljMaxResults (\ s a -> s{_ljMaxResults = a}) . mapping _Nat

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
ljTargetSelection :: Lens' ListJobs (Maybe TargetSelection)
ljTargetSelection = lens _ljTargetSelection (\ s a -> s{_ljTargetSelection = a})

instance AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListJobsResponse' <$>
                   (x .?> "jobs" .!@ mempty) <*> (x .?> "nextToken") <*>
                     (pure (fromEnum s)))

instance Hashable ListJobs where

instance NFData ListJobs where

instance ToHeaders ListJobs where
        toHeaders = const mempty

instance ToPath ListJobs where
        toPath = const "/jobs"

instance ToQuery ListJobs where
        toQuery ListJobs'{..}
          = mconcat
              ["status" =: _ljStatus,
               "thingGroupId" =: _ljThingGroupId,
               "nextToken" =: _ljNextToken,
               "thingGroupName" =: _ljThingGroupName,
               "maxResults" =: _ljMaxResults,
               "targetSelection" =: _ljTargetSelection]

-- | /See:/ 'listJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { _ljrsJobs           :: !(Maybe [JobSummary])
  , _ljrsNextToken      :: !(Maybe Text)
  , _ljrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljrsJobs' - A list of jobs.
--
-- * 'ljrsNextToken' - The token for the next set of results, or __null__ if there are no additional results.
--
-- * 'ljrsResponseStatus' - -- | The response status code.
listJobsResponse
    :: Int -- ^ 'ljrsResponseStatus'
    -> ListJobsResponse
listJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { _ljrsJobs = Nothing
    , _ljrsNextToken = Nothing
    , _ljrsResponseStatus = pResponseStatus_
    }


-- | A list of jobs.
ljrsJobs :: Lens' ListJobsResponse [JobSummary]
ljrsJobs = lens _ljrsJobs (\ s a -> s{_ljrsJobs = a}) . _Default . _Coerce

-- | The token for the next set of results, or __null__ if there are no additional results.
ljrsNextToken :: Lens' ListJobsResponse (Maybe Text)
ljrsNextToken = lens _ljrsNextToken (\ s a -> s{_ljrsNextToken = a})

-- | -- | The response status code.
ljrsResponseStatus :: Lens' ListJobsResponse Int
ljrsResponseStatus = lens _ljrsResponseStatus (\ s a -> s{_ljrsResponseStatus = a})

instance NFData ListJobsResponse where
