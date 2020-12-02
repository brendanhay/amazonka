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
-- Module      : Network.AWS.MediaConvert.ListJobs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your most recently created jobs. This array includes in-process, completed, and errored jobs. This will return the jobs themselves, not just a list of the jobs. To retrieve the twenty next most recent jobs, use the nextToken string returned with the array.
module Network.AWS.MediaConvert.ListJobs
    (
    -- * Creating a Request
      listJobs
    , ListJobs
    -- * Request Lenses
    , ljStatus
    , ljQueue
    , ljNextToken
    , ljOrder
    , ljMaxResults

    -- * Destructuring the Response
    , listJobsResponse
    , ListJobsResponse
    -- * Response Lenses
    , ljrsNextToken
    , ljrsJobs
    , ljrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listJobs' smart constructor.
data ListJobs = ListJobs'
  { _ljStatus     :: !(Maybe JobStatus)
  , _ljQueue      :: !(Maybe Text)
  , _ljNextToken  :: !(Maybe Text)
  , _ljOrder      :: !(Maybe Order)
  , _ljMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljStatus' - Undocumented member.
--
-- * 'ljQueue' - Provide a queue name to get back only jobs from that queue.
--
-- * 'ljNextToken' - Use this string, provided with the response to a previous request, to request the next batch of jobs.
--
-- * 'ljOrder' - Undocumented member.
--
-- * 'ljMaxResults' - Optional. Number of jobs, up to twenty, that will be returned at one time.
listJobs
    :: ListJobs
listJobs =
  ListJobs'
    { _ljStatus = Nothing
    , _ljQueue = Nothing
    , _ljNextToken = Nothing
    , _ljOrder = Nothing
    , _ljMaxResults = Nothing
    }


-- | Undocumented member.
ljStatus :: Lens' ListJobs (Maybe JobStatus)
ljStatus = lens _ljStatus (\ s a -> s{_ljStatus = a})

-- | Provide a queue name to get back only jobs from that queue.
ljQueue :: Lens' ListJobs (Maybe Text)
ljQueue = lens _ljQueue (\ s a -> s{_ljQueue = a})

-- | Use this string, provided with the response to a previous request, to request the next batch of jobs.
ljNextToken :: Lens' ListJobs (Maybe Text)
ljNextToken = lens _ljNextToken (\ s a -> s{_ljNextToken = a})

-- | Undocumented member.
ljOrder :: Lens' ListJobs (Maybe Order)
ljOrder = lens _ljOrder (\ s a -> s{_ljOrder = a})

-- | Optional. Number of jobs, up to twenty, that will be returned at one time.
ljMaxResults :: Lens' ListJobs (Maybe Int)
ljMaxResults = lens _ljMaxResults (\ s a -> s{_ljMaxResults = a})

instance AWSRequest ListJobs where
        type Rs ListJobs = ListJobsResponse
        request = get mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 ListJobsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "jobs" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListJobs where

instance NFData ListJobs where

instance ToHeaders ListJobs where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListJobs where
        toPath = const "/2017-08-29/jobs"

instance ToQuery ListJobs where
        toQuery ListJobs'{..}
          = mconcat
              ["status" =: _ljStatus, "queue" =: _ljQueue,
               "nextToken" =: _ljNextToken, "order" =: _ljOrder,
               "maxResults" =: _ljMaxResults]

-- | /See:/ 'listJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { _ljrsNextToken      :: !(Maybe Text)
  , _ljrsJobs           :: !(Maybe [Job])
  , _ljrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljrsNextToken' - Use this string to request the next batch of jobs.
--
-- * 'ljrsJobs' - List of jobs
--
-- * 'ljrsResponseStatus' - -- | The response status code.
listJobsResponse
    :: Int -- ^ 'ljrsResponseStatus'
    -> ListJobsResponse
listJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { _ljrsNextToken = Nothing
    , _ljrsJobs = Nothing
    , _ljrsResponseStatus = pResponseStatus_
    }


-- | Use this string to request the next batch of jobs.
ljrsNextToken :: Lens' ListJobsResponse (Maybe Text)
ljrsNextToken = lens _ljrsNextToken (\ s a -> s{_ljrsNextToken = a})

-- | List of jobs
ljrsJobs :: Lens' ListJobsResponse [Job]
ljrsJobs = lens _ljrsJobs (\ s a -> s{_ljrsJobs = a}) . _Default . _Coerce

-- | -- | The response status code.
ljrsResponseStatus :: Lens' ListJobsResponse Int
ljrsResponseStatus = lens _ljrsResponseStatus (\ s a -> s{_ljrsResponseStatus = a})

instance NFData ListJobsResponse where
