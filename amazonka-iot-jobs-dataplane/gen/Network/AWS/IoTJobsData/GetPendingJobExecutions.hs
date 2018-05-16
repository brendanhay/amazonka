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
-- Module      : Network.AWS.IoTJobsData.GetPendingJobExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the list of all jobs for a thing that are not in a terminal status.
--
--
module Network.AWS.IoTJobsData.GetPendingJobExecutions
    (
    -- * Creating a Request
      getPendingJobExecutions
    , GetPendingJobExecutions
    -- * Request Lenses
    , gpjeThingName

    -- * Destructuring the Response
    , getPendingJobExecutionsResponse
    , GetPendingJobExecutionsResponse
    -- * Response Lenses
    , gpjersInProgressJobs
    , gpjersQueuedJobs
    , gpjersResponseStatus
    ) where

import Network.AWS.IoTJobsData.Types
import Network.AWS.IoTJobsData.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPendingJobExecutions' smart constructor.
newtype GetPendingJobExecutions = GetPendingJobExecutions'
  { _gpjeThingName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPendingJobExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpjeThingName' - The name of the thing that is executing the job.
getPendingJobExecutions
    :: Text -- ^ 'gpjeThingName'
    -> GetPendingJobExecutions
getPendingJobExecutions pThingName_ =
  GetPendingJobExecutions' {_gpjeThingName = pThingName_}


-- | The name of the thing that is executing the job.
gpjeThingName :: Lens' GetPendingJobExecutions Text
gpjeThingName = lens _gpjeThingName (\ s a -> s{_gpjeThingName = a})

instance AWSRequest GetPendingJobExecutions where
        type Rs GetPendingJobExecutions =
             GetPendingJobExecutionsResponse
        request = get ioTJobsData
        response
          = receiveJSON
              (\ s h x ->
                 GetPendingJobExecutionsResponse' <$>
                   (x .?> "inProgressJobs" .!@ mempty) <*>
                     (x .?> "queuedJobs" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetPendingJobExecutions where

instance NFData GetPendingJobExecutions where

instance ToHeaders GetPendingJobExecutions where
        toHeaders = const mempty

instance ToPath GetPendingJobExecutions where
        toPath GetPendingJobExecutions'{..}
          = mconcat ["/things/", toBS _gpjeThingName, "/jobs"]

instance ToQuery GetPendingJobExecutions where
        toQuery = const mempty

-- | /See:/ 'getPendingJobExecutionsResponse' smart constructor.
data GetPendingJobExecutionsResponse = GetPendingJobExecutionsResponse'
  { _gpjersInProgressJobs :: !(Maybe [JobExecutionSummary])
  , _gpjersQueuedJobs     :: !(Maybe [JobExecutionSummary])
  , _gpjersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPendingJobExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpjersInProgressJobs' - A list of JobExecutionSummary objects with status IN_PROGRESS.
--
-- * 'gpjersQueuedJobs' - A list of JobExecutionSummary objects with status QUEUED.
--
-- * 'gpjersResponseStatus' - -- | The response status code.
getPendingJobExecutionsResponse
    :: Int -- ^ 'gpjersResponseStatus'
    -> GetPendingJobExecutionsResponse
getPendingJobExecutionsResponse pResponseStatus_ =
  GetPendingJobExecutionsResponse'
    { _gpjersInProgressJobs = Nothing
    , _gpjersQueuedJobs = Nothing
    , _gpjersResponseStatus = pResponseStatus_
    }


-- | A list of JobExecutionSummary objects with status IN_PROGRESS.
gpjersInProgressJobs :: Lens' GetPendingJobExecutionsResponse [JobExecutionSummary]
gpjersInProgressJobs = lens _gpjersInProgressJobs (\ s a -> s{_gpjersInProgressJobs = a}) . _Default . _Coerce

-- | A list of JobExecutionSummary objects with status QUEUED.
gpjersQueuedJobs :: Lens' GetPendingJobExecutionsResponse [JobExecutionSummary]
gpjersQueuedJobs = lens _gpjersQueuedJobs (\ s a -> s{_gpjersQueuedJobs = a}) . _Default . _Coerce

-- | -- | The response status code.
gpjersResponseStatus :: Lens' GetPendingJobExecutionsResponse Int
gpjersResponseStatus = lens _gpjersResponseStatus (\ s a -> s{_gpjersResponseStatus = a})

instance NFData GetPendingJobExecutionsResponse where
