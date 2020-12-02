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
-- Module      : Network.AWS.Batch.UpdateJobQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a job queue.
--
--
module Network.AWS.Batch.UpdateJobQueue
    (
    -- * Creating a Request
      updateJobQueue
    , UpdateJobQueue
    -- * Request Lenses
    , ujqState
    , ujqPriority
    , ujqComputeEnvironmentOrder
    , ujqJobQueue

    -- * Destructuring the Response
    , updateJobQueueResponse
    , UpdateJobQueueResponse
    -- * Response Lenses
    , ujqrsJobQueueARN
    , ujqrsJobQueueName
    , ujqrsResponseStatus
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateJobQueue' smart constructor.
data UpdateJobQueue = UpdateJobQueue'
  { _ujqState                   :: !(Maybe JQState)
  , _ujqPriority                :: !(Maybe Int)
  , _ujqComputeEnvironmentOrder :: !(Maybe [ComputeEnvironmentOrder])
  , _ujqJobQueue                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJobQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujqState' - Describes the queue's ability to accept new jobs.
--
-- * 'ujqPriority' - The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
--
-- * 'ujqComputeEnvironmentOrder' - Details the set of compute environments mapped to a job queue and their order relative to each other. This is one of the parameters used by the job scheduler to determine which compute environment should execute a given job.
--
-- * 'ujqJobQueue' - The name or the Amazon Resource Name (ARN) of the job queue.
updateJobQueue
    :: Text -- ^ 'ujqJobQueue'
    -> UpdateJobQueue
updateJobQueue pJobQueue_ =
  UpdateJobQueue'
    { _ujqState = Nothing
    , _ujqPriority = Nothing
    , _ujqComputeEnvironmentOrder = Nothing
    , _ujqJobQueue = pJobQueue_
    }


-- | Describes the queue's ability to accept new jobs.
ujqState :: Lens' UpdateJobQueue (Maybe JQState)
ujqState = lens _ujqState (\ s a -> s{_ujqState = a})

-- | The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
ujqPriority :: Lens' UpdateJobQueue (Maybe Int)
ujqPriority = lens _ujqPriority (\ s a -> s{_ujqPriority = a})

-- | Details the set of compute environments mapped to a job queue and their order relative to each other. This is one of the parameters used by the job scheduler to determine which compute environment should execute a given job.
ujqComputeEnvironmentOrder :: Lens' UpdateJobQueue [ComputeEnvironmentOrder]
ujqComputeEnvironmentOrder = lens _ujqComputeEnvironmentOrder (\ s a -> s{_ujqComputeEnvironmentOrder = a}) . _Default . _Coerce

-- | The name or the Amazon Resource Name (ARN) of the job queue.
ujqJobQueue :: Lens' UpdateJobQueue Text
ujqJobQueue = lens _ujqJobQueue (\ s a -> s{_ujqJobQueue = a})

instance AWSRequest UpdateJobQueue where
        type Rs UpdateJobQueue = UpdateJobQueueResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 UpdateJobQueueResponse' <$>
                   (x .?> "jobQueueArn") <*> (x .?> "jobQueueName") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateJobQueue where

instance NFData UpdateJobQueue where

instance ToHeaders UpdateJobQueue where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateJobQueue where
        toJSON UpdateJobQueue'{..}
          = object
              (catMaybes
                 [("state" .=) <$> _ujqState,
                  ("priority" .=) <$> _ujqPriority,
                  ("computeEnvironmentOrder" .=) <$>
                    _ujqComputeEnvironmentOrder,
                  Just ("jobQueue" .= _ujqJobQueue)])

instance ToPath UpdateJobQueue where
        toPath = const "/v1/updatejobqueue"

instance ToQuery UpdateJobQueue where
        toQuery = const mempty

-- | /See:/ 'updateJobQueueResponse' smart constructor.
data UpdateJobQueueResponse = UpdateJobQueueResponse'
  { _ujqrsJobQueueARN    :: !(Maybe Text)
  , _ujqrsJobQueueName   :: !(Maybe Text)
  , _ujqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJobQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujqrsJobQueueARN' - The Amazon Resource Name (ARN) of the job queue.
--
-- * 'ujqrsJobQueueName' - The name of the job queue.
--
-- * 'ujqrsResponseStatus' - -- | The response status code.
updateJobQueueResponse
    :: Int -- ^ 'ujqrsResponseStatus'
    -> UpdateJobQueueResponse
updateJobQueueResponse pResponseStatus_ =
  UpdateJobQueueResponse'
    { _ujqrsJobQueueARN = Nothing
    , _ujqrsJobQueueName = Nothing
    , _ujqrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the job queue.
ujqrsJobQueueARN :: Lens' UpdateJobQueueResponse (Maybe Text)
ujqrsJobQueueARN = lens _ujqrsJobQueueARN (\ s a -> s{_ujqrsJobQueueARN = a})

-- | The name of the job queue.
ujqrsJobQueueName :: Lens' UpdateJobQueueResponse (Maybe Text)
ujqrsJobQueueName = lens _ujqrsJobQueueName (\ s a -> s{_ujqrsJobQueueName = a})

-- | -- | The response status code.
ujqrsResponseStatus :: Lens' UpdateJobQueueResponse Int
ujqrsResponseStatus = lens _ujqrsResponseStatus (\ s a -> s{_ujqrsResponseStatus = a})

instance NFData UpdateJobQueueResponse where
