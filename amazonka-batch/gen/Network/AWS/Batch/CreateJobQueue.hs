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
-- Module      : Network.AWS.Batch.CreateJobQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Batch job queue. When you create a job queue, you associate one or more compute environments to the queue and assign an order of preference for the compute environments.
--
--
-- You also set a priority to the job queue that determines the order in which the AWS Batch scheduler places jobs onto its associated compute environments. For example, if a compute environment is associated with more than one job queue, the job queue with a higher priority is given preference for scheduling jobs to that compute environment.
--
module Network.AWS.Batch.CreateJobQueue
    (
    -- * Creating a Request
      createJobQueue
    , CreateJobQueue
    -- * Request Lenses
    , cjqState
    , cjqJobQueueName
    , cjqPriority
    , cjqComputeEnvironmentOrder

    -- * Destructuring the Response
    , createJobQueueResponse
    , CreateJobQueueResponse
    -- * Response Lenses
    , cjqrsResponseStatus
    , cjqrsJobQueueName
    , cjqrsJobQueueARN
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createJobQueue' smart constructor.
data CreateJobQueue = CreateJobQueue'
  { _cjqState                   :: !(Maybe JQState)
  , _cjqJobQueueName            :: !Text
  , _cjqPriority                :: !Int
  , _cjqComputeEnvironmentOrder :: ![ComputeEnvironmentOrder]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjqState' - The state of the job queue. If the job queue state is @ENABLED@ , it is able to accept jobs.
--
-- * 'cjqJobQueueName' - The name of the job queue.
--
-- * 'cjqPriority' - The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
--
-- * 'cjqComputeEnvironmentOrder' - The set of compute environments mapped to a job queue and their order relative to each other. The job scheduler uses this parameter to determine which compute environment should execute a given job. Compute environments must be in the @VALID@ state before you can associate them with a job queue. You can associate up to three compute environments with a job queue.
createJobQueue
    :: Text -- ^ 'cjqJobQueueName'
    -> Int -- ^ 'cjqPriority'
    -> CreateJobQueue
createJobQueue pJobQueueName_ pPriority_ =
  CreateJobQueue'
    { _cjqState = Nothing
    , _cjqJobQueueName = pJobQueueName_
    , _cjqPriority = pPriority_
    , _cjqComputeEnvironmentOrder = mempty
    }


-- | The state of the job queue. If the job queue state is @ENABLED@ , it is able to accept jobs.
cjqState :: Lens' CreateJobQueue (Maybe JQState)
cjqState = lens _cjqState (\ s a -> s{_cjqState = a})

-- | The name of the job queue.
cjqJobQueueName :: Lens' CreateJobQueue Text
cjqJobQueueName = lens _cjqJobQueueName (\ s a -> s{_cjqJobQueueName = a})

-- | The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
cjqPriority :: Lens' CreateJobQueue Int
cjqPriority = lens _cjqPriority (\ s a -> s{_cjqPriority = a})

-- | The set of compute environments mapped to a job queue and their order relative to each other. The job scheduler uses this parameter to determine which compute environment should execute a given job. Compute environments must be in the @VALID@ state before you can associate them with a job queue. You can associate up to three compute environments with a job queue.
cjqComputeEnvironmentOrder :: Lens' CreateJobQueue [ComputeEnvironmentOrder]
cjqComputeEnvironmentOrder = lens _cjqComputeEnvironmentOrder (\ s a -> s{_cjqComputeEnvironmentOrder = a}) . _Coerce

instance AWSRequest CreateJobQueue where
        type Rs CreateJobQueue = CreateJobQueueResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 CreateJobQueueResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "jobQueueName") <*>
                     (x .:> "jobQueueArn"))

instance Hashable CreateJobQueue where

instance NFData CreateJobQueue where

instance ToHeaders CreateJobQueue where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateJobQueue where
        toJSON CreateJobQueue'{..}
          = object
              (catMaybes
                 [("state" .=) <$> _cjqState,
                  Just ("jobQueueName" .= _cjqJobQueueName),
                  Just ("priority" .= _cjqPriority),
                  Just
                    ("computeEnvironmentOrder" .=
                       _cjqComputeEnvironmentOrder)])

instance ToPath CreateJobQueue where
        toPath = const "/v1/createjobqueue"

instance ToQuery CreateJobQueue where
        toQuery = const mempty

-- | /See:/ 'createJobQueueResponse' smart constructor.
data CreateJobQueueResponse = CreateJobQueueResponse'
  { _cjqrsResponseStatus :: !Int
  , _cjqrsJobQueueName   :: !Text
  , _cjqrsJobQueueARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjqrsResponseStatus' - -- | The response status code.
--
-- * 'cjqrsJobQueueName' - The name of the job queue.
--
-- * 'cjqrsJobQueueARN' - The Amazon Resource Name (ARN) of the job queue.
createJobQueueResponse
    :: Int -- ^ 'cjqrsResponseStatus'
    -> Text -- ^ 'cjqrsJobQueueName'
    -> Text -- ^ 'cjqrsJobQueueARN'
    -> CreateJobQueueResponse
createJobQueueResponse pResponseStatus_ pJobQueueName_ pJobQueueARN_ =
  CreateJobQueueResponse'
    { _cjqrsResponseStatus = pResponseStatus_
    , _cjqrsJobQueueName = pJobQueueName_
    , _cjqrsJobQueueARN = pJobQueueARN_
    }


-- | -- | The response status code.
cjqrsResponseStatus :: Lens' CreateJobQueueResponse Int
cjqrsResponseStatus = lens _cjqrsResponseStatus (\ s a -> s{_cjqrsResponseStatus = a})

-- | The name of the job queue.
cjqrsJobQueueName :: Lens' CreateJobQueueResponse Text
cjqrsJobQueueName = lens _cjqrsJobQueueName (\ s a -> s{_cjqrsJobQueueName = a})

-- | The Amazon Resource Name (ARN) of the job queue.
cjqrsJobQueueARN :: Lens' CreateJobQueueResponse Text
cjqrsJobQueueARN = lens _cjqrsJobQueueARN (\ s a -> s{_cjqrsJobQueueARN = a})

instance NFData CreateJobQueueResponse where
