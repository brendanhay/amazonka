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
-- Module      : Network.AWS.ECS.StopTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running task.
--
--
-- When 'StopTask' is called on a task, the equivalent of @docker stop@ is issued to the containers running in the task. This results in a @SIGTERM@ and a default 30-second timeout, after which @SIGKILL@ is sent and the containers are forcibly stopped. If the container handles the @SIGTERM@ gracefully and exits within 30 seconds from receiving it, no @SIGKILL@ is sent.
--
module Network.AWS.ECS.StopTask
    (
    -- * Creating a Request
      stopTask
    , StopTask
    -- * Request Lenses
    , stCluster
    , stReason
    , stTask

    -- * Destructuring the Response
    , stopTaskResponse
    , StopTaskResponse
    -- * Response Lenses
    , srsTask
    , srsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopTask' smart constructor.
data StopTask = StopTask'
  { _stCluster :: !(Maybe Text)
  , _stReason  :: !(Maybe Text)
  , _stTask    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task to stop. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'stReason' - An optional message specified when a task is stopped. For example, if you are using a custom scheduler, you can use this parameter to specify the reason for stopping the task here, and the message appears in subsequent 'DescribeTasks' API operations on this task. Up to 255 characters are allowed in this message.
--
-- * 'stTask' - The task ID or full ARN entry of the task to stop.
stopTask
    :: Text -- ^ 'stTask'
    -> StopTask
stopTask pTask_ =
  StopTask' {_stCluster = Nothing, _stReason = Nothing, _stTask = pTask_}


-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task to stop. If you do not specify a cluster, the default cluster is assumed.
stCluster :: Lens' StopTask (Maybe Text)
stCluster = lens _stCluster (\ s a -> s{_stCluster = a})

-- | An optional message specified when a task is stopped. For example, if you are using a custom scheduler, you can use this parameter to specify the reason for stopping the task here, and the message appears in subsequent 'DescribeTasks' API operations on this task. Up to 255 characters are allowed in this message.
stReason :: Lens' StopTask (Maybe Text)
stReason = lens _stReason (\ s a -> s{_stReason = a})

-- | The task ID or full ARN entry of the task to stop.
stTask :: Lens' StopTask Text
stTask = lens _stTask (\ s a -> s{_stTask = a})

instance AWSRequest StopTask where
        type Rs StopTask = StopTaskResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 StopTaskResponse' <$>
                   (x .?> "task") <*> (pure (fromEnum s)))

instance Hashable StopTask where

instance NFData StopTask where

instance ToHeaders StopTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.StopTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopTask where
        toJSON StopTask'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _stCluster,
                  ("reason" .=) <$> _stReason,
                  Just ("task" .= _stTask)])

instance ToPath StopTask where
        toPath = const "/"

instance ToQuery StopTask where
        toQuery = const mempty

-- | /See:/ 'stopTaskResponse' smart constructor.
data StopTaskResponse = StopTaskResponse'
  { _srsTask           :: !(Maybe Task)
  , _srsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsTask' - The task that was stopped.
--
-- * 'srsResponseStatus' - -- | The response status code.
stopTaskResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopTaskResponse
stopTaskResponse pResponseStatus_ =
  StopTaskResponse' {_srsTask = Nothing, _srsResponseStatus = pResponseStatus_}


-- | The task that was stopped.
srsTask :: Lens' StopTaskResponse (Maybe Task)
srsTask = lens _srsTask (\ s a -> s{_srsTask = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StopTaskResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StopTaskResponse where
