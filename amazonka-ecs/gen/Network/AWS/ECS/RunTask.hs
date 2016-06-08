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
-- Module      : Network.AWS.ECS.RunTask
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start a task using random placement and the default Amazon ECS scheduler. To use your own scheduler or place a task on a specific container instance, use 'StartTask' instead.
--
-- The 'count' parameter is limited to 10 tasks per call.
module Network.AWS.ECS.RunTask
    (
    -- * Creating a Request
      runTask
    , RunTask
    -- * Request Lenses
    , rtOverrides
    , rtCluster
    , rtCount
    , rtStartedBy
    , rtTaskDefinition

    -- * Destructuring the Response
    , runTaskResponse
    , RunTaskResponse
    -- * Response Lenses
    , rtrsFailures
    , rtrsTasks
    , rtrsResponseStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'runTask' smart constructor.
data RunTask = RunTask'
    { _rtOverrides      :: !(Maybe TaskOverride)
    , _rtCluster        :: !(Maybe Text)
    , _rtCount          :: !(Maybe Int)
    , _rtStartedBy      :: !(Maybe Text)
    , _rtTaskDefinition :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RunTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtOverrides'
--
-- * 'rtCluster'
--
-- * 'rtCount'
--
-- * 'rtStartedBy'
--
-- * 'rtTaskDefinition'
runTask
    :: Text -- ^ 'rtTaskDefinition'
    -> RunTask
runTask pTaskDefinition_ =
    RunTask'
    { _rtOverrides = Nothing
    , _rtCluster = Nothing
    , _rtCount = Nothing
    , _rtStartedBy = Nothing
    , _rtTaskDefinition = pTaskDefinition_
    }

-- | A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a 'command' override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an 'environment' override.
--
-- A total of 8192 characters are allowed for overrides. This limit includes the JSON formatting characters of the override structure.
rtOverrides :: Lens' RunTask (Maybe TaskOverride)
rtOverrides = lens _rtOverrides (\ s a -> s{_rtOverrides = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster on which to run your task. If you do not specify a cluster, the default cluster is assumed..
rtCluster :: Lens' RunTask (Maybe Text)
rtCluster = lens _rtCluster (\ s a -> s{_rtCluster = a});

-- | The number of instantiations of the specified task to place on your cluster.
--
-- The 'count' parameter is limited to 10 tasks per call.
rtCount :: Lens' RunTask (Maybe Int)
rtCount = lens _rtCount (\ s a -> s{_rtCount = a});

-- | An optional tag specified when a task is started. For example if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the 'startedBy' parameter. You can then identify which tasks belong to that job by filtering the results of a < ListTasks> call with the 'startedBy' value.
--
-- If a task is started by an Amazon ECS service, then the 'startedBy' parameter contains the deployment ID of the service that starts it.
rtStartedBy :: Lens' RunTask (Maybe Text)
rtStartedBy = lens _rtStartedBy (\ s a -> s{_rtStartedBy = a});

-- | The 'family' and 'revision' ('family:revision') or full Amazon Resource Name (ARN) of the task definition to run. If a 'revision' is not specified, the latest 'ACTIVE' revision is used.
rtTaskDefinition :: Lens' RunTask Text
rtTaskDefinition = lens _rtTaskDefinition (\ s a -> s{_rtTaskDefinition = a});

instance AWSRequest RunTask where
        type Rs RunTask = RunTaskResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 RunTaskResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "tasks" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable RunTask

instance NFData RunTask

instance ToHeaders RunTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.RunTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RunTask where
        toJSON RunTask'{..}
          = object
              (catMaybes
                 [("overrides" .=) <$> _rtOverrides,
                  ("cluster" .=) <$> _rtCluster,
                  ("count" .=) <$> _rtCount,
                  ("startedBy" .=) <$> _rtStartedBy,
                  Just ("taskDefinition" .= _rtTaskDefinition)])

instance ToPath RunTask where
        toPath = const "/"

instance ToQuery RunTask where
        toQuery = const mempty

-- | /See:/ 'runTaskResponse' smart constructor.
data RunTaskResponse = RunTaskResponse'
    { _rtrsFailures       :: !(Maybe [Failure])
    , _rtrsTasks          :: !(Maybe [Task])
    , _rtrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RunTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrsFailures'
--
-- * 'rtrsTasks'
--
-- * 'rtrsResponseStatus'
runTaskResponse
    :: Int -- ^ 'rtrsResponseStatus'
    -> RunTaskResponse
runTaskResponse pResponseStatus_ =
    RunTaskResponse'
    { _rtrsFailures = Nothing
    , _rtrsTasks = Nothing
    , _rtrsResponseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
rtrsFailures :: Lens' RunTaskResponse [Failure]
rtrsFailures = lens _rtrsFailures (\ s a -> s{_rtrsFailures = a}) . _Default . _Coerce;

-- | A full description of the tasks that were run. Each task that was successfully placed on your cluster are described here.
rtrsTasks :: Lens' RunTaskResponse [Task]
rtrsTasks = lens _rtrsTasks (\ s a -> s{_rtrsTasks = a}) . _Default . _Coerce;

-- | The response status code.
rtrsResponseStatus :: Lens' RunTaskResponse Int
rtrsResponseStatus = lens _rtrsResponseStatus (\ s a -> s{_rtrsResponseStatus = a});

instance NFData RunTaskResponse
