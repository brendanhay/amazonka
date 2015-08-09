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
-- Module      : Network.AWS.ECS.StartTask
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new task from the specified task definition on the specified
-- container instance or instances. If you want to use the default Amazon
-- ECS scheduler to place your task, use 'RunTask' instead.
--
-- The list of container instances to start tasks on is limited to 10.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_StartTask.html AWS API Reference> for StartTask.
module Network.AWS.ECS.StartTask
    (
    -- * Creating a Request
      startTask
    , StartTask
    -- * Request Lenses
    , sOverrides
    , sCluster
    , sStartedBy
    , sTaskDefinition
    , sContainerInstances

    -- * Destructuring the Response
    , startTaskResponse
    , StartTaskResponse
    -- * Response Lenses
    , strsFailures
    , strsTasks
    , strsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startTask' smart constructor.
data StartTask = StartTask'
    { _sOverrides          :: !(Maybe TaskOverride)
    , _sCluster            :: !(Maybe Text)
    , _sStartedBy          :: !(Maybe Text)
    , _sTaskDefinition     :: !Text
    , _sContainerInstances :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sOverrides'
--
-- * 'sCluster'
--
-- * 'sStartedBy'
--
-- * 'sTaskDefinition'
--
-- * 'sContainerInstances'
startTask
    :: Text -- ^ 'sTaskDefinition'
    -> StartTask
startTask pTaskDefinition_ =
    StartTask'
    { _sOverrides = Nothing
    , _sCluster = Nothing
    , _sStartedBy = Nothing
    , _sTaskDefinition = pTaskDefinition_
    , _sContainerInstances = mempty
    }

-- | A list of container overrides in JSON format that specify the name of a
-- container in the specified task definition and the overrides it should
-- receive. You can override the default command for a container (that is
-- specified in the task definition or Docker image) with a 'command'
-- override. You can also override existing environment variables (that are
-- specified in the task definition or Docker image) on a container or add
-- new environment variables to it with an 'environment' override.
--
-- A total of 8192 characters are allowed for overrides. This limit
-- includes the JSON formatting characters of the override structure.
sOverrides :: Lens' StartTask (Maybe TaskOverride)
sOverrides = lens _sOverrides (\ s a -> s{_sOverrides = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to start your task on. If you do not specify a cluster, the
-- default cluster is assumed..
sCluster :: Lens' StartTask (Maybe Text)
sCluster = lens _sCluster (\ s a -> s{_sCluster = a});

-- | An optional tag specified when a task is started. For example if you
-- automatically trigger a task to run a batch process job, you could apply
-- a unique identifier for that job to your task with the 'startedBy'
-- parameter. You can then identify which tasks belong to that job by
-- filtering the results of a ListTasks call with the 'startedBy' value.
--
-- If a task is started by an Amazon ECS service, then the 'startedBy'
-- parameter contains the deployment ID of the service that starts it.
sStartedBy :: Lens' StartTask (Maybe Text)
sStartedBy = lens _sStartedBy (\ s a -> s{_sStartedBy = a});

-- | The 'family' and 'revision' ('family:revision') or full Amazon Resource
-- Name (ARN) of the task definition that you want to start. If a
-- 'revision' is not specified, the latest 'ACTIVE' revision is used.
sTaskDefinition :: Lens' StartTask Text
sTaskDefinition = lens _sTaskDefinition (\ s a -> s{_sTaskDefinition = a});

-- | The container instance UUIDs or full Amazon Resource Name (ARN) entries
-- for the container instances on which you would like to place your task.
--
-- The list of container instances to start tasks on is limited to 10.
sContainerInstances :: Lens' StartTask [Text]
sContainerInstances = lens _sContainerInstances (\ s a -> s{_sContainerInstances = a}) . _Coerce;

instance AWSRequest StartTask where
        type Sv StartTask = ECS
        type Rs StartTask = StartTaskResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 StartTaskResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "tasks" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders StartTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.StartTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartTask where
        toJSON StartTask'{..}
          = object
              ["overrides" .= _sOverrides, "cluster" .= _sCluster,
               "startedBy" .= _sStartedBy,
               "taskDefinition" .= _sTaskDefinition,
               "containerInstances" .= _sContainerInstances]

instance ToPath StartTask where
        toPath = const "/"

instance ToQuery StartTask where
        toQuery = const mempty

-- | /See:/ 'startTaskResponse' smart constructor.
data StartTaskResponse = StartTaskResponse'
    { _strsFailures :: !(Maybe [Failure])
    , _strsTasks    :: !(Maybe [Task])
    , _strsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'strsFailures'
--
-- * 'strsTasks'
--
-- * 'strsStatus'
startTaskResponse
    :: Int -- ^ 'strsStatus'
    -> StartTaskResponse
startTaskResponse pStatus_ =
    StartTaskResponse'
    { _strsFailures = Nothing
    , _strsTasks = Nothing
    , _strsStatus = pStatus_
    }

-- | Any failed tasks from your 'StartTask' action are listed here.
strsFailures :: Lens' StartTaskResponse [Failure]
strsFailures = lens _strsFailures (\ s a -> s{_strsFailures = a}) . _Default . _Coerce;

-- | A full description of the tasks that were started. Each task that was
-- successfully placed on your container instances will be described here.
strsTasks :: Lens' StartTaskResponse [Task]
strsTasks = lens _strsTasks (\ s a -> s{_strsTasks = a}) . _Default . _Coerce;

-- | The response status code.
strsStatus :: Lens' StartTaskResponse Int
strsStatus = lens _strsStatus (\ s a -> s{_strsStatus = a});
