{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ECS.StartTask
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Starts a new task from the specified task definition on the specified
-- container instance or instances. If you want to use the default Amazon
-- ECS scheduler to place your task, use @RunTask@ instead.
--
-- The list of container instances to start tasks on is limited to 10.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_StartTask.html>
module Network.AWS.ECS.StartTask
    (
    -- * Request
      StartTask
    -- ** Request constructor
    , startTask
    -- ** Request lenses
    , staOverrides
    , staCluster
    , staStartedBy
    , staTaskDefinition
    , staContainerInstances

    -- * Response
    , StartTaskResponse
    -- ** Response constructor
    , startTaskResponse
    -- ** Response lenses
    , strFailures
    , strTasks
    , strStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staOverrides'
--
-- * 'staCluster'
--
-- * 'staStartedBy'
--
-- * 'staTaskDefinition'
--
-- * 'staContainerInstances'
data StartTask = StartTask'
    { _staOverrides          :: !(Maybe TaskOverride)
    , _staCluster            :: !(Maybe Text)
    , _staStartedBy          :: !(Maybe Text)
    , _staTaskDefinition     :: !Text
    , _staContainerInstances :: ![Text]
    } deriving (Eq,Read,Show)

-- | 'StartTask' smart constructor.
startTask :: Text -> StartTask
startTask pTaskDefinition =
    StartTask'
    { _staOverrides = Nothing
    , _staCluster = Nothing
    , _staStartedBy = Nothing
    , _staTaskDefinition = pTaskDefinition
    , _staContainerInstances = mempty
    }

-- | A list of container overrides in JSON format that specify the name of a
-- container in the specified task definition and the overrides it should
-- receive. You can override the default command for a container (that is
-- specified in the task definition or Docker image) with a @command@
-- override. You can also override existing environment variables (that are
-- specified in the task definition or Docker image) on a container or add
-- new environment variables to it with an @environment@ override.
--
-- A total of 8192 characters are allowed for overrides. This limit
-- includes the JSON formatting characters of the override structure.
staOverrides :: Lens' StartTask (Maybe TaskOverride)
staOverrides = lens _staOverrides (\ s a -> s{_staOverrides = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to start your task on. If you do not specify a cluster, the
-- default cluster is assumed..
staCluster :: Lens' StartTask (Maybe Text)
staCluster = lens _staCluster (\ s a -> s{_staCluster = a});

-- | An optional tag specified when a task is started. For example if you
-- automatically trigger a task to run a batch process job, you could apply
-- a unique identifier for that job to your task with the @startedBy@
-- parameter. You can then identify which tasks belong to that job by
-- filtering the results of a ListTasks call with the @startedBy@ value.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@
-- parameter contains the deployment ID of the service that starts it.
staStartedBy :: Lens' StartTask (Maybe Text)
staStartedBy = lens _staStartedBy (\ s a -> s{_staStartedBy = a});

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition that you want to start. If a
-- @revision@ is not specified, the latest @ACTIVE@ revision is used.
staTaskDefinition :: Lens' StartTask Text
staTaskDefinition = lens _staTaskDefinition (\ s a -> s{_staTaskDefinition = a});

-- | The container instance UUIDs or full Amazon Resource Name (ARN) entries
-- for the container instances on which you would like to place your task.
--
-- The list of container instances to start tasks on is limited to 10.
staContainerInstances :: Lens' StartTask [Text]
staContainerInstances = lens _staContainerInstances (\ s a -> s{_staContainerInstances = a});

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
              ["overrides" .= _staOverrides,
               "cluster" .= _staCluster,
               "startedBy" .= _staStartedBy,
               "taskDefinition" .= _staTaskDefinition,
               "containerInstances" .= _staContainerInstances]

instance ToPath StartTask where
        toPath = const "/"

instance ToQuery StartTask where
        toQuery = const mempty

-- | /See:/ 'startTaskResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'strFailures'
--
-- * 'strTasks'
--
-- * 'strStatus'
data StartTaskResponse = StartTaskResponse'
    { _strFailures :: !(Maybe [Failure])
    , _strTasks    :: !(Maybe [Task])
    , _strStatus   :: !Int
    } deriving (Eq,Read,Show)

-- | 'StartTaskResponse' smart constructor.
startTaskResponse :: Int -> StartTaskResponse
startTaskResponse pStatus =
    StartTaskResponse'
    { _strFailures = Nothing
    , _strTasks = Nothing
    , _strStatus = pStatus
    }

-- | Any failed tasks from your @StartTask@ action are listed here.
strFailures :: Lens' StartTaskResponse [Failure]
strFailures = lens _strFailures (\ s a -> s{_strFailures = a}) . _Default;

-- | A full description of the tasks that were started. Each task that was
-- successfully placed on your container instances will be described here.
strTasks :: Lens' StartTaskResponse [Task]
strTasks = lens _strTasks (\ s a -> s{_strTasks = a}) . _Default;

-- | FIXME: Undocumented member.
strStatus :: Lens' StartTaskResponse Int
strStatus = lens _strStatus (\ s a -> s{_strStatus = a});
