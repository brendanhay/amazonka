{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ECS.RunTask
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

-- | Start a task using random placement and the default Amazon ECS
-- scheduler. If you want to use your own scheduler or place a task on a
-- specific container instance, use @StartTask@ instead.
--
-- The @count@ parameter is limited to 10 tasks per call.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RunTask.html>
module Network.AWS.ECS.RunTask
    (
    -- * Request
      RunTask
    -- ** Request constructor
    , runTask
    -- ** Request lenses
    , rtOverrides
    , rtCluster
    , rtCount
    , rtStartedBy
    , rtTaskDefinition

    -- * Response
    , RunTaskResponse
    -- ** Response constructor
    , runTaskResponse
    -- ** Response lenses
    , rtrFailures
    , rtrTasks
    , rtrStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'runTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data RunTask = RunTask'
    { _rtOverrides      :: !(Maybe TaskOverride)
    , _rtCluster        :: !(Maybe Text)
    , _rtCount          :: !(Maybe Int)
    , _rtStartedBy      :: !(Maybe Text)
    , _rtTaskDefinition :: !Text
    } deriving (Eq,Read,Show)

-- | 'RunTask' smart constructor.
runTask :: Text -> RunTask
runTask pTaskDefinition =
    RunTask'
    { _rtOverrides = Nothing
    , _rtCluster = Nothing
    , _rtCount = Nothing
    , _rtStartedBy = Nothing
    , _rtTaskDefinition = pTaskDefinition
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
rtOverrides :: Lens' RunTask (Maybe TaskOverride)
rtOverrides = lens _rtOverrides (\ s a -> s{_rtOverrides = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to run your task on. If you do not specify a cluster, the
-- default cluster is assumed..
rtCluster :: Lens' RunTask (Maybe Text)
rtCluster = lens _rtCluster (\ s a -> s{_rtCluster = a});

-- | The number of instantiations of the specified task that you would like
-- to place on your cluster.
--
-- The @count@ parameter is limited to 10 tasks per call.
rtCount :: Lens' RunTask (Maybe Int)
rtCount = lens _rtCount (\ s a -> s{_rtCount = a});

-- | An optional tag specified when a task is started. For example if you
-- automatically trigger a task to run a batch process job, you could apply
-- a unique identifier for that job to your task with the @startedBy@
-- parameter. You can then identify which tasks belong to that job by
-- filtering the results of a ListTasks call with the @startedBy@ value.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@
-- parameter contains the deployment ID of the service that starts it.
rtStartedBy :: Lens' RunTask (Maybe Text)
rtStartedBy = lens _rtStartedBy (\ s a -> s{_rtStartedBy = a});

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition that you want to run. If a @revision@
-- is not specified, the latest @ACTIVE@ revision is used.
rtTaskDefinition :: Lens' RunTask Text
rtTaskDefinition = lens _rtTaskDefinition (\ s a -> s{_rtTaskDefinition = a});

instance AWSRequest RunTask where
        type Sv RunTask = ECS
        type Rs RunTask = RunTaskResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RunTaskResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "tasks" .!@ mempty)
                     <*> (pure s))

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
              ["overrides" .= _rtOverrides,
               "cluster" .= _rtCluster, "count" .= _rtCount,
               "startedBy" .= _rtStartedBy,
               "taskDefinition" .= _rtTaskDefinition]

instance ToPath RunTask where
        toPath = const "/"

instance ToQuery RunTask where
        toQuery = const mempty

-- | /See:/ 'runTaskResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrFailures'
--
-- * 'rtrTasks'
--
-- * 'rtrStatus'
data RunTaskResponse = RunTaskResponse'
    { _rtrFailures :: !(Maybe [Failure])
    , _rtrTasks    :: !(Maybe [Task])
    , _rtrStatus   :: !Status
    } deriving (Eq,Show)

-- | 'RunTaskResponse' smart constructor.
runTaskResponse :: Status -> RunTaskResponse
runTaskResponse pStatus =
    RunTaskResponse'
    { _rtrFailures = Nothing
    , _rtrTasks = Nothing
    , _rtrStatus = pStatus
    }

-- | Any failed tasks from your @RunTask@ action are listed here.
rtrFailures :: Lens' RunTaskResponse [Failure]
rtrFailures = lens _rtrFailures (\ s a -> s{_rtrFailures = a}) . _Default;

-- | A full description of the tasks that were run. Each task that was
-- successfully placed on your cluster will be described here.
rtrTasks :: Lens' RunTaskResponse [Task]
rtrTasks = lens _rtrTasks (\ s a -> s{_rtrTasks = a}) . _Default;

-- | FIXME: Undocumented member.
rtrStatus :: Lens' RunTaskResponse Status
rtrStatus = lens _rtrStatus (\ s a -> s{_rtrStatus = a});
