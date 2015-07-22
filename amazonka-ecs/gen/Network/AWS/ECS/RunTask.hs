{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.RunTask
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Start a task using random placement and the default Amazon ECS
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
    , rtrqOverrides
    , rtrqCluster
    , rtrqCount
    , rtrqStartedBy
    , rtrqTaskDefinition

    -- * Response
    , RunTaskResponse
    -- ** Response constructor
    , runTaskResponse
    -- ** Response lenses
    , rtrsFailures
    , rtrsTasks
    , rtrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'runTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrqOverrides'
--
-- * 'rtrqCluster'
--
-- * 'rtrqCount'
--
-- * 'rtrqStartedBy'
--
-- * 'rtrqTaskDefinition'
data RunTask = RunTask'
    { _rtrqOverrides      :: !(Maybe TaskOverride)
    , _rtrqCluster        :: !(Maybe Text)
    , _rtrqCount          :: !(Maybe Int)
    , _rtrqStartedBy      :: !(Maybe Text)
    , _rtrqTaskDefinition :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RunTask' smart constructor.
runTask :: Text -> RunTask
runTask pTaskDefinition =
    RunTask'
    { _rtrqOverrides = Nothing
    , _rtrqCluster = Nothing
    , _rtrqCount = Nothing
    , _rtrqStartedBy = Nothing
    , _rtrqTaskDefinition = pTaskDefinition
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
rtrqOverrides :: Lens' RunTask (Maybe TaskOverride)
rtrqOverrides = lens _rtrqOverrides (\ s a -> s{_rtrqOverrides = a});

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- you want to run your task on. If you do not specify a cluster, the
-- default cluster is assumed..
rtrqCluster :: Lens' RunTask (Maybe Text)
rtrqCluster = lens _rtrqCluster (\ s a -> s{_rtrqCluster = a});

-- | The number of instantiations of the specified task that you would like
-- to place on your cluster.
--
-- The @count@ parameter is limited to 10 tasks per call.
rtrqCount :: Lens' RunTask (Maybe Int)
rtrqCount = lens _rtrqCount (\ s a -> s{_rtrqCount = a});

-- | An optional tag specified when a task is started. For example if you
-- automatically trigger a task to run a batch process job, you could apply
-- a unique identifier for that job to your task with the @startedBy@
-- parameter. You can then identify which tasks belong to that job by
-- filtering the results of a ListTasks call with the @startedBy@ value.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@
-- parameter contains the deployment ID of the service that starts it.
rtrqStartedBy :: Lens' RunTask (Maybe Text)
rtrqStartedBy = lens _rtrqStartedBy (\ s a -> s{_rtrqStartedBy = a});

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition that you want to run. If a @revision@
-- is not specified, the latest @ACTIVE@ revision is used.
rtrqTaskDefinition :: Lens' RunTask Text
rtrqTaskDefinition = lens _rtrqTaskDefinition (\ s a -> s{_rtrqTaskDefinition = a});

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
                     <*> (pure (fromEnum s)))

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
              ["overrides" .= _rtrqOverrides,
               "cluster" .= _rtrqCluster, "count" .= _rtrqCount,
               "startedBy" .= _rtrqStartedBy,
               "taskDefinition" .= _rtrqTaskDefinition]

instance ToPath RunTask where
        toPath = const "/"

instance ToQuery RunTask where
        toQuery = const mempty

-- | /See:/ 'runTaskResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrsFailures'
--
-- * 'rtrsTasks'
--
-- * 'rtrsStatus'
data RunTaskResponse = RunTaskResponse'
    { _rtrsFailures :: !(Maybe [Failure])
    , _rtrsTasks    :: !(Maybe [Task])
    , _rtrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RunTaskResponse' smart constructor.
runTaskResponse :: Int -> RunTaskResponse
runTaskResponse pStatus =
    RunTaskResponse'
    { _rtrsFailures = Nothing
    , _rtrsTasks = Nothing
    , _rtrsStatus = pStatus
    }

-- | Any failed tasks from your @RunTask@ action are listed here.
rtrsFailures :: Lens' RunTaskResponse [Failure]
rtrsFailures = lens _rtrsFailures (\ s a -> s{_rtrsFailures = a}) . _Default;

-- | A full description of the tasks that were run. Each task that was
-- successfully placed on your cluster will be described here.
rtrsTasks :: Lens' RunTaskResponse [Task]
rtrsTasks = lens _rtrsTasks (\ s a -> s{_rtrsTasks = a}) . _Default;

-- | FIXME: Undocumented member.
rtrsStatus :: Lens' RunTaskResponse Int
rtrsStatus = lens _rtrsStatus (\ s a -> s{_rtrsStatus = a});
