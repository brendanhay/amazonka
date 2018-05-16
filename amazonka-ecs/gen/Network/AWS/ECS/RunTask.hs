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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new task using the specified task definition.
--
--
-- You can allow Amazon ECS to place tasks for you, or you can customize how Amazon ECS places tasks using placement constraints and placement strategies. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html Scheduling Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- Alternatively, you can use 'StartTask' to use your own scheduler or place tasks manually on specific container instances.
--
-- The Amazon ECS API follows an eventual consistency model, due to the distributed nature of the system supporting the API. This means that the result of an API command you run that affects your Amazon ECS resources might not be immediately visible to all subsequent commands you run. You should keep this in mind when you carry out an API command that immediately follows a previous API command.
--
-- To manage eventual consistency, you can do the following:
--
--     * Confirm the state of the resource before you run a command to modify it. Run the DescribeTasks command using an exponential backoff algorithm to ensure that you allow enough time for the previous command to propagate through the system. To do this, run the DescribeTasks command repeatedly, starting with a couple of seconds of wait time, and increasing gradually up to five minutes of wait time.
--
--     * Add wait time between subsequent commands, even if the DescribeTasks command returns an accurate response. Apply an exponential backoff algorithm starting with a couple of seconds of wait time, and increase gradually up to about five minutes of wait time.
--
--
--
module Network.AWS.ECS.RunTask
    (
    -- * Creating a Request
      runTask
    , RunTask
    -- * Request Lenses
    , rtOverrides
    , rtGroup
    , rtCluster
    , rtPlatformVersion
    , rtCount
    , rtPlacementConstraints
    , rtPlacementStrategy
    , rtStartedBy
    , rtLaunchType
    , rtNetworkConfiguration
    , rtTaskDefinition

    -- * Destructuring the Response
    , runTaskResponse
    , RunTaskResponse
    -- * Response Lenses
    , rtrsFailures
    , rtrsTasks
    , rtrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'runTask' smart constructor.
data RunTask = RunTask'
  { _rtOverrides            :: !(Maybe TaskOverride)
  , _rtGroup                :: !(Maybe Text)
  , _rtCluster              :: !(Maybe Text)
  , _rtPlatformVersion      :: !(Maybe Text)
  , _rtCount                :: !(Maybe Int)
  , _rtPlacementConstraints :: !(Maybe [PlacementConstraint])
  , _rtPlacementStrategy    :: !(Maybe [PlacementStrategy])
  , _rtStartedBy            :: !(Maybe Text)
  , _rtLaunchType           :: !(Maybe LaunchType)
  , _rtNetworkConfiguration :: !(Maybe NetworkConfiguration)
  , _rtTaskDefinition       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtOverrides' - A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
--
-- * 'rtGroup' - The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
--
-- * 'rtCluster' - The short name or full Amazon Resource Name (ARN) of the cluster on which to run your task. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'rtPlatformVersion' - The platform version on which to run your task. If one is not specified, the latest version is used by default.
--
-- * 'rtCount' - The number of instantiations of the specified task to place on your cluster. You can specify up to 10 tasks per call.
--
-- * 'rtPlacementConstraints' - An array of placement constraint objects to use for the task. You can specify up to 10 constraints per task (including constraints in the task definition and those specified at run time).
--
-- * 'rtPlacementStrategy' - The placement strategy objects to use for the task. You can specify a maximum of five strategy rules per task.
--
-- * 'rtStartedBy' - An optional tag specified when a task is started. For example if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- * 'rtLaunchType' - The launch type on which to run your task.
--
-- * 'rtNetworkConfiguration' - The network configuration for the task. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own Elastic Network Interface, and it is not supported for other network modes. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'rtTaskDefinition' - The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
runTask
    :: Text -- ^ 'rtTaskDefinition'
    -> RunTask
runTask pTaskDefinition_ =
  RunTask'
    { _rtOverrides = Nothing
    , _rtGroup = Nothing
    , _rtCluster = Nothing
    , _rtPlatformVersion = Nothing
    , _rtCount = Nothing
    , _rtPlacementConstraints = Nothing
    , _rtPlacementStrategy = Nothing
    , _rtStartedBy = Nothing
    , _rtLaunchType = Nothing
    , _rtNetworkConfiguration = Nothing
    , _rtTaskDefinition = pTaskDefinition_
    }


-- | A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
rtOverrides :: Lens' RunTask (Maybe TaskOverride)
rtOverrides = lens _rtOverrides (\ s a -> s{_rtOverrides = a})

-- | The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
rtGroup :: Lens' RunTask (Maybe Text)
rtGroup = lens _rtGroup (\ s a -> s{_rtGroup = a})

-- | The short name or full Amazon Resource Name (ARN) of the cluster on which to run your task. If you do not specify a cluster, the default cluster is assumed.
rtCluster :: Lens' RunTask (Maybe Text)
rtCluster = lens _rtCluster (\ s a -> s{_rtCluster = a})

-- | The platform version on which to run your task. If one is not specified, the latest version is used by default.
rtPlatformVersion :: Lens' RunTask (Maybe Text)
rtPlatformVersion = lens _rtPlatformVersion (\ s a -> s{_rtPlatformVersion = a})

-- | The number of instantiations of the specified task to place on your cluster. You can specify up to 10 tasks per call.
rtCount :: Lens' RunTask (Maybe Int)
rtCount = lens _rtCount (\ s a -> s{_rtCount = a})

-- | An array of placement constraint objects to use for the task. You can specify up to 10 constraints per task (including constraints in the task definition and those specified at run time).
rtPlacementConstraints :: Lens' RunTask [PlacementConstraint]
rtPlacementConstraints = lens _rtPlacementConstraints (\ s a -> s{_rtPlacementConstraints = a}) . _Default . _Coerce

-- | The placement strategy objects to use for the task. You can specify a maximum of five strategy rules per task.
rtPlacementStrategy :: Lens' RunTask [PlacementStrategy]
rtPlacementStrategy = lens _rtPlacementStrategy (\ s a -> s{_rtPlacementStrategy = a}) . _Default . _Coerce

-- | An optional tag specified when a task is started. For example if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
rtStartedBy :: Lens' RunTask (Maybe Text)
rtStartedBy = lens _rtStartedBy (\ s a -> s{_rtStartedBy = a})

-- | The launch type on which to run your task.
rtLaunchType :: Lens' RunTask (Maybe LaunchType)
rtLaunchType = lens _rtLaunchType (\ s a -> s{_rtLaunchType = a})

-- | The network configuration for the task. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own Elastic Network Interface, and it is not supported for other network modes. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
rtNetworkConfiguration :: Lens' RunTask (Maybe NetworkConfiguration)
rtNetworkConfiguration = lens _rtNetworkConfiguration (\ s a -> s{_rtNetworkConfiguration = a})

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
rtTaskDefinition :: Lens' RunTask Text
rtTaskDefinition = lens _rtTaskDefinition (\ s a -> s{_rtTaskDefinition = a})

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

instance Hashable RunTask where

instance NFData RunTask where

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
                  ("group" .=) <$> _rtGroup,
                  ("cluster" .=) <$> _rtCluster,
                  ("platformVersion" .=) <$> _rtPlatformVersion,
                  ("count" .=) <$> _rtCount,
                  ("placementConstraints" .=) <$>
                    _rtPlacementConstraints,
                  ("placementStrategy" .=) <$> _rtPlacementStrategy,
                  ("startedBy" .=) <$> _rtStartedBy,
                  ("launchType" .=) <$> _rtLaunchType,
                  ("networkConfiguration" .=) <$>
                    _rtNetworkConfiguration,
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrsFailures' - Any failures associated with the call.
--
-- * 'rtrsTasks' - A full description of the tasks that were run. The tasks that were successfully placed on your cluster are described here.
--
-- * 'rtrsResponseStatus' - -- | The response status code.
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
rtrsFailures = lens _rtrsFailures (\ s a -> s{_rtrsFailures = a}) . _Default . _Coerce

-- | A full description of the tasks that were run. The tasks that were successfully placed on your cluster are described here.
rtrsTasks :: Lens' RunTaskResponse [Task]
rtrsTasks = lens _rtrsTasks (\ s a -> s{_rtrsTasks = a}) . _Default . _Coerce

-- | -- | The response status code.
rtrsResponseStatus :: Lens' RunTaskResponse Int
rtrsResponseStatus = lens _rtrsResponseStatus (\ s a -> s{_rtrsResponseStatus = a})

instance NFData RunTaskResponse where
