{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.StartTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new task from the specified task definition on the specified container instance or instances.
--
--
-- Alternatively, you can use 'RunTask' to place tasks for you. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html Scheduling Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.StartTask
  ( -- * Creating a Request
    startTask,
    StartTask,

    -- * Request Lenses
    sOverrides,
    sGroup,
    sCluster,
    sPropagateTags,
    sEnableECSManagedTags,
    sReferenceId,
    sStartedBy,
    sNetworkConfiguration,
    sTags,
    sContainerInstances,
    sTaskDefinition,

    -- * Destructuring the Response
    startTaskResponse,
    StartTaskResponse,

    -- * Response Lenses
    strsFailures,
    strsTasks,
    strsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startTask' smart constructor.
data StartTask = StartTask'
  { _sOverrides :: !(Maybe TaskOverride),
    _sGroup :: !(Maybe Text),
    _sCluster :: !(Maybe Text),
    _sPropagateTags :: !(Maybe PropagateTags),
    _sEnableECSManagedTags :: !(Maybe Bool),
    _sReferenceId :: !(Maybe Text),
    _sStartedBy :: !(Maybe Text),
    _sNetworkConfiguration :: !(Maybe NetworkConfiguration),
    _sTags :: !(Maybe [Tag]),
    _sContainerInstances :: ![Text],
    _sTaskDefinition :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sOverrides' - A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
--
-- * 'sGroup' - The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
--
-- * 'sCluster' - The short name or full Amazon Resource Name (ARN) of the cluster on which to start your task. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'sPropagateTags' - Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
--
-- * 'sEnableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'sReferenceId' - The reference ID to use for the task.
--
-- * 'sStartedBy' - An optional tag specified when a task is started. For example, if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- * 'sNetworkConfiguration' - The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
--
-- * 'sTags' - The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
-- * 'sContainerInstances' - The container instance IDs or full ARN entries for the container instances on which you would like to place your task. You can specify up to 10 container instances.
--
-- * 'sTaskDefinition' - The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to start. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
startTask ::
  -- | 'sTaskDefinition'
  Text ->
  StartTask
startTask pTaskDefinition_ =
  StartTask'
    { _sOverrides = Nothing,
      _sGroup = Nothing,
      _sCluster = Nothing,
      _sPropagateTags = Nothing,
      _sEnableECSManagedTags = Nothing,
      _sReferenceId = Nothing,
      _sStartedBy = Nothing,
      _sNetworkConfiguration = Nothing,
      _sTags = Nothing,
      _sContainerInstances = mempty,
      _sTaskDefinition = pTaskDefinition_
    }

-- | A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
sOverrides :: Lens' StartTask (Maybe TaskOverride)
sOverrides = lens _sOverrides (\s a -> s {_sOverrides = a})

-- | The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
sGroup :: Lens' StartTask (Maybe Text)
sGroup = lens _sGroup (\s a -> s {_sGroup = a})

-- | The short name or full Amazon Resource Name (ARN) of the cluster on which to start your task. If you do not specify a cluster, the default cluster is assumed.
sCluster :: Lens' StartTask (Maybe Text)
sCluster = lens _sCluster (\s a -> s {_sCluster = a})

-- | Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
sPropagateTags :: Lens' StartTask (Maybe PropagateTags)
sPropagateTags = lens _sPropagateTags (\s a -> s {_sPropagateTags = a})

-- | Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
sEnableECSManagedTags :: Lens' StartTask (Maybe Bool)
sEnableECSManagedTags = lens _sEnableECSManagedTags (\s a -> s {_sEnableECSManagedTags = a})

-- | The reference ID to use for the task.
sReferenceId :: Lens' StartTask (Maybe Text)
sReferenceId = lens _sReferenceId (\s a -> s {_sReferenceId = a})

-- | An optional tag specified when a task is started. For example, if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
sStartedBy :: Lens' StartTask (Maybe Text)
sStartedBy = lens _sStartedBy (\s a -> s {_sStartedBy = a})

-- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
sNetworkConfiguration :: Lens' StartTask (Maybe NetworkConfiguration)
sNetworkConfiguration = lens _sNetworkConfiguration (\s a -> s {_sNetworkConfiguration = a})

-- | The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
sTags :: Lens' StartTask [Tag]
sTags = lens _sTags (\s a -> s {_sTags = a}) . _Default . _Coerce

-- | The container instance IDs or full ARN entries for the container instances on which you would like to place your task. You can specify up to 10 container instances.
sContainerInstances :: Lens' StartTask [Text]
sContainerInstances = lens _sContainerInstances (\s a -> s {_sContainerInstances = a}) . _Coerce

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to start. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
sTaskDefinition :: Lens' StartTask Text
sTaskDefinition = lens _sTaskDefinition (\s a -> s {_sTaskDefinition = a})

instance AWSRequest StartTask where
  type Rs StartTask = StartTaskResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          StartTaskResponse'
            <$> (x .?> "failures" .!@ mempty)
            <*> (x .?> "tasks" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable StartTask

instance NFData StartTask

instance ToHeaders StartTask where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonEC2ContainerServiceV20141113.StartTask" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartTask where
  toJSON StartTask' {..} =
    object
      ( catMaybes
          [ ("overrides" .=) <$> _sOverrides,
            ("group" .=) <$> _sGroup,
            ("cluster" .=) <$> _sCluster,
            ("propagateTags" .=) <$> _sPropagateTags,
            ("enableECSManagedTags" .=) <$> _sEnableECSManagedTags,
            ("referenceId" .=) <$> _sReferenceId,
            ("startedBy" .=) <$> _sStartedBy,
            ("networkConfiguration" .=) <$> _sNetworkConfiguration,
            ("tags" .=) <$> _sTags,
            Just ("containerInstances" .= _sContainerInstances),
            Just ("taskDefinition" .= _sTaskDefinition)
          ]
      )

instance ToPath StartTask where
  toPath = const "/"

instance ToQuery StartTask where
  toQuery = const mempty

-- | /See:/ 'startTaskResponse' smart constructor.
data StartTaskResponse = StartTaskResponse'
  { _strsFailures ::
      !(Maybe [Failure]),
    _strsTasks :: !(Maybe [Task]),
    _strsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'strsFailures' - Any failures associated with the call.
--
-- * 'strsTasks' - A full description of the tasks that were started. Each task that was successfully placed on your container instances is described.
--
-- * 'strsResponseStatus' - -- | The response status code.
startTaskResponse ::
  -- | 'strsResponseStatus'
  Int ->
  StartTaskResponse
startTaskResponse pResponseStatus_ =
  StartTaskResponse'
    { _strsFailures = Nothing,
      _strsTasks = Nothing,
      _strsResponseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
strsFailures :: Lens' StartTaskResponse [Failure]
strsFailures = lens _strsFailures (\s a -> s {_strsFailures = a}) . _Default . _Coerce

-- | A full description of the tasks that were started. Each task that was successfully placed on your container instances is described.
strsTasks :: Lens' StartTaskResponse [Task]
strsTasks = lens _strsTasks (\s a -> s {_strsTasks = a}) . _Default . _Coerce

-- | -- | The response status code.
strsResponseStatus :: Lens' StartTaskResponse Int
strsResponseStatus = lens _strsResponseStatus (\s a -> s {_strsResponseStatus = a})

instance NFData StartTaskResponse
