{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Task
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Task where

import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.Connectivity
import Network.AWS.ECS.Types.Container
import Network.AWS.ECS.Types.HealthStatus
import Network.AWS.ECS.Types.InferenceAccelerator
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.TaskOverride
import Network.AWS.ECS.Types.TaskStopCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on a task in a cluster.
--
--
--
-- /See:/ 'task' smart constructor.
data Task = Task'
  { _tStoppedAt :: !(Maybe POSIX),
    _tDesiredStatus :: !(Maybe Text),
    _tOverrides :: !(Maybe TaskOverride),
    _tInferenceAccelerators :: !(Maybe [InferenceAccelerator]),
    _tClusterARN :: !(Maybe Text),
    _tGroup :: !(Maybe Text),
    _tAttachments :: !(Maybe [Attachment]),
    _tCreatedAt :: !(Maybe POSIX),
    _tStopCode :: !(Maybe TaskStopCode),
    _tPlatformVersion :: !(Maybe Text),
    _tTaskARN :: !(Maybe Text),
    _tContainerInstanceARN :: !(Maybe Text),
    _tExecutionStoppedAt :: !(Maybe POSIX),
    _tLastStatus :: !(Maybe Text),
    _tMemory :: !(Maybe Text),
    _tPullStoppedAt :: !(Maybe POSIX),
    _tContainers :: !(Maybe [Container]),
    _tStartedAt :: !(Maybe POSIX),
    _tAvailabilityZone :: !(Maybe Text),
    _tAttributes :: !(Maybe [Attribute]),
    _tVersion :: !(Maybe Integer),
    _tCapacityProviderName :: !(Maybe Text),
    _tStartedBy :: !(Maybe Text),
    _tStoppedReason :: !(Maybe Text),
    _tConnectivity :: !(Maybe Connectivity),
    _tStoppingAt :: !(Maybe POSIX),
    _tLaunchType :: !(Maybe LaunchType),
    _tTaskDefinitionARN :: !(Maybe Text),
    _tHealthStatus :: !(Maybe HealthStatus),
    _tConnectivityAt :: !(Maybe POSIX),
    _tCpu :: !(Maybe Text),
    _tPullStartedAt :: !(Maybe POSIX),
    _tTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Task' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStoppedAt' - The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
--
-- * 'tDesiredStatus' - The desired status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
--
-- * 'tOverrides' - One or more container overrides.
--
-- * 'tInferenceAccelerators' - The Elastic Inference accelerator associated with the task.
--
-- * 'tClusterARN' - The ARN of the cluster that hosts the task.
--
-- * 'tGroup' - The name of the task group associated with the task.
--
-- * 'tAttachments' - The Elastic Network Adapter associated with the task if the task uses the @awsvpc@ network mode.
--
-- * 'tCreatedAt' - The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
--
-- * 'tStopCode' - The stop code indicating why a task was stopped. The @stoppedReason@ may contain additional details.
--
-- * 'tPlatformVersion' - The platform version on which your task is running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tTaskARN' - The Amazon Resource Name (ARN) of the task.
--
-- * 'tContainerInstanceARN' - The ARN of the container instances that host the task.
--
-- * 'tExecutionStoppedAt' - The Unix timestamp for when the task execution stopped.
--
-- * 'tLastStatus' - The last known status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
--
-- * 'tMemory' - The amount of memory (in MiB) used by the task as expressed in a task definition. It can be expressed as an integer using MiB, for example @1024@ . It can also be expressed as a string using GB, for example @1GB@ or @1 GB@ . String values are converted to an integer indicating the MiB when the task definition is registered. If you are using the EC2 launch type, this field is optional. If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @cpu@ parameter:     * 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256 (.25 vCPU)     * 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@ values: 512 (.5 vCPU)     * 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)     * Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 2048 (2 vCPU)     * Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 4096 (4 vCPU)
--
-- * 'tPullStoppedAt' - The Unix timestamp for when the container image pull completed.
--
-- * 'tContainers' - The containers associated with the task.
--
-- * 'tStartedAt' - The Unix timestamp for when the task started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
--
-- * 'tAvailabilityZone' - The availability zone of the task.
--
-- * 'tAttributes' - The attributes of the task
--
-- * 'tVersion' - The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch Events, you can compare the version of a task reported by the Amazon ECS API actions with the version reported in CloudWatch Events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
--
-- * 'tCapacityProviderName' - The capacity provider associated with the task.
--
-- * 'tStartedBy' - The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- * 'tStoppedReason' - The reason that the task was stopped.
--
-- * 'tConnectivity' - The connectivity status of a task.
--
-- * 'tStoppingAt' - The Unix timestamp for when the task stops (transitions from the @RUNNING@ state to @STOPPED@ ).
--
-- * 'tLaunchType' - The launch type on which your task is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tTaskDefinitionARN' - The ARN of the task definition that creates the task.
--
-- * 'tHealthStatus' - The health status for the task, which is determined by the health of the essential containers in the task. If all essential containers in the task are reporting as @HEALTHY@ , then the task status also reports as @HEALTHY@ . If any essential containers in the task are reporting as @UNHEALTHY@ or @UNKNOWN@ , then the task status also reports as @UNHEALTHY@ or @UNKNOWN@ , accordingly.
--
-- * 'tConnectivityAt' - The Unix timestamp for when the task last went into @CONNECTED@ status.
--
-- * 'tCpu' - The number of CPU units used by the task as expressed in a task definition. It can be expressed as an integer using CPU units, for example @1024@ . It can also be expressed as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ . String values are converted to an integer indicating the CPU units when the task definition is registered. If you are using the EC2 launch type, this field is optional. Supported values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units (@10@ vCPUs). If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @memory@ parameter:     * 256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)     * 512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)     * 1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)     * 2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)     * 4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
--
-- * 'tPullStartedAt' - The Unix timestamp for when the container image pull began.
--
-- * 'tTags' - The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
task ::
  Task
task =
  Task'
    { _tStoppedAt = Nothing,
      _tDesiredStatus = Nothing,
      _tOverrides = Nothing,
      _tInferenceAccelerators = Nothing,
      _tClusterARN = Nothing,
      _tGroup = Nothing,
      _tAttachments = Nothing,
      _tCreatedAt = Nothing,
      _tStopCode = Nothing,
      _tPlatformVersion = Nothing,
      _tTaskARN = Nothing,
      _tContainerInstanceARN = Nothing,
      _tExecutionStoppedAt = Nothing,
      _tLastStatus = Nothing,
      _tMemory = Nothing,
      _tPullStoppedAt = Nothing,
      _tContainers = Nothing,
      _tStartedAt = Nothing,
      _tAvailabilityZone = Nothing,
      _tAttributes = Nothing,
      _tVersion = Nothing,
      _tCapacityProviderName = Nothing,
      _tStartedBy = Nothing,
      _tStoppedReason = Nothing,
      _tConnectivity = Nothing,
      _tStoppingAt = Nothing,
      _tLaunchType = Nothing,
      _tTaskDefinitionARN = Nothing,
      _tHealthStatus = Nothing,
      _tConnectivityAt = Nothing,
      _tCpu = Nothing,
      _tPullStartedAt = Nothing,
      _tTags = Nothing
    }

-- | The Unix timestamp for when the task was stopped (the task transitioned from the @RUNNING@ state to the @STOPPED@ state).
tStoppedAt :: Lens' Task (Maybe UTCTime)
tStoppedAt = lens _tStoppedAt (\s a -> s {_tStoppedAt = a}) . mapping _Time

-- | The desired status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
tDesiredStatus :: Lens' Task (Maybe Text)
tDesiredStatus = lens _tDesiredStatus (\s a -> s {_tDesiredStatus = a})

-- | One or more container overrides.
tOverrides :: Lens' Task (Maybe TaskOverride)
tOverrides = lens _tOverrides (\s a -> s {_tOverrides = a})

-- | The Elastic Inference accelerator associated with the task.
tInferenceAccelerators :: Lens' Task [InferenceAccelerator]
tInferenceAccelerators = lens _tInferenceAccelerators (\s a -> s {_tInferenceAccelerators = a}) . _Default . _Coerce

-- | The ARN of the cluster that hosts the task.
tClusterARN :: Lens' Task (Maybe Text)
tClusterARN = lens _tClusterARN (\s a -> s {_tClusterARN = a})

-- | The name of the task group associated with the task.
tGroup :: Lens' Task (Maybe Text)
tGroup = lens _tGroup (\s a -> s {_tGroup = a})

-- | The Elastic Network Adapter associated with the task if the task uses the @awsvpc@ network mode.
tAttachments :: Lens' Task [Attachment]
tAttachments = lens _tAttachments (\s a -> s {_tAttachments = a}) . _Default . _Coerce

-- | The Unix timestamp for when the task was created (the task entered the @PENDING@ state).
tCreatedAt :: Lens' Task (Maybe UTCTime)
tCreatedAt = lens _tCreatedAt (\s a -> s {_tCreatedAt = a}) . mapping _Time

-- | The stop code indicating why a task was stopped. The @stoppedReason@ may contain additional details.
tStopCode :: Lens' Task (Maybe TaskStopCode)
tStopCode = lens _tStopCode (\s a -> s {_tStopCode = a})

-- | The platform version on which your task is running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
tPlatformVersion :: Lens' Task (Maybe Text)
tPlatformVersion = lens _tPlatformVersion (\s a -> s {_tPlatformVersion = a})

-- | The Amazon Resource Name (ARN) of the task.
tTaskARN :: Lens' Task (Maybe Text)
tTaskARN = lens _tTaskARN (\s a -> s {_tTaskARN = a})

-- | The ARN of the container instances that host the task.
tContainerInstanceARN :: Lens' Task (Maybe Text)
tContainerInstanceARN = lens _tContainerInstanceARN (\s a -> s {_tContainerInstanceARN = a})

-- | The Unix timestamp for when the task execution stopped.
tExecutionStoppedAt :: Lens' Task (Maybe UTCTime)
tExecutionStoppedAt = lens _tExecutionStoppedAt (\s a -> s {_tExecutionStoppedAt = a}) . mapping _Time

-- | The last known status of the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-lifecycle.html Task Lifecycle> .
tLastStatus :: Lens' Task (Maybe Text)
tLastStatus = lens _tLastStatus (\s a -> s {_tLastStatus = a})

-- | The amount of memory (in MiB) used by the task as expressed in a task definition. It can be expressed as an integer using MiB, for example @1024@ . It can also be expressed as a string using GB, for example @1GB@ or @1 GB@ . String values are converted to an integer indicating the MiB when the task definition is registered. If you are using the EC2 launch type, this field is optional. If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @cpu@ parameter:     * 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB) - Available @cpu@ values: 256 (.25 vCPU)     * 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB) - Available @cpu@ values: 512 (.5 vCPU)     * 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB) - Available @cpu@ values: 1024 (1 vCPU)     * Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 2048 (2 vCPU)     * Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB) - Available @cpu@ values: 4096 (4 vCPU)
tMemory :: Lens' Task (Maybe Text)
tMemory = lens _tMemory (\s a -> s {_tMemory = a})

-- | The Unix timestamp for when the container image pull completed.
tPullStoppedAt :: Lens' Task (Maybe UTCTime)
tPullStoppedAt = lens _tPullStoppedAt (\s a -> s {_tPullStoppedAt = a}) . mapping _Time

-- | The containers associated with the task.
tContainers :: Lens' Task [Container]
tContainers = lens _tContainers (\s a -> s {_tContainers = a}) . _Default . _Coerce

-- | The Unix timestamp for when the task started (the task transitioned from the @PENDING@ state to the @RUNNING@ state).
tStartedAt :: Lens' Task (Maybe UTCTime)
tStartedAt = lens _tStartedAt (\s a -> s {_tStartedAt = a}) . mapping _Time

-- | The availability zone of the task.
tAvailabilityZone :: Lens' Task (Maybe Text)
tAvailabilityZone = lens _tAvailabilityZone (\s a -> s {_tAvailabilityZone = a})

-- | The attributes of the task
tAttributes :: Lens' Task [Attribute]
tAttributes = lens _tAttributes (\s a -> s {_tAttributes = a}) . _Default . _Coerce

-- | The version counter for the task. Every time a task experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS task state with CloudWatch Events, you can compare the version of a task reported by the Amazon ECS API actions with the version reported in CloudWatch Events for the task (inside the @detail@ object) to verify that the version in your event stream is current.
tVersion :: Lens' Task (Maybe Integer)
tVersion = lens _tVersion (\s a -> s {_tVersion = a})

-- | The capacity provider associated with the task.
tCapacityProviderName :: Lens' Task (Maybe Text)
tCapacityProviderName = lens _tCapacityProviderName (\s a -> s {_tCapacityProviderName = a})

-- | The tag specified when a task is started. If the task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
tStartedBy :: Lens' Task (Maybe Text)
tStartedBy = lens _tStartedBy (\s a -> s {_tStartedBy = a})

-- | The reason that the task was stopped.
tStoppedReason :: Lens' Task (Maybe Text)
tStoppedReason = lens _tStoppedReason (\s a -> s {_tStoppedReason = a})

-- | The connectivity status of a task.
tConnectivity :: Lens' Task (Maybe Connectivity)
tConnectivity = lens _tConnectivity (\s a -> s {_tConnectivity = a})

-- | The Unix timestamp for when the task stops (transitions from the @RUNNING@ state to @STOPPED@ ).
tStoppingAt :: Lens' Task (Maybe UTCTime)
tStoppingAt = lens _tStoppingAt (\s a -> s {_tStoppingAt = a}) . mapping _Time

-- | The launch type on which your task is running. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
tLaunchType :: Lens' Task (Maybe LaunchType)
tLaunchType = lens _tLaunchType (\s a -> s {_tLaunchType = a})

-- | The ARN of the task definition that creates the task.
tTaskDefinitionARN :: Lens' Task (Maybe Text)
tTaskDefinitionARN = lens _tTaskDefinitionARN (\s a -> s {_tTaskDefinitionARN = a})

-- | The health status for the task, which is determined by the health of the essential containers in the task. If all essential containers in the task are reporting as @HEALTHY@ , then the task status also reports as @HEALTHY@ . If any essential containers in the task are reporting as @UNHEALTHY@ or @UNKNOWN@ , then the task status also reports as @UNHEALTHY@ or @UNKNOWN@ , accordingly.
tHealthStatus :: Lens' Task (Maybe HealthStatus)
tHealthStatus = lens _tHealthStatus (\s a -> s {_tHealthStatus = a})

-- | The Unix timestamp for when the task last went into @CONNECTED@ status.
tConnectivityAt :: Lens' Task (Maybe UTCTime)
tConnectivityAt = lens _tConnectivityAt (\s a -> s {_tConnectivityAt = a}) . mapping _Time

-- | The number of CPU units used by the task as expressed in a task definition. It can be expressed as an integer using CPU units, for example @1024@ . It can also be expressed as a string using vCPUs, for example @1 vCPU@ or @1 vcpu@ . String values are converted to an integer indicating the CPU units when the task definition is registered. If you are using the EC2 launch type, this field is optional. Supported values are between @128@ CPU units (@0.125@ vCPUs) and @10240@ CPU units (@10@ vCPUs). If you are using the Fargate launch type, this field is required and you must use one of the following values, which determines your range of supported values for the @memory@ parameter:     * 256 (.25 vCPU) - Available @memory@ values: 512 (0.5 GB), 1024 (1 GB), 2048 (2 GB)     * 512 (.5 vCPU) - Available @memory@ values: 1024 (1 GB), 2048 (2 GB), 3072 (3 GB), 4096 (4 GB)     * 1024 (1 vCPU) - Available @memory@ values: 2048 (2 GB), 3072 (3 GB), 4096 (4 GB), 5120 (5 GB), 6144 (6 GB), 7168 (7 GB), 8192 (8 GB)     * 2048 (2 vCPU) - Available @memory@ values: Between 4096 (4 GB) and 16384 (16 GB) in increments of 1024 (1 GB)     * 4096 (4 vCPU) - Available @memory@ values: Between 8192 (8 GB) and 30720 (30 GB) in increments of 1024 (1 GB)
tCpu :: Lens' Task (Maybe Text)
tCpu = lens _tCpu (\s a -> s {_tCpu = a})

-- | The Unix timestamp for when the container image pull began.
tPullStartedAt :: Lens' Task (Maybe UTCTime)
tPullStartedAt = lens _tPullStartedAt (\s a -> s {_tPullStartedAt = a}) . mapping _Time

-- | The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. The following basic restrictions apply to tags:     * Maximum number of tags per resource - 50     * For each resource, each tag key must be unique, and each tag key can have only one value.     * Maximum key length - 128 Unicode characters in UTF-8     * Maximum value length - 256 Unicode characters in UTF-8     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.     * Tag keys and values are case-sensitive.     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
tTags :: Lens' Task [Tag]
tTags = lens _tTags (\s a -> s {_tTags = a}) . _Default . _Coerce

instance FromJSON Task where
  parseJSON =
    withObject
      "Task"
      ( \x ->
          Task'
            <$> (x .:? "stoppedAt")
            <*> (x .:? "desiredStatus")
            <*> (x .:? "overrides")
            <*> (x .:? "inferenceAccelerators" .!= mempty)
            <*> (x .:? "clusterArn")
            <*> (x .:? "group")
            <*> (x .:? "attachments" .!= mempty)
            <*> (x .:? "createdAt")
            <*> (x .:? "stopCode")
            <*> (x .:? "platformVersion")
            <*> (x .:? "taskArn")
            <*> (x .:? "containerInstanceArn")
            <*> (x .:? "executionStoppedAt")
            <*> (x .:? "lastStatus")
            <*> (x .:? "memory")
            <*> (x .:? "pullStoppedAt")
            <*> (x .:? "containers" .!= mempty)
            <*> (x .:? "startedAt")
            <*> (x .:? "availabilityZone")
            <*> (x .:? "attributes" .!= mempty)
            <*> (x .:? "version")
            <*> (x .:? "capacityProviderName")
            <*> (x .:? "startedBy")
            <*> (x .:? "stoppedReason")
            <*> (x .:? "connectivity")
            <*> (x .:? "stoppingAt")
            <*> (x .:? "launchType")
            <*> (x .:? "taskDefinitionArn")
            <*> (x .:? "healthStatus")
            <*> (x .:? "connectivityAt")
            <*> (x .:? "cpu")
            <*> (x .:? "pullStartedAt")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable Task

instance NFData Task
