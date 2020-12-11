{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.RunTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new task using the specified task definition.
--
-- You can allow Amazon ECS to place tasks for you, or you can customize how Amazon ECS places tasks using placement constraints and placement strategies. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html Scheduling Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
-- Alternatively, you can use 'StartTask' to use your own scheduler or place tasks manually on specific container instances.
-- The Amazon ECS API follows an eventual consistency model, due to the distributed nature of the system supporting the API. This means that the result of an API command you run that affects your Amazon ECS resources might not be immediately visible to all subsequent commands you run. Keep this in mind when you carry out an API command that immediately follows a previous API command.
-- To manage eventual consistency, you can do the following:
--
--     * Confirm the state of the resource before you run a command to modify it. Run the DescribeTasks command using an exponential backoff algorithm to ensure that you allow enough time for the previous command to propagate through the system. To do this, run the DescribeTasks command repeatedly, starting with a couple of seconds of wait time and increasing gradually up to five minutes of wait time.
--
--
--     * Add wait time between subsequent commands, even if the DescribeTasks command returns an accurate response. Apply an exponential backoff algorithm starting with a couple of seconds of wait time, and increase gradually up to about five minutes of wait time.
module Network.AWS.ECS.RunTask
  ( -- * Creating a request
    RunTask (..),
    mkRunTask,

    -- ** Request lenses
    rtOverrides,
    rtGroup,
    rtCluster,
    rtPropagateTags,
    rtPlatformVersion,
    rtEnableECSManagedTags,
    rtCount,
    rtReferenceId,
    rtPlacementConstraints,
    rtPlacementStrategy,
    rtStartedBy,
    rtLaunchType,
    rtNetworkConfiguration,
    rtCapacityProviderStrategy,
    rtTags,
    rtTaskDefinition,

    -- * Destructuring the response
    RunTaskResponse (..),
    mkRunTaskResponse,

    -- ** Response lenses
    rtrsFailures,
    rtrsTasks,
    rtrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRunTask' smart constructor.
data RunTask = RunTask'
  { overrides :: Lude.Maybe TaskOverride,
    group :: Lude.Maybe Lude.Text,
    cluster :: Lude.Maybe Lude.Text,
    propagateTags :: Lude.Maybe PropagateTags,
    platformVersion :: Lude.Maybe Lude.Text,
    enableECSManagedTags :: Lude.Maybe Lude.Bool,
    count :: Lude.Maybe Lude.Int,
    referenceId :: Lude.Maybe Lude.Text,
    placementConstraints :: Lude.Maybe [PlacementConstraint],
    placementStrategy :: Lude.Maybe [PlacementStrategy],
    startedBy :: Lude.Maybe Lude.Text,
    launchType :: Lude.Maybe LaunchType,
    networkConfiguration :: Lude.Maybe NetworkConfiguration,
    capacityProviderStrategy ::
      Lude.Maybe [CapacityProviderStrategyItem],
    tags :: Lude.Maybe [Tag],
    taskDefinition :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RunTask' with the minimum fields required to make a request.
--
-- * 'capacityProviderStrategy' - The capacity provider strategy to use for the task.
--
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster on which to run your task. If you do not specify a cluster, the default cluster is assumed.
-- * 'count' - The number of instantiations of the specified task to place on your cluster. You can specify up to 10 tasks per call.
-- * 'enableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'group' - The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
-- * 'launchType' - The launch type on which to run your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
-- * 'networkConfiguration' - The network configuration for the task. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own elastic network interface, and it is not supported for other network modes. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'overrides' - A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
-- * 'placementConstraints' - An array of placement constraint objects to use for the task. You can specify up to 10 constraints per task (including constraints in the task definition and those specified at runtime).
-- * 'placementStrategy' - The placement strategy objects to use for the task. You can specify a maximum of five strategy rules per task.
-- * 'platformVersion' - The platform version the task should run. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'propagateTags' - Specifies whether to propagate the tags from the task definition to the task. If no value is specified, the tags are not propagated. Tags can only be propagated to the task during task creation. To add tags to a task after task creation, use the 'TagResource' API action.
-- * 'referenceId' - The reference ID to use for the task.
-- * 'startedBy' - An optional tag specified when a task is started. For example, if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
-- * 'tags' - The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
-- * 'taskDefinition' - The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
mkRunTask ::
  -- | 'taskDefinition'
  Lude.Text ->
  RunTask
mkRunTask pTaskDefinition_ =
  RunTask'
    { overrides = Lude.Nothing,
      group = Lude.Nothing,
      cluster = Lude.Nothing,
      propagateTags = Lude.Nothing,
      platformVersion = Lude.Nothing,
      enableECSManagedTags = Lude.Nothing,
      count = Lude.Nothing,
      referenceId = Lude.Nothing,
      placementConstraints = Lude.Nothing,
      placementStrategy = Lude.Nothing,
      startedBy = Lude.Nothing,
      launchType = Lude.Nothing,
      networkConfiguration = Lude.Nothing,
      capacityProviderStrategy = Lude.Nothing,
      tags = Lude.Nothing,
      taskDefinition = pTaskDefinition_
    }

-- | A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtOverrides :: Lens.Lens' RunTask (Lude.Maybe TaskOverride)
rtOverrides = Lens.lens (overrides :: RunTask -> Lude.Maybe TaskOverride) (\s a -> s {overrides = a} :: RunTask)
{-# DEPRECATED rtOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

-- | The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtGroup :: Lens.Lens' RunTask (Lude.Maybe Lude.Text)
rtGroup = Lens.lens (group :: RunTask -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: RunTask)
{-# DEPRECATED rtGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster on which to run your task. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCluster :: Lens.Lens' RunTask (Lude.Maybe Lude.Text)
rtCluster = Lens.lens (cluster :: RunTask -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: RunTask)
{-# DEPRECATED rtCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | Specifies whether to propagate the tags from the task definition to the task. If no value is specified, the tags are not propagated. Tags can only be propagated to the task during task creation. To add tags to a task after task creation, use the 'TagResource' API action.
--
-- /Note:/ Consider using 'propagateTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPropagateTags :: Lens.Lens' RunTask (Lude.Maybe PropagateTags)
rtPropagateTags = Lens.lens (propagateTags :: RunTask -> Lude.Maybe PropagateTags) (\s a -> s {propagateTags = a} :: RunTask)
{-# DEPRECATED rtPropagateTags "Use generic-lens or generic-optics with 'propagateTags' instead." #-}

-- | The platform version the task should run. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPlatformVersion :: Lens.Lens' RunTask (Lude.Maybe Lude.Text)
rtPlatformVersion = Lens.lens (platformVersion :: RunTask -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: RunTask)
{-# DEPRECATED rtPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'enableECSManagedTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtEnableECSManagedTags :: Lens.Lens' RunTask (Lude.Maybe Lude.Bool)
rtEnableECSManagedTags = Lens.lens (enableECSManagedTags :: RunTask -> Lude.Maybe Lude.Bool) (\s a -> s {enableECSManagedTags = a} :: RunTask)
{-# DEPRECATED rtEnableECSManagedTags "Use generic-lens or generic-optics with 'enableECSManagedTags' instead." #-}

-- | The number of instantiations of the specified task to place on your cluster. You can specify up to 10 tasks per call.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCount :: Lens.Lens' RunTask (Lude.Maybe Lude.Int)
rtCount = Lens.lens (count :: RunTask -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: RunTask)
{-# DEPRECATED rtCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The reference ID to use for the task.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReferenceId :: Lens.Lens' RunTask (Lude.Maybe Lude.Text)
rtReferenceId = Lens.lens (referenceId :: RunTask -> Lude.Maybe Lude.Text) (\s a -> s {referenceId = a} :: RunTask)
{-# DEPRECATED rtReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

-- | An array of placement constraint objects to use for the task. You can specify up to 10 constraints per task (including constraints in the task definition and those specified at runtime).
--
-- /Note:/ Consider using 'placementConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPlacementConstraints :: Lens.Lens' RunTask (Lude.Maybe [PlacementConstraint])
rtPlacementConstraints = Lens.lens (placementConstraints :: RunTask -> Lude.Maybe [PlacementConstraint]) (\s a -> s {placementConstraints = a} :: RunTask)
{-# DEPRECATED rtPlacementConstraints "Use generic-lens or generic-optics with 'placementConstraints' instead." #-}

-- | The placement strategy objects to use for the task. You can specify a maximum of five strategy rules per task.
--
-- /Note:/ Consider using 'placementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPlacementStrategy :: Lens.Lens' RunTask (Lude.Maybe [PlacementStrategy])
rtPlacementStrategy = Lens.lens (placementStrategy :: RunTask -> Lude.Maybe [PlacementStrategy]) (\s a -> s {placementStrategy = a} :: RunTask)
{-# DEPRECATED rtPlacementStrategy "Use generic-lens or generic-optics with 'placementStrategy' instead." #-}

-- | An optional tag specified when a task is started. For example, if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- /Note:/ Consider using 'startedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtStartedBy :: Lens.Lens' RunTask (Lude.Maybe Lude.Text)
rtStartedBy = Lens.lens (startedBy :: RunTask -> Lude.Maybe Lude.Text) (\s a -> s {startedBy = a} :: RunTask)
{-# DEPRECATED rtStartedBy "Use generic-lens or generic-optics with 'startedBy' instead." #-}

-- | The launch type on which to run your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtLaunchType :: Lens.Lens' RunTask (Lude.Maybe LaunchType)
rtLaunchType = Lens.lens (launchType :: RunTask -> Lude.Maybe LaunchType) (\s a -> s {launchType = a} :: RunTask)
{-# DEPRECATED rtLaunchType "Use generic-lens or generic-optics with 'launchType' instead." #-}

-- | The network configuration for the task. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own elastic network interface, and it is not supported for other network modes. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtNetworkConfiguration :: Lens.Lens' RunTask (Lude.Maybe NetworkConfiguration)
rtNetworkConfiguration = Lens.lens (networkConfiguration :: RunTask -> Lude.Maybe NetworkConfiguration) (\s a -> s {networkConfiguration = a} :: RunTask)
{-# DEPRECATED rtNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

-- | The capacity provider strategy to use for the task.
--
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCapacityProviderStrategy :: Lens.Lens' RunTask (Lude.Maybe [CapacityProviderStrategyItem])
rtCapacityProviderStrategy = Lens.lens (capacityProviderStrategy :: RunTask -> Lude.Maybe [CapacityProviderStrategyItem]) (\s a -> s {capacityProviderStrategy = a} :: RunTask)
{-# DEPRECATED rtCapacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead." #-}

-- | The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTags :: Lens.Lens' RunTask (Lude.Maybe [Tag])
rtTags = Lens.lens (tags :: RunTask -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RunTask)
{-# DEPRECATED rtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTaskDefinition :: Lens.Lens' RunTask Lude.Text
rtTaskDefinition = Lens.lens (taskDefinition :: RunTask -> Lude.Text) (\s a -> s {taskDefinition = a} :: RunTask)
{-# DEPRECATED rtTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

instance Lude.AWSRequest RunTask where
  type Rs RunTask = RunTaskResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RunTaskResponse'
            Lude.<$> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "tasks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RunTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonEC2ContainerServiceV20141113.RunTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RunTask where
  toJSON RunTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("overrides" Lude..=) Lude.<$> overrides,
            ("group" Lude..=) Lude.<$> group,
            ("cluster" Lude..=) Lude.<$> cluster,
            ("propagateTags" Lude..=) Lude.<$> propagateTags,
            ("platformVersion" Lude..=) Lude.<$> platformVersion,
            ("enableECSManagedTags" Lude..=) Lude.<$> enableECSManagedTags,
            ("count" Lude..=) Lude.<$> count,
            ("referenceId" Lude..=) Lude.<$> referenceId,
            ("placementConstraints" Lude..=) Lude.<$> placementConstraints,
            ("placementStrategy" Lude..=) Lude.<$> placementStrategy,
            ("startedBy" Lude..=) Lude.<$> startedBy,
            ("launchType" Lude..=) Lude.<$> launchType,
            ("networkConfiguration" Lude..=) Lude.<$> networkConfiguration,
            ("capacityProviderStrategy" Lude..=)
              Lude.<$> capacityProviderStrategy,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("taskDefinition" Lude..= taskDefinition)
          ]
      )

instance Lude.ToPath RunTask where
  toPath = Lude.const "/"

instance Lude.ToQuery RunTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRunTaskResponse' smart constructor.
data RunTaskResponse = RunTaskResponse'
  { failures ::
      Lude.Maybe [Failure],
    tasks :: Lude.Maybe [Task],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RunTaskResponse' with the minimum fields required to make a request.
--
-- * 'failures' - Any failures associated with the call.
-- * 'responseStatus' - The response status code.
-- * 'tasks' - A full description of the tasks that were run. The tasks that were successfully placed on your cluster are described here.
mkRunTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RunTaskResponse
mkRunTaskResponse pResponseStatus_ =
  RunTaskResponse'
    { failures = Lude.Nothing,
      tasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsFailures :: Lens.Lens' RunTaskResponse (Lude.Maybe [Failure])
rtrsFailures = Lens.lens (failures :: RunTaskResponse -> Lude.Maybe [Failure]) (\s a -> s {failures = a} :: RunTaskResponse)
{-# DEPRECATED rtrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | A full description of the tasks that were run. The tasks that were successfully placed on your cluster are described here.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsTasks :: Lens.Lens' RunTaskResponse (Lude.Maybe [Task])
rtrsTasks = Lens.lens (tasks :: RunTaskResponse -> Lude.Maybe [Task]) (\s a -> s {tasks = a} :: RunTaskResponse)
{-# DEPRECATED rtrsTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsResponseStatus :: Lens.Lens' RunTaskResponse Lude.Int
rtrsResponseStatus = Lens.lens (responseStatus :: RunTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RunTaskResponse)
{-# DEPRECATED rtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
