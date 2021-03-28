{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.ECS.RunTask
    (
    -- * Creating a request
      RunTask (..)
    , mkRunTask
    -- ** Request lenses
    , rtTaskDefinition
    , rtCapacityProviderStrategy
    , rtCluster
    , rtCount
    , rtEnableECSManagedTags
    , rtGroup
    , rtLaunchType
    , rtNetworkConfiguration
    , rtOverrides
    , rtPlacementConstraints
    , rtPlacementStrategy
    , rtPlatformVersion
    , rtPropagateTags
    , rtReferenceId
    , rtStartedBy
    , rtTags

    -- * Destructuring the response
    , RunTaskResponse (..)
    , mkRunTaskResponse
    -- ** Response lenses
    , rtrrsFailures
    , rtrrsTasks
    , rtrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRunTask' smart constructor.
data RunTask = RunTask'
  { taskDefinition :: Core.Text
    -- ^ The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
  , capacityProviderStrategy :: Core.Maybe [Types.CapacityProviderStrategyItem]
    -- ^ The capacity provider strategy to use for the task.
--
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster on which to run your task. If you do not specify a cluster, the default cluster is assumed.
  , count :: Core.Maybe Core.Int
    -- ^ The number of instantiations of the specified task to place on your cluster. You can specify up to 10 tasks per call.
  , enableECSManagedTags :: Core.Maybe Core.Bool
    -- ^ Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
  , group :: Core.Maybe Core.Text
    -- ^ The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
  , launchType :: Core.Maybe Types.LaunchType
    -- ^ The launch type on which to run your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
  , networkConfiguration :: Core.Maybe Types.NetworkConfiguration
    -- ^ The network configuration for the task. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own elastic network interface, and it is not supported for other network modes. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
  , overrides :: Core.Maybe Types.TaskOverride
    -- ^ A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
  , placementConstraints :: Core.Maybe [Types.PlacementConstraint]
    -- ^ An array of placement constraint objects to use for the task. You can specify up to 10 constraints per task (including constraints in the task definition and those specified at runtime).
  , placementStrategy :: Core.Maybe [Types.PlacementStrategy]
    -- ^ The placement strategy objects to use for the task. You can specify a maximum of five strategy rules per task.
  , platformVersion :: Core.Maybe Core.Text
    -- ^ The platform version the task should run. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
  , propagateTags :: Core.Maybe Types.PropagateTags
    -- ^ Specifies whether to propagate the tags from the task definition to the task. If no value is specified, the tags are not propagated. Tags can only be propagated to the task during task creation. To add tags to a task after task creation, use the 'TagResource' API action.
  , referenceId :: Core.Maybe Core.Text
    -- ^ The reference ID to use for the task.
  , startedBy :: Core.Maybe Core.Text
    -- ^ An optional tag specified when a task is started. For example, if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The metadata that you apply to the task to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RunTask' value with any optional fields omitted.
mkRunTask
    :: Core.Text -- ^ 'taskDefinition'
    -> RunTask
mkRunTask taskDefinition
  = RunTask'{taskDefinition, capacityProviderStrategy = Core.Nothing,
             cluster = Core.Nothing, count = Core.Nothing,
             enableECSManagedTags = Core.Nothing, group = Core.Nothing,
             launchType = Core.Nothing, networkConfiguration = Core.Nothing,
             overrides = Core.Nothing, placementConstraints = Core.Nothing,
             placementStrategy = Core.Nothing, platformVersion = Core.Nothing,
             propagateTags = Core.Nothing, referenceId = Core.Nothing,
             startedBy = Core.Nothing, tags = Core.Nothing}

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to run. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTaskDefinition :: Lens.Lens' RunTask Core.Text
rtTaskDefinition = Lens.field @"taskDefinition"
{-# INLINEABLE rtTaskDefinition #-}
{-# DEPRECATED taskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead"  #-}

-- | The capacity provider strategy to use for the task.
--
-- A capacity provider strategy consists of one or more capacity providers along with the @base@ and @weight@ to assign to them. A capacity provider must be associated with the cluster to be used in a capacity provider strategy. The 'PutClusterCapacityProviders' API is used to associate a capacity provider with a cluster. Only capacity providers with an @ACTIVE@ or @UPDATING@ status can be used.
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter must be omitted. If no @capacityProviderStrategy@ or @launchType@ is specified, the @defaultCapacityProviderStrategy@ for the cluster is used.
-- If specifying a capacity provider that uses an Auto Scaling group, the capacity provider must already be created. New capacity providers can be created with the 'CreateCapacityProvider' API operation.
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers are available to all accounts and only need to be associated with a cluster to be used.
-- The 'PutClusterCapacityProviders' API operation is used to update the list of available capacity providers for a cluster after the cluster is created.
--
-- /Note:/ Consider using 'capacityProviderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCapacityProviderStrategy :: Lens.Lens' RunTask (Core.Maybe [Types.CapacityProviderStrategyItem])
rtCapacityProviderStrategy = Lens.field @"capacityProviderStrategy"
{-# INLINEABLE rtCapacityProviderStrategy #-}
{-# DEPRECATED capacityProviderStrategy "Use generic-lens or generic-optics with 'capacityProviderStrategy' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster on which to run your task. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCluster :: Lens.Lens' RunTask (Core.Maybe Core.Text)
rtCluster = Lens.field @"cluster"
{-# INLINEABLE rtCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The number of instantiations of the specified task to place on your cluster. You can specify up to 10 tasks per call.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCount :: Lens.Lens' RunTask (Core.Maybe Core.Int)
rtCount = Lens.field @"count"
{-# INLINEABLE rtCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'enableECSManagedTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtEnableECSManagedTags :: Lens.Lens' RunTask (Core.Maybe Core.Bool)
rtEnableECSManagedTags = Lens.field @"enableECSManagedTags"
{-# INLINEABLE rtEnableECSManagedTags #-}
{-# DEPRECATED enableECSManagedTags "Use generic-lens or generic-optics with 'enableECSManagedTags' instead"  #-}

-- | The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtGroup :: Lens.Lens' RunTask (Core.Maybe Core.Text)
rtGroup = Lens.field @"group"
{-# INLINEABLE rtGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The launch type on which to run your task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter must be omitted.
--
-- /Note:/ Consider using 'launchType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtLaunchType :: Lens.Lens' RunTask (Core.Maybe Types.LaunchType)
rtLaunchType = Lens.field @"launchType"
{-# INLINEABLE rtLaunchType #-}
{-# DEPRECATED launchType "Use generic-lens or generic-optics with 'launchType' instead"  #-}

-- | The network configuration for the task. This parameter is required for task definitions that use the @awsvpc@ network mode to receive their own elastic network interface, and it is not supported for other network modes. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtNetworkConfiguration :: Lens.Lens' RunTask (Core.Maybe Types.NetworkConfiguration)
rtNetworkConfiguration = Lens.field @"networkConfiguration"
{-# INLINEABLE rtNetworkConfiguration #-}
{-# DEPRECATED networkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead"  #-}

-- | A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtOverrides :: Lens.Lens' RunTask (Core.Maybe Types.TaskOverride)
rtOverrides = Lens.field @"overrides"
{-# INLINEABLE rtOverrides #-}
{-# DEPRECATED overrides "Use generic-lens or generic-optics with 'overrides' instead"  #-}

-- | An array of placement constraint objects to use for the task. You can specify up to 10 constraints per task (including constraints in the task definition and those specified at runtime).
--
-- /Note:/ Consider using 'placementConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPlacementConstraints :: Lens.Lens' RunTask (Core.Maybe [Types.PlacementConstraint])
rtPlacementConstraints = Lens.field @"placementConstraints"
{-# INLINEABLE rtPlacementConstraints #-}
{-# DEPRECATED placementConstraints "Use generic-lens or generic-optics with 'placementConstraints' instead"  #-}

-- | The placement strategy objects to use for the task. You can specify a maximum of five strategy rules per task.
--
-- /Note:/ Consider using 'placementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPlacementStrategy :: Lens.Lens' RunTask (Core.Maybe [Types.PlacementStrategy])
rtPlacementStrategy = Lens.field @"placementStrategy"
{-# INLINEABLE rtPlacementStrategy #-}
{-# DEPRECATED placementStrategy "Use generic-lens or generic-optics with 'placementStrategy' instead"  #-}

-- | The platform version the task should run. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPlatformVersion :: Lens.Lens' RunTask (Core.Maybe Core.Text)
rtPlatformVersion = Lens.field @"platformVersion"
{-# INLINEABLE rtPlatformVersion #-}
{-# DEPRECATED platformVersion "Use generic-lens or generic-optics with 'platformVersion' instead"  #-}

-- | Specifies whether to propagate the tags from the task definition to the task. If no value is specified, the tags are not propagated. Tags can only be propagated to the task during task creation. To add tags to a task after task creation, use the 'TagResource' API action.
--
-- /Note:/ Consider using 'propagateTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtPropagateTags :: Lens.Lens' RunTask (Core.Maybe Types.PropagateTags)
rtPropagateTags = Lens.field @"propagateTags"
{-# INLINEABLE rtPropagateTags #-}
{-# DEPRECATED propagateTags "Use generic-lens or generic-optics with 'propagateTags' instead"  #-}

-- | The reference ID to use for the task.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtReferenceId :: Lens.Lens' RunTask (Core.Maybe Core.Text)
rtReferenceId = Lens.field @"referenceId"
{-# INLINEABLE rtReferenceId #-}
{-# DEPRECATED referenceId "Use generic-lens or generic-optics with 'referenceId' instead"  #-}

-- | An optional tag specified when a task is started. For example, if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- /Note:/ Consider using 'startedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtStartedBy :: Lens.Lens' RunTask (Core.Maybe Core.Text)
rtStartedBy = Lens.field @"startedBy"
{-# INLINEABLE rtStartedBy #-}
{-# DEPRECATED startedBy "Use generic-lens or generic-optics with 'startedBy' instead"  #-}

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
rtTags :: Lens.Lens' RunTask (Core.Maybe [Types.Tag])
rtTags = Lens.field @"tags"
{-# INLINEABLE rtTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery RunTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RunTask where
        toHeaders RunTask{..}
          = Core.pure
              ("X-Amz-Target", "AmazonEC2ContainerServiceV20141113.RunTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RunTask where
        toJSON RunTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("taskDefinition" Core..= taskDefinition),
                  ("capacityProviderStrategy" Core..=) Core.<$>
                    capacityProviderStrategy,
                  ("cluster" Core..=) Core.<$> cluster,
                  ("count" Core..=) Core.<$> count,
                  ("enableECSManagedTags" Core..=) Core.<$> enableECSManagedTags,
                  ("group" Core..=) Core.<$> group,
                  ("launchType" Core..=) Core.<$> launchType,
                  ("networkConfiguration" Core..=) Core.<$> networkConfiguration,
                  ("overrides" Core..=) Core.<$> overrides,
                  ("placementConstraints" Core..=) Core.<$> placementConstraints,
                  ("placementStrategy" Core..=) Core.<$> placementStrategy,
                  ("platformVersion" Core..=) Core.<$> platformVersion,
                  ("propagateTags" Core..=) Core.<$> propagateTags,
                  ("referenceId" Core..=) Core.<$> referenceId,
                  ("startedBy" Core..=) Core.<$> startedBy,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest RunTask where
        type Rs RunTask = RunTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RunTaskResponse' Core.<$>
                   (x Core..:? "failures") Core.<*> x Core..:? "tasks" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRunTaskResponse' smart constructor.
data RunTaskResponse = RunTaskResponse'
  { failures :: Core.Maybe [Types.Failure]
    -- ^ Any failures associated with the call.
  , tasks :: Core.Maybe [Types.Task]
    -- ^ A full description of the tasks that were run. The tasks that were successfully placed on your cluster are described here.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RunTaskResponse' value with any optional fields omitted.
mkRunTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RunTaskResponse
mkRunTaskResponse responseStatus
  = RunTaskResponse'{failures = Core.Nothing, tasks = Core.Nothing,
                     responseStatus}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsFailures :: Lens.Lens' RunTaskResponse (Core.Maybe [Types.Failure])
rtrrsFailures = Lens.field @"failures"
{-# INLINEABLE rtrrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | A full description of the tasks that were run. The tasks that were successfully placed on your cluster are described here.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsTasks :: Lens.Lens' RunTaskResponse (Core.Maybe [Types.Task])
rtrrsTasks = Lens.field @"tasks"
{-# INLINEABLE rtrrsTasks #-}
{-# DEPRECATED tasks "Use generic-lens or generic-optics with 'tasks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResponseStatus :: Lens.Lens' RunTaskResponse Core.Int
rtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
