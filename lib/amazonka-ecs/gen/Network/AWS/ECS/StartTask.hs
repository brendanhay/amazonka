{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
-- Alternatively, you can use 'RunTask' to place tasks for you. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html Scheduling Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.StartTask
    (
    -- * Creating a request
      StartTask (..)
    , mkStartTask
    -- ** Request lenses
    , sContainerInstances
    , sTaskDefinition
    , sCluster
    , sEnableECSManagedTags
    , sGroup
    , sNetworkConfiguration
    , sOverrides
    , sPropagateTags
    , sReferenceId
    , sStartedBy
    , sTags

    -- * Destructuring the response
    , StartTaskResponse (..)
    , mkStartTaskResponse
    -- ** Response lenses
    , strrsFailures
    , strrsTasks
    , strrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartTask' smart constructor.
data StartTask = StartTask'
  { containerInstances :: [Core.Text]
    -- ^ The container instance IDs or full ARN entries for the container instances on which you would like to place your task. You can specify up to 10 container instances.
  , taskDefinition :: Core.Text
    -- ^ The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to start. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster on which to start your task. If you do not specify a cluster, the default cluster is assumed.
  , enableECSManagedTags :: Core.Maybe Core.Bool
    -- ^ Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
  , group :: Core.Maybe Core.Text
    -- ^ The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
  , networkConfiguration :: Core.Maybe Types.NetworkConfiguration
    -- ^ The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
  , overrides :: Core.Maybe Types.TaskOverride
    -- ^ A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
  , propagateTags :: Core.Maybe Types.PropagateTags
    -- ^ Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
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

-- | Creates a 'StartTask' value with any optional fields omitted.
mkStartTask
    :: Core.Text -- ^ 'taskDefinition'
    -> StartTask
mkStartTask taskDefinition
  = StartTask'{containerInstances = Core.mempty, taskDefinition,
               cluster = Core.Nothing, enableECSManagedTags = Core.Nothing,
               group = Core.Nothing, networkConfiguration = Core.Nothing,
               overrides = Core.Nothing, propagateTags = Core.Nothing,
               referenceId = Core.Nothing, startedBy = Core.Nothing,
               tags = Core.Nothing}

-- | The container instance IDs or full ARN entries for the container instances on which you would like to place your task. You can specify up to 10 container instances.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sContainerInstances :: Lens.Lens' StartTask [Core.Text]
sContainerInstances = Lens.field @"containerInstances"
{-# INLINEABLE sContainerInstances #-}
{-# DEPRECATED containerInstances "Use generic-lens or generic-optics with 'containerInstances' instead"  #-}

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to start. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTaskDefinition :: Lens.Lens' StartTask Core.Text
sTaskDefinition = Lens.field @"taskDefinition"
{-# INLINEABLE sTaskDefinition #-}
{-# DEPRECATED taskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster on which to start your task. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCluster :: Lens.Lens' StartTask (Core.Maybe Core.Text)
sCluster = Lens.field @"cluster"
{-# INLINEABLE sCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'enableECSManagedTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEnableECSManagedTags :: Lens.Lens' StartTask (Core.Maybe Core.Bool)
sEnableECSManagedTags = Lens.field @"enableECSManagedTags"
{-# INLINEABLE sEnableECSManagedTags #-}
{-# DEPRECATED enableECSManagedTags "Use generic-lens or generic-optics with 'enableECSManagedTags' instead"  #-}

-- | The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sGroup :: Lens.Lens' StartTask (Core.Maybe Core.Text)
sGroup = Lens.field @"group"
{-# INLINEABLE sGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNetworkConfiguration :: Lens.Lens' StartTask (Core.Maybe Types.NetworkConfiguration)
sNetworkConfiguration = Lens.field @"networkConfiguration"
{-# INLINEABLE sNetworkConfiguration #-}
{-# DEPRECATED networkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead"  #-}

-- | A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOverrides :: Lens.Lens' StartTask (Core.Maybe Types.TaskOverride)
sOverrides = Lens.field @"overrides"
{-# INLINEABLE sOverrides #-}
{-# DEPRECATED overrides "Use generic-lens or generic-optics with 'overrides' instead"  #-}

-- | Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
--
-- /Note:/ Consider using 'propagateTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPropagateTags :: Lens.Lens' StartTask (Core.Maybe Types.PropagateTags)
sPropagateTags = Lens.field @"propagateTags"
{-# INLINEABLE sPropagateTags #-}
{-# DEPRECATED propagateTags "Use generic-lens or generic-optics with 'propagateTags' instead"  #-}

-- | The reference ID to use for the task.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReferenceId :: Lens.Lens' StartTask (Core.Maybe Core.Text)
sReferenceId = Lens.field @"referenceId"
{-# INLINEABLE sReferenceId #-}
{-# DEPRECATED referenceId "Use generic-lens or generic-optics with 'referenceId' instead"  #-}

-- | An optional tag specified when a task is started. For example, if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- /Note:/ Consider using 'startedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartedBy :: Lens.Lens' StartTask (Core.Maybe Core.Text)
sStartedBy = Lens.field @"startedBy"
{-# INLINEABLE sStartedBy #-}
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
sTags :: Lens.Lens' StartTask (Core.Maybe [Types.Tag])
sTags = Lens.field @"tags"
{-# INLINEABLE sTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery StartTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartTask where
        toHeaders StartTask{..}
          = Core.pure
              ("X-Amz-Target", "AmazonEC2ContainerServiceV20141113.StartTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartTask where
        toJSON StartTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("containerInstances" Core..= containerInstances),
                  Core.Just ("taskDefinition" Core..= taskDefinition),
                  ("cluster" Core..=) Core.<$> cluster,
                  ("enableECSManagedTags" Core..=) Core.<$> enableECSManagedTags,
                  ("group" Core..=) Core.<$> group,
                  ("networkConfiguration" Core..=) Core.<$> networkConfiguration,
                  ("overrides" Core..=) Core.<$> overrides,
                  ("propagateTags" Core..=) Core.<$> propagateTags,
                  ("referenceId" Core..=) Core.<$> referenceId,
                  ("startedBy" Core..=) Core.<$> startedBy,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest StartTask where
        type Rs StartTask = StartTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartTaskResponse' Core.<$>
                   (x Core..:? "failures") Core.<*> x Core..:? "tasks" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartTaskResponse' smart constructor.
data StartTaskResponse = StartTaskResponse'
  { failures :: Core.Maybe [Types.Failure]
    -- ^ Any failures associated with the call.
  , tasks :: Core.Maybe [Types.Task]
    -- ^ A full description of the tasks that were started. Each task that was successfully placed on your container instances is described.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartTaskResponse' value with any optional fields omitted.
mkStartTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartTaskResponse
mkStartTaskResponse responseStatus
  = StartTaskResponse'{failures = Core.Nothing, tasks = Core.Nothing,
                       responseStatus}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strrsFailures :: Lens.Lens' StartTaskResponse (Core.Maybe [Types.Failure])
strrsFailures = Lens.field @"failures"
{-# INLINEABLE strrsFailures #-}
{-# DEPRECATED failures "Use generic-lens or generic-optics with 'failures' instead"  #-}

-- | A full description of the tasks that were started. Each task that was successfully placed on your container instances is described.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strrsTasks :: Lens.Lens' StartTaskResponse (Core.Maybe [Types.Task])
strrsTasks = Lens.field @"tasks"
{-# INLINEABLE strrsTasks #-}
{-# DEPRECATED tasks "Use generic-lens or generic-optics with 'tasks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strrsResponseStatus :: Lens.Lens' StartTaskResponse Core.Int
strrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE strrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
