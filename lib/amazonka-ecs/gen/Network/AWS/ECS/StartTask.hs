{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- Alternatively, you can use 'RunTask' to place tasks for you. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html Scheduling Tasks> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.StartTask
  ( -- * Creating a request
    StartTask (..),
    mkStartTask,

    -- ** Request lenses
    sOverrides,
    sGroup,
    sCluster,
    sPropagateTags,
    sEnableECSManagedTags,
    sReferenceId,
    sStartedBy,
    sContainerInstances,
    sTaskDefinition,
    sNetworkConfiguration,
    sTags,

    -- * Destructuring the response
    StartTaskResponse (..),
    mkStartTaskResponse,

    -- ** Response lenses
    strsFailures,
    strsTasks,
    strsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartTask' smart constructor.
data StartTask = StartTask'
  { -- | A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
    overrides :: Lude.Maybe TaskOverride,
    -- | The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
    group :: Lude.Maybe Lude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster on which to start your task. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Lude.Maybe Lude.Text,
    -- | Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
    propagateTags :: Lude.Maybe PropagateTags,
    -- | Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
    enableECSManagedTags :: Lude.Maybe Lude.Bool,
    -- | The reference ID to use for the task.
    referenceId :: Lude.Maybe Lude.Text,
    -- | An optional tag specified when a task is started. For example, if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
    --
    -- If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
    startedBy :: Lude.Maybe Lude.Text,
    -- | The container instance IDs or full ARN entries for the container instances on which you would like to place your task. You can specify up to 10 container instances.
    containerInstances :: [Lude.Text],
    -- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to start. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
    taskDefinition :: Lude.Text,
    -- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
    networkConfiguration :: Lude.Maybe NetworkConfiguration,
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
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTask' with the minimum fields required to make a request.
--
-- * 'overrides' - A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
-- * 'group' - The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster on which to start your task. If you do not specify a cluster, the default cluster is assumed.
-- * 'propagateTags' - Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
-- * 'enableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'referenceId' - The reference ID to use for the task.
-- * 'startedBy' - An optional tag specified when a task is started. For example, if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
-- * 'containerInstances' - The container instance IDs or full ARN entries for the container instances on which you would like to place your task. You can specify up to 10 container instances.
-- * 'taskDefinition' - The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to start. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
-- * 'networkConfiguration' - The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
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
mkStartTask ::
  -- | 'taskDefinition'
  Lude.Text ->
  StartTask
mkStartTask pTaskDefinition_ =
  StartTask'
    { overrides = Lude.Nothing,
      group = Lude.Nothing,
      cluster = Lude.Nothing,
      propagateTags = Lude.Nothing,
      enableECSManagedTags = Lude.Nothing,
      referenceId = Lude.Nothing,
      startedBy = Lude.Nothing,
      containerInstances = Lude.mempty,
      taskDefinition = pTaskDefinition_,
      networkConfiguration = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A list of container overrides in JSON format that specify the name of a container in the specified task definition and the overrides it should receive. You can override the default command for a container (that is specified in the task definition or Docker image) with a @command@ override. You can also override existing environment variables (that are specified in the task definition or Docker image) on a container or add new environment variables to it with an @environment@ override.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOverrides :: Lens.Lens' StartTask (Lude.Maybe TaskOverride)
sOverrides = Lens.lens (overrides :: StartTask -> Lude.Maybe TaskOverride) (\s a -> s {overrides = a} :: StartTask)
{-# DEPRECATED sOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

-- | The name of the task group to associate with the task. The default value is the family name of the task definition (for example, family:my-family-name).
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sGroup :: Lens.Lens' StartTask (Lude.Maybe Lude.Text)
sGroup = Lens.lens (group :: StartTask -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: StartTask)
{-# DEPRECATED sGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster on which to start your task. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCluster :: Lens.Lens' StartTask (Lude.Maybe Lude.Text)
sCluster = Lens.lens (cluster :: StartTask -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: StartTask)
{-# DEPRECATED sCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | Specifies whether to propagate the tags from the task definition or the service to the task. If no value is specified, the tags are not propagated.
--
-- /Note:/ Consider using 'propagateTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPropagateTags :: Lens.Lens' StartTask (Lude.Maybe PropagateTags)
sPropagateTags = Lens.lens (propagateTags :: StartTask -> Lude.Maybe PropagateTags) (\s a -> s {propagateTags = a} :: StartTask)
{-# DEPRECATED sPropagateTags "Use generic-lens or generic-optics with 'propagateTags' instead." #-}

-- | Specifies whether to enable Amazon ECS managed tags for the task. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'enableECSManagedTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEnableECSManagedTags :: Lens.Lens' StartTask (Lude.Maybe Lude.Bool)
sEnableECSManagedTags = Lens.lens (enableECSManagedTags :: StartTask -> Lude.Maybe Lude.Bool) (\s a -> s {enableECSManagedTags = a} :: StartTask)
{-# DEPRECATED sEnableECSManagedTags "Use generic-lens or generic-optics with 'enableECSManagedTags' instead." #-}

-- | The reference ID to use for the task.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReferenceId :: Lens.Lens' StartTask (Lude.Maybe Lude.Text)
sReferenceId = Lens.lens (referenceId :: StartTask -> Lude.Maybe Lude.Text) (\s a -> s {referenceId = a} :: StartTask)
{-# DEPRECATED sReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

-- | An optional tag specified when a task is started. For example, if you automatically trigger a task to run a batch process job, you could apply a unique identifier for that job to your task with the @startedBy@ parameter. You can then identify which tasks belong to that job by filtering the results of a 'ListTasks' call with the @startedBy@ value. Up to 36 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@ parameter contains the deployment ID of the service that starts it.
--
-- /Note:/ Consider using 'startedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStartedBy :: Lens.Lens' StartTask (Lude.Maybe Lude.Text)
sStartedBy = Lens.lens (startedBy :: StartTask -> Lude.Maybe Lude.Text) (\s a -> s {startedBy = a} :: StartTask)
{-# DEPRECATED sStartedBy "Use generic-lens or generic-optics with 'startedBy' instead." #-}

-- | The container instance IDs or full ARN entries for the container instances on which you would like to place your task. You can specify up to 10 container instances.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sContainerInstances :: Lens.Lens' StartTask [Lude.Text]
sContainerInstances = Lens.lens (containerInstances :: StartTask -> [Lude.Text]) (\s a -> s {containerInstances = a} :: StartTask)
{-# DEPRECATED sContainerInstances "Use generic-lens or generic-optics with 'containerInstances' instead." #-}

-- | The @family@ and @revision@ (@family:revision@ ) or full ARN of the task definition to start. If a @revision@ is not specified, the latest @ACTIVE@ revision is used.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTaskDefinition :: Lens.Lens' StartTask Lude.Text
sTaskDefinition = Lens.lens (taskDefinition :: StartTask -> Lude.Text) (\s a -> s {taskDefinition = a} :: StartTask)
{-# DEPRECATED sTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
--
-- /Note:/ Consider using 'networkConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNetworkConfiguration :: Lens.Lens' StartTask (Lude.Maybe NetworkConfiguration)
sNetworkConfiguration = Lens.lens (networkConfiguration :: StartTask -> Lude.Maybe NetworkConfiguration) (\s a -> s {networkConfiguration = a} :: StartTask)
{-# DEPRECATED sNetworkConfiguration "Use generic-lens or generic-optics with 'networkConfiguration' instead." #-}

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
sTags :: Lens.Lens' StartTask (Lude.Maybe [Tag])
sTags = Lens.lens (tags :: StartTask -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: StartTask)
{-# DEPRECATED sTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest StartTask where
  type Rs StartTask = StartTaskResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartTaskResponse'
            Lude.<$> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "tasks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.StartTask" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartTask where
  toJSON StartTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("overrides" Lude..=) Lude.<$> overrides,
            ("group" Lude..=) Lude.<$> group,
            ("cluster" Lude..=) Lude.<$> cluster,
            ("propagateTags" Lude..=) Lude.<$> propagateTags,
            ("enableECSManagedTags" Lude..=) Lude.<$> enableECSManagedTags,
            ("referenceId" Lude..=) Lude.<$> referenceId,
            ("startedBy" Lude..=) Lude.<$> startedBy,
            Lude.Just ("containerInstances" Lude..= containerInstances),
            Lude.Just ("taskDefinition" Lude..= taskDefinition),
            ("networkConfiguration" Lude..=) Lude.<$> networkConfiguration,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath StartTask where
  toPath = Lude.const "/"

instance Lude.ToQuery StartTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartTaskResponse' smart constructor.
data StartTaskResponse = StartTaskResponse'
  { -- | Any failures associated with the call.
    failures :: Lude.Maybe [Failure],
    -- | A full description of the tasks that were started. Each task that was successfully placed on your container instances is described.
    tasks :: Lude.Maybe [Task],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTaskResponse' with the minimum fields required to make a request.
--
-- * 'failures' - Any failures associated with the call.
-- * 'tasks' - A full description of the tasks that were started. Each task that was successfully placed on your container instances is described.
-- * 'responseStatus' - The response status code.
mkStartTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartTaskResponse
mkStartTaskResponse pResponseStatus_ =
  StartTaskResponse'
    { failures = Lude.Nothing,
      tasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strsFailures :: Lens.Lens' StartTaskResponse (Lude.Maybe [Failure])
strsFailures = Lens.lens (failures :: StartTaskResponse -> Lude.Maybe [Failure]) (\s a -> s {failures = a} :: StartTaskResponse)
{-# DEPRECATED strsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | A full description of the tasks that were started. Each task that was successfully placed on your container instances is described.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strsTasks :: Lens.Lens' StartTaskResponse (Lude.Maybe [Task])
strsTasks = Lens.lens (tasks :: StartTaskResponse -> Lude.Maybe [Task]) (\s a -> s {tasks = a} :: StartTaskResponse)
{-# DEPRECATED strsTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strsResponseStatus :: Lens.Lens' StartTaskResponse Lude.Int
strsResponseStatus = Lens.lens (responseStatus :: StartTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartTaskResponse)
{-# DEPRECATED strsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
