{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.StartTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new task from the specified task definition on the specified
-- container instance or instances.
--
-- Alternatively, you can use RunTask to place tasks for you. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html Scheduling Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Network.AWS.ECS.StartTask
  ( -- * Creating a Request
    StartTask (..),
    newStartTask,

    -- * Request Lenses
    startTask_networkConfiguration,
    startTask_referenceId,
    startTask_enableECSManagedTags,
    startTask_startedBy,
    startTask_group,
    startTask_overrides,
    startTask_tags,
    startTask_cluster,
    startTask_propagateTags,
    startTask_containerInstances,
    startTask_taskDefinition,

    -- * Destructuring the Response
    StartTaskResponse (..),
    newStartTaskResponse,

    -- * Response Lenses
    startTaskResponse_tasks,
    startTaskResponse_failures,
    startTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartTask' smart constructor.
data StartTask = StartTask'
  { -- | The VPC subnet and security group configuration for tasks that receive
    -- their own elastic network interface by using the @awsvpc@ networking
    -- mode.
    networkConfiguration :: Core.Maybe NetworkConfiguration,
    -- | The reference ID to use for the task.
    referenceId :: Core.Maybe Core.Text,
    -- | Specifies whether to enable Amazon ECS managed tags for the task. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    enableECSManagedTags :: Core.Maybe Core.Bool,
    -- | An optional tag specified when a task is started. For example, if you
    -- automatically trigger a task to run a batch process job, you could apply
    -- a unique identifier for that job to your task with the @startedBy@
    -- parameter. You can then identify which tasks belong to that job by
    -- filtering the results of a ListTasks call with the @startedBy@ value. Up
    -- to 36 letters (uppercase and lowercase), numbers, hyphens, and
    -- underscores are allowed.
    --
    -- If a task is started by an Amazon ECS service, then the @startedBy@
    -- parameter contains the deployment ID of the service that starts it.
    startedBy :: Core.Maybe Core.Text,
    -- | The name of the task group to associate with the task. The default value
    -- is the family name of the task definition (for example,
    -- family:my-family-name).
    group' :: Core.Maybe Core.Text,
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
    overrides :: Core.Maybe TaskOverride,
    -- | The metadata that you apply to the task to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case-sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for either keys or values as it is reserved for AWS
    --     use. You cannot edit or delete tag keys or values with this prefix.
    --     Tags with this prefix do not count against your tags per resource
    --     limit.
    tags :: Core.Maybe [Tag],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster on
    -- which to start your task. If you do not specify a cluster, the default
    -- cluster is assumed.
    cluster :: Core.Maybe Core.Text,
    -- | Specifies whether to propagate the tags from the task definition or the
    -- service to the task. If no value is specified, the tags are not
    -- propagated.
    propagateTags :: Core.Maybe PropagateTags,
    -- | The container instance IDs or full ARN entries for the container
    -- instances on which you would like to place your task. You can specify up
    -- to 10 container instances.
    containerInstances :: [Core.Text],
    -- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
    -- definition to start. If a @revision@ is not specified, the latest
    -- @ACTIVE@ revision is used.
    taskDefinition :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfiguration', 'startTask_networkConfiguration' - The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
--
-- 'referenceId', 'startTask_referenceId' - The reference ID to use for the task.
--
-- 'enableECSManagedTags', 'startTask_enableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the task. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'startedBy', 'startTask_startedBy' - An optional tag specified when a task is started. For example, if you
-- automatically trigger a task to run a batch process job, you could apply
-- a unique identifier for that job to your task with the @startedBy@
-- parameter. You can then identify which tasks belong to that job by
-- filtering the results of a ListTasks call with the @startedBy@ value. Up
-- to 36 letters (uppercase and lowercase), numbers, hyphens, and
-- underscores are allowed.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@
-- parameter contains the deployment ID of the service that starts it.
--
-- 'group'', 'startTask_group' - The name of the task group to associate with the task. The default value
-- is the family name of the task definition (for example,
-- family:my-family-name).
--
-- 'overrides', 'startTask_overrides' - A list of container overrides in JSON format that specify the name of a
-- container in the specified task definition and the overrides it should
-- receive. You can override the default command for a container (that is
-- specified in the task definition or Docker image) with a @command@
-- override. You can also override existing environment variables (that are
-- specified in the task definition or Docker image) on a container or add
-- new environment variables to it with an @environment@ override.
--
-- A total of 8192 characters are allowed for overrides. This limit
-- includes the JSON formatting characters of the override structure.
--
-- 'tags', 'startTask_tags' - The metadata that you apply to the task to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
--
-- 'cluster', 'startTask_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster on
-- which to start your task. If you do not specify a cluster, the default
-- cluster is assumed.
--
-- 'propagateTags', 'startTask_propagateTags' - Specifies whether to propagate the tags from the task definition or the
-- service to the task. If no value is specified, the tags are not
-- propagated.
--
-- 'containerInstances', 'startTask_containerInstances' - The container instance IDs or full ARN entries for the container
-- instances on which you would like to place your task. You can specify up
-- to 10 container instances.
--
-- 'taskDefinition', 'startTask_taskDefinition' - The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to start. If a @revision@ is not specified, the latest
-- @ACTIVE@ revision is used.
newStartTask ::
  -- | 'taskDefinition'
  Core.Text ->
  StartTask
newStartTask pTaskDefinition_ =
  StartTask'
    { networkConfiguration = Core.Nothing,
      referenceId = Core.Nothing,
      enableECSManagedTags = Core.Nothing,
      startedBy = Core.Nothing,
      group' = Core.Nothing,
      overrides = Core.Nothing,
      tags = Core.Nothing,
      cluster = Core.Nothing,
      propagateTags = Core.Nothing,
      containerInstances = Core.mempty,
      taskDefinition = pTaskDefinition_
    }

-- | The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
startTask_networkConfiguration :: Lens.Lens' StartTask (Core.Maybe NetworkConfiguration)
startTask_networkConfiguration = Lens.lens (\StartTask' {networkConfiguration} -> networkConfiguration) (\s@StartTask' {} a -> s {networkConfiguration = a} :: StartTask)

-- | The reference ID to use for the task.
startTask_referenceId :: Lens.Lens' StartTask (Core.Maybe Core.Text)
startTask_referenceId = Lens.lens (\StartTask' {referenceId} -> referenceId) (\s@StartTask' {} a -> s {referenceId = a} :: StartTask)

-- | Specifies whether to enable Amazon ECS managed tags for the task. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
startTask_enableECSManagedTags :: Lens.Lens' StartTask (Core.Maybe Core.Bool)
startTask_enableECSManagedTags = Lens.lens (\StartTask' {enableECSManagedTags} -> enableECSManagedTags) (\s@StartTask' {} a -> s {enableECSManagedTags = a} :: StartTask)

-- | An optional tag specified when a task is started. For example, if you
-- automatically trigger a task to run a batch process job, you could apply
-- a unique identifier for that job to your task with the @startedBy@
-- parameter. You can then identify which tasks belong to that job by
-- filtering the results of a ListTasks call with the @startedBy@ value. Up
-- to 36 letters (uppercase and lowercase), numbers, hyphens, and
-- underscores are allowed.
--
-- If a task is started by an Amazon ECS service, then the @startedBy@
-- parameter contains the deployment ID of the service that starts it.
startTask_startedBy :: Lens.Lens' StartTask (Core.Maybe Core.Text)
startTask_startedBy = Lens.lens (\StartTask' {startedBy} -> startedBy) (\s@StartTask' {} a -> s {startedBy = a} :: StartTask)

-- | The name of the task group to associate with the task. The default value
-- is the family name of the task definition (for example,
-- family:my-family-name).
startTask_group :: Lens.Lens' StartTask (Core.Maybe Core.Text)
startTask_group = Lens.lens (\StartTask' {group'} -> group') (\s@StartTask' {} a -> s {group' = a} :: StartTask)

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
startTask_overrides :: Lens.Lens' StartTask (Core.Maybe TaskOverride)
startTask_overrides = Lens.lens (\StartTask' {overrides} -> overrides) (\s@StartTask' {} a -> s {overrides = a} :: StartTask)

-- | The metadata that you apply to the task to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
startTask_tags :: Lens.Lens' StartTask (Core.Maybe [Tag])
startTask_tags = Lens.lens (\StartTask' {tags} -> tags) (\s@StartTask' {} a -> s {tags = a} :: StartTask) Core.. Lens.mapping Lens._Coerce

-- | The short name or full Amazon Resource Name (ARN) of the cluster on
-- which to start your task. If you do not specify a cluster, the default
-- cluster is assumed.
startTask_cluster :: Lens.Lens' StartTask (Core.Maybe Core.Text)
startTask_cluster = Lens.lens (\StartTask' {cluster} -> cluster) (\s@StartTask' {} a -> s {cluster = a} :: StartTask)

-- | Specifies whether to propagate the tags from the task definition or the
-- service to the task. If no value is specified, the tags are not
-- propagated.
startTask_propagateTags :: Lens.Lens' StartTask (Core.Maybe PropagateTags)
startTask_propagateTags = Lens.lens (\StartTask' {propagateTags} -> propagateTags) (\s@StartTask' {} a -> s {propagateTags = a} :: StartTask)

-- | The container instance IDs or full ARN entries for the container
-- instances on which you would like to place your task. You can specify up
-- to 10 container instances.
startTask_containerInstances :: Lens.Lens' StartTask [Core.Text]
startTask_containerInstances = Lens.lens (\StartTask' {containerInstances} -> containerInstances) (\s@StartTask' {} a -> s {containerInstances = a} :: StartTask) Core.. Lens._Coerce

-- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to start. If a @revision@ is not specified, the latest
-- @ACTIVE@ revision is used.
startTask_taskDefinition :: Lens.Lens' StartTask Core.Text
startTask_taskDefinition = Lens.lens (\StartTask' {taskDefinition} -> taskDefinition) (\s@StartTask' {} a -> s {taskDefinition = a} :: StartTask)

instance Core.AWSRequest StartTask where
  type AWSResponse StartTask = StartTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTaskResponse'
            Core.<$> (x Core..?> "tasks" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failures" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartTask

instance Core.NFData StartTask

instance Core.ToHeaders StartTask where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.StartTask" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartTask where
  toJSON StartTask' {..} =
    Core.object
      ( Core.catMaybes
          [ ("networkConfiguration" Core..=)
              Core.<$> networkConfiguration,
            ("referenceId" Core..=) Core.<$> referenceId,
            ("enableECSManagedTags" Core..=)
              Core.<$> enableECSManagedTags,
            ("startedBy" Core..=) Core.<$> startedBy,
            ("group" Core..=) Core.<$> group',
            ("overrides" Core..=) Core.<$> overrides,
            ("tags" Core..=) Core.<$> tags,
            ("cluster" Core..=) Core.<$> cluster,
            ("propagateTags" Core..=) Core.<$> propagateTags,
            Core.Just
              ("containerInstances" Core..= containerInstances),
            Core.Just ("taskDefinition" Core..= taskDefinition)
          ]
      )

instance Core.ToPath StartTask where
  toPath = Core.const "/"

instance Core.ToQuery StartTask where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartTaskResponse' smart constructor.
data StartTaskResponse = StartTaskResponse'
  { -- | A full description of the tasks that were started. Each task that was
    -- successfully placed on your container instances is described.
    tasks :: Core.Maybe [Task],
    -- | Any failures associated with the call.
    failures :: Core.Maybe [Failure],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tasks', 'startTaskResponse_tasks' - A full description of the tasks that were started. Each task that was
-- successfully placed on your container instances is described.
--
-- 'failures', 'startTaskResponse_failures' - Any failures associated with the call.
--
-- 'httpStatus', 'startTaskResponse_httpStatus' - The response's http status code.
newStartTaskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartTaskResponse
newStartTaskResponse pHttpStatus_ =
  StartTaskResponse'
    { tasks = Core.Nothing,
      failures = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A full description of the tasks that were started. Each task that was
-- successfully placed on your container instances is described.
startTaskResponse_tasks :: Lens.Lens' StartTaskResponse (Core.Maybe [Task])
startTaskResponse_tasks = Lens.lens (\StartTaskResponse' {tasks} -> tasks) (\s@StartTaskResponse' {} a -> s {tasks = a} :: StartTaskResponse) Core.. Lens.mapping Lens._Coerce

-- | Any failures associated with the call.
startTaskResponse_failures :: Lens.Lens' StartTaskResponse (Core.Maybe [Failure])
startTaskResponse_failures = Lens.lens (\StartTaskResponse' {failures} -> failures) (\s@StartTaskResponse' {} a -> s {failures = a} :: StartTaskResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
startTaskResponse_httpStatus :: Lens.Lens' StartTaskResponse Core.Int
startTaskResponse_httpStatus = Lens.lens (\StartTaskResponse' {httpStatus} -> httpStatus) (\s@StartTaskResponse' {} a -> s {httpStatus = a} :: StartTaskResponse)

instance Core.NFData StartTaskResponse
