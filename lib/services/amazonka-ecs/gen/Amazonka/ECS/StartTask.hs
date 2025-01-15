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
-- Module      : Amazonka.ECS.StartTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.ECS.StartTask
  ( -- * Creating a Request
    StartTask (..),
    newStartTask,

    -- * Request Lenses
    startTask_cluster,
    startTask_enableECSManagedTags,
    startTask_enableExecuteCommand,
    startTask_group,
    startTask_networkConfiguration,
    startTask_overrides,
    startTask_propagateTags,
    startTask_referenceId,
    startTask_startedBy,
    startTask_tags,
    startTask_containerInstances,
    startTask_taskDefinition,

    -- * Destructuring the Response
    StartTaskResponse (..),
    newStartTaskResponse,

    -- * Response Lenses
    startTaskResponse_failures,
    startTaskResponse_tasks,
    startTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartTask' smart constructor.
data StartTask = StartTask'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster where
    -- to start your task. If you do not specify a cluster, the default cluster
    -- is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to use Amazon ECS managed tags for the task. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    enableECSManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | Whether or not the execute command functionality is enabled for the
    -- task. If @true@, this enables execute command functionality on all
    -- containers in the task.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool,
    -- | The name of the task group to associate with the task. The default value
    -- is the family name of the task definition (for example,
    -- family:my-family-name).
    group' :: Prelude.Maybe Prelude.Text,
    -- | The VPC subnet and security group configuration for tasks that receive
    -- their own elastic network interface by using the @awsvpc@ networking
    -- mode.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | A list of container overrides in JSON format that specify the name of a
    -- container in the specified task definition and the overrides it
    -- receives. You can override the default command for a container (that\'s
    -- specified in the task definition or Docker image) with a @command@
    -- override. You can also override existing environment variables (that are
    -- specified in the task definition or Docker image) on a container or add
    -- new environment variables to it with an @environment@ override.
    --
    -- A total of 8192 characters are allowed for overrides. This limit
    -- includes the JSON formatting characters of the override structure.
    overrides :: Prelude.Maybe TaskOverride,
    -- | Specifies whether to propagate the tags from the task definition or the
    -- service to the task. If no value is specified, the tags aren\'t
    -- propagated.
    propagateTags :: Prelude.Maybe PropagateTags,
    -- | The reference ID to use for the task.
    referenceId :: Prelude.Maybe Prelude.Text,
    -- | An optional tag specified when a task is started. For example, if you
    -- automatically trigger a task to run a batch process job, you could apply
    -- a unique identifier for that job to your task with the @startedBy@
    -- parameter. You can then identify which tasks belong to that job by
    -- filtering the results of a ListTasks call with the @startedBy@ value. Up
    -- to 36 letters (uppercase and lowercase), numbers, hyphens (-), and
    -- underscores (_) are allowed.
    --
    -- If a task is started by an Amazon ECS service, the @startedBy@ parameter
    -- contains the deployment ID of the service that starts it.
    startedBy :: Prelude.Maybe Prelude.Text,
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
    --     such as a prefix for either keys or values as it is reserved for
    --     Amazon Web Services use. You cannot edit or delete tag keys or
    --     values with this prefix. Tags with this prefix do not count against
    --     your tags per resource limit.
    tags :: Prelude.Maybe [Tag],
    -- | The container instance IDs or full ARN entries for the container
    -- instances where you would like to place your task. You can specify up to
    -- 10 container instances.
    containerInstances :: [Prelude.Text],
    -- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
    -- definition to start. If a @revision@ isn\'t specified, the latest
    -- @ACTIVE@ revision is used.
    taskDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'startTask_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster where
-- to start your task. If you do not specify a cluster, the default cluster
-- is assumed.
--
-- 'enableECSManagedTags', 'startTask_enableECSManagedTags' - Specifies whether to use Amazon ECS managed tags for the task. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'enableExecuteCommand', 'startTask_enableExecuteCommand' - Whether or not the execute command functionality is enabled for the
-- task. If @true@, this enables execute command functionality on all
-- containers in the task.
--
-- 'group'', 'startTask_group' - The name of the task group to associate with the task. The default value
-- is the family name of the task definition (for example,
-- family:my-family-name).
--
-- 'networkConfiguration', 'startTask_networkConfiguration' - The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
--
-- 'overrides', 'startTask_overrides' - A list of container overrides in JSON format that specify the name of a
-- container in the specified task definition and the overrides it
-- receives. You can override the default command for a container (that\'s
-- specified in the task definition or Docker image) with a @command@
-- override. You can also override existing environment variables (that are
-- specified in the task definition or Docker image) on a container or add
-- new environment variables to it with an @environment@ override.
--
-- A total of 8192 characters are allowed for overrides. This limit
-- includes the JSON formatting characters of the override structure.
--
-- 'propagateTags', 'startTask_propagateTags' - Specifies whether to propagate the tags from the task definition or the
-- service to the task. If no value is specified, the tags aren\'t
-- propagated.
--
-- 'referenceId', 'startTask_referenceId' - The reference ID to use for the task.
--
-- 'startedBy', 'startTask_startedBy' - An optional tag specified when a task is started. For example, if you
-- automatically trigger a task to run a batch process job, you could apply
-- a unique identifier for that job to your task with the @startedBy@
-- parameter. You can then identify which tasks belong to that job by
-- filtering the results of a ListTasks call with the @startedBy@ value. Up
-- to 36 letters (uppercase and lowercase), numbers, hyphens (-), and
-- underscores (_) are allowed.
--
-- If a task is started by an Amazon ECS service, the @startedBy@ parameter
-- contains the deployment ID of the service that starts it.
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
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
--
-- 'containerInstances', 'startTask_containerInstances' - The container instance IDs or full ARN entries for the container
-- instances where you would like to place your task. You can specify up to
-- 10 container instances.
--
-- 'taskDefinition', 'startTask_taskDefinition' - The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to start. If a @revision@ isn\'t specified, the latest
-- @ACTIVE@ revision is used.
newStartTask ::
  -- | 'taskDefinition'
  Prelude.Text ->
  StartTask
newStartTask pTaskDefinition_ =
  StartTask'
    { cluster = Prelude.Nothing,
      enableECSManagedTags = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing,
      group' = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      overrides = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      referenceId = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      tags = Prelude.Nothing,
      containerInstances = Prelude.mempty,
      taskDefinition = pTaskDefinition_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster where
-- to start your task. If you do not specify a cluster, the default cluster
-- is assumed.
startTask_cluster :: Lens.Lens' StartTask (Prelude.Maybe Prelude.Text)
startTask_cluster = Lens.lens (\StartTask' {cluster} -> cluster) (\s@StartTask' {} a -> s {cluster = a} :: StartTask)

-- | Specifies whether to use Amazon ECS managed tags for the task. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
startTask_enableECSManagedTags :: Lens.Lens' StartTask (Prelude.Maybe Prelude.Bool)
startTask_enableECSManagedTags = Lens.lens (\StartTask' {enableECSManagedTags} -> enableECSManagedTags) (\s@StartTask' {} a -> s {enableECSManagedTags = a} :: StartTask)

-- | Whether or not the execute command functionality is enabled for the
-- task. If @true@, this enables execute command functionality on all
-- containers in the task.
startTask_enableExecuteCommand :: Lens.Lens' StartTask (Prelude.Maybe Prelude.Bool)
startTask_enableExecuteCommand = Lens.lens (\StartTask' {enableExecuteCommand} -> enableExecuteCommand) (\s@StartTask' {} a -> s {enableExecuteCommand = a} :: StartTask)

-- | The name of the task group to associate with the task. The default value
-- is the family name of the task definition (for example,
-- family:my-family-name).
startTask_group :: Lens.Lens' StartTask (Prelude.Maybe Prelude.Text)
startTask_group = Lens.lens (\StartTask' {group'} -> group') (\s@StartTask' {} a -> s {group' = a} :: StartTask)

-- | The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
startTask_networkConfiguration :: Lens.Lens' StartTask (Prelude.Maybe NetworkConfiguration)
startTask_networkConfiguration = Lens.lens (\StartTask' {networkConfiguration} -> networkConfiguration) (\s@StartTask' {} a -> s {networkConfiguration = a} :: StartTask)

-- | A list of container overrides in JSON format that specify the name of a
-- container in the specified task definition and the overrides it
-- receives. You can override the default command for a container (that\'s
-- specified in the task definition or Docker image) with a @command@
-- override. You can also override existing environment variables (that are
-- specified in the task definition or Docker image) on a container or add
-- new environment variables to it with an @environment@ override.
--
-- A total of 8192 characters are allowed for overrides. This limit
-- includes the JSON formatting characters of the override structure.
startTask_overrides :: Lens.Lens' StartTask (Prelude.Maybe TaskOverride)
startTask_overrides = Lens.lens (\StartTask' {overrides} -> overrides) (\s@StartTask' {} a -> s {overrides = a} :: StartTask)

-- | Specifies whether to propagate the tags from the task definition or the
-- service to the task. If no value is specified, the tags aren\'t
-- propagated.
startTask_propagateTags :: Lens.Lens' StartTask (Prelude.Maybe PropagateTags)
startTask_propagateTags = Lens.lens (\StartTask' {propagateTags} -> propagateTags) (\s@StartTask' {} a -> s {propagateTags = a} :: StartTask)

-- | The reference ID to use for the task.
startTask_referenceId :: Lens.Lens' StartTask (Prelude.Maybe Prelude.Text)
startTask_referenceId = Lens.lens (\StartTask' {referenceId} -> referenceId) (\s@StartTask' {} a -> s {referenceId = a} :: StartTask)

-- | An optional tag specified when a task is started. For example, if you
-- automatically trigger a task to run a batch process job, you could apply
-- a unique identifier for that job to your task with the @startedBy@
-- parameter. You can then identify which tasks belong to that job by
-- filtering the results of a ListTasks call with the @startedBy@ value. Up
-- to 36 letters (uppercase and lowercase), numbers, hyphens (-), and
-- underscores (_) are allowed.
--
-- If a task is started by an Amazon ECS service, the @startedBy@ parameter
-- contains the deployment ID of the service that starts it.
startTask_startedBy :: Lens.Lens' StartTask (Prelude.Maybe Prelude.Text)
startTask_startedBy = Lens.lens (\StartTask' {startedBy} -> startedBy) (\s@StartTask' {} a -> s {startedBy = a} :: StartTask)

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
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
startTask_tags :: Lens.Lens' StartTask (Prelude.Maybe [Tag])
startTask_tags = Lens.lens (\StartTask' {tags} -> tags) (\s@StartTask' {} a -> s {tags = a} :: StartTask) Prelude.. Lens.mapping Lens.coerced

-- | The container instance IDs or full ARN entries for the container
-- instances where you would like to place your task. You can specify up to
-- 10 container instances.
startTask_containerInstances :: Lens.Lens' StartTask [Prelude.Text]
startTask_containerInstances = Lens.lens (\StartTask' {containerInstances} -> containerInstances) (\s@StartTask' {} a -> s {containerInstances = a} :: StartTask) Prelude.. Lens.coerced

-- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to start. If a @revision@ isn\'t specified, the latest
-- @ACTIVE@ revision is used.
startTask_taskDefinition :: Lens.Lens' StartTask Prelude.Text
startTask_taskDefinition = Lens.lens (\StartTask' {taskDefinition} -> taskDefinition) (\s@StartTask' {} a -> s {taskDefinition = a} :: StartTask)

instance Core.AWSRequest StartTask where
  type AWSResponse StartTask = StartTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTaskResponse'
            Prelude.<$> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTask where
  hashWithSalt _salt StartTask' {..} =
    _salt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` enableECSManagedTags
      `Prelude.hashWithSalt` enableExecuteCommand
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` overrides
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` referenceId
      `Prelude.hashWithSalt` startedBy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` containerInstances
      `Prelude.hashWithSalt` taskDefinition

instance Prelude.NFData StartTask where
  rnf StartTask' {..} =
    Prelude.rnf cluster `Prelude.seq`
      Prelude.rnf enableECSManagedTags `Prelude.seq`
        Prelude.rnf enableExecuteCommand `Prelude.seq`
          Prelude.rnf group' `Prelude.seq`
            Prelude.rnf networkConfiguration `Prelude.seq`
              Prelude.rnf overrides `Prelude.seq`
                Prelude.rnf propagateTags `Prelude.seq`
                  Prelude.rnf referenceId `Prelude.seq`
                    Prelude.rnf startedBy `Prelude.seq`
                      Prelude.rnf tags `Prelude.seq`
                        Prelude.rnf containerInstances `Prelude.seq`
                          Prelude.rnf taskDefinition

instance Data.ToHeaders StartTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.StartTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartTask where
  toJSON StartTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cluster" Data..=) Prelude.<$> cluster,
            ("enableECSManagedTags" Data..=)
              Prelude.<$> enableECSManagedTags,
            ("enableExecuteCommand" Data..=)
              Prelude.<$> enableExecuteCommand,
            ("group" Data..=) Prelude.<$> group',
            ("networkConfiguration" Data..=)
              Prelude.<$> networkConfiguration,
            ("overrides" Data..=) Prelude.<$> overrides,
            ("propagateTags" Data..=) Prelude.<$> propagateTags,
            ("referenceId" Data..=) Prelude.<$> referenceId,
            ("startedBy" Data..=) Prelude.<$> startedBy,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("containerInstances" Data..= containerInstances),
            Prelude.Just
              ("taskDefinition" Data..= taskDefinition)
          ]
      )

instance Data.ToPath StartTask where
  toPath = Prelude.const "/"

instance Data.ToQuery StartTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTaskResponse' smart constructor.
data StartTaskResponse = StartTaskResponse'
  { -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | A full description of the tasks that were started. Each task that was
    -- successfully placed on your container instances is described.
    tasks :: Prelude.Maybe [Task],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'startTaskResponse_failures' - Any failures associated with the call.
--
-- 'tasks', 'startTaskResponse_tasks' - A full description of the tasks that were started. Each task that was
-- successfully placed on your container instances is described.
--
-- 'httpStatus', 'startTaskResponse_httpStatus' - The response's http status code.
newStartTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTaskResponse
newStartTaskResponse pHttpStatus_ =
  StartTaskResponse'
    { failures = Prelude.Nothing,
      tasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
startTaskResponse_failures :: Lens.Lens' StartTaskResponse (Prelude.Maybe [Failure])
startTaskResponse_failures = Lens.lens (\StartTaskResponse' {failures} -> failures) (\s@StartTaskResponse' {} a -> s {failures = a} :: StartTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | A full description of the tasks that were started. Each task that was
-- successfully placed on your container instances is described.
startTaskResponse_tasks :: Lens.Lens' StartTaskResponse (Prelude.Maybe [Task])
startTaskResponse_tasks = Lens.lens (\StartTaskResponse' {tasks} -> tasks) (\s@StartTaskResponse' {} a -> s {tasks = a} :: StartTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startTaskResponse_httpStatus :: Lens.Lens' StartTaskResponse Prelude.Int
startTaskResponse_httpStatus = Lens.lens (\StartTaskResponse' {httpStatus} -> httpStatus) (\s@StartTaskResponse' {} a -> s {httpStatus = a} :: StartTaskResponse)

instance Prelude.NFData StartTaskResponse where
  rnf StartTaskResponse' {..} =
    Prelude.rnf failures `Prelude.seq`
      Prelude.rnf tasks `Prelude.seq`
        Prelude.rnf httpStatus
