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
-- Module      : Amazonka.ECS.RunTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new task using the specified task definition.
--
-- You can allow Amazon ECS to place tasks for you, or you can customize
-- how Amazon ECS places tasks using placement constraints and placement
-- strategies. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html Scheduling Tasks>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- Alternatively, you can use StartTask to use your own scheduler or place
-- tasks manually on specific container instances.
--
-- The Amazon ECS API follows an eventual consistency model, due to the
-- distributed nature of the system supporting the API. This means that the
-- result of an API command you run that affects your Amazon ECS resources
-- might not be immediately visible to all subsequent commands you run.
-- Keep this in mind when you carry out an API command that immediately
-- follows a previous API command.
--
-- To manage eventual consistency, you can do the following:
--
-- -   Confirm the state of the resource before you run a command to modify
--     it. Run the DescribeTasks command using an exponential backoff
--     algorithm to ensure that you allow enough time for the previous
--     command to propagate through the system. To do this, run the
--     DescribeTasks command repeatedly, starting with a couple of seconds
--     of wait time and increasing gradually up to five minutes of wait
--     time.
--
-- -   Add wait time between subsequent commands, even if the DescribeTasks
--     command returns an accurate response. Apply an exponential backoff
--     algorithm starting with a couple of seconds of wait time, and
--     increase gradually up to about five minutes of wait time.
module Amazonka.ECS.RunTask
  ( -- * Creating a Request
    RunTask (..),
    newRunTask,

    -- * Request Lenses
    runTask_overrides,
    runTask_group,
    runTask_cluster,
    runTask_propagateTags,
    runTask_platformVersion,
    runTask_enableECSManagedTags,
    runTask_count,
    runTask_referenceId,
    runTask_placementConstraints,
    runTask_placementStrategy,
    runTask_startedBy,
    runTask_launchType,
    runTask_networkConfiguration,
    runTask_capacityProviderStrategy,
    runTask_enableExecuteCommand,
    runTask_tags,
    runTask_taskDefinition,

    -- * Destructuring the Response
    RunTaskResponse (..),
    newRunTaskResponse,

    -- * Response Lenses
    runTaskResponse_failures,
    runTaskResponse_tasks,
    runTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRunTask' smart constructor.
data RunTask = RunTask'
  { -- | A list of container overrides in JSON format that specify the name of a
    -- container in the specified task definition and the overrides it should
    -- receive. You can override the default command for a container (that is
    -- specified in the task definition or Docker image) with a @command@
    -- override. You can also override existing environment variables (that are
    -- specified in the task definition or Docker image) on a container or add
    -- new environment variables to it with an @environment@ override.
    --
    -- A total of 8192 characters are allowed for overrides. This limit
    -- includes the JSON formatting characters of the override structure.
    overrides :: Prelude.Maybe TaskOverride,
    -- | The name of the task group to associate with the task. The default value
    -- is the family name of the task definition (for example,
    -- @family:my-family-name@).
    group' :: Prelude.Maybe Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster on
    -- which to run your task. If you do not specify a cluster, the default
    -- cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to propagate the tags from the task definition to the
    -- task. If no value is specified, the tags are not propagated. Tags can
    -- only be propagated to the task during task creation. To add tags to a
    -- task after task creation, use the TagResource API action.
    --
    -- An error will be received if you specify the @SERVICE@ option when
    -- running a task.
    propagateTags :: Prelude.Maybe PropagateTags,
    -- | The platform version the task should use. A platform version is only
    -- specified for tasks hosted on Fargate. If one is not specified, the
    -- @LATEST@ platform version is used by default. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to enable Amazon ECS managed tags for the task. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    enableECSManagedTags :: Prelude.Maybe Prelude.Bool,
    -- | The number of instantiations of the specified task to place on your
    -- cluster. You can specify up to 10 tasks per call.
    count :: Prelude.Maybe Prelude.Int,
    -- | The reference ID to use for the task. The reference ID can have a
    -- maximum length of 1024 characters.
    referenceId :: Prelude.Maybe Prelude.Text,
    -- | An array of placement constraint objects to use for the task. You can
    -- specify up to 10 constraints per task (including constraints in the task
    -- definition and those specified at runtime).
    placementConstraints :: Prelude.Maybe [PlacementConstraint],
    -- | The placement strategy objects to use for the task. You can specify a
    -- maximum of 5 strategy rules per task.
    placementStrategy :: Prelude.Maybe [PlacementStrategy],
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
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The infrastructure on which to run your standalone task. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- The @FARGATE@ launch type runs your tasks on Fargate On-Demand
    -- infrastructure.
    --
    -- Fargate Spot infrastructure is available for use but a capacity provider
    -- strategy must be used. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/fargate-capacity-providers.html Fargate capacity providers>
    -- in the /Amazon ECS User Guide for Fargate/.
    --
    -- The @EC2@ launch type runs your tasks on Amazon EC2 instances registered
    -- to your cluster.
    --
    -- The @EXTERNAL@ launch type runs your tasks on your on-premise server or
    -- virtual machine (VM) capacity registered to your cluster.
    --
    -- A task can use either a launch type or a capacity provider strategy. If
    -- a @launchType@ is specified, the @capacityProviderStrategy@ parameter
    -- must be omitted.
    --
    -- When you use cluster auto scaling, you must specify
    -- @capacityProviderStrategy@ and not @launchType@.
    launchType :: Prelude.Maybe LaunchType,
    -- | The network configuration for the task. This parameter is required for
    -- task definitions that use the @awsvpc@ network mode to receive their own
    -- elastic network interface, and it is not supported for other network
    -- modes. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task networking>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The capacity provider strategy to use for the task.
    --
    -- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
    -- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
    -- specified, the @defaultCapacityProviderStrategy@ for the cluster is
    -- used.
    --
    -- When you use cluster auto scaling, you must specify
    -- @capacityProviderStrategy@ and not @launchType@.
    --
    -- A capacity provider strategy may contain a maximum of 6 capacity
    -- providers.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | Whether or not to enable the execute command functionality for the
    -- containers in this task. If @true@, this enables execute command
    -- functionality on all containers in the task.
    enableExecuteCommand :: Prelude.Maybe Prelude.Bool,
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
    -- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
    -- definition to run. If a @revision@ is not specified, the latest @ACTIVE@
    -- revision is used.
    --
    -- The full ARN value must match the value that you specified ias the
    -- @Resource@ of the IAM principal\'s permissions policy. For example, if
    -- the @Resource@ is
    -- arn:aws:ecs:us-east-1:111122223333:task-definition\/TaskFamilyName:*,
    -- the @taskDefinition@ ARN value must be
    -- @arn:aws:ecs:us-east-1:111122223333:task-definition\/TaskFamilyName@.
    taskDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrides', 'runTask_overrides' - A list of container overrides in JSON format that specify the name of a
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
-- 'group'', 'runTask_group' - The name of the task group to associate with the task. The default value
-- is the family name of the task definition (for example,
-- @family:my-family-name@).
--
-- 'cluster', 'runTask_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster on
-- which to run your task. If you do not specify a cluster, the default
-- cluster is assumed.
--
-- 'propagateTags', 'runTask_propagateTags' - Specifies whether to propagate the tags from the task definition to the
-- task. If no value is specified, the tags are not propagated. Tags can
-- only be propagated to the task during task creation. To add tags to a
-- task after task creation, use the TagResource API action.
--
-- An error will be received if you specify the @SERVICE@ option when
-- running a task.
--
-- 'platformVersion', 'runTask_platformVersion' - The platform version the task should use. A platform version is only
-- specified for tasks hosted on Fargate. If one is not specified, the
-- @LATEST@ platform version is used by default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'enableECSManagedTags', 'runTask_enableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the task. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'count', 'runTask_count' - The number of instantiations of the specified task to place on your
-- cluster. You can specify up to 10 tasks per call.
--
-- 'referenceId', 'runTask_referenceId' - The reference ID to use for the task. The reference ID can have a
-- maximum length of 1024 characters.
--
-- 'placementConstraints', 'runTask_placementConstraints' - An array of placement constraint objects to use for the task. You can
-- specify up to 10 constraints per task (including constraints in the task
-- definition and those specified at runtime).
--
-- 'placementStrategy', 'runTask_placementStrategy' - The placement strategy objects to use for the task. You can specify a
-- maximum of 5 strategy rules per task.
--
-- 'startedBy', 'runTask_startedBy' - An optional tag specified when a task is started. For example, if you
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
-- 'launchType', 'runTask_launchType' - The infrastructure on which to run your standalone task. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- The @FARGATE@ launch type runs your tasks on Fargate On-Demand
-- infrastructure.
--
-- Fargate Spot infrastructure is available for use but a capacity provider
-- strategy must be used. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/fargate-capacity-providers.html Fargate capacity providers>
-- in the /Amazon ECS User Guide for Fargate/.
--
-- The @EC2@ launch type runs your tasks on Amazon EC2 instances registered
-- to your cluster.
--
-- The @EXTERNAL@ launch type runs your tasks on your on-premise server or
-- virtual machine (VM) capacity registered to your cluster.
--
-- A task can use either a launch type or a capacity provider strategy. If
-- a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
--
-- When you use cluster auto scaling, you must specify
-- @capacityProviderStrategy@ and not @launchType@.
--
-- 'networkConfiguration', 'runTask_networkConfiguration' - The network configuration for the task. This parameter is required for
-- task definitions that use the @awsvpc@ network mode to receive their own
-- elastic network interface, and it is not supported for other network
-- modes. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'capacityProviderStrategy', 'runTask_capacityProviderStrategy' - The capacity provider strategy to use for the task.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
--
-- When you use cluster auto scaling, you must specify
-- @capacityProviderStrategy@ and not @launchType@.
--
-- A capacity provider strategy may contain a maximum of 6 capacity
-- providers.
--
-- 'enableExecuteCommand', 'runTask_enableExecuteCommand' - Whether or not to enable the execute command functionality for the
-- containers in this task. If @true@, this enables execute command
-- functionality on all containers in the task.
--
-- 'tags', 'runTask_tags' - The metadata that you apply to the task to help you categorize and
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
-- 'taskDefinition', 'runTask_taskDefinition' - The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run. If a @revision@ is not specified, the latest @ACTIVE@
-- revision is used.
--
-- The full ARN value must match the value that you specified ias the
-- @Resource@ of the IAM principal\'s permissions policy. For example, if
-- the @Resource@ is
-- arn:aws:ecs:us-east-1:111122223333:task-definition\/TaskFamilyName:*,
-- the @taskDefinition@ ARN value must be
-- @arn:aws:ecs:us-east-1:111122223333:task-definition\/TaskFamilyName@.
newRunTask ::
  -- | 'taskDefinition'
  Prelude.Text ->
  RunTask
newRunTask pTaskDefinition_ =
  RunTask'
    { overrides = Prelude.Nothing,
      group' = Prelude.Nothing,
      cluster = Prelude.Nothing,
      propagateTags = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      enableECSManagedTags = Prelude.Nothing,
      count = Prelude.Nothing,
      referenceId = Prelude.Nothing,
      placementConstraints = Prelude.Nothing,
      placementStrategy = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      launchType = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      capacityProviderStrategy = Prelude.Nothing,
      enableExecuteCommand = Prelude.Nothing,
      tags = Prelude.Nothing,
      taskDefinition = pTaskDefinition_
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
runTask_overrides :: Lens.Lens' RunTask (Prelude.Maybe TaskOverride)
runTask_overrides = Lens.lens (\RunTask' {overrides} -> overrides) (\s@RunTask' {} a -> s {overrides = a} :: RunTask)

-- | The name of the task group to associate with the task. The default value
-- is the family name of the task definition (for example,
-- @family:my-family-name@).
runTask_group :: Lens.Lens' RunTask (Prelude.Maybe Prelude.Text)
runTask_group = Lens.lens (\RunTask' {group'} -> group') (\s@RunTask' {} a -> s {group' = a} :: RunTask)

-- | The short name or full Amazon Resource Name (ARN) of the cluster on
-- which to run your task. If you do not specify a cluster, the default
-- cluster is assumed.
runTask_cluster :: Lens.Lens' RunTask (Prelude.Maybe Prelude.Text)
runTask_cluster = Lens.lens (\RunTask' {cluster} -> cluster) (\s@RunTask' {} a -> s {cluster = a} :: RunTask)

-- | Specifies whether to propagate the tags from the task definition to the
-- task. If no value is specified, the tags are not propagated. Tags can
-- only be propagated to the task during task creation. To add tags to a
-- task after task creation, use the TagResource API action.
--
-- An error will be received if you specify the @SERVICE@ option when
-- running a task.
runTask_propagateTags :: Lens.Lens' RunTask (Prelude.Maybe PropagateTags)
runTask_propagateTags = Lens.lens (\RunTask' {propagateTags} -> propagateTags) (\s@RunTask' {} a -> s {propagateTags = a} :: RunTask)

-- | The platform version the task should use. A platform version is only
-- specified for tasks hosted on Fargate. If one is not specified, the
-- @LATEST@ platform version is used by default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
runTask_platformVersion :: Lens.Lens' RunTask (Prelude.Maybe Prelude.Text)
runTask_platformVersion = Lens.lens (\RunTask' {platformVersion} -> platformVersion) (\s@RunTask' {} a -> s {platformVersion = a} :: RunTask)

-- | Specifies whether to enable Amazon ECS managed tags for the task. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
runTask_enableECSManagedTags :: Lens.Lens' RunTask (Prelude.Maybe Prelude.Bool)
runTask_enableECSManagedTags = Lens.lens (\RunTask' {enableECSManagedTags} -> enableECSManagedTags) (\s@RunTask' {} a -> s {enableECSManagedTags = a} :: RunTask)

-- | The number of instantiations of the specified task to place on your
-- cluster. You can specify up to 10 tasks per call.
runTask_count :: Lens.Lens' RunTask (Prelude.Maybe Prelude.Int)
runTask_count = Lens.lens (\RunTask' {count} -> count) (\s@RunTask' {} a -> s {count = a} :: RunTask)

-- | The reference ID to use for the task. The reference ID can have a
-- maximum length of 1024 characters.
runTask_referenceId :: Lens.Lens' RunTask (Prelude.Maybe Prelude.Text)
runTask_referenceId = Lens.lens (\RunTask' {referenceId} -> referenceId) (\s@RunTask' {} a -> s {referenceId = a} :: RunTask)

-- | An array of placement constraint objects to use for the task. You can
-- specify up to 10 constraints per task (including constraints in the task
-- definition and those specified at runtime).
runTask_placementConstraints :: Lens.Lens' RunTask (Prelude.Maybe [PlacementConstraint])
runTask_placementConstraints = Lens.lens (\RunTask' {placementConstraints} -> placementConstraints) (\s@RunTask' {} a -> s {placementConstraints = a} :: RunTask) Prelude.. Lens.mapping Lens.coerced

-- | The placement strategy objects to use for the task. You can specify a
-- maximum of 5 strategy rules per task.
runTask_placementStrategy :: Lens.Lens' RunTask (Prelude.Maybe [PlacementStrategy])
runTask_placementStrategy = Lens.lens (\RunTask' {placementStrategy} -> placementStrategy) (\s@RunTask' {} a -> s {placementStrategy = a} :: RunTask) Prelude.. Lens.mapping Lens.coerced

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
runTask_startedBy :: Lens.Lens' RunTask (Prelude.Maybe Prelude.Text)
runTask_startedBy = Lens.lens (\RunTask' {startedBy} -> startedBy) (\s@RunTask' {} a -> s {startedBy = a} :: RunTask)

-- | The infrastructure on which to run your standalone task. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- The @FARGATE@ launch type runs your tasks on Fargate On-Demand
-- infrastructure.
--
-- Fargate Spot infrastructure is available for use but a capacity provider
-- strategy must be used. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/fargate-capacity-providers.html Fargate capacity providers>
-- in the /Amazon ECS User Guide for Fargate/.
--
-- The @EC2@ launch type runs your tasks on Amazon EC2 instances registered
-- to your cluster.
--
-- The @EXTERNAL@ launch type runs your tasks on your on-premise server or
-- virtual machine (VM) capacity registered to your cluster.
--
-- A task can use either a launch type or a capacity provider strategy. If
-- a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
--
-- When you use cluster auto scaling, you must specify
-- @capacityProviderStrategy@ and not @launchType@.
runTask_launchType :: Lens.Lens' RunTask (Prelude.Maybe LaunchType)
runTask_launchType = Lens.lens (\RunTask' {launchType} -> launchType) (\s@RunTask' {} a -> s {launchType = a} :: RunTask)

-- | The network configuration for the task. This parameter is required for
-- task definitions that use the @awsvpc@ network mode to receive their own
-- elastic network interface, and it is not supported for other network
-- modes. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
runTask_networkConfiguration :: Lens.Lens' RunTask (Prelude.Maybe NetworkConfiguration)
runTask_networkConfiguration = Lens.lens (\RunTask' {networkConfiguration} -> networkConfiguration) (\s@RunTask' {} a -> s {networkConfiguration = a} :: RunTask)

-- | The capacity provider strategy to use for the task.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
--
-- When you use cluster auto scaling, you must specify
-- @capacityProviderStrategy@ and not @launchType@.
--
-- A capacity provider strategy may contain a maximum of 6 capacity
-- providers.
runTask_capacityProviderStrategy :: Lens.Lens' RunTask (Prelude.Maybe [CapacityProviderStrategyItem])
runTask_capacityProviderStrategy = Lens.lens (\RunTask' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@RunTask' {} a -> s {capacityProviderStrategy = a} :: RunTask) Prelude.. Lens.mapping Lens.coerced

-- | Whether or not to enable the execute command functionality for the
-- containers in this task. If @true@, this enables execute command
-- functionality on all containers in the task.
runTask_enableExecuteCommand :: Lens.Lens' RunTask (Prelude.Maybe Prelude.Bool)
runTask_enableExecuteCommand = Lens.lens (\RunTask' {enableExecuteCommand} -> enableExecuteCommand) (\s@RunTask' {} a -> s {enableExecuteCommand = a} :: RunTask)

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
runTask_tags :: Lens.Lens' RunTask (Prelude.Maybe [Tag])
runTask_tags = Lens.lens (\RunTask' {tags} -> tags) (\s@RunTask' {} a -> s {tags = a} :: RunTask) Prelude.. Lens.mapping Lens.coerced

-- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run. If a @revision@ is not specified, the latest @ACTIVE@
-- revision is used.
--
-- The full ARN value must match the value that you specified ias the
-- @Resource@ of the IAM principal\'s permissions policy. For example, if
-- the @Resource@ is
-- arn:aws:ecs:us-east-1:111122223333:task-definition\/TaskFamilyName:*,
-- the @taskDefinition@ ARN value must be
-- @arn:aws:ecs:us-east-1:111122223333:task-definition\/TaskFamilyName@.
runTask_taskDefinition :: Lens.Lens' RunTask Prelude.Text
runTask_taskDefinition = Lens.lens (\RunTask' {taskDefinition} -> taskDefinition) (\s@RunTask' {} a -> s {taskDefinition = a} :: RunTask)

instance Core.AWSRequest RunTask where
  type AWSResponse RunTask = RunTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RunTaskResponse'
            Prelude.<$> (x Core..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RunTask where
  hashWithSalt _salt RunTask' {..} =
    _salt `Prelude.hashWithSalt` overrides
      `Prelude.hashWithSalt` group'
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` propagateTags
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` enableECSManagedTags
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` referenceId
      `Prelude.hashWithSalt` placementConstraints
      `Prelude.hashWithSalt` placementStrategy
      `Prelude.hashWithSalt` startedBy
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` enableExecuteCommand
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` taskDefinition

instance Prelude.NFData RunTask where
  rnf RunTask' {..} =
    Prelude.rnf overrides
      `Prelude.seq` Prelude.rnf group'
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf propagateTags
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf enableECSManagedTags
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf referenceId
      `Prelude.seq` Prelude.rnf placementConstraints
      `Prelude.seq` Prelude.rnf placementStrategy
      `Prelude.seq` Prelude.rnf startedBy
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf enableExecuteCommand
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf taskDefinition

instance Core.ToHeaders RunTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.RunTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RunTask where
  toJSON RunTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("overrides" Core..=) Prelude.<$> overrides,
            ("group" Core..=) Prelude.<$> group',
            ("cluster" Core..=) Prelude.<$> cluster,
            ("propagateTags" Core..=) Prelude.<$> propagateTags,
            ("platformVersion" Core..=)
              Prelude.<$> platformVersion,
            ("enableECSManagedTags" Core..=)
              Prelude.<$> enableECSManagedTags,
            ("count" Core..=) Prelude.<$> count,
            ("referenceId" Core..=) Prelude.<$> referenceId,
            ("placementConstraints" Core..=)
              Prelude.<$> placementConstraints,
            ("placementStrategy" Core..=)
              Prelude.<$> placementStrategy,
            ("startedBy" Core..=) Prelude.<$> startedBy,
            ("launchType" Core..=) Prelude.<$> launchType,
            ("networkConfiguration" Core..=)
              Prelude.<$> networkConfiguration,
            ("capacityProviderStrategy" Core..=)
              Prelude.<$> capacityProviderStrategy,
            ("enableExecuteCommand" Core..=)
              Prelude.<$> enableExecuteCommand,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("taskDefinition" Core..= taskDefinition)
          ]
      )

instance Core.ToPath RunTask where
  toPath = Prelude.const "/"

instance Core.ToQuery RunTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRunTaskResponse' smart constructor.
data RunTaskResponse = RunTaskResponse'
  { -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | A full description of the tasks that were run. The tasks that were
    -- successfully placed on your cluster are described here.
    tasks :: Prelude.Maybe [Task],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'runTaskResponse_failures' - Any failures associated with the call.
--
-- 'tasks', 'runTaskResponse_tasks' - A full description of the tasks that were run. The tasks that were
-- successfully placed on your cluster are described here.
--
-- 'httpStatus', 'runTaskResponse_httpStatus' - The response's http status code.
newRunTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RunTaskResponse
newRunTaskResponse pHttpStatus_ =
  RunTaskResponse'
    { failures = Prelude.Nothing,
      tasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
runTaskResponse_failures :: Lens.Lens' RunTaskResponse (Prelude.Maybe [Failure])
runTaskResponse_failures = Lens.lens (\RunTaskResponse' {failures} -> failures) (\s@RunTaskResponse' {} a -> s {failures = a} :: RunTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | A full description of the tasks that were run. The tasks that were
-- successfully placed on your cluster are described here.
runTaskResponse_tasks :: Lens.Lens' RunTaskResponse (Prelude.Maybe [Task])
runTaskResponse_tasks = Lens.lens (\RunTaskResponse' {tasks} -> tasks) (\s@RunTaskResponse' {} a -> s {tasks = a} :: RunTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
runTaskResponse_httpStatus :: Lens.Lens' RunTaskResponse Prelude.Int
runTaskResponse_httpStatus = Lens.lens (\RunTaskResponse' {httpStatus} -> httpStatus) (\s@RunTaskResponse' {} a -> s {httpStatus = a} :: RunTaskResponse)

instance Prelude.NFData RunTaskResponse where
  rnf RunTaskResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf tasks
      `Prelude.seq` Prelude.rnf httpStatus
