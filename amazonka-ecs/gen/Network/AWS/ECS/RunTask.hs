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
-- Module      : Network.AWS.ECS.RunTask
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
module Network.AWS.ECS.RunTask
  ( -- * Creating a Request
    RunTask (..),
    newRunTask,

    -- * Request Lenses
    runTask_networkConfiguration,
    runTask_capacityProviderStrategy,
    runTask_referenceId,
    runTask_enableECSManagedTags,
    runTask_launchType,
    runTask_platformVersion,
    runTask_placementStrategy,
    runTask_startedBy,
    runTask_placementConstraints,
    runTask_group,
    runTask_overrides,
    runTask_tags,
    runTask_count,
    runTask_cluster,
    runTask_propagateTags,
    runTask_taskDefinition,

    -- * Destructuring the Response
    RunTaskResponse (..),
    newRunTaskResponse,

    -- * Response Lenses
    runTaskResponse_tasks,
    runTaskResponse_failures,
    runTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRunTask' smart constructor.
data RunTask = RunTask'
  { -- | The network configuration for the task. This parameter is required for
    -- task definitions that use the @awsvpc@ network mode to receive their own
    -- elastic network interface, and it is not supported for other network
    -- modes. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    networkConfiguration :: Core.Maybe NetworkConfiguration,
    -- | The capacity provider strategy to use for the task.
    --
    -- A capacity provider strategy consists of one or more capacity providers
    -- along with the @base@ and @weight@ to assign to them. A capacity
    -- provider must be associated with the cluster to be used in a capacity
    -- provider strategy. The PutClusterCapacityProviders API is used to
    -- associate a capacity provider with a cluster. Only capacity providers
    -- with an @ACTIVE@ or @UPDATING@ status can be used.
    --
    -- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
    -- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
    -- specified, the @defaultCapacityProviderStrategy@ for the cluster is
    -- used.
    --
    -- If specifying a capacity provider that uses an Auto Scaling group, the
    -- capacity provider must already be created. New capacity providers can be
    -- created with the CreateCapacityProvider API operation.
    --
    -- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
    -- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
    -- are available to all accounts and only need to be associated with a
    -- cluster to be used.
    --
    -- The PutClusterCapacityProviders API operation is used to update the list
    -- of available capacity providers for a cluster after the cluster is
    -- created.
    capacityProviderStrategy :: Core.Maybe [CapacityProviderStrategyItem],
    -- | The reference ID to use for the task.
    referenceId :: Core.Maybe Core.Text,
    -- | Specifies whether to enable Amazon ECS managed tags for the task. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    enableECSManagedTags :: Core.Maybe Core.Bool,
    -- | The launch type on which to run your task. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
    -- must be omitted.
    launchType :: Core.Maybe LaunchType,
    -- | The platform version the task should run. A platform version is only
    -- specified for tasks using the Fargate launch type. If one is not
    -- specified, the @LATEST@ platform version is used by default. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Core.Maybe Core.Text,
    -- | The placement strategy objects to use for the task. You can specify a
    -- maximum of five strategy rules per task.
    placementStrategy :: Core.Maybe [PlacementStrategy],
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
    -- | An array of placement constraint objects to use for the task. You can
    -- specify up to 10 constraints per task (including constraints in the task
    -- definition and those specified at runtime).
    placementConstraints :: Core.Maybe [PlacementConstraint],
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
    -- | The number of instantiations of the specified task to place on your
    -- cluster. You can specify up to 10 tasks per call.
    count :: Core.Maybe Core.Int,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster on
    -- which to run your task. If you do not specify a cluster, the default
    -- cluster is assumed.
    cluster :: Core.Maybe Core.Text,
    -- | Specifies whether to propagate the tags from the task definition to the
    -- task. If no value is specified, the tags are not propagated. Tags can
    -- only be propagated to the task during task creation. To add tags to a
    -- task after task creation, use the TagResource API action.
    --
    -- An error will be received if you specify the @SERVICE@ option when
    -- running a task.
    propagateTags :: Core.Maybe PropagateTags,
    -- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
    -- definition to run. If a @revision@ is not specified, the latest @ACTIVE@
    -- revision is used.
    taskDefinition :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RunTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConfiguration', 'runTask_networkConfiguration' - The network configuration for the task. This parameter is required for
-- task definitions that use the @awsvpc@ network mode to receive their own
-- elastic network interface, and it is not supported for other network
-- modes. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'capacityProviderStrategy', 'runTask_capacityProviderStrategy' - The capacity provider strategy to use for the task.
--
-- A capacity provider strategy consists of one or more capacity providers
-- along with the @base@ and @weight@ to assign to them. A capacity
-- provider must be associated with the cluster to be used in a capacity
-- provider strategy. The PutClusterCapacityProviders API is used to
-- associate a capacity provider with a cluster. Only capacity providers
-- with an @ACTIVE@ or @UPDATING@ status can be used.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
--
-- 'referenceId', 'runTask_referenceId' - The reference ID to use for the task.
--
-- 'enableECSManagedTags', 'runTask_enableECSManagedTags' - Specifies whether to enable Amazon ECS managed tags for the task. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'launchType', 'runTask_launchType' - The launch type on which to run your task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
--
-- 'platformVersion', 'runTask_platformVersion' - The platform version the task should run. A platform version is only
-- specified for tasks using the Fargate launch type. If one is not
-- specified, the @LATEST@ platform version is used by default. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'placementStrategy', 'runTask_placementStrategy' - The placement strategy objects to use for the task. You can specify a
-- maximum of five strategy rules per task.
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
-- 'placementConstraints', 'runTask_placementConstraints' - An array of placement constraint objects to use for the task. You can
-- specify up to 10 constraints per task (including constraints in the task
-- definition and those specified at runtime).
--
-- 'group'', 'runTask_group' - The name of the task group to associate with the task. The default value
-- is the family name of the task definition (for example,
-- family:my-family-name).
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
--
-- 'count', 'runTask_count' - The number of instantiations of the specified task to place on your
-- cluster. You can specify up to 10 tasks per call.
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
-- 'taskDefinition', 'runTask_taskDefinition' - The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run. If a @revision@ is not specified, the latest @ACTIVE@
-- revision is used.
newRunTask ::
  -- | 'taskDefinition'
  Core.Text ->
  RunTask
newRunTask pTaskDefinition_ =
  RunTask'
    { networkConfiguration = Core.Nothing,
      capacityProviderStrategy = Core.Nothing,
      referenceId = Core.Nothing,
      enableECSManagedTags = Core.Nothing,
      launchType = Core.Nothing,
      platformVersion = Core.Nothing,
      placementStrategy = Core.Nothing,
      startedBy = Core.Nothing,
      placementConstraints = Core.Nothing,
      group' = Core.Nothing,
      overrides = Core.Nothing,
      tags = Core.Nothing,
      count = Core.Nothing,
      cluster = Core.Nothing,
      propagateTags = Core.Nothing,
      taskDefinition = pTaskDefinition_
    }

-- | The network configuration for the task. This parameter is required for
-- task definitions that use the @awsvpc@ network mode to receive their own
-- elastic network interface, and it is not supported for other network
-- modes. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html Task Networking>
-- in the /Amazon Elastic Container Service Developer Guide/.
runTask_networkConfiguration :: Lens.Lens' RunTask (Core.Maybe NetworkConfiguration)
runTask_networkConfiguration = Lens.lens (\RunTask' {networkConfiguration} -> networkConfiguration) (\s@RunTask' {} a -> s {networkConfiguration = a} :: RunTask)

-- | The capacity provider strategy to use for the task.
--
-- A capacity provider strategy consists of one or more capacity providers
-- along with the @base@ and @weight@ to assign to them. A capacity
-- provider must be associated with the cluster to be used in a capacity
-- provider strategy. The PutClusterCapacityProviders API is used to
-- associate a capacity provider with a cluster. Only capacity providers
-- with an @ACTIVE@ or @UPDATING@ status can be used.
--
-- If a @capacityProviderStrategy@ is specified, the @launchType@ parameter
-- must be omitted. If no @capacityProviderStrategy@ or @launchType@ is
-- specified, the @defaultCapacityProviderStrategy@ for the cluster is
-- used.
--
-- If specifying a capacity provider that uses an Auto Scaling group, the
-- capacity provider must already be created. New capacity providers can be
-- created with the CreateCapacityProvider API operation.
--
-- To use a AWS Fargate capacity provider, specify either the @FARGATE@ or
-- @FARGATE_SPOT@ capacity providers. The AWS Fargate capacity providers
-- are available to all accounts and only need to be associated with a
-- cluster to be used.
--
-- The PutClusterCapacityProviders API operation is used to update the list
-- of available capacity providers for a cluster after the cluster is
-- created.
runTask_capacityProviderStrategy :: Lens.Lens' RunTask (Core.Maybe [CapacityProviderStrategyItem])
runTask_capacityProviderStrategy = Lens.lens (\RunTask' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@RunTask' {} a -> s {capacityProviderStrategy = a} :: RunTask) Core.. Lens.mapping Lens._Coerce

-- | The reference ID to use for the task.
runTask_referenceId :: Lens.Lens' RunTask (Core.Maybe Core.Text)
runTask_referenceId = Lens.lens (\RunTask' {referenceId} -> referenceId) (\s@RunTask' {} a -> s {referenceId = a} :: RunTask)

-- | Specifies whether to enable Amazon ECS managed tags for the task. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html Tagging Your Amazon ECS Resources>
-- in the /Amazon Elastic Container Service Developer Guide/.
runTask_enableECSManagedTags :: Lens.Lens' RunTask (Core.Maybe Core.Bool)
runTask_enableECSManagedTags = Lens.lens (\RunTask' {enableECSManagedTags} -> enableECSManagedTags) (\s@RunTask' {} a -> s {enableECSManagedTags = a} :: RunTask)

-- | The launch type on which to run your task. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If a @launchType@ is specified, the @capacityProviderStrategy@ parameter
-- must be omitted.
runTask_launchType :: Lens.Lens' RunTask (Core.Maybe LaunchType)
runTask_launchType = Lens.lens (\RunTask' {launchType} -> launchType) (\s@RunTask' {} a -> s {launchType = a} :: RunTask)

-- | The platform version the task should run. A platform version is only
-- specified for tasks using the Fargate launch type. If one is not
-- specified, the @LATEST@ platform version is used by default. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
runTask_platformVersion :: Lens.Lens' RunTask (Core.Maybe Core.Text)
runTask_platformVersion = Lens.lens (\RunTask' {platformVersion} -> platformVersion) (\s@RunTask' {} a -> s {platformVersion = a} :: RunTask)

-- | The placement strategy objects to use for the task. You can specify a
-- maximum of five strategy rules per task.
runTask_placementStrategy :: Lens.Lens' RunTask (Core.Maybe [PlacementStrategy])
runTask_placementStrategy = Lens.lens (\RunTask' {placementStrategy} -> placementStrategy) (\s@RunTask' {} a -> s {placementStrategy = a} :: RunTask) Core.. Lens.mapping Lens._Coerce

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
runTask_startedBy :: Lens.Lens' RunTask (Core.Maybe Core.Text)
runTask_startedBy = Lens.lens (\RunTask' {startedBy} -> startedBy) (\s@RunTask' {} a -> s {startedBy = a} :: RunTask)

-- | An array of placement constraint objects to use for the task. You can
-- specify up to 10 constraints per task (including constraints in the task
-- definition and those specified at runtime).
runTask_placementConstraints :: Lens.Lens' RunTask (Core.Maybe [PlacementConstraint])
runTask_placementConstraints = Lens.lens (\RunTask' {placementConstraints} -> placementConstraints) (\s@RunTask' {} a -> s {placementConstraints = a} :: RunTask) Core.. Lens.mapping Lens._Coerce

-- | The name of the task group to associate with the task. The default value
-- is the family name of the task definition (for example,
-- family:my-family-name).
runTask_group :: Lens.Lens' RunTask (Core.Maybe Core.Text)
runTask_group = Lens.lens (\RunTask' {group'} -> group') (\s@RunTask' {} a -> s {group' = a} :: RunTask)

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
runTask_overrides :: Lens.Lens' RunTask (Core.Maybe TaskOverride)
runTask_overrides = Lens.lens (\RunTask' {overrides} -> overrides) (\s@RunTask' {} a -> s {overrides = a} :: RunTask)

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
runTask_tags :: Lens.Lens' RunTask (Core.Maybe [Tag])
runTask_tags = Lens.lens (\RunTask' {tags} -> tags) (\s@RunTask' {} a -> s {tags = a} :: RunTask) Core.. Lens.mapping Lens._Coerce

-- | The number of instantiations of the specified task to place on your
-- cluster. You can specify up to 10 tasks per call.
runTask_count :: Lens.Lens' RunTask (Core.Maybe Core.Int)
runTask_count = Lens.lens (\RunTask' {count} -> count) (\s@RunTask' {} a -> s {count = a} :: RunTask)

-- | The short name or full Amazon Resource Name (ARN) of the cluster on
-- which to run your task. If you do not specify a cluster, the default
-- cluster is assumed.
runTask_cluster :: Lens.Lens' RunTask (Core.Maybe Core.Text)
runTask_cluster = Lens.lens (\RunTask' {cluster} -> cluster) (\s@RunTask' {} a -> s {cluster = a} :: RunTask)

-- | Specifies whether to propagate the tags from the task definition to the
-- task. If no value is specified, the tags are not propagated. Tags can
-- only be propagated to the task during task creation. To add tags to a
-- task after task creation, use the TagResource API action.
--
-- An error will be received if you specify the @SERVICE@ option when
-- running a task.
runTask_propagateTags :: Lens.Lens' RunTask (Core.Maybe PropagateTags)
runTask_propagateTags = Lens.lens (\RunTask' {propagateTags} -> propagateTags) (\s@RunTask' {} a -> s {propagateTags = a} :: RunTask)

-- | The @family@ and @revision@ (@family:revision@) or full ARN of the task
-- definition to run. If a @revision@ is not specified, the latest @ACTIVE@
-- revision is used.
runTask_taskDefinition :: Lens.Lens' RunTask Core.Text
runTask_taskDefinition = Lens.lens (\RunTask' {taskDefinition} -> taskDefinition) (\s@RunTask' {} a -> s {taskDefinition = a} :: RunTask)

instance Core.AWSRequest RunTask where
  type AWSResponse RunTask = RunTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RunTaskResponse'
            Core.<$> (x Core..?> "tasks" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failures" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RunTask

instance Core.NFData RunTask

instance Core.ToHeaders RunTask where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.RunTask" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RunTask where
  toJSON RunTask' {..} =
    Core.object
      ( Core.catMaybes
          [ ("networkConfiguration" Core..=)
              Core.<$> networkConfiguration,
            ("capacityProviderStrategy" Core..=)
              Core.<$> capacityProviderStrategy,
            ("referenceId" Core..=) Core.<$> referenceId,
            ("enableECSManagedTags" Core..=)
              Core.<$> enableECSManagedTags,
            ("launchType" Core..=) Core.<$> launchType,
            ("platformVersion" Core..=) Core.<$> platformVersion,
            ("placementStrategy" Core..=)
              Core.<$> placementStrategy,
            ("startedBy" Core..=) Core.<$> startedBy,
            ("placementConstraints" Core..=)
              Core.<$> placementConstraints,
            ("group" Core..=) Core.<$> group',
            ("overrides" Core..=) Core.<$> overrides,
            ("tags" Core..=) Core.<$> tags,
            ("count" Core..=) Core.<$> count,
            ("cluster" Core..=) Core.<$> cluster,
            ("propagateTags" Core..=) Core.<$> propagateTags,
            Core.Just ("taskDefinition" Core..= taskDefinition)
          ]
      )

instance Core.ToPath RunTask where
  toPath = Core.const "/"

instance Core.ToQuery RunTask where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRunTaskResponse' smart constructor.
data RunTaskResponse = RunTaskResponse'
  { -- | A full description of the tasks that were run. The tasks that were
    -- successfully placed on your cluster are described here.
    tasks :: Core.Maybe [Task],
    -- | Any failures associated with the call.
    failures :: Core.Maybe [Failure],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RunTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tasks', 'runTaskResponse_tasks' - A full description of the tasks that were run. The tasks that were
-- successfully placed on your cluster are described here.
--
-- 'failures', 'runTaskResponse_failures' - Any failures associated with the call.
--
-- 'httpStatus', 'runTaskResponse_httpStatus' - The response's http status code.
newRunTaskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RunTaskResponse
newRunTaskResponse pHttpStatus_ =
  RunTaskResponse'
    { tasks = Core.Nothing,
      failures = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A full description of the tasks that were run. The tasks that were
-- successfully placed on your cluster are described here.
runTaskResponse_tasks :: Lens.Lens' RunTaskResponse (Core.Maybe [Task])
runTaskResponse_tasks = Lens.lens (\RunTaskResponse' {tasks} -> tasks) (\s@RunTaskResponse' {} a -> s {tasks = a} :: RunTaskResponse) Core.. Lens.mapping Lens._Coerce

-- | Any failures associated with the call.
runTaskResponse_failures :: Lens.Lens' RunTaskResponse (Core.Maybe [Failure])
runTaskResponse_failures = Lens.lens (\RunTaskResponse' {failures} -> failures) (\s@RunTaskResponse' {} a -> s {failures = a} :: RunTaskResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
runTaskResponse_httpStatus :: Lens.Lens' RunTaskResponse Core.Int
runTaskResponse_httpStatus = Lens.lens (\RunTaskResponse' {httpStatus} -> httpStatus) (\s@RunTaskResponse' {} a -> s {httpStatus = a} :: RunTaskResponse)

instance Core.NFData RunTaskResponse
