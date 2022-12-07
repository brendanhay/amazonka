{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.Types.TaskSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.TaskSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.CapacityProviderStrategyItem
import Amazonka.ECS.Types.LaunchType
import Amazonka.ECS.Types.LoadBalancer
import Amazonka.ECS.Types.NetworkConfiguration
import Amazonka.ECS.Types.Scale
import Amazonka.ECS.Types.ServiceRegistry
import Amazonka.ECS.Types.StabilityStatus
import Amazonka.ECS.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Information about a set of Amazon ECS tasks in either an CodeDeploy or
-- an @EXTERNAL@ deployment. An Amazon ECS task set includes details such
-- as the desired number of tasks, how many tasks are running, and whether
-- the task set serves production traffic.
--
-- /See:/ 'newTaskSet' smart constructor.
data TaskSet = TaskSet'
  { -- | The metadata that you apply to the task set to help you categorize and
    -- organize them. Each tag consists of a key and an optional value. You
    -- define both.
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
    -- | The Amazon Resource Name (ARN) of the cluster that the service that
    -- hosts the task set exists in.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The details for the service discovery registries to assign to this task
    -- set. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
    serviceRegistries :: Prelude.Maybe [ServiceRegistry],
    -- | The operating system that your tasks in the set are running on. A
    -- platform family is specified only for tasks that use the Fargate launch
    -- type.
    --
    -- All tasks in the set must have the same value.
    platformFamily :: Prelude.Maybe Prelude.Text,
    -- | The task definition that the task set is using.
    taskDefinition :: Prelude.Maybe Prelude.Text,
    -- | The stability status. This indicates whether the task set has reached a
    -- steady state. If the following conditions are met, the task set sre in
    -- @STEADY_STATE@:
    --
    -- -   The task @runningCount@ is equal to the @computedDesiredCount@.
    --
    -- -   The @pendingCount@ is @0@.
    --
    -- -   There are no tasks that are running on container instances in the
    --     @DRAINING@ status.
    --
    -- -   All tasks are reporting a healthy status from the load balancers,
    --     service discovery, and container health checks.
    --
    -- If any of those conditions aren\'t met, the stability status returns
    -- @STABILIZING@.
    stabilityStatus :: Prelude.Maybe StabilityStatus,
    -- | The external ID associated with the task set.
    --
    -- If an CodeDeploy deployment created a task set, the @externalId@
    -- parameter contains the CodeDeploy deployment ID.
    --
    -- If a task set is created for an external deployment and is associated
    -- with a service discovery registry, the @externalId@ parameter contains
    -- the @ECS_TASK_SET_EXTERNAL_ID@ Cloud Map attribute.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The network configuration for the task set.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The status of the task set. The following describes each state.
    --
    -- [PRIMARY]
    --     The task set is serving production traffic.
    --
    -- [ACTIVE]
    --     The task set isn\'t serving production traffic.
    --
    -- [DRAINING]
    --     The tasks in the task set are being stopped, and their corresponding
    --     targets are being deregistered from their target group.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ID of the task set.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the task set.
    taskSetArn :: Prelude.Maybe Prelude.Text,
    -- | The number of tasks in the task set that are in the @PENDING@ status
    -- during a deployment. A task in the @PENDING@ state is preparing to enter
    -- the @RUNNING@ state. A task set enters the @PENDING@ status when it
    -- launches for the first time or when it\'s restarted after being in the
    -- @STOPPED@ state.
    pendingCount :: Prelude.Maybe Prelude.Int,
    -- | The tag specified when a task set is started. If an CodeDeploy
    -- deployment created the task set, the @startedBy@ parameter is
    -- @CODE_DEPLOY@. If an external deployment created the task set, the
    -- startedBy field isn\'t used.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The capacity provider strategy that are associated with the task set.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | Details on a load balancer that are used with a task set.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | The launch type the tasks in the task set are using. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    launchType :: Prelude.Maybe LaunchType,
    -- | The number of tasks in the task set that are in the @RUNNING@ status
    -- during a deployment. A task in the @RUNNING@ state is running and ready
    -- for use.
    runningCount :: Prelude.Maybe Prelude.Int,
    -- | The Fargate platform version where the tasks in the task set are
    -- running. A platform version is only specified for tasks run on Fargate.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The computed desired count for the task set. This is calculated by
    -- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
    -- percentage. The result is always rounded up. For example, if the
    -- computed desired count is 1.2, it rounds up to 2 tasks.
    computedDesiredCount :: Prelude.Maybe Prelude.Int,
    -- | The Unix timestamp for the time when the task set stability status was
    -- retrieved.
    stabilityStatusAt :: Prelude.Maybe Data.POSIX,
    -- | A floating-point percentage of your desired number of tasks to place and
    -- keep running in the task set.
    scale :: Prelude.Maybe Scale,
    -- | The Amazon Resource Name (ARN) of the service the task set exists in.
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for the time when the task set was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Unix timestamp for the time when the task set was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'taskSet_tags' - The metadata that you apply to the task set to help you categorize and
-- organize them. Each tag consists of a key and an optional value. You
-- define both.
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
-- 'clusterArn', 'taskSet_clusterArn' - The Amazon Resource Name (ARN) of the cluster that the service that
-- hosts the task set exists in.
--
-- 'serviceRegistries', 'taskSet_serviceRegistries' - The details for the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
--
-- 'platformFamily', 'taskSet_platformFamily' - The operating system that your tasks in the set are running on. A
-- platform family is specified only for tasks that use the Fargate launch
-- type.
--
-- All tasks in the set must have the same value.
--
-- 'taskDefinition', 'taskSet_taskDefinition' - The task definition that the task set is using.
--
-- 'stabilityStatus', 'taskSet_stabilityStatus' - The stability status. This indicates whether the task set has reached a
-- steady state. If the following conditions are met, the task set sre in
-- @STEADY_STATE@:
--
-- -   The task @runningCount@ is equal to the @computedDesiredCount@.
--
-- -   The @pendingCount@ is @0@.
--
-- -   There are no tasks that are running on container instances in the
--     @DRAINING@ status.
--
-- -   All tasks are reporting a healthy status from the load balancers,
--     service discovery, and container health checks.
--
-- If any of those conditions aren\'t met, the stability status returns
-- @STABILIZING@.
--
-- 'externalId', 'taskSet_externalId' - The external ID associated with the task set.
--
-- If an CodeDeploy deployment created a task set, the @externalId@
-- parameter contains the CodeDeploy deployment ID.
--
-- If a task set is created for an external deployment and is associated
-- with a service discovery registry, the @externalId@ parameter contains
-- the @ECS_TASK_SET_EXTERNAL_ID@ Cloud Map attribute.
--
-- 'networkConfiguration', 'taskSet_networkConfiguration' - The network configuration for the task set.
--
-- 'status', 'taskSet_status' - The status of the task set. The following describes each state.
--
-- [PRIMARY]
--     The task set is serving production traffic.
--
-- [ACTIVE]
--     The task set isn\'t serving production traffic.
--
-- [DRAINING]
--     The tasks in the task set are being stopped, and their corresponding
--     targets are being deregistered from their target group.
--
-- 'id', 'taskSet_id' - The ID of the task set.
--
-- 'taskSetArn', 'taskSet_taskSetArn' - The Amazon Resource Name (ARN) of the task set.
--
-- 'pendingCount', 'taskSet_pendingCount' - The number of tasks in the task set that are in the @PENDING@ status
-- during a deployment. A task in the @PENDING@ state is preparing to enter
-- the @RUNNING@ state. A task set enters the @PENDING@ status when it
-- launches for the first time or when it\'s restarted after being in the
-- @STOPPED@ state.
--
-- 'startedBy', 'taskSet_startedBy' - The tag specified when a task set is started. If an CodeDeploy
-- deployment created the task set, the @startedBy@ parameter is
-- @CODE_DEPLOY@. If an external deployment created the task set, the
-- startedBy field isn\'t used.
--
-- 'capacityProviderStrategy', 'taskSet_capacityProviderStrategy' - The capacity provider strategy that are associated with the task set.
--
-- 'loadBalancers', 'taskSet_loadBalancers' - Details on a load balancer that are used with a task set.
--
-- 'launchType', 'taskSet_launchType' - The launch type the tasks in the task set are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'runningCount', 'taskSet_runningCount' - The number of tasks in the task set that are in the @RUNNING@ status
-- during a deployment. A task in the @RUNNING@ state is running and ready
-- for use.
--
-- 'platformVersion', 'taskSet_platformVersion' - The Fargate platform version where the tasks in the task set are
-- running. A platform version is only specified for tasks run on Fargate.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'computedDesiredCount', 'taskSet_computedDesiredCount' - The computed desired count for the task set. This is calculated by
-- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
-- percentage. The result is always rounded up. For example, if the
-- computed desired count is 1.2, it rounds up to 2 tasks.
--
-- 'stabilityStatusAt', 'taskSet_stabilityStatusAt' - The Unix timestamp for the time when the task set stability status was
-- retrieved.
--
-- 'scale', 'taskSet_scale' - A floating-point percentage of your desired number of tasks to place and
-- keep running in the task set.
--
-- 'serviceArn', 'taskSet_serviceArn' - The Amazon Resource Name (ARN) of the service the task set exists in.
--
-- 'createdAt', 'taskSet_createdAt' - The Unix timestamp for the time when the task set was created.
--
-- 'updatedAt', 'taskSet_updatedAt' - The Unix timestamp for the time when the task set was last updated.
newTaskSet ::
  TaskSet
newTaskSet =
  TaskSet'
    { tags = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      platformFamily = Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      stabilityStatus = Prelude.Nothing,
      externalId = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      taskSetArn = Prelude.Nothing,
      pendingCount = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      capacityProviderStrategy = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      launchType = Prelude.Nothing,
      runningCount = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      computedDesiredCount = Prelude.Nothing,
      stabilityStatusAt = Prelude.Nothing,
      scale = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The metadata that you apply to the task set to help you categorize and
-- organize them. Each tag consists of a key and an optional value. You
-- define both.
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
taskSet_tags :: Lens.Lens' TaskSet (Prelude.Maybe [Tag])
taskSet_tags = Lens.lens (\TaskSet' {tags} -> tags) (\s@TaskSet' {} a -> s {tags = a} :: TaskSet) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the cluster that the service that
-- hosts the task set exists in.
taskSet_clusterArn :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_clusterArn = Lens.lens (\TaskSet' {clusterArn} -> clusterArn) (\s@TaskSet' {} a -> s {clusterArn = a} :: TaskSet)

-- | The details for the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
taskSet_serviceRegistries :: Lens.Lens' TaskSet (Prelude.Maybe [ServiceRegistry])
taskSet_serviceRegistries = Lens.lens (\TaskSet' {serviceRegistries} -> serviceRegistries) (\s@TaskSet' {} a -> s {serviceRegistries = a} :: TaskSet) Prelude.. Lens.mapping Lens.coerced

-- | The operating system that your tasks in the set are running on. A
-- platform family is specified only for tasks that use the Fargate launch
-- type.
--
-- All tasks in the set must have the same value.
taskSet_platformFamily :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_platformFamily = Lens.lens (\TaskSet' {platformFamily} -> platformFamily) (\s@TaskSet' {} a -> s {platformFamily = a} :: TaskSet)

-- | The task definition that the task set is using.
taskSet_taskDefinition :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_taskDefinition = Lens.lens (\TaskSet' {taskDefinition} -> taskDefinition) (\s@TaskSet' {} a -> s {taskDefinition = a} :: TaskSet)

-- | The stability status. This indicates whether the task set has reached a
-- steady state. If the following conditions are met, the task set sre in
-- @STEADY_STATE@:
--
-- -   The task @runningCount@ is equal to the @computedDesiredCount@.
--
-- -   The @pendingCount@ is @0@.
--
-- -   There are no tasks that are running on container instances in the
--     @DRAINING@ status.
--
-- -   All tasks are reporting a healthy status from the load balancers,
--     service discovery, and container health checks.
--
-- If any of those conditions aren\'t met, the stability status returns
-- @STABILIZING@.
taskSet_stabilityStatus :: Lens.Lens' TaskSet (Prelude.Maybe StabilityStatus)
taskSet_stabilityStatus = Lens.lens (\TaskSet' {stabilityStatus} -> stabilityStatus) (\s@TaskSet' {} a -> s {stabilityStatus = a} :: TaskSet)

-- | The external ID associated with the task set.
--
-- If an CodeDeploy deployment created a task set, the @externalId@
-- parameter contains the CodeDeploy deployment ID.
--
-- If a task set is created for an external deployment and is associated
-- with a service discovery registry, the @externalId@ parameter contains
-- the @ECS_TASK_SET_EXTERNAL_ID@ Cloud Map attribute.
taskSet_externalId :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_externalId = Lens.lens (\TaskSet' {externalId} -> externalId) (\s@TaskSet' {} a -> s {externalId = a} :: TaskSet)

-- | The network configuration for the task set.
taskSet_networkConfiguration :: Lens.Lens' TaskSet (Prelude.Maybe NetworkConfiguration)
taskSet_networkConfiguration = Lens.lens (\TaskSet' {networkConfiguration} -> networkConfiguration) (\s@TaskSet' {} a -> s {networkConfiguration = a} :: TaskSet)

-- | The status of the task set. The following describes each state.
--
-- [PRIMARY]
--     The task set is serving production traffic.
--
-- [ACTIVE]
--     The task set isn\'t serving production traffic.
--
-- [DRAINING]
--     The tasks in the task set are being stopped, and their corresponding
--     targets are being deregistered from their target group.
taskSet_status :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_status = Lens.lens (\TaskSet' {status} -> status) (\s@TaskSet' {} a -> s {status = a} :: TaskSet)

-- | The ID of the task set.
taskSet_id :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_id = Lens.lens (\TaskSet' {id} -> id) (\s@TaskSet' {} a -> s {id = a} :: TaskSet)

-- | The Amazon Resource Name (ARN) of the task set.
taskSet_taskSetArn :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_taskSetArn = Lens.lens (\TaskSet' {taskSetArn} -> taskSetArn) (\s@TaskSet' {} a -> s {taskSetArn = a} :: TaskSet)

-- | The number of tasks in the task set that are in the @PENDING@ status
-- during a deployment. A task in the @PENDING@ state is preparing to enter
-- the @RUNNING@ state. A task set enters the @PENDING@ status when it
-- launches for the first time or when it\'s restarted after being in the
-- @STOPPED@ state.
taskSet_pendingCount :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Int)
taskSet_pendingCount = Lens.lens (\TaskSet' {pendingCount} -> pendingCount) (\s@TaskSet' {} a -> s {pendingCount = a} :: TaskSet)

-- | The tag specified when a task set is started. If an CodeDeploy
-- deployment created the task set, the @startedBy@ parameter is
-- @CODE_DEPLOY@. If an external deployment created the task set, the
-- startedBy field isn\'t used.
taskSet_startedBy :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_startedBy = Lens.lens (\TaskSet' {startedBy} -> startedBy) (\s@TaskSet' {} a -> s {startedBy = a} :: TaskSet)

-- | The capacity provider strategy that are associated with the task set.
taskSet_capacityProviderStrategy :: Lens.Lens' TaskSet (Prelude.Maybe [CapacityProviderStrategyItem])
taskSet_capacityProviderStrategy = Lens.lens (\TaskSet' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@TaskSet' {} a -> s {capacityProviderStrategy = a} :: TaskSet) Prelude.. Lens.mapping Lens.coerced

-- | Details on a load balancer that are used with a task set.
taskSet_loadBalancers :: Lens.Lens' TaskSet (Prelude.Maybe [LoadBalancer])
taskSet_loadBalancers = Lens.lens (\TaskSet' {loadBalancers} -> loadBalancers) (\s@TaskSet' {} a -> s {loadBalancers = a} :: TaskSet) Prelude.. Lens.mapping Lens.coerced

-- | The launch type the tasks in the task set are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskSet_launchType :: Lens.Lens' TaskSet (Prelude.Maybe LaunchType)
taskSet_launchType = Lens.lens (\TaskSet' {launchType} -> launchType) (\s@TaskSet' {} a -> s {launchType = a} :: TaskSet)

-- | The number of tasks in the task set that are in the @RUNNING@ status
-- during a deployment. A task in the @RUNNING@ state is running and ready
-- for use.
taskSet_runningCount :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Int)
taskSet_runningCount = Lens.lens (\TaskSet' {runningCount} -> runningCount) (\s@TaskSet' {} a -> s {runningCount = a} :: TaskSet)

-- | The Fargate platform version where the tasks in the task set are
-- running. A platform version is only specified for tasks run on Fargate.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskSet_platformVersion :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_platformVersion = Lens.lens (\TaskSet' {platformVersion} -> platformVersion) (\s@TaskSet' {} a -> s {platformVersion = a} :: TaskSet)

-- | The computed desired count for the task set. This is calculated by
-- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
-- percentage. The result is always rounded up. For example, if the
-- computed desired count is 1.2, it rounds up to 2 tasks.
taskSet_computedDesiredCount :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Int)
taskSet_computedDesiredCount = Lens.lens (\TaskSet' {computedDesiredCount} -> computedDesiredCount) (\s@TaskSet' {} a -> s {computedDesiredCount = a} :: TaskSet)

-- | The Unix timestamp for the time when the task set stability status was
-- retrieved.
taskSet_stabilityStatusAt :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.UTCTime)
taskSet_stabilityStatusAt = Lens.lens (\TaskSet' {stabilityStatusAt} -> stabilityStatusAt) (\s@TaskSet' {} a -> s {stabilityStatusAt = a} :: TaskSet) Prelude.. Lens.mapping Data._Time

-- | A floating-point percentage of your desired number of tasks to place and
-- keep running in the task set.
taskSet_scale :: Lens.Lens' TaskSet (Prelude.Maybe Scale)
taskSet_scale = Lens.lens (\TaskSet' {scale} -> scale) (\s@TaskSet' {} a -> s {scale = a} :: TaskSet)

-- | The Amazon Resource Name (ARN) of the service the task set exists in.
taskSet_serviceArn :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_serviceArn = Lens.lens (\TaskSet' {serviceArn} -> serviceArn) (\s@TaskSet' {} a -> s {serviceArn = a} :: TaskSet)

-- | The Unix timestamp for the time when the task set was created.
taskSet_createdAt :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.UTCTime)
taskSet_createdAt = Lens.lens (\TaskSet' {createdAt} -> createdAt) (\s@TaskSet' {} a -> s {createdAt = a} :: TaskSet) Prelude.. Lens.mapping Data._Time

-- | The Unix timestamp for the time when the task set was last updated.
taskSet_updatedAt :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.UTCTime)
taskSet_updatedAt = Lens.lens (\TaskSet' {updatedAt} -> updatedAt) (\s@TaskSet' {} a -> s {updatedAt = a} :: TaskSet) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TaskSet where
  parseJSON =
    Data.withObject
      "TaskSet"
      ( \x ->
          TaskSet'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "clusterArn")
            Prelude.<*> ( x Data..:? "serviceRegistries"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "platformFamily")
            Prelude.<*> (x Data..:? "taskDefinition")
            Prelude.<*> (x Data..:? "stabilityStatus")
            Prelude.<*> (x Data..:? "externalId")
            Prelude.<*> (x Data..:? "networkConfiguration")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "taskSetArn")
            Prelude.<*> (x Data..:? "pendingCount")
            Prelude.<*> (x Data..:? "startedBy")
            Prelude.<*> ( x Data..:? "capacityProviderStrategy"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "loadBalancers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "launchType")
            Prelude.<*> (x Data..:? "runningCount")
            Prelude.<*> (x Data..:? "platformVersion")
            Prelude.<*> (x Data..:? "computedDesiredCount")
            Prelude.<*> (x Data..:? "stabilityStatusAt")
            Prelude.<*> (x Data..:? "scale")
            Prelude.<*> (x Data..:? "serviceArn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable TaskSet where
  hashWithSalt _salt TaskSet' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` serviceRegistries
      `Prelude.hashWithSalt` platformFamily
      `Prelude.hashWithSalt` taskDefinition
      `Prelude.hashWithSalt` stabilityStatus
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` taskSetArn
      `Prelude.hashWithSalt` pendingCount
      `Prelude.hashWithSalt` startedBy
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` loadBalancers
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` runningCount
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` computedDesiredCount
      `Prelude.hashWithSalt` stabilityStatusAt
      `Prelude.hashWithSalt` scale
      `Prelude.hashWithSalt` serviceArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData TaskSet where
  rnf TaskSet' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf serviceRegistries
      `Prelude.seq` Prelude.rnf platformFamily
      `Prelude.seq` Prelude.rnf taskDefinition
      `Prelude.seq` Prelude.rnf stabilityStatus
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf taskSetArn
      `Prelude.seq` Prelude.rnf pendingCount
      `Prelude.seq` Prelude.rnf startedBy
      `Prelude.seq` Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf runningCount
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf computedDesiredCount
      `Prelude.seq` Prelude.rnf stabilityStatusAt
      `Prelude.seq` Prelude.rnf scale
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
