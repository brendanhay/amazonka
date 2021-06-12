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
-- Module      : Network.AWS.ECS.Types.TaskSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskSet where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.Scale
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.StabilityStatus
import Network.AWS.ECS.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Information about a set of Amazon ECS tasks in either an AWS CodeDeploy
-- or an @EXTERNAL@ deployment. An Amazon ECS task set includes details
-- such as the desired number of tasks, how many tasks are running, and
-- whether the task set serves production traffic.
--
-- /See:/ 'newTaskSet' smart constructor.
data TaskSet = TaskSet'
  { -- | The Amazon Resource Name (ARN) of the cluster that the service that
    -- hosts the task set exists in.
    clusterArn :: Core.Maybe Core.Text,
    -- | The status of the task set. The following describes each state:
    --
    -- [PRIMARY]
    --     The task set is serving production traffic.
    --
    -- [ACTIVE]
    --     The task set is not serving production traffic.
    --
    -- [DRAINING]
    --     The tasks in the task set are being stopped and their corresponding
    --     targets are being deregistered from their target group.
    status :: Core.Maybe Core.Text,
    -- | The Unix timestamp for when the task set stability status was retrieved.
    stabilityStatusAt :: Core.Maybe Core.POSIX,
    -- | The number of tasks in the task set that are in the @RUNNING@ status
    -- during a deployment. A task in the @RUNNING@ state is running and ready
    -- for use.
    runningCount :: Core.Maybe Core.Int,
    -- | The stability status, which indicates whether the task set has reached a
    -- steady state. If the following conditions are met, the task set will be
    -- in @STEADY_STATE@:
    --
    -- -   The task @runningCount@ is equal to the @computedDesiredCount@.
    --
    -- -   The @pendingCount@ is @0@.
    --
    -- -   There are no tasks running on container instances in the @DRAINING@
    --     status.
    --
    -- -   All tasks are reporting a healthy status from the load balancers,
    --     service discovery, and container health checks.
    --
    -- If any of those conditions are not met, the stability status returns
    -- @STABILIZING@.
    stabilityStatus :: Core.Maybe StabilityStatus,
    -- | The network configuration for the task set.
    networkConfiguration :: Core.Maybe NetworkConfiguration,
    -- | The capacity provider strategy associated with the task set.
    capacityProviderStrategy :: Core.Maybe [CapacityProviderStrategyItem],
    -- | The Unix timestamp for when the task set was last updated.
    updatedAt :: Core.Maybe Core.POSIX,
    -- | The launch type the tasks in the task set are using. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    launchType :: Core.Maybe LaunchType,
    -- | The Unix timestamp for when the task set was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The platform version on which the tasks in the task set are running. A
    -- platform version is only specified for tasks using the Fargate launch
    -- type. If one is not specified, the @LATEST@ platform version is used by
    -- default. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Core.Maybe Core.Text,
    -- | The ID of the task set.
    id :: Core.Maybe Core.Text,
    -- | The tag specified when a task set is started. If the task set is created
    -- by an AWS CodeDeploy deployment, the @startedBy@ parameter is
    -- @CODE_DEPLOY@. For a task set created for an external deployment, the
    -- startedBy field isn\'t used.
    startedBy :: Core.Maybe Core.Text,
    -- | The computed desired count for the task set. This is calculated by
    -- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
    -- percentage. The result is always rounded up. For example, if the
    -- computed desired count is 1.2, it rounds up to 2 tasks.
    computedDesiredCount :: Core.Maybe Core.Int,
    -- | The number of tasks in the task set that are in the @PENDING@ status
    -- during a deployment. A task in the @PENDING@ state is preparing to enter
    -- the @RUNNING@ state. A task set enters the @PENDING@ status when it
    -- launches for the first time or when it is restarted after being in the
    -- @STOPPED@ state.
    pendingCount :: Core.Maybe Core.Int,
    -- | Details on a load balancer that is used with a task set.
    loadBalancers :: Core.Maybe [LoadBalancer],
    -- | The metadata that you apply to the task set to help you categorize and
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
    -- | The details of the service discovery registries to assign to this task
    -- set. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
    serviceRegistries :: Core.Maybe [ServiceRegistry],
    -- | The task definition the task set is using.
    taskDefinition :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the service the task set exists in.
    serviceArn :: Core.Maybe Core.Text,
    -- | The external ID associated with the task set.
    --
    -- If a task set is created by an AWS CodeDeploy deployment, the
    -- @externalId@ parameter contains the AWS CodeDeploy deployment ID.
    --
    -- If a task set is created for an external deployment and is associated
    -- with a service discovery registry, the @externalId@ parameter contains
    -- the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
    externalId :: Core.Maybe Core.Text,
    -- | A floating-point percentage of the desired number of tasks to place and
    -- keep running in the task set.
    scale :: Core.Maybe Scale,
    -- | The Amazon Resource Name (ARN) of the task set.
    taskSetArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TaskSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'taskSet_clusterArn' - The Amazon Resource Name (ARN) of the cluster that the service that
-- hosts the task set exists in.
--
-- 'status', 'taskSet_status' - The status of the task set. The following describes each state:
--
-- [PRIMARY]
--     The task set is serving production traffic.
--
-- [ACTIVE]
--     The task set is not serving production traffic.
--
-- [DRAINING]
--     The tasks in the task set are being stopped and their corresponding
--     targets are being deregistered from their target group.
--
-- 'stabilityStatusAt', 'taskSet_stabilityStatusAt' - The Unix timestamp for when the task set stability status was retrieved.
--
-- 'runningCount', 'taskSet_runningCount' - The number of tasks in the task set that are in the @RUNNING@ status
-- during a deployment. A task in the @RUNNING@ state is running and ready
-- for use.
--
-- 'stabilityStatus', 'taskSet_stabilityStatus' - The stability status, which indicates whether the task set has reached a
-- steady state. If the following conditions are met, the task set will be
-- in @STEADY_STATE@:
--
-- -   The task @runningCount@ is equal to the @computedDesiredCount@.
--
-- -   The @pendingCount@ is @0@.
--
-- -   There are no tasks running on container instances in the @DRAINING@
--     status.
--
-- -   All tasks are reporting a healthy status from the load balancers,
--     service discovery, and container health checks.
--
-- If any of those conditions are not met, the stability status returns
-- @STABILIZING@.
--
-- 'networkConfiguration', 'taskSet_networkConfiguration' - The network configuration for the task set.
--
-- 'capacityProviderStrategy', 'taskSet_capacityProviderStrategy' - The capacity provider strategy associated with the task set.
--
-- 'updatedAt', 'taskSet_updatedAt' - The Unix timestamp for when the task set was last updated.
--
-- 'launchType', 'taskSet_launchType' - The launch type the tasks in the task set are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'createdAt', 'taskSet_createdAt' - The Unix timestamp for when the task set was created.
--
-- 'platformVersion', 'taskSet_platformVersion' - The platform version on which the tasks in the task set are running. A
-- platform version is only specified for tasks using the Fargate launch
-- type. If one is not specified, the @LATEST@ platform version is used by
-- default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'id', 'taskSet_id' - The ID of the task set.
--
-- 'startedBy', 'taskSet_startedBy' - The tag specified when a task set is started. If the task set is created
-- by an AWS CodeDeploy deployment, the @startedBy@ parameter is
-- @CODE_DEPLOY@. For a task set created for an external deployment, the
-- startedBy field isn\'t used.
--
-- 'computedDesiredCount', 'taskSet_computedDesiredCount' - The computed desired count for the task set. This is calculated by
-- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
-- percentage. The result is always rounded up. For example, if the
-- computed desired count is 1.2, it rounds up to 2 tasks.
--
-- 'pendingCount', 'taskSet_pendingCount' - The number of tasks in the task set that are in the @PENDING@ status
-- during a deployment. A task in the @PENDING@ state is preparing to enter
-- the @RUNNING@ state. A task set enters the @PENDING@ status when it
-- launches for the first time or when it is restarted after being in the
-- @STOPPED@ state.
--
-- 'loadBalancers', 'taskSet_loadBalancers' - Details on a load balancer that is used with a task set.
--
-- 'tags', 'taskSet_tags' - The metadata that you apply to the task set to help you categorize and
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
-- 'serviceRegistries', 'taskSet_serviceRegistries' - The details of the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
--
-- 'taskDefinition', 'taskSet_taskDefinition' - The task definition the task set is using.
--
-- 'serviceArn', 'taskSet_serviceArn' - The Amazon Resource Name (ARN) of the service the task set exists in.
--
-- 'externalId', 'taskSet_externalId' - The external ID associated with the task set.
--
-- If a task set is created by an AWS CodeDeploy deployment, the
-- @externalId@ parameter contains the AWS CodeDeploy deployment ID.
--
-- If a task set is created for an external deployment and is associated
-- with a service discovery registry, the @externalId@ parameter contains
-- the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
--
-- 'scale', 'taskSet_scale' - A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
--
-- 'taskSetArn', 'taskSet_taskSetArn' - The Amazon Resource Name (ARN) of the task set.
newTaskSet ::
  TaskSet
newTaskSet =
  TaskSet'
    { clusterArn = Core.Nothing,
      status = Core.Nothing,
      stabilityStatusAt = Core.Nothing,
      runningCount = Core.Nothing,
      stabilityStatus = Core.Nothing,
      networkConfiguration = Core.Nothing,
      capacityProviderStrategy = Core.Nothing,
      updatedAt = Core.Nothing,
      launchType = Core.Nothing,
      createdAt = Core.Nothing,
      platformVersion = Core.Nothing,
      id = Core.Nothing,
      startedBy = Core.Nothing,
      computedDesiredCount = Core.Nothing,
      pendingCount = Core.Nothing,
      loadBalancers = Core.Nothing,
      tags = Core.Nothing,
      serviceRegistries = Core.Nothing,
      taskDefinition = Core.Nothing,
      serviceArn = Core.Nothing,
      externalId = Core.Nothing,
      scale = Core.Nothing,
      taskSetArn = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the cluster that the service that
-- hosts the task set exists in.
taskSet_clusterArn :: Lens.Lens' TaskSet (Core.Maybe Core.Text)
taskSet_clusterArn = Lens.lens (\TaskSet' {clusterArn} -> clusterArn) (\s@TaskSet' {} a -> s {clusterArn = a} :: TaskSet)

-- | The status of the task set. The following describes each state:
--
-- [PRIMARY]
--     The task set is serving production traffic.
--
-- [ACTIVE]
--     The task set is not serving production traffic.
--
-- [DRAINING]
--     The tasks in the task set are being stopped and their corresponding
--     targets are being deregistered from their target group.
taskSet_status :: Lens.Lens' TaskSet (Core.Maybe Core.Text)
taskSet_status = Lens.lens (\TaskSet' {status} -> status) (\s@TaskSet' {} a -> s {status = a} :: TaskSet)

-- | The Unix timestamp for when the task set stability status was retrieved.
taskSet_stabilityStatusAt :: Lens.Lens' TaskSet (Core.Maybe Core.UTCTime)
taskSet_stabilityStatusAt = Lens.lens (\TaskSet' {stabilityStatusAt} -> stabilityStatusAt) (\s@TaskSet' {} a -> s {stabilityStatusAt = a} :: TaskSet) Core.. Lens.mapping Core._Time

-- | The number of tasks in the task set that are in the @RUNNING@ status
-- during a deployment. A task in the @RUNNING@ state is running and ready
-- for use.
taskSet_runningCount :: Lens.Lens' TaskSet (Core.Maybe Core.Int)
taskSet_runningCount = Lens.lens (\TaskSet' {runningCount} -> runningCount) (\s@TaskSet' {} a -> s {runningCount = a} :: TaskSet)

-- | The stability status, which indicates whether the task set has reached a
-- steady state. If the following conditions are met, the task set will be
-- in @STEADY_STATE@:
--
-- -   The task @runningCount@ is equal to the @computedDesiredCount@.
--
-- -   The @pendingCount@ is @0@.
--
-- -   There are no tasks running on container instances in the @DRAINING@
--     status.
--
-- -   All tasks are reporting a healthy status from the load balancers,
--     service discovery, and container health checks.
--
-- If any of those conditions are not met, the stability status returns
-- @STABILIZING@.
taskSet_stabilityStatus :: Lens.Lens' TaskSet (Core.Maybe StabilityStatus)
taskSet_stabilityStatus = Lens.lens (\TaskSet' {stabilityStatus} -> stabilityStatus) (\s@TaskSet' {} a -> s {stabilityStatus = a} :: TaskSet)

-- | The network configuration for the task set.
taskSet_networkConfiguration :: Lens.Lens' TaskSet (Core.Maybe NetworkConfiguration)
taskSet_networkConfiguration = Lens.lens (\TaskSet' {networkConfiguration} -> networkConfiguration) (\s@TaskSet' {} a -> s {networkConfiguration = a} :: TaskSet)

-- | The capacity provider strategy associated with the task set.
taskSet_capacityProviderStrategy :: Lens.Lens' TaskSet (Core.Maybe [CapacityProviderStrategyItem])
taskSet_capacityProviderStrategy = Lens.lens (\TaskSet' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@TaskSet' {} a -> s {capacityProviderStrategy = a} :: TaskSet) Core.. Lens.mapping Lens._Coerce

-- | The Unix timestamp for when the task set was last updated.
taskSet_updatedAt :: Lens.Lens' TaskSet (Core.Maybe Core.UTCTime)
taskSet_updatedAt = Lens.lens (\TaskSet' {updatedAt} -> updatedAt) (\s@TaskSet' {} a -> s {updatedAt = a} :: TaskSet) Core.. Lens.mapping Core._Time

-- | The launch type the tasks in the task set are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskSet_launchType :: Lens.Lens' TaskSet (Core.Maybe LaunchType)
taskSet_launchType = Lens.lens (\TaskSet' {launchType} -> launchType) (\s@TaskSet' {} a -> s {launchType = a} :: TaskSet)

-- | The Unix timestamp for when the task set was created.
taskSet_createdAt :: Lens.Lens' TaskSet (Core.Maybe Core.UTCTime)
taskSet_createdAt = Lens.lens (\TaskSet' {createdAt} -> createdAt) (\s@TaskSet' {} a -> s {createdAt = a} :: TaskSet) Core.. Lens.mapping Core._Time

-- | The platform version on which the tasks in the task set are running. A
-- platform version is only specified for tasks using the Fargate launch
-- type. If one is not specified, the @LATEST@ platform version is used by
-- default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskSet_platformVersion :: Lens.Lens' TaskSet (Core.Maybe Core.Text)
taskSet_platformVersion = Lens.lens (\TaskSet' {platformVersion} -> platformVersion) (\s@TaskSet' {} a -> s {platformVersion = a} :: TaskSet)

-- | The ID of the task set.
taskSet_id :: Lens.Lens' TaskSet (Core.Maybe Core.Text)
taskSet_id = Lens.lens (\TaskSet' {id} -> id) (\s@TaskSet' {} a -> s {id = a} :: TaskSet)

-- | The tag specified when a task set is started. If the task set is created
-- by an AWS CodeDeploy deployment, the @startedBy@ parameter is
-- @CODE_DEPLOY@. For a task set created for an external deployment, the
-- startedBy field isn\'t used.
taskSet_startedBy :: Lens.Lens' TaskSet (Core.Maybe Core.Text)
taskSet_startedBy = Lens.lens (\TaskSet' {startedBy} -> startedBy) (\s@TaskSet' {} a -> s {startedBy = a} :: TaskSet)

-- | The computed desired count for the task set. This is calculated by
-- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
-- percentage. The result is always rounded up. For example, if the
-- computed desired count is 1.2, it rounds up to 2 tasks.
taskSet_computedDesiredCount :: Lens.Lens' TaskSet (Core.Maybe Core.Int)
taskSet_computedDesiredCount = Lens.lens (\TaskSet' {computedDesiredCount} -> computedDesiredCount) (\s@TaskSet' {} a -> s {computedDesiredCount = a} :: TaskSet)

-- | The number of tasks in the task set that are in the @PENDING@ status
-- during a deployment. A task in the @PENDING@ state is preparing to enter
-- the @RUNNING@ state. A task set enters the @PENDING@ status when it
-- launches for the first time or when it is restarted after being in the
-- @STOPPED@ state.
taskSet_pendingCount :: Lens.Lens' TaskSet (Core.Maybe Core.Int)
taskSet_pendingCount = Lens.lens (\TaskSet' {pendingCount} -> pendingCount) (\s@TaskSet' {} a -> s {pendingCount = a} :: TaskSet)

-- | Details on a load balancer that is used with a task set.
taskSet_loadBalancers :: Lens.Lens' TaskSet (Core.Maybe [LoadBalancer])
taskSet_loadBalancers = Lens.lens (\TaskSet' {loadBalancers} -> loadBalancers) (\s@TaskSet' {} a -> s {loadBalancers = a} :: TaskSet) Core.. Lens.mapping Lens._Coerce

-- | The metadata that you apply to the task set to help you categorize and
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
taskSet_tags :: Lens.Lens' TaskSet (Core.Maybe [Tag])
taskSet_tags = Lens.lens (\TaskSet' {tags} -> tags) (\s@TaskSet' {} a -> s {tags = a} :: TaskSet) Core.. Lens.mapping Lens._Coerce

-- | The details of the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
taskSet_serviceRegistries :: Lens.Lens' TaskSet (Core.Maybe [ServiceRegistry])
taskSet_serviceRegistries = Lens.lens (\TaskSet' {serviceRegistries} -> serviceRegistries) (\s@TaskSet' {} a -> s {serviceRegistries = a} :: TaskSet) Core.. Lens.mapping Lens._Coerce

-- | The task definition the task set is using.
taskSet_taskDefinition :: Lens.Lens' TaskSet (Core.Maybe Core.Text)
taskSet_taskDefinition = Lens.lens (\TaskSet' {taskDefinition} -> taskDefinition) (\s@TaskSet' {} a -> s {taskDefinition = a} :: TaskSet)

-- | The Amazon Resource Name (ARN) of the service the task set exists in.
taskSet_serviceArn :: Lens.Lens' TaskSet (Core.Maybe Core.Text)
taskSet_serviceArn = Lens.lens (\TaskSet' {serviceArn} -> serviceArn) (\s@TaskSet' {} a -> s {serviceArn = a} :: TaskSet)

-- | The external ID associated with the task set.
--
-- If a task set is created by an AWS CodeDeploy deployment, the
-- @externalId@ parameter contains the AWS CodeDeploy deployment ID.
--
-- If a task set is created for an external deployment and is associated
-- with a service discovery registry, the @externalId@ parameter contains
-- the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
taskSet_externalId :: Lens.Lens' TaskSet (Core.Maybe Core.Text)
taskSet_externalId = Lens.lens (\TaskSet' {externalId} -> externalId) (\s@TaskSet' {} a -> s {externalId = a} :: TaskSet)

-- | A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
taskSet_scale :: Lens.Lens' TaskSet (Core.Maybe Scale)
taskSet_scale = Lens.lens (\TaskSet' {scale} -> scale) (\s@TaskSet' {} a -> s {scale = a} :: TaskSet)

-- | The Amazon Resource Name (ARN) of the task set.
taskSet_taskSetArn :: Lens.Lens' TaskSet (Core.Maybe Core.Text)
taskSet_taskSetArn = Lens.lens (\TaskSet' {taskSetArn} -> taskSetArn) (\s@TaskSet' {} a -> s {taskSetArn = a} :: TaskSet)

instance Core.FromJSON TaskSet where
  parseJSON =
    Core.withObject
      "TaskSet"
      ( \x ->
          TaskSet'
            Core.<$> (x Core..:? "clusterArn")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "stabilityStatusAt")
            Core.<*> (x Core..:? "runningCount")
            Core.<*> (x Core..:? "stabilityStatus")
            Core.<*> (x Core..:? "networkConfiguration")
            Core.<*> ( x Core..:? "capacityProviderStrategy"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "updatedAt")
            Core.<*> (x Core..:? "launchType")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "platformVersion")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "startedBy")
            Core.<*> (x Core..:? "computedDesiredCount")
            Core.<*> (x Core..:? "pendingCount")
            Core.<*> (x Core..:? "loadBalancers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "serviceRegistries" Core..!= Core.mempty)
            Core.<*> (x Core..:? "taskDefinition")
            Core.<*> (x Core..:? "serviceArn")
            Core.<*> (x Core..:? "externalId")
            Core.<*> (x Core..:? "scale")
            Core.<*> (x Core..:? "taskSetArn")
      )

instance Core.Hashable TaskSet

instance Core.NFData TaskSet
