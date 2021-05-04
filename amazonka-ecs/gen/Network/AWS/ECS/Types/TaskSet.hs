{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.Scale
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.StabilityStatus
import Network.AWS.ECS.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a set of Amazon ECS tasks in either an AWS CodeDeploy
-- or an @EXTERNAL@ deployment. An Amazon ECS task set includes details
-- such as the desired number of tasks, how many tasks are running, and
-- whether the task set serves production traffic.
--
-- /See:/ 'newTaskSet' smart constructor.
data TaskSet = TaskSet'
  { -- | The Amazon Resource Name (ARN) of the cluster that the service that
    -- hosts the task set exists in.
    clusterArn :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for when the task set stability status was retrieved.
    stabilityStatusAt :: Prelude.Maybe Prelude.POSIX,
    -- | The number of tasks in the task set that are in the @RUNNING@ status
    -- during a deployment. A task in the @RUNNING@ state is running and ready
    -- for use.
    runningCount :: Prelude.Maybe Prelude.Int,
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
    stabilityStatus :: Prelude.Maybe StabilityStatus,
    -- | The network configuration for the task set.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The capacity provider strategy associated with the task set.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | The Unix timestamp for when the task set was last updated.
    updatedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The launch type the tasks in the task set are using. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    launchType :: Prelude.Maybe LaunchType,
    -- | The Unix timestamp for when the task set was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The platform version on which the tasks in the task set are running. A
    -- platform version is only specified for tasks using the Fargate launch
    -- type. If one is not specified, the @LATEST@ platform version is used by
    -- default. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the task set.
    id :: Prelude.Maybe Prelude.Text,
    -- | The tag specified when a task set is started. If the task set is created
    -- by an AWS CodeDeploy deployment, the @startedBy@ parameter is
    -- @CODE_DEPLOY@. For a task set created for an external deployment, the
    -- startedBy field isn\'t used.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The computed desired count for the task set. This is calculated by
    -- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
    -- percentage. The result is always rounded up. For example, if the
    -- computed desired count is 1.2, it rounds up to 2 tasks.
    computedDesiredCount :: Prelude.Maybe Prelude.Int,
    -- | The number of tasks in the task set that are in the @PENDING@ status
    -- during a deployment. A task in the @PENDING@ state is preparing to enter
    -- the @RUNNING@ state. A task set enters the @PENDING@ status when it
    -- launches for the first time or when it is restarted after being in the
    -- @STOPPED@ state.
    pendingCount :: Prelude.Maybe Prelude.Int,
    -- | Details on a load balancer that is used with a task set.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
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
    tags :: Prelude.Maybe [Tag],
    -- | The details of the service discovery registries to assign to this task
    -- set. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
    serviceRegistries :: Prelude.Maybe [ServiceRegistry],
    -- | The task definition the task set is using.
    taskDefinition :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service the task set exists in.
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | The external ID associated with the task set.
    --
    -- If a task set is created by an AWS CodeDeploy deployment, the
    -- @externalId@ parameter contains the AWS CodeDeploy deployment ID.
    --
    -- If a task set is created for an external deployment and is associated
    -- with a service discovery registry, the @externalId@ parameter contains
    -- the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | A floating-point percentage of the desired number of tasks to place and
    -- keep running in the task set.
    scale :: Prelude.Maybe Scale,
    -- | The Amazon Resource Name (ARN) of the task set.
    taskSetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { clusterArn = Prelude.Nothing,
      status = Prelude.Nothing,
      stabilityStatusAt = Prelude.Nothing,
      runningCount = Prelude.Nothing,
      stabilityStatus = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      capacityProviderStrategy = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      launchType = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      id = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      computedDesiredCount = Prelude.Nothing,
      pendingCount = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      tags = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      externalId = Prelude.Nothing,
      scale = Prelude.Nothing,
      taskSetArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the cluster that the service that
-- hosts the task set exists in.
taskSet_clusterArn :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
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
taskSet_status :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_status = Lens.lens (\TaskSet' {status} -> status) (\s@TaskSet' {} a -> s {status = a} :: TaskSet)

-- | The Unix timestamp for when the task set stability status was retrieved.
taskSet_stabilityStatusAt :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.UTCTime)
taskSet_stabilityStatusAt = Lens.lens (\TaskSet' {stabilityStatusAt} -> stabilityStatusAt) (\s@TaskSet' {} a -> s {stabilityStatusAt = a} :: TaskSet) Prelude.. Lens.mapping Prelude._Time

-- | The number of tasks in the task set that are in the @RUNNING@ status
-- during a deployment. A task in the @RUNNING@ state is running and ready
-- for use.
taskSet_runningCount :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Int)
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
taskSet_stabilityStatus :: Lens.Lens' TaskSet (Prelude.Maybe StabilityStatus)
taskSet_stabilityStatus = Lens.lens (\TaskSet' {stabilityStatus} -> stabilityStatus) (\s@TaskSet' {} a -> s {stabilityStatus = a} :: TaskSet)

-- | The network configuration for the task set.
taskSet_networkConfiguration :: Lens.Lens' TaskSet (Prelude.Maybe NetworkConfiguration)
taskSet_networkConfiguration = Lens.lens (\TaskSet' {networkConfiguration} -> networkConfiguration) (\s@TaskSet' {} a -> s {networkConfiguration = a} :: TaskSet)

-- | The capacity provider strategy associated with the task set.
taskSet_capacityProviderStrategy :: Lens.Lens' TaskSet (Prelude.Maybe [CapacityProviderStrategyItem])
taskSet_capacityProviderStrategy = Lens.lens (\TaskSet' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@TaskSet' {} a -> s {capacityProviderStrategy = a} :: TaskSet) Prelude.. Lens.mapping Prelude._Coerce

-- | The Unix timestamp for when the task set was last updated.
taskSet_updatedAt :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.UTCTime)
taskSet_updatedAt = Lens.lens (\TaskSet' {updatedAt} -> updatedAt) (\s@TaskSet' {} a -> s {updatedAt = a} :: TaskSet) Prelude.. Lens.mapping Prelude._Time

-- | The launch type the tasks in the task set are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskSet_launchType :: Lens.Lens' TaskSet (Prelude.Maybe LaunchType)
taskSet_launchType = Lens.lens (\TaskSet' {launchType} -> launchType) (\s@TaskSet' {} a -> s {launchType = a} :: TaskSet)

-- | The Unix timestamp for when the task set was created.
taskSet_createdAt :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.UTCTime)
taskSet_createdAt = Lens.lens (\TaskSet' {createdAt} -> createdAt) (\s@TaskSet' {} a -> s {createdAt = a} :: TaskSet) Prelude.. Lens.mapping Prelude._Time

-- | The platform version on which the tasks in the task set are running. A
-- platform version is only specified for tasks using the Fargate launch
-- type. If one is not specified, the @LATEST@ platform version is used by
-- default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskSet_platformVersion :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_platformVersion = Lens.lens (\TaskSet' {platformVersion} -> platformVersion) (\s@TaskSet' {} a -> s {platformVersion = a} :: TaskSet)

-- | The ID of the task set.
taskSet_id :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_id = Lens.lens (\TaskSet' {id} -> id) (\s@TaskSet' {} a -> s {id = a} :: TaskSet)

-- | The tag specified when a task set is started. If the task set is created
-- by an AWS CodeDeploy deployment, the @startedBy@ parameter is
-- @CODE_DEPLOY@. For a task set created for an external deployment, the
-- startedBy field isn\'t used.
taskSet_startedBy :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_startedBy = Lens.lens (\TaskSet' {startedBy} -> startedBy) (\s@TaskSet' {} a -> s {startedBy = a} :: TaskSet)

-- | The computed desired count for the task set. This is calculated by
-- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
-- percentage. The result is always rounded up. For example, if the
-- computed desired count is 1.2, it rounds up to 2 tasks.
taskSet_computedDesiredCount :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Int)
taskSet_computedDesiredCount = Lens.lens (\TaskSet' {computedDesiredCount} -> computedDesiredCount) (\s@TaskSet' {} a -> s {computedDesiredCount = a} :: TaskSet)

-- | The number of tasks in the task set that are in the @PENDING@ status
-- during a deployment. A task in the @PENDING@ state is preparing to enter
-- the @RUNNING@ state. A task set enters the @PENDING@ status when it
-- launches for the first time or when it is restarted after being in the
-- @STOPPED@ state.
taskSet_pendingCount :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Int)
taskSet_pendingCount = Lens.lens (\TaskSet' {pendingCount} -> pendingCount) (\s@TaskSet' {} a -> s {pendingCount = a} :: TaskSet)

-- | Details on a load balancer that is used with a task set.
taskSet_loadBalancers :: Lens.Lens' TaskSet (Prelude.Maybe [LoadBalancer])
taskSet_loadBalancers = Lens.lens (\TaskSet' {loadBalancers} -> loadBalancers) (\s@TaskSet' {} a -> s {loadBalancers = a} :: TaskSet) Prelude.. Lens.mapping Prelude._Coerce

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
taskSet_tags :: Lens.Lens' TaskSet (Prelude.Maybe [Tag])
taskSet_tags = Lens.lens (\TaskSet' {tags} -> tags) (\s@TaskSet' {} a -> s {tags = a} :: TaskSet) Prelude.. Lens.mapping Prelude._Coerce

-- | The details of the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service Discovery>.
taskSet_serviceRegistries :: Lens.Lens' TaskSet (Prelude.Maybe [ServiceRegistry])
taskSet_serviceRegistries = Lens.lens (\TaskSet' {serviceRegistries} -> serviceRegistries) (\s@TaskSet' {} a -> s {serviceRegistries = a} :: TaskSet) Prelude.. Lens.mapping Prelude._Coerce

-- | The task definition the task set is using.
taskSet_taskDefinition :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_taskDefinition = Lens.lens (\TaskSet' {taskDefinition} -> taskDefinition) (\s@TaskSet' {} a -> s {taskDefinition = a} :: TaskSet)

-- | The Amazon Resource Name (ARN) of the service the task set exists in.
taskSet_serviceArn :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_serviceArn = Lens.lens (\TaskSet' {serviceArn} -> serviceArn) (\s@TaskSet' {} a -> s {serviceArn = a} :: TaskSet)

-- | The external ID associated with the task set.
--
-- If a task set is created by an AWS CodeDeploy deployment, the
-- @externalId@ parameter contains the AWS CodeDeploy deployment ID.
--
-- If a task set is created for an external deployment and is associated
-- with a service discovery registry, the @externalId@ parameter contains
-- the @ECS_TASK_SET_EXTERNAL_ID@ AWS Cloud Map attribute.
taskSet_externalId :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_externalId = Lens.lens (\TaskSet' {externalId} -> externalId) (\s@TaskSet' {} a -> s {externalId = a} :: TaskSet)

-- | A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
taskSet_scale :: Lens.Lens' TaskSet (Prelude.Maybe Scale)
taskSet_scale = Lens.lens (\TaskSet' {scale} -> scale) (\s@TaskSet' {} a -> s {scale = a} :: TaskSet)

-- | The Amazon Resource Name (ARN) of the task set.
taskSet_taskSetArn :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_taskSetArn = Lens.lens (\TaskSet' {taskSetArn} -> taskSetArn) (\s@TaskSet' {} a -> s {taskSetArn = a} :: TaskSet)

instance Prelude.FromJSON TaskSet where
  parseJSON =
    Prelude.withObject
      "TaskSet"
      ( \x ->
          TaskSet'
            Prelude.<$> (x Prelude..:? "clusterArn")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "stabilityStatusAt")
            Prelude.<*> (x Prelude..:? "runningCount")
            Prelude.<*> (x Prelude..:? "stabilityStatus")
            Prelude.<*> (x Prelude..:? "networkConfiguration")
            Prelude.<*> ( x Prelude..:? "capacityProviderStrategy"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "updatedAt")
            Prelude.<*> (x Prelude..:? "launchType")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "platformVersion")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "startedBy")
            Prelude.<*> (x Prelude..:? "computedDesiredCount")
            Prelude.<*> (x Prelude..:? "pendingCount")
            Prelude.<*> ( x Prelude..:? "loadBalancers"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "serviceRegistries"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "taskDefinition")
            Prelude.<*> (x Prelude..:? "serviceArn")
            Prelude.<*> (x Prelude..:? "externalId")
            Prelude.<*> (x Prelude..:? "scale")
            Prelude.<*> (x Prelude..:? "taskSetArn")
      )

instance Prelude.Hashable TaskSet

instance Prelude.NFData TaskSet
