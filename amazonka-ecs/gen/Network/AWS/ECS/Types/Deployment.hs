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
-- Module      : Network.AWS.ECS.Types.Deployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Deployment where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.DeploymentRolloutState
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.NetworkConfiguration
import qualified Network.AWS.Lens as Lens

-- | The details of an Amazon ECS service deployment. This is used only when
-- a service uses the @ECS@ deployment controller type.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The @rolloutState@ of a service is only returned for services that use
    -- the rolling update (@ECS@) deployment type that are not behind a Classic
    -- Load Balancer.
    --
    -- The rollout state of the deployment. When a service deployment is
    -- started, it begins in an @IN_PROGRESS@ state. When the service reaches a
    -- steady state, the deployment will transition to a @COMPLETED@ state. If
    -- the service fails to reach a steady state and circuit breaker is
    -- enabled, the deployment will transition to a @FAILED@ state. A
    -- deployment in @FAILED@ state will launch no new tasks. For more
    -- information, see DeploymentCircuitBreaker.
    rolloutState :: Core.Maybe DeploymentRolloutState,
    -- | The status of the deployment. The following describes each state:
    --
    -- [PRIMARY]
    --     The most recent deployment of a service.
    --
    -- [ACTIVE]
    --     A service deployment that still has running tasks, but are in the
    --     process of being replaced with a new @PRIMARY@ deployment.
    --
    -- [INACTIVE]
    --     A deployment that has been completely replaced.
    status :: Core.Maybe Core.Text,
    -- | The number of tasks in the deployment that are in the @RUNNING@ status.
    runningCount :: Core.Maybe Core.Int,
    -- | The VPC subnet and security group configuration for tasks that receive
    -- their own elastic network interface by using the @awsvpc@ networking
    -- mode.
    networkConfiguration :: Core.Maybe NetworkConfiguration,
    -- | The capacity provider strategy that the deployment is using.
    capacityProviderStrategy :: Core.Maybe [CapacityProviderStrategyItem],
    -- | The most recent desired count of tasks that was specified for the
    -- service to deploy or maintain.
    desiredCount :: Core.Maybe Core.Int,
    -- | The Unix timestamp for when the service deployment was last updated.
    updatedAt :: Core.Maybe Core.POSIX,
    -- | The launch type the tasks in the service are using. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    launchType :: Core.Maybe LaunchType,
    -- | The Unix timestamp for when the service deployment was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The platform version on which your tasks in the service are running. A
    -- platform version is only specified for tasks using the Fargate launch
    -- type. If one is not specified, the @LATEST@ platform version is used by
    -- default. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Core.Maybe Core.Text,
    -- | The ID of the deployment.
    id :: Core.Maybe Core.Text,
    -- | The number of tasks in the deployment that are in the @PENDING@ status.
    pendingCount :: Core.Maybe Core.Int,
    -- | A description of the rollout state of a deployment.
    rolloutStateReason :: Core.Maybe Core.Text,
    -- | The most recent task definition that was specified for the tasks in the
    -- service to use.
    taskDefinition :: Core.Maybe Core.Text,
    -- | The number of consecutively failed tasks in the deployment. A task is
    -- considered a failure if the service scheduler can\'t launch the task,
    -- the task doesn\'t transition to a @RUNNING@ state, or if it fails any of
    -- its defined health checks and is stopped.
    --
    -- Once a service deployment has one or more successfully running tasks,
    -- the failed task count resets to zero and stops being evaluated.
    failedTasks :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Deployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rolloutState', 'deployment_rolloutState' - The @rolloutState@ of a service is only returned for services that use
-- the rolling update (@ECS@) deployment type that are not behind a Classic
-- Load Balancer.
--
-- The rollout state of the deployment. When a service deployment is
-- started, it begins in an @IN_PROGRESS@ state. When the service reaches a
-- steady state, the deployment will transition to a @COMPLETED@ state. If
-- the service fails to reach a steady state and circuit breaker is
-- enabled, the deployment will transition to a @FAILED@ state. A
-- deployment in @FAILED@ state will launch no new tasks. For more
-- information, see DeploymentCircuitBreaker.
--
-- 'status', 'deployment_status' - The status of the deployment. The following describes each state:
--
-- [PRIMARY]
--     The most recent deployment of a service.
--
-- [ACTIVE]
--     A service deployment that still has running tasks, but are in the
--     process of being replaced with a new @PRIMARY@ deployment.
--
-- [INACTIVE]
--     A deployment that has been completely replaced.
--
-- 'runningCount', 'deployment_runningCount' - The number of tasks in the deployment that are in the @RUNNING@ status.
--
-- 'networkConfiguration', 'deployment_networkConfiguration' - The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
--
-- 'capacityProviderStrategy', 'deployment_capacityProviderStrategy' - The capacity provider strategy that the deployment is using.
--
-- 'desiredCount', 'deployment_desiredCount' - The most recent desired count of tasks that was specified for the
-- service to deploy or maintain.
--
-- 'updatedAt', 'deployment_updatedAt' - The Unix timestamp for when the service deployment was last updated.
--
-- 'launchType', 'deployment_launchType' - The launch type the tasks in the service are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'createdAt', 'deployment_createdAt' - The Unix timestamp for when the service deployment was created.
--
-- 'platformVersion', 'deployment_platformVersion' - The platform version on which your tasks in the service are running. A
-- platform version is only specified for tasks using the Fargate launch
-- type. If one is not specified, the @LATEST@ platform version is used by
-- default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'id', 'deployment_id' - The ID of the deployment.
--
-- 'pendingCount', 'deployment_pendingCount' - The number of tasks in the deployment that are in the @PENDING@ status.
--
-- 'rolloutStateReason', 'deployment_rolloutStateReason' - A description of the rollout state of a deployment.
--
-- 'taskDefinition', 'deployment_taskDefinition' - The most recent task definition that was specified for the tasks in the
-- service to use.
--
-- 'failedTasks', 'deployment_failedTasks' - The number of consecutively failed tasks in the deployment. A task is
-- considered a failure if the service scheduler can\'t launch the task,
-- the task doesn\'t transition to a @RUNNING@ state, or if it fails any of
-- its defined health checks and is stopped.
--
-- Once a service deployment has one or more successfully running tasks,
-- the failed task count resets to zero and stops being evaluated.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { rolloutState = Core.Nothing,
      status = Core.Nothing,
      runningCount = Core.Nothing,
      networkConfiguration = Core.Nothing,
      capacityProviderStrategy = Core.Nothing,
      desiredCount = Core.Nothing,
      updatedAt = Core.Nothing,
      launchType = Core.Nothing,
      createdAt = Core.Nothing,
      platformVersion = Core.Nothing,
      id = Core.Nothing,
      pendingCount = Core.Nothing,
      rolloutStateReason = Core.Nothing,
      taskDefinition = Core.Nothing,
      failedTasks = Core.Nothing
    }

-- | The @rolloutState@ of a service is only returned for services that use
-- the rolling update (@ECS@) deployment type that are not behind a Classic
-- Load Balancer.
--
-- The rollout state of the deployment. When a service deployment is
-- started, it begins in an @IN_PROGRESS@ state. When the service reaches a
-- steady state, the deployment will transition to a @COMPLETED@ state. If
-- the service fails to reach a steady state and circuit breaker is
-- enabled, the deployment will transition to a @FAILED@ state. A
-- deployment in @FAILED@ state will launch no new tasks. For more
-- information, see DeploymentCircuitBreaker.
deployment_rolloutState :: Lens.Lens' Deployment (Core.Maybe DeploymentRolloutState)
deployment_rolloutState = Lens.lens (\Deployment' {rolloutState} -> rolloutState) (\s@Deployment' {} a -> s {rolloutState = a} :: Deployment)

-- | The status of the deployment. The following describes each state:
--
-- [PRIMARY]
--     The most recent deployment of a service.
--
-- [ACTIVE]
--     A service deployment that still has running tasks, but are in the
--     process of being replaced with a new @PRIMARY@ deployment.
--
-- [INACTIVE]
--     A deployment that has been completely replaced.
deployment_status :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_status = Lens.lens (\Deployment' {status} -> status) (\s@Deployment' {} a -> s {status = a} :: Deployment)

-- | The number of tasks in the deployment that are in the @RUNNING@ status.
deployment_runningCount :: Lens.Lens' Deployment (Core.Maybe Core.Int)
deployment_runningCount = Lens.lens (\Deployment' {runningCount} -> runningCount) (\s@Deployment' {} a -> s {runningCount = a} :: Deployment)

-- | The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
deployment_networkConfiguration :: Lens.Lens' Deployment (Core.Maybe NetworkConfiguration)
deployment_networkConfiguration = Lens.lens (\Deployment' {networkConfiguration} -> networkConfiguration) (\s@Deployment' {} a -> s {networkConfiguration = a} :: Deployment)

-- | The capacity provider strategy that the deployment is using.
deployment_capacityProviderStrategy :: Lens.Lens' Deployment (Core.Maybe [CapacityProviderStrategyItem])
deployment_capacityProviderStrategy = Lens.lens (\Deployment' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@Deployment' {} a -> s {capacityProviderStrategy = a} :: Deployment) Core.. Lens.mapping Lens._Coerce

-- | The most recent desired count of tasks that was specified for the
-- service to deploy or maintain.
deployment_desiredCount :: Lens.Lens' Deployment (Core.Maybe Core.Int)
deployment_desiredCount = Lens.lens (\Deployment' {desiredCount} -> desiredCount) (\s@Deployment' {} a -> s {desiredCount = a} :: Deployment)

-- | The Unix timestamp for when the service deployment was last updated.
deployment_updatedAt :: Lens.Lens' Deployment (Core.Maybe Core.UTCTime)
deployment_updatedAt = Lens.lens (\Deployment' {updatedAt} -> updatedAt) (\s@Deployment' {} a -> s {updatedAt = a} :: Deployment) Core.. Lens.mapping Core._Time

-- | The launch type the tasks in the service are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
deployment_launchType :: Lens.Lens' Deployment (Core.Maybe LaunchType)
deployment_launchType = Lens.lens (\Deployment' {launchType} -> launchType) (\s@Deployment' {} a -> s {launchType = a} :: Deployment)

-- | The Unix timestamp for when the service deployment was created.
deployment_createdAt :: Lens.Lens' Deployment (Core.Maybe Core.UTCTime)
deployment_createdAt = Lens.lens (\Deployment' {createdAt} -> createdAt) (\s@Deployment' {} a -> s {createdAt = a} :: Deployment) Core.. Lens.mapping Core._Time

-- | The platform version on which your tasks in the service are running. A
-- platform version is only specified for tasks using the Fargate launch
-- type. If one is not specified, the @LATEST@ platform version is used by
-- default. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
deployment_platformVersion :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_platformVersion = Lens.lens (\Deployment' {platformVersion} -> platformVersion) (\s@Deployment' {} a -> s {platformVersion = a} :: Deployment)

-- | The ID of the deployment.
deployment_id :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_id = Lens.lens (\Deployment' {id} -> id) (\s@Deployment' {} a -> s {id = a} :: Deployment)

-- | The number of tasks in the deployment that are in the @PENDING@ status.
deployment_pendingCount :: Lens.Lens' Deployment (Core.Maybe Core.Int)
deployment_pendingCount = Lens.lens (\Deployment' {pendingCount} -> pendingCount) (\s@Deployment' {} a -> s {pendingCount = a} :: Deployment)

-- | A description of the rollout state of a deployment.
deployment_rolloutStateReason :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_rolloutStateReason = Lens.lens (\Deployment' {rolloutStateReason} -> rolloutStateReason) (\s@Deployment' {} a -> s {rolloutStateReason = a} :: Deployment)

-- | The most recent task definition that was specified for the tasks in the
-- service to use.
deployment_taskDefinition :: Lens.Lens' Deployment (Core.Maybe Core.Text)
deployment_taskDefinition = Lens.lens (\Deployment' {taskDefinition} -> taskDefinition) (\s@Deployment' {} a -> s {taskDefinition = a} :: Deployment)

-- | The number of consecutively failed tasks in the deployment. A task is
-- considered a failure if the service scheduler can\'t launch the task,
-- the task doesn\'t transition to a @RUNNING@ state, or if it fails any of
-- its defined health checks and is stopped.
--
-- Once a service deployment has one or more successfully running tasks,
-- the failed task count resets to zero and stops being evaluated.
deployment_failedTasks :: Lens.Lens' Deployment (Core.Maybe Core.Int)
deployment_failedTasks = Lens.lens (\Deployment' {failedTasks} -> failedTasks) (\s@Deployment' {} a -> s {failedTasks = a} :: Deployment)

instance Core.FromJSON Deployment where
  parseJSON =
    Core.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Core.<$> (x Core..:? "rolloutState")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "runningCount")
            Core.<*> (x Core..:? "networkConfiguration")
            Core.<*> ( x Core..:? "capacityProviderStrategy"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "desiredCount")
            Core.<*> (x Core..:? "updatedAt")
            Core.<*> (x Core..:? "launchType")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "platformVersion")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "pendingCount")
            Core.<*> (x Core..:? "rolloutStateReason")
            Core.<*> (x Core..:? "taskDefinition")
            Core.<*> (x Core..:? "failedTasks")
      )

instance Core.Hashable Deployment

instance Core.NFData Deployment
