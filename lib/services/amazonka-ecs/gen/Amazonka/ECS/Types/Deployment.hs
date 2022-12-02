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
-- Module      : Amazonka.ECS.Types.Deployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Deployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.CapacityProviderStrategyItem
import Amazonka.ECS.Types.DeploymentRolloutState
import Amazonka.ECS.Types.LaunchType
import Amazonka.ECS.Types.NetworkConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The details of an Amazon ECS service deployment. This is used only when
-- a service uses the @ECS@ deployment controller type.
--
-- /See:/ 'newDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The operating system that your tasks in the service, or tasks are
    -- running on. A platform family is specified only for tasks using the
    -- Fargate launch type.
    --
    -- All tasks that run as part of this service must use the same
    -- @platformFamily@ value as the service, for example, @ LINUX.@.
    platformFamily :: Prelude.Maybe Prelude.Text,
    -- | The most recent task definition that was specified for the tasks in the
    -- service to use.
    taskDefinition :: Prelude.Maybe Prelude.Text,
    -- | The number of consecutively failed tasks in the deployment. A task is
    -- considered a failure if the service scheduler can\'t launch the task,
    -- the task doesn\'t transition to a @RUNNING@ state, or if it fails any of
    -- its defined health checks and is stopped.
    --
    -- Once a service deployment has one or more successfully running tasks,
    -- the failed task count resets to zero and stops being evaluated.
    failedTasks :: Prelude.Maybe Prelude.Int,
    -- | The VPC subnet and security group configuration for tasks that receive
    -- their own elastic network interface by using the @awsvpc@ networking
    -- mode.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The status of the deployment. The following describes each state.
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
    status :: Prelude.Maybe Prelude.Text,
    -- | The ID of the deployment.
    id :: Prelude.Maybe Prelude.Text,
    -- | The most recent desired count of tasks that was specified for the
    -- service to deploy or maintain.
    desiredCount :: Prelude.Maybe Prelude.Int,
    -- | The number of tasks in the deployment that are in the @PENDING@ status.
    pendingCount :: Prelude.Maybe Prelude.Int,
    -- | The capacity provider strategy that the deployment is using.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | A description of the rollout state of a deployment.
    rolloutStateReason :: Prelude.Maybe Prelude.Text,
    -- | The launch type the tasks in the service are using. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    launchType :: Prelude.Maybe LaunchType,
    -- | The number of tasks in the deployment that are in the @RUNNING@ status.
    runningCount :: Prelude.Maybe Prelude.Int,
    -- | The platform version that your tasks in the service run on. A platform
    -- version is only specified for tasks using the Fargate launch type. If
    -- one isn\'t specified, the @LATEST@ platform version is used. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The @rolloutState@ of a service is only returned for services that use
    -- the rolling update (@ECS@) deployment type that aren\'t behind a Classic
    -- Load Balancer.
    --
    -- The rollout state of the deployment. When a service deployment is
    -- started, it begins in an @IN_PROGRESS@ state. When the service reaches a
    -- steady state, the deployment transitions to a @COMPLETED@ state. If the
    -- service fails to reach a steady state and circuit breaker is enabled,
    -- the deployment transitions to a @FAILED@ state. A deployment in @FAILED@
    -- state doesn\'t launch any new tasks. For more information, see
    -- DeploymentCircuitBreaker.
    rolloutState :: Prelude.Maybe DeploymentRolloutState,
    -- | The Unix timestamp for the time when the service deployment was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Unix timestamp for the time when the service deployment was last
    -- updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Deployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformFamily', 'deployment_platformFamily' - The operating system that your tasks in the service, or tasks are
-- running on. A platform family is specified only for tasks using the
-- Fargate launch type.
--
-- All tasks that run as part of this service must use the same
-- @platformFamily@ value as the service, for example, @ LINUX.@.
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
--
-- 'networkConfiguration', 'deployment_networkConfiguration' - The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
--
-- 'status', 'deployment_status' - The status of the deployment. The following describes each state.
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
-- 'id', 'deployment_id' - The ID of the deployment.
--
-- 'desiredCount', 'deployment_desiredCount' - The most recent desired count of tasks that was specified for the
-- service to deploy or maintain.
--
-- 'pendingCount', 'deployment_pendingCount' - The number of tasks in the deployment that are in the @PENDING@ status.
--
-- 'capacityProviderStrategy', 'deployment_capacityProviderStrategy' - The capacity provider strategy that the deployment is using.
--
-- 'rolloutStateReason', 'deployment_rolloutStateReason' - A description of the rollout state of a deployment.
--
-- 'launchType', 'deployment_launchType' - The launch type the tasks in the service are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'runningCount', 'deployment_runningCount' - The number of tasks in the deployment that are in the @RUNNING@ status.
--
-- 'platformVersion', 'deployment_platformVersion' - The platform version that your tasks in the service run on. A platform
-- version is only specified for tasks using the Fargate launch type. If
-- one isn\'t specified, the @LATEST@ platform version is used. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'rolloutState', 'deployment_rolloutState' - The @rolloutState@ of a service is only returned for services that use
-- the rolling update (@ECS@) deployment type that aren\'t behind a Classic
-- Load Balancer.
--
-- The rollout state of the deployment. When a service deployment is
-- started, it begins in an @IN_PROGRESS@ state. When the service reaches a
-- steady state, the deployment transitions to a @COMPLETED@ state. If the
-- service fails to reach a steady state and circuit breaker is enabled,
-- the deployment transitions to a @FAILED@ state. A deployment in @FAILED@
-- state doesn\'t launch any new tasks. For more information, see
-- DeploymentCircuitBreaker.
--
-- 'createdAt', 'deployment_createdAt' - The Unix timestamp for the time when the service deployment was created.
--
-- 'updatedAt', 'deployment_updatedAt' - The Unix timestamp for the time when the service deployment was last
-- updated.
newDeployment ::
  Deployment
newDeployment =
  Deployment'
    { platformFamily = Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      failedTasks = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      desiredCount = Prelude.Nothing,
      pendingCount = Prelude.Nothing,
      capacityProviderStrategy = Prelude.Nothing,
      rolloutStateReason = Prelude.Nothing,
      launchType = Prelude.Nothing,
      runningCount = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      rolloutState = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The operating system that your tasks in the service, or tasks are
-- running on. A platform family is specified only for tasks using the
-- Fargate launch type.
--
-- All tasks that run as part of this service must use the same
-- @platformFamily@ value as the service, for example, @ LINUX.@.
deployment_platformFamily :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_platformFamily = Lens.lens (\Deployment' {platformFamily} -> platformFamily) (\s@Deployment' {} a -> s {platformFamily = a} :: Deployment)

-- | The most recent task definition that was specified for the tasks in the
-- service to use.
deployment_taskDefinition :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_taskDefinition = Lens.lens (\Deployment' {taskDefinition} -> taskDefinition) (\s@Deployment' {} a -> s {taskDefinition = a} :: Deployment)

-- | The number of consecutively failed tasks in the deployment. A task is
-- considered a failure if the service scheduler can\'t launch the task,
-- the task doesn\'t transition to a @RUNNING@ state, or if it fails any of
-- its defined health checks and is stopped.
--
-- Once a service deployment has one or more successfully running tasks,
-- the failed task count resets to zero and stops being evaluated.
deployment_failedTasks :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Int)
deployment_failedTasks = Lens.lens (\Deployment' {failedTasks} -> failedTasks) (\s@Deployment' {} a -> s {failedTasks = a} :: Deployment)

-- | The VPC subnet and security group configuration for tasks that receive
-- their own elastic network interface by using the @awsvpc@ networking
-- mode.
deployment_networkConfiguration :: Lens.Lens' Deployment (Prelude.Maybe NetworkConfiguration)
deployment_networkConfiguration = Lens.lens (\Deployment' {networkConfiguration} -> networkConfiguration) (\s@Deployment' {} a -> s {networkConfiguration = a} :: Deployment)

-- | The status of the deployment. The following describes each state.
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
deployment_status :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_status = Lens.lens (\Deployment' {status} -> status) (\s@Deployment' {} a -> s {status = a} :: Deployment)

-- | The ID of the deployment.
deployment_id :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_id = Lens.lens (\Deployment' {id} -> id) (\s@Deployment' {} a -> s {id = a} :: Deployment)

-- | The most recent desired count of tasks that was specified for the
-- service to deploy or maintain.
deployment_desiredCount :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Int)
deployment_desiredCount = Lens.lens (\Deployment' {desiredCount} -> desiredCount) (\s@Deployment' {} a -> s {desiredCount = a} :: Deployment)

-- | The number of tasks in the deployment that are in the @PENDING@ status.
deployment_pendingCount :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Int)
deployment_pendingCount = Lens.lens (\Deployment' {pendingCount} -> pendingCount) (\s@Deployment' {} a -> s {pendingCount = a} :: Deployment)

-- | The capacity provider strategy that the deployment is using.
deployment_capacityProviderStrategy :: Lens.Lens' Deployment (Prelude.Maybe [CapacityProviderStrategyItem])
deployment_capacityProviderStrategy = Lens.lens (\Deployment' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@Deployment' {} a -> s {capacityProviderStrategy = a} :: Deployment) Prelude.. Lens.mapping Lens.coerced

-- | A description of the rollout state of a deployment.
deployment_rolloutStateReason :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_rolloutStateReason = Lens.lens (\Deployment' {rolloutStateReason} -> rolloutStateReason) (\s@Deployment' {} a -> s {rolloutStateReason = a} :: Deployment)

-- | The launch type the tasks in the service are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
deployment_launchType :: Lens.Lens' Deployment (Prelude.Maybe LaunchType)
deployment_launchType = Lens.lens (\Deployment' {launchType} -> launchType) (\s@Deployment' {} a -> s {launchType = a} :: Deployment)

-- | The number of tasks in the deployment that are in the @RUNNING@ status.
deployment_runningCount :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Int)
deployment_runningCount = Lens.lens (\Deployment' {runningCount} -> runningCount) (\s@Deployment' {} a -> s {runningCount = a} :: Deployment)

-- | The platform version that your tasks in the service run on. A platform
-- version is only specified for tasks using the Fargate launch type. If
-- one isn\'t specified, the @LATEST@ platform version is used. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate Platform Versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
deployment_platformVersion :: Lens.Lens' Deployment (Prelude.Maybe Prelude.Text)
deployment_platformVersion = Lens.lens (\Deployment' {platformVersion} -> platformVersion) (\s@Deployment' {} a -> s {platformVersion = a} :: Deployment)

-- | The @rolloutState@ of a service is only returned for services that use
-- the rolling update (@ECS@) deployment type that aren\'t behind a Classic
-- Load Balancer.
--
-- The rollout state of the deployment. When a service deployment is
-- started, it begins in an @IN_PROGRESS@ state. When the service reaches a
-- steady state, the deployment transitions to a @COMPLETED@ state. If the
-- service fails to reach a steady state and circuit breaker is enabled,
-- the deployment transitions to a @FAILED@ state. A deployment in @FAILED@
-- state doesn\'t launch any new tasks. For more information, see
-- DeploymentCircuitBreaker.
deployment_rolloutState :: Lens.Lens' Deployment (Prelude.Maybe DeploymentRolloutState)
deployment_rolloutState = Lens.lens (\Deployment' {rolloutState} -> rolloutState) (\s@Deployment' {} a -> s {rolloutState = a} :: Deployment)

-- | The Unix timestamp for the time when the service deployment was created.
deployment_createdAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_createdAt = Lens.lens (\Deployment' {createdAt} -> createdAt) (\s@Deployment' {} a -> s {createdAt = a} :: Deployment) Prelude.. Lens.mapping Data._Time

-- | The Unix timestamp for the time when the service deployment was last
-- updated.
deployment_updatedAt :: Lens.Lens' Deployment (Prelude.Maybe Prelude.UTCTime)
deployment_updatedAt = Lens.lens (\Deployment' {updatedAt} -> updatedAt) (\s@Deployment' {} a -> s {updatedAt = a} :: Deployment) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Deployment where
  parseJSON =
    Data.withObject
      "Deployment"
      ( \x ->
          Deployment'
            Prelude.<$> (x Data..:? "platformFamily")
            Prelude.<*> (x Data..:? "taskDefinition")
            Prelude.<*> (x Data..:? "failedTasks")
            Prelude.<*> (x Data..:? "networkConfiguration")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "desiredCount")
            Prelude.<*> (x Data..:? "pendingCount")
            Prelude.<*> ( x Data..:? "capacityProviderStrategy"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "rolloutStateReason")
            Prelude.<*> (x Data..:? "launchType")
            Prelude.<*> (x Data..:? "runningCount")
            Prelude.<*> (x Data..:? "platformVersion")
            Prelude.<*> (x Data..:? "rolloutState")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable Deployment where
  hashWithSalt _salt Deployment' {..} =
    _salt `Prelude.hashWithSalt` platformFamily
      `Prelude.hashWithSalt` taskDefinition
      `Prelude.hashWithSalt` failedTasks
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` desiredCount
      `Prelude.hashWithSalt` pendingCount
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` rolloutStateReason
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` runningCount
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` rolloutState
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Deployment where
  rnf Deployment' {..} =
    Prelude.rnf platformFamily
      `Prelude.seq` Prelude.rnf taskDefinition
      `Prelude.seq` Prelude.rnf failedTasks
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf desiredCount
      `Prelude.seq` Prelude.rnf pendingCount
      `Prelude.seq` Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf rolloutStateReason
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf runningCount
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf rolloutState
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
