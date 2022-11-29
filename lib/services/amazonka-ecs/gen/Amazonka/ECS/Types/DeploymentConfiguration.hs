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
-- Module      : Amazonka.ECS.Types.DeploymentConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.DeploymentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECS.Types.DeploymentCircuitBreaker
import qualified Amazonka.Prelude as Prelude

-- | Optional deployment parameters that control how many tasks run during a
-- deployment and the ordering of stopping and starting tasks.
--
-- /See:/ 'newDeploymentConfiguration' smart constructor.
data DeploymentConfiguration = DeploymentConfiguration'
  { -- | If a service is using the rolling update (@ECS@) deployment type, the
    -- @minimumHealthyPercent@ represents a lower limit on the number of your
    -- service\'s tasks that must remain in the @RUNNING@ state during a
    -- deployment, as a percentage of the @desiredCount@ (rounded up to the
    -- nearest integer). This parameter enables you to deploy without using
    -- additional cluster capacity. For example, if your service has a
    -- @desiredCount@ of four tasks and a @minimumHealthyPercent@ of 50%, the
    -- service scheduler may stop two existing tasks to free up cluster
    -- capacity before starting two new tasks.
    --
    -- For services that /do not/ use a load balancer, the following should be
    -- noted:
    --
    -- -   A service is considered healthy if all essential containers within
    --     the tasks in the service pass their health checks.
    --
    -- -   If a task has no essential containers with a health check defined,
    --     the service scheduler will wait for 40 seconds after a task reaches
    --     a @RUNNING@ state before the task is counted towards the minimum
    --     healthy percent total.
    --
    -- -   If a task has one or more essential containers with a health check
    --     defined, the service scheduler will wait for the task to reach a
    --     healthy status before counting it towards the minimum healthy
    --     percent total. A task is considered healthy when all essential
    --     containers within the task have passed their health checks. The
    --     amount of time the service scheduler can wait for is determined by
    --     the container health check settings.
    --
    -- For services are that /do/ use a load balancer, the following should be
    -- noted:
    --
    -- -   If a task has no essential containers with a health check defined,
    --     the service scheduler will wait for the load balancer target group
    --     health check to return a healthy status before counting the task
    --     towards the minimum healthy percent total.
    --
    -- -   If a task has an essential container with a health check defined,
    --     the service scheduler will wait for both the task to reach a healthy
    --     status and the load balancer target group health check to return a
    --     healthy status before counting the task towards the minimum healthy
    --     percent total.
    --
    -- If a service is using either the blue\/green (@CODE_DEPLOY@) or
    -- @EXTERNAL@ deployment types and is running tasks that use the EC2 launch
    -- type, the __minimum healthy percent__ value is set to the default value
    -- and is used to define the lower limit on the number of the tasks in the
    -- service that remain in the @RUNNING@ state while the container instances
    -- are in the @DRAINING@ state. If a service is using either the
    -- blue\/green (@CODE_DEPLOY@) or @EXTERNAL@ deployment types and is
    -- running tasks that use the Fargate launch type, the minimum healthy
    -- percent value is not used, although it is returned when describing your
    -- service.
    minimumHealthyPercent :: Prelude.Maybe Prelude.Int,
    -- | If a service is using the rolling update (@ECS@) deployment type, the
    -- @maximumPercent@ parameter represents an upper limit on the number of
    -- your service\'s tasks that are allowed in the @RUNNING@ or @PENDING@
    -- state during a deployment, as a percentage of the @desiredCount@
    -- (rounded down to the nearest integer). This parameter enables you to
    -- define the deployment batch size. For example, if your service is using
    -- the @REPLICA@ service scheduler and has a @desiredCount@ of four tasks
    -- and a @maximumPercent@ value of 200%, the scheduler may start four new
    -- tasks before stopping the four older tasks (provided that the cluster
    -- resources required to do this are available). The default
    -- @maximumPercent@ value for a service using the @REPLICA@ service
    -- scheduler is 200%.
    --
    -- If a service is using either the blue\/green (@CODE_DEPLOY@) or
    -- @EXTERNAL@ deployment types and tasks that use the EC2 launch type, the
    -- __maximum percent__ value is set to the default value and is used to
    -- define the upper limit on the number of the tasks in the service that
    -- remain in the @RUNNING@ state while the container instances are in the
    -- @DRAINING@ state. If the tasks in the service use the Fargate launch
    -- type, the maximum percent value is not used, although it is returned
    -- when describing your service.
    maximumPercent :: Prelude.Maybe Prelude.Int,
    -- | The deployment circuit breaker can only be used for services using the
    -- rolling update (@ECS@) deployment type.
    --
    -- The __deployment circuit breaker__ determines whether a service
    -- deployment will fail if the service can\'t reach a steady state. If
    -- deployment circuit breaker is enabled, a service deployment will
    -- transition to a failed state and stop launching new tasks. If rollback
    -- is enabled, when a service deployment fails, the service is rolled back
    -- to the last deployment that completed successfully.
    deploymentCircuitBreaker :: Prelude.Maybe DeploymentCircuitBreaker
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimumHealthyPercent', 'deploymentConfiguration_minimumHealthyPercent' - If a service is using the rolling update (@ECS@) deployment type, the
-- @minimumHealthyPercent@ represents a lower limit on the number of your
-- service\'s tasks that must remain in the @RUNNING@ state during a
-- deployment, as a percentage of the @desiredCount@ (rounded up to the
-- nearest integer). This parameter enables you to deploy without using
-- additional cluster capacity. For example, if your service has a
-- @desiredCount@ of four tasks and a @minimumHealthyPercent@ of 50%, the
-- service scheduler may stop two existing tasks to free up cluster
-- capacity before starting two new tasks.
--
-- For services that /do not/ use a load balancer, the following should be
-- noted:
--
-- -   A service is considered healthy if all essential containers within
--     the tasks in the service pass their health checks.
--
-- -   If a task has no essential containers with a health check defined,
--     the service scheduler will wait for 40 seconds after a task reaches
--     a @RUNNING@ state before the task is counted towards the minimum
--     healthy percent total.
--
-- -   If a task has one or more essential containers with a health check
--     defined, the service scheduler will wait for the task to reach a
--     healthy status before counting it towards the minimum healthy
--     percent total. A task is considered healthy when all essential
--     containers within the task have passed their health checks. The
--     amount of time the service scheduler can wait for is determined by
--     the container health check settings.
--
-- For services are that /do/ use a load balancer, the following should be
-- noted:
--
-- -   If a task has no essential containers with a health check defined,
--     the service scheduler will wait for the load balancer target group
--     health check to return a healthy status before counting the task
--     towards the minimum healthy percent total.
--
-- -   If a task has an essential container with a health check defined,
--     the service scheduler will wait for both the task to reach a healthy
--     status and the load balancer target group health check to return a
--     healthy status before counting the task towards the minimum healthy
--     percent total.
--
-- If a service is using either the blue\/green (@CODE_DEPLOY@) or
-- @EXTERNAL@ deployment types and is running tasks that use the EC2 launch
-- type, the __minimum healthy percent__ value is set to the default value
-- and is used to define the lower limit on the number of the tasks in the
-- service that remain in the @RUNNING@ state while the container instances
-- are in the @DRAINING@ state. If a service is using either the
-- blue\/green (@CODE_DEPLOY@) or @EXTERNAL@ deployment types and is
-- running tasks that use the Fargate launch type, the minimum healthy
-- percent value is not used, although it is returned when describing your
-- service.
--
-- 'maximumPercent', 'deploymentConfiguration_maximumPercent' - If a service is using the rolling update (@ECS@) deployment type, the
-- @maximumPercent@ parameter represents an upper limit on the number of
-- your service\'s tasks that are allowed in the @RUNNING@ or @PENDING@
-- state during a deployment, as a percentage of the @desiredCount@
-- (rounded down to the nearest integer). This parameter enables you to
-- define the deployment batch size. For example, if your service is using
-- the @REPLICA@ service scheduler and has a @desiredCount@ of four tasks
-- and a @maximumPercent@ value of 200%, the scheduler may start four new
-- tasks before stopping the four older tasks (provided that the cluster
-- resources required to do this are available). The default
-- @maximumPercent@ value for a service using the @REPLICA@ service
-- scheduler is 200%.
--
-- If a service is using either the blue\/green (@CODE_DEPLOY@) or
-- @EXTERNAL@ deployment types and tasks that use the EC2 launch type, the
-- __maximum percent__ value is set to the default value and is used to
-- define the upper limit on the number of the tasks in the service that
-- remain in the @RUNNING@ state while the container instances are in the
-- @DRAINING@ state. If the tasks in the service use the Fargate launch
-- type, the maximum percent value is not used, although it is returned
-- when describing your service.
--
-- 'deploymentCircuitBreaker', 'deploymentConfiguration_deploymentCircuitBreaker' - The deployment circuit breaker can only be used for services using the
-- rolling update (@ECS@) deployment type.
--
-- The __deployment circuit breaker__ determines whether a service
-- deployment will fail if the service can\'t reach a steady state. If
-- deployment circuit breaker is enabled, a service deployment will
-- transition to a failed state and stop launching new tasks. If rollback
-- is enabled, when a service deployment fails, the service is rolled back
-- to the last deployment that completed successfully.
newDeploymentConfiguration ::
  DeploymentConfiguration
newDeploymentConfiguration =
  DeploymentConfiguration'
    { minimumHealthyPercent =
        Prelude.Nothing,
      maximumPercent = Prelude.Nothing,
      deploymentCircuitBreaker = Prelude.Nothing
    }

-- | If a service is using the rolling update (@ECS@) deployment type, the
-- @minimumHealthyPercent@ represents a lower limit on the number of your
-- service\'s tasks that must remain in the @RUNNING@ state during a
-- deployment, as a percentage of the @desiredCount@ (rounded up to the
-- nearest integer). This parameter enables you to deploy without using
-- additional cluster capacity. For example, if your service has a
-- @desiredCount@ of four tasks and a @minimumHealthyPercent@ of 50%, the
-- service scheduler may stop two existing tasks to free up cluster
-- capacity before starting two new tasks.
--
-- For services that /do not/ use a load balancer, the following should be
-- noted:
--
-- -   A service is considered healthy if all essential containers within
--     the tasks in the service pass their health checks.
--
-- -   If a task has no essential containers with a health check defined,
--     the service scheduler will wait for 40 seconds after a task reaches
--     a @RUNNING@ state before the task is counted towards the minimum
--     healthy percent total.
--
-- -   If a task has one or more essential containers with a health check
--     defined, the service scheduler will wait for the task to reach a
--     healthy status before counting it towards the minimum healthy
--     percent total. A task is considered healthy when all essential
--     containers within the task have passed their health checks. The
--     amount of time the service scheduler can wait for is determined by
--     the container health check settings.
--
-- For services are that /do/ use a load balancer, the following should be
-- noted:
--
-- -   If a task has no essential containers with a health check defined,
--     the service scheduler will wait for the load balancer target group
--     health check to return a healthy status before counting the task
--     towards the minimum healthy percent total.
--
-- -   If a task has an essential container with a health check defined,
--     the service scheduler will wait for both the task to reach a healthy
--     status and the load balancer target group health check to return a
--     healthy status before counting the task towards the minimum healthy
--     percent total.
--
-- If a service is using either the blue\/green (@CODE_DEPLOY@) or
-- @EXTERNAL@ deployment types and is running tasks that use the EC2 launch
-- type, the __minimum healthy percent__ value is set to the default value
-- and is used to define the lower limit on the number of the tasks in the
-- service that remain in the @RUNNING@ state while the container instances
-- are in the @DRAINING@ state. If a service is using either the
-- blue\/green (@CODE_DEPLOY@) or @EXTERNAL@ deployment types and is
-- running tasks that use the Fargate launch type, the minimum healthy
-- percent value is not used, although it is returned when describing your
-- service.
deploymentConfiguration_minimumHealthyPercent :: Lens.Lens' DeploymentConfiguration (Prelude.Maybe Prelude.Int)
deploymentConfiguration_minimumHealthyPercent = Lens.lens (\DeploymentConfiguration' {minimumHealthyPercent} -> minimumHealthyPercent) (\s@DeploymentConfiguration' {} a -> s {minimumHealthyPercent = a} :: DeploymentConfiguration)

-- | If a service is using the rolling update (@ECS@) deployment type, the
-- @maximumPercent@ parameter represents an upper limit on the number of
-- your service\'s tasks that are allowed in the @RUNNING@ or @PENDING@
-- state during a deployment, as a percentage of the @desiredCount@
-- (rounded down to the nearest integer). This parameter enables you to
-- define the deployment batch size. For example, if your service is using
-- the @REPLICA@ service scheduler and has a @desiredCount@ of four tasks
-- and a @maximumPercent@ value of 200%, the scheduler may start four new
-- tasks before stopping the four older tasks (provided that the cluster
-- resources required to do this are available). The default
-- @maximumPercent@ value for a service using the @REPLICA@ service
-- scheduler is 200%.
--
-- If a service is using either the blue\/green (@CODE_DEPLOY@) or
-- @EXTERNAL@ deployment types and tasks that use the EC2 launch type, the
-- __maximum percent__ value is set to the default value and is used to
-- define the upper limit on the number of the tasks in the service that
-- remain in the @RUNNING@ state while the container instances are in the
-- @DRAINING@ state. If the tasks in the service use the Fargate launch
-- type, the maximum percent value is not used, although it is returned
-- when describing your service.
deploymentConfiguration_maximumPercent :: Lens.Lens' DeploymentConfiguration (Prelude.Maybe Prelude.Int)
deploymentConfiguration_maximumPercent = Lens.lens (\DeploymentConfiguration' {maximumPercent} -> maximumPercent) (\s@DeploymentConfiguration' {} a -> s {maximumPercent = a} :: DeploymentConfiguration)

-- | The deployment circuit breaker can only be used for services using the
-- rolling update (@ECS@) deployment type.
--
-- The __deployment circuit breaker__ determines whether a service
-- deployment will fail if the service can\'t reach a steady state. If
-- deployment circuit breaker is enabled, a service deployment will
-- transition to a failed state and stop launching new tasks. If rollback
-- is enabled, when a service deployment fails, the service is rolled back
-- to the last deployment that completed successfully.
deploymentConfiguration_deploymentCircuitBreaker :: Lens.Lens' DeploymentConfiguration (Prelude.Maybe DeploymentCircuitBreaker)
deploymentConfiguration_deploymentCircuitBreaker = Lens.lens (\DeploymentConfiguration' {deploymentCircuitBreaker} -> deploymentCircuitBreaker) (\s@DeploymentConfiguration' {} a -> s {deploymentCircuitBreaker = a} :: DeploymentConfiguration)

instance Core.FromJSON DeploymentConfiguration where
  parseJSON =
    Core.withObject
      "DeploymentConfiguration"
      ( \x ->
          DeploymentConfiguration'
            Prelude.<$> (x Core..:? "minimumHealthyPercent")
            Prelude.<*> (x Core..:? "maximumPercent")
            Prelude.<*> (x Core..:? "deploymentCircuitBreaker")
      )

instance Prelude.Hashable DeploymentConfiguration where
  hashWithSalt _salt DeploymentConfiguration' {..} =
    _salt `Prelude.hashWithSalt` minimumHealthyPercent
      `Prelude.hashWithSalt` maximumPercent
      `Prelude.hashWithSalt` deploymentCircuitBreaker

instance Prelude.NFData DeploymentConfiguration where
  rnf DeploymentConfiguration' {..} =
    Prelude.rnf minimumHealthyPercent
      `Prelude.seq` Prelude.rnf maximumPercent
      `Prelude.seq` Prelude.rnf deploymentCircuitBreaker

instance Core.ToJSON DeploymentConfiguration where
  toJSON DeploymentConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("minimumHealthyPercent" Core..=)
              Prelude.<$> minimumHealthyPercent,
            ("maximumPercent" Core..=)
              Prelude.<$> maximumPercent,
            ("deploymentCircuitBreaker" Core..=)
              Prelude.<$> deploymentCircuitBreaker
          ]
      )
