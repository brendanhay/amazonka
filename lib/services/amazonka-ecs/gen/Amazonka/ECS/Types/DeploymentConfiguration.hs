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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.DeploymentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.DeploymentAlarms
import Amazonka.ECS.Types.DeploymentCircuitBreaker
import qualified Amazonka.Prelude as Prelude

-- | Optional deployment parameters that control how many tasks run during a
-- deployment and the ordering of stopping and starting tasks.
--
-- /See:/ 'newDeploymentConfiguration' smart constructor.
data DeploymentConfiguration = DeploymentConfiguration'
  { -- | Information about the CloudWatch alarms.
    alarms :: Prelude.Maybe DeploymentAlarms,
    -- | The deployment circuit breaker can only be used for services using the
    -- rolling update (@ECS@) deployment type.
    --
    -- The __deployment circuit breaker__ determines whether a service
    -- deployment will fail if the service can\'t reach a steady state. If you
    -- use the deployment circuit breaker, a service deployment will transition
    -- to a failed state and stop launching new tasks. If you use the rollback
    -- option, when a service deployment fails, the service is rolled back to
    -- the last deployment that completed successfully. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html Rolling update>
    -- in the /Amazon Elastic Container Service Developer Guide/
    deploymentCircuitBreaker :: Prelude.Maybe DeploymentCircuitBreaker,
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
    minimumHealthyPercent :: Prelude.Maybe Prelude.Int
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
-- 'alarms', 'deploymentConfiguration_alarms' - Information about the CloudWatch alarms.
--
-- 'deploymentCircuitBreaker', 'deploymentConfiguration_deploymentCircuitBreaker' - The deployment circuit breaker can only be used for services using the
-- rolling update (@ECS@) deployment type.
--
-- The __deployment circuit breaker__ determines whether a service
-- deployment will fail if the service can\'t reach a steady state. If you
-- use the deployment circuit breaker, a service deployment will transition
-- to a failed state and stop launching new tasks. If you use the rollback
-- option, when a service deployment fails, the service is rolled back to
-- the last deployment that completed successfully. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html Rolling update>
-- in the /Amazon Elastic Container Service Developer Guide/
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
newDeploymentConfiguration ::
  DeploymentConfiguration
newDeploymentConfiguration =
  DeploymentConfiguration'
    { alarms = Prelude.Nothing,
      deploymentCircuitBreaker = Prelude.Nothing,
      maximumPercent = Prelude.Nothing,
      minimumHealthyPercent = Prelude.Nothing
    }

-- | Information about the CloudWatch alarms.
deploymentConfiguration_alarms :: Lens.Lens' DeploymentConfiguration (Prelude.Maybe DeploymentAlarms)
deploymentConfiguration_alarms = Lens.lens (\DeploymentConfiguration' {alarms} -> alarms) (\s@DeploymentConfiguration' {} a -> s {alarms = a} :: DeploymentConfiguration)

-- | The deployment circuit breaker can only be used for services using the
-- rolling update (@ECS@) deployment type.
--
-- The __deployment circuit breaker__ determines whether a service
-- deployment will fail if the service can\'t reach a steady state. If you
-- use the deployment circuit breaker, a service deployment will transition
-- to a failed state and stop launching new tasks. If you use the rollback
-- option, when a service deployment fails, the service is rolled back to
-- the last deployment that completed successfully. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html Rolling update>
-- in the /Amazon Elastic Container Service Developer Guide/
deploymentConfiguration_deploymentCircuitBreaker :: Lens.Lens' DeploymentConfiguration (Prelude.Maybe DeploymentCircuitBreaker)
deploymentConfiguration_deploymentCircuitBreaker = Lens.lens (\DeploymentConfiguration' {deploymentCircuitBreaker} -> deploymentCircuitBreaker) (\s@DeploymentConfiguration' {} a -> s {deploymentCircuitBreaker = a} :: DeploymentConfiguration)

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

instance Data.FromJSON DeploymentConfiguration where
  parseJSON =
    Data.withObject
      "DeploymentConfiguration"
      ( \x ->
          DeploymentConfiguration'
            Prelude.<$> (x Data..:? "alarms")
            Prelude.<*> (x Data..:? "deploymentCircuitBreaker")
            Prelude.<*> (x Data..:? "maximumPercent")
            Prelude.<*> (x Data..:? "minimumHealthyPercent")
      )

instance Prelude.Hashable DeploymentConfiguration where
  hashWithSalt _salt DeploymentConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` alarms
      `Prelude.hashWithSalt` deploymentCircuitBreaker
      `Prelude.hashWithSalt` maximumPercent
      `Prelude.hashWithSalt` minimumHealthyPercent

instance Prelude.NFData DeploymentConfiguration where
  rnf DeploymentConfiguration' {..} =
    Prelude.rnf alarms
      `Prelude.seq` Prelude.rnf deploymentCircuitBreaker
      `Prelude.seq` Prelude.rnf maximumPercent
      `Prelude.seq` Prelude.rnf minimumHealthyPercent

instance Data.ToJSON DeploymentConfiguration where
  toJSON DeploymentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alarms" Data..=) Prelude.<$> alarms,
            ("deploymentCircuitBreaker" Data..=)
              Prelude.<$> deploymentCircuitBreaker,
            ("maximumPercent" Data..=)
              Prelude.<$> maximumPercent,
            ("minimumHealthyPercent" Data..=)
              Prelude.<$> minimumHealthyPercent
          ]
      )
