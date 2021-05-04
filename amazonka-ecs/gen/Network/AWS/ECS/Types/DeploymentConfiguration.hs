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
-- Module      : Network.AWS.ECS.Types.DeploymentConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentConfiguration where

import Network.AWS.ECS.Types.DeploymentCircuitBreaker
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Optional deployment parameters that control how many tasks run during a
-- deployment and the ordering of stopping and starting tasks.
--
-- /See:/ 'newDeploymentConfiguration' smart constructor.
data DeploymentConfiguration = DeploymentConfiguration'
  { -- | If a service is using the rolling update (@ECS@) deployment type, the
    -- __maximum percent__ parameter represents an upper limit on the number of
    -- tasks in a service that are allowed in the @RUNNING@ or @PENDING@ state
    -- during a deployment, as a percentage of the desired number of tasks
    -- (rounded down to the nearest integer), and while any container instances
    -- are in the @DRAINING@ state if the service contains tasks using the EC2
    -- launch type. This parameter enables you to define the deployment batch
    -- size. For example, if your service has a desired number of four tasks
    -- and a maximum percent value of 200%, the scheduler may start four new
    -- tasks before stopping the four older tasks (provided that the cluster
    -- resources required to do this are available). The default value for
    -- maximum percent is 200%.
    --
    -- If a service is using the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
    -- deployment types and tasks that use the EC2 launch type, the __maximum
    -- percent__ value is set to the default value and is used to define the
    -- upper limit on the number of the tasks in the service that remain in the
    -- @RUNNING@ state while the container instances are in the @DRAINING@
    -- state. If the tasks in the service use the Fargate launch type, the
    -- maximum percent value is not used, although it is returned when
    -- describing your service.
    maximumPercent :: Prelude.Maybe Prelude.Int,
    -- | If a service is using the rolling update (@ECS@) deployment type, the
    -- __minimum healthy percent__ represents a lower limit on the number of
    -- tasks in a service that must remain in the @RUNNING@ state during a
    -- deployment, as a percentage of the desired number of tasks (rounded up
    -- to the nearest integer), and while any container instances are in the
    -- @DRAINING@ state if the service contains tasks using the EC2 launch
    -- type. This parameter enables you to deploy without using additional
    -- cluster capacity. For example, if your service has a desired number of
    -- four tasks and a minimum healthy percent of 50%, the scheduler may stop
    -- two existing tasks to free up cluster capacity before starting two new
    -- tasks. Tasks for services that /do not/ use a load balancer are
    -- considered healthy if they are in the @RUNNING@ state; tasks for
    -- services that /do/ use a load balancer are considered healthy if they
    -- are in the @RUNNING@ state and they are reported as healthy by the load
    -- balancer. The default value for minimum healthy percent is 100%.
    --
    -- If a service is using the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
    -- deployment types and tasks that use the EC2 launch type, the __minimum
    -- healthy percent__ value is set to the default value and is used to
    -- define the lower limit on the number of the tasks in the service that
    -- remain in the @RUNNING@ state while the container instances are in the
    -- @DRAINING@ state. If the tasks in the service use the Fargate launch
    -- type, the minimum healthy percent value is not used, although it is
    -- returned when describing your service.
    minimumHealthyPercent :: Prelude.Maybe Prelude.Int,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeploymentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumPercent', 'deploymentConfiguration_maximumPercent' - If a service is using the rolling update (@ECS@) deployment type, the
-- __maximum percent__ parameter represents an upper limit on the number of
-- tasks in a service that are allowed in the @RUNNING@ or @PENDING@ state
-- during a deployment, as a percentage of the desired number of tasks
-- (rounded down to the nearest integer), and while any container instances
-- are in the @DRAINING@ state if the service contains tasks using the EC2
-- launch type. This parameter enables you to define the deployment batch
-- size. For example, if your service has a desired number of four tasks
-- and a maximum percent value of 200%, the scheduler may start four new
-- tasks before stopping the four older tasks (provided that the cluster
-- resources required to do this are available). The default value for
-- maximum percent is 200%.
--
-- If a service is using the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
-- deployment types and tasks that use the EC2 launch type, the __maximum
-- percent__ value is set to the default value and is used to define the
-- upper limit on the number of the tasks in the service that remain in the
-- @RUNNING@ state while the container instances are in the @DRAINING@
-- state. If the tasks in the service use the Fargate launch type, the
-- maximum percent value is not used, although it is returned when
-- describing your service.
--
-- 'minimumHealthyPercent', 'deploymentConfiguration_minimumHealthyPercent' - If a service is using the rolling update (@ECS@) deployment type, the
-- __minimum healthy percent__ represents a lower limit on the number of
-- tasks in a service that must remain in the @RUNNING@ state during a
-- deployment, as a percentage of the desired number of tasks (rounded up
-- to the nearest integer), and while any container instances are in the
-- @DRAINING@ state if the service contains tasks using the EC2 launch
-- type. This parameter enables you to deploy without using additional
-- cluster capacity. For example, if your service has a desired number of
-- four tasks and a minimum healthy percent of 50%, the scheduler may stop
-- two existing tasks to free up cluster capacity before starting two new
-- tasks. Tasks for services that /do not/ use a load balancer are
-- considered healthy if they are in the @RUNNING@ state; tasks for
-- services that /do/ use a load balancer are considered healthy if they
-- are in the @RUNNING@ state and they are reported as healthy by the load
-- balancer. The default value for minimum healthy percent is 100%.
--
-- If a service is using the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
-- deployment types and tasks that use the EC2 launch type, the __minimum
-- healthy percent__ value is set to the default value and is used to
-- define the lower limit on the number of the tasks in the service that
-- remain in the @RUNNING@ state while the container instances are in the
-- @DRAINING@ state. If the tasks in the service use the Fargate launch
-- type, the minimum healthy percent value is not used, although it is
-- returned when describing your service.
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
    { maximumPercent =
        Prelude.Nothing,
      minimumHealthyPercent = Prelude.Nothing,
      deploymentCircuitBreaker = Prelude.Nothing
    }

-- | If a service is using the rolling update (@ECS@) deployment type, the
-- __maximum percent__ parameter represents an upper limit on the number of
-- tasks in a service that are allowed in the @RUNNING@ or @PENDING@ state
-- during a deployment, as a percentage of the desired number of tasks
-- (rounded down to the nearest integer), and while any container instances
-- are in the @DRAINING@ state if the service contains tasks using the EC2
-- launch type. This parameter enables you to define the deployment batch
-- size. For example, if your service has a desired number of four tasks
-- and a maximum percent value of 200%, the scheduler may start four new
-- tasks before stopping the four older tasks (provided that the cluster
-- resources required to do this are available). The default value for
-- maximum percent is 200%.
--
-- If a service is using the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
-- deployment types and tasks that use the EC2 launch type, the __maximum
-- percent__ value is set to the default value and is used to define the
-- upper limit on the number of the tasks in the service that remain in the
-- @RUNNING@ state while the container instances are in the @DRAINING@
-- state. If the tasks in the service use the Fargate launch type, the
-- maximum percent value is not used, although it is returned when
-- describing your service.
deploymentConfiguration_maximumPercent :: Lens.Lens' DeploymentConfiguration (Prelude.Maybe Prelude.Int)
deploymentConfiguration_maximumPercent = Lens.lens (\DeploymentConfiguration' {maximumPercent} -> maximumPercent) (\s@DeploymentConfiguration' {} a -> s {maximumPercent = a} :: DeploymentConfiguration)

-- | If a service is using the rolling update (@ECS@) deployment type, the
-- __minimum healthy percent__ represents a lower limit on the number of
-- tasks in a service that must remain in the @RUNNING@ state during a
-- deployment, as a percentage of the desired number of tasks (rounded up
-- to the nearest integer), and while any container instances are in the
-- @DRAINING@ state if the service contains tasks using the EC2 launch
-- type. This parameter enables you to deploy without using additional
-- cluster capacity. For example, if your service has a desired number of
-- four tasks and a minimum healthy percent of 50%, the scheduler may stop
-- two existing tasks to free up cluster capacity before starting two new
-- tasks. Tasks for services that /do not/ use a load balancer are
-- considered healthy if they are in the @RUNNING@ state; tasks for
-- services that /do/ use a load balancer are considered healthy if they
-- are in the @RUNNING@ state and they are reported as healthy by the load
-- balancer. The default value for minimum healthy percent is 100%.
--
-- If a service is using the blue\/green (@CODE_DEPLOY@) or @EXTERNAL@
-- deployment types and tasks that use the EC2 launch type, the __minimum
-- healthy percent__ value is set to the default value and is used to
-- define the lower limit on the number of the tasks in the service that
-- remain in the @RUNNING@ state while the container instances are in the
-- @DRAINING@ state. If the tasks in the service use the Fargate launch
-- type, the minimum healthy percent value is not used, although it is
-- returned when describing your service.
deploymentConfiguration_minimumHealthyPercent :: Lens.Lens' DeploymentConfiguration (Prelude.Maybe Prelude.Int)
deploymentConfiguration_minimumHealthyPercent = Lens.lens (\DeploymentConfiguration' {minimumHealthyPercent} -> minimumHealthyPercent) (\s@DeploymentConfiguration' {} a -> s {minimumHealthyPercent = a} :: DeploymentConfiguration)

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

instance Prelude.FromJSON DeploymentConfiguration where
  parseJSON =
    Prelude.withObject
      "DeploymentConfiguration"
      ( \x ->
          DeploymentConfiguration'
            Prelude.<$> (x Prelude..:? "maximumPercent")
            Prelude.<*> (x Prelude..:? "minimumHealthyPercent")
            Prelude.<*> (x Prelude..:? "deploymentCircuitBreaker")
      )

instance Prelude.Hashable DeploymentConfiguration

instance Prelude.NFData DeploymentConfiguration

instance Prelude.ToJSON DeploymentConfiguration where
  toJSON DeploymentConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("maximumPercent" Prelude..=)
              Prelude.<$> maximumPercent,
            ("minimumHealthyPercent" Prelude..=)
              Prelude.<$> minimumHealthyPercent,
            ("deploymentCircuitBreaker" Prelude..=)
              Prelude.<$> deploymentCircuitBreaker
          ]
      )
