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
-- Module      : Network.AWS.ECS.Types.DeploymentCircuitBreaker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentCircuitBreaker where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The deployment circuit breaker can only be used for services using the
-- rolling update (@ECS@) deployment type that are not behind a Classic
-- Load Balancer.
--
-- The __deployment circuit breaker__ determines whether a service
-- deployment will fail if the service can\'t reach a steady state. If
-- enabled, a service deployment will transition to a failed state and stop
-- launching new tasks. You can also enable Amazon ECS to roll back your
-- service to the last completed deployment after a failure. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html Rolling update>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newDeploymentCircuitBreaker' smart constructor.
data DeploymentCircuitBreaker = DeploymentCircuitBreaker'
  { -- | Whether to enable the deployment circuit breaker logic for the service.
    enable :: Core.Bool,
    -- | Whether to enable Amazon ECS to roll back the service if a service
    -- deployment fails. If rollback is enabled, when a service deployment
    -- fails, the service is rolled back to the last deployment that completed
    -- successfully.
    rollback :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeploymentCircuitBreaker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enable', 'deploymentCircuitBreaker_enable' - Whether to enable the deployment circuit breaker logic for the service.
--
-- 'rollback', 'deploymentCircuitBreaker_rollback' - Whether to enable Amazon ECS to roll back the service if a service
-- deployment fails. If rollback is enabled, when a service deployment
-- fails, the service is rolled back to the last deployment that completed
-- successfully.
newDeploymentCircuitBreaker ::
  -- | 'enable'
  Core.Bool ->
  -- | 'rollback'
  Core.Bool ->
  DeploymentCircuitBreaker
newDeploymentCircuitBreaker pEnable_ pRollback_ =
  DeploymentCircuitBreaker'
    { enable = pEnable_,
      rollback = pRollback_
    }

-- | Whether to enable the deployment circuit breaker logic for the service.
deploymentCircuitBreaker_enable :: Lens.Lens' DeploymentCircuitBreaker Core.Bool
deploymentCircuitBreaker_enable = Lens.lens (\DeploymentCircuitBreaker' {enable} -> enable) (\s@DeploymentCircuitBreaker' {} a -> s {enable = a} :: DeploymentCircuitBreaker)

-- | Whether to enable Amazon ECS to roll back the service if a service
-- deployment fails. If rollback is enabled, when a service deployment
-- fails, the service is rolled back to the last deployment that completed
-- successfully.
deploymentCircuitBreaker_rollback :: Lens.Lens' DeploymentCircuitBreaker Core.Bool
deploymentCircuitBreaker_rollback = Lens.lens (\DeploymentCircuitBreaker' {rollback} -> rollback) (\s@DeploymentCircuitBreaker' {} a -> s {rollback = a} :: DeploymentCircuitBreaker)

instance Core.FromJSON DeploymentCircuitBreaker where
  parseJSON =
    Core.withObject
      "DeploymentCircuitBreaker"
      ( \x ->
          DeploymentCircuitBreaker'
            Core.<$> (x Core..: "enable") Core.<*> (x Core..: "rollback")
      )

instance Core.Hashable DeploymentCircuitBreaker

instance Core.NFData DeploymentCircuitBreaker

instance Core.ToJSON DeploymentCircuitBreaker where
  toJSON DeploymentCircuitBreaker' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("enable" Core..= enable),
            Core.Just ("rollback" Core..= rollback)
          ]
      )
