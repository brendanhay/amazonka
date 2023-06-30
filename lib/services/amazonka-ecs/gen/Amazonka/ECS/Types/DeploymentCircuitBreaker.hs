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
-- Module      : Amazonka.ECS.Types.DeploymentCircuitBreaker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.DeploymentCircuitBreaker where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The deployment circuit breaker can only be used for services using the
-- rolling update (@ECS@) deployment type that aren\'t behind a Classic
-- Load Balancer.
--
-- The __deployment circuit breaker__ determines whether a service
-- deployment will fail if the service can\'t reach a steady state. If
-- enabled, a service deployment will transition to a failed state and stop
-- launching new tasks. You can also configure Amazon ECS to roll back your
-- service to the last completed deployment after a failure. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html Rolling update>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newDeploymentCircuitBreaker' smart constructor.
data DeploymentCircuitBreaker = DeploymentCircuitBreaker'
  { -- | Determines whether to use the deployment circuit breaker logic for the
    -- service.
    enable :: Prelude.Bool,
    -- | Determines whether to configure Amazon ECS to roll back the service if a
    -- service deployment fails. If rollback is on, when a service deployment
    -- fails, the service is rolled back to the last deployment that completed
    -- successfully.
    rollback :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentCircuitBreaker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enable', 'deploymentCircuitBreaker_enable' - Determines whether to use the deployment circuit breaker logic for the
-- service.
--
-- 'rollback', 'deploymentCircuitBreaker_rollback' - Determines whether to configure Amazon ECS to roll back the service if a
-- service deployment fails. If rollback is on, when a service deployment
-- fails, the service is rolled back to the last deployment that completed
-- successfully.
newDeploymentCircuitBreaker ::
  -- | 'enable'
  Prelude.Bool ->
  -- | 'rollback'
  Prelude.Bool ->
  DeploymentCircuitBreaker
newDeploymentCircuitBreaker pEnable_ pRollback_ =
  DeploymentCircuitBreaker'
    { enable = pEnable_,
      rollback = pRollback_
    }

-- | Determines whether to use the deployment circuit breaker logic for the
-- service.
deploymentCircuitBreaker_enable :: Lens.Lens' DeploymentCircuitBreaker Prelude.Bool
deploymentCircuitBreaker_enable = Lens.lens (\DeploymentCircuitBreaker' {enable} -> enable) (\s@DeploymentCircuitBreaker' {} a -> s {enable = a} :: DeploymentCircuitBreaker)

-- | Determines whether to configure Amazon ECS to roll back the service if a
-- service deployment fails. If rollback is on, when a service deployment
-- fails, the service is rolled back to the last deployment that completed
-- successfully.
deploymentCircuitBreaker_rollback :: Lens.Lens' DeploymentCircuitBreaker Prelude.Bool
deploymentCircuitBreaker_rollback = Lens.lens (\DeploymentCircuitBreaker' {rollback} -> rollback) (\s@DeploymentCircuitBreaker' {} a -> s {rollback = a} :: DeploymentCircuitBreaker)

instance Data.FromJSON DeploymentCircuitBreaker where
  parseJSON =
    Data.withObject
      "DeploymentCircuitBreaker"
      ( \x ->
          DeploymentCircuitBreaker'
            Prelude.<$> (x Data..: "enable")
            Prelude.<*> (x Data..: "rollback")
      )

instance Prelude.Hashable DeploymentCircuitBreaker where
  hashWithSalt _salt DeploymentCircuitBreaker' {..} =
    _salt
      `Prelude.hashWithSalt` enable
      `Prelude.hashWithSalt` rollback

instance Prelude.NFData DeploymentCircuitBreaker where
  rnf DeploymentCircuitBreaker' {..} =
    Prelude.rnf enable
      `Prelude.seq` Prelude.rnf rollback

instance Data.ToJSON DeploymentCircuitBreaker where
  toJSON DeploymentCircuitBreaker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("enable" Data..= enable),
            Prelude.Just ("rollback" Data..= rollback)
          ]
      )
