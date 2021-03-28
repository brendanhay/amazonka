{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeploymentCircuitBreaker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.DeploymentCircuitBreaker
  ( DeploymentCircuitBreaker (..)
  -- * Smart constructor
  , mkDeploymentCircuitBreaker
  -- * Lenses
  , dcbEnable
  , dcbRollback
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The __deployment circuit breaker__ determines whether a service deployment will fail if the service can't reach a steady state. If enabled, a service deployment will transition to a failed state and stop launching new tasks. You can also enable Amazon ECS to roll back your service to the last completed deployment after a failure. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html Rolling update> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkDeploymentCircuitBreaker' smart constructor.
data DeploymentCircuitBreaker = DeploymentCircuitBreaker'
  { enable :: Core.Bool
    -- ^ Whether to enable the deployment circuit breaker logic for the service.
  , rollback :: Core.Bool
    -- ^ Whether to enable Amazon ECS to roll back the service if a service deployment fails. If rollback is enabled, when a service deployment fails, the service is rolled back to the last deployment that completed successfully.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeploymentCircuitBreaker' value with any optional fields omitted.
mkDeploymentCircuitBreaker
    :: Core.Bool -- ^ 'enable'
    -> Core.Bool -- ^ 'rollback'
    -> DeploymentCircuitBreaker
mkDeploymentCircuitBreaker enable rollback
  = DeploymentCircuitBreaker'{enable, rollback}

-- | Whether to enable the deployment circuit breaker logic for the service.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbEnable :: Lens.Lens' DeploymentCircuitBreaker Core.Bool
dcbEnable = Lens.field @"enable"
{-# INLINEABLE dcbEnable #-}
{-# DEPRECATED enable "Use generic-lens or generic-optics with 'enable' instead"  #-}

-- | Whether to enable Amazon ECS to roll back the service if a service deployment fails. If rollback is enabled, when a service deployment fails, the service is rolled back to the last deployment that completed successfully.
--
-- /Note:/ Consider using 'rollback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbRollback :: Lens.Lens' DeploymentCircuitBreaker Core.Bool
dcbRollback = Lens.field @"rollback"
{-# INLINEABLE dcbRollback #-}
{-# DEPRECATED rollback "Use generic-lens or generic-optics with 'rollback' instead"  #-}

instance Core.FromJSON DeploymentCircuitBreaker where
        toJSON DeploymentCircuitBreaker{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("enable" Core..= enable),
                  Core.Just ("rollback" Core..= rollback)])

instance Core.FromJSON DeploymentCircuitBreaker where
        parseJSON
          = Core.withObject "DeploymentCircuitBreaker" Core.$
              \ x ->
                DeploymentCircuitBreaker' Core.<$>
                  (x Core..: "enable") Core.<*> x Core..: "rollback"
