{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeploymentCircuitBreaker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentCircuitBreaker
  ( DeploymentCircuitBreaker (..),

    -- * Smart constructor
    mkDeploymentCircuitBreaker,

    -- * Lenses
    dcbEnable,
    dcbRollback,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The __deployment circuit breaker__ determines whether a service deployment will fail if the service can't reach a steady state. If enabled, a service deployment will transition to a failed state and stop launching new tasks. You can also enable Amazon ECS to roll back your service to the last completed deployment after a failure. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-type-ecs.html Rolling update> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkDeploymentCircuitBreaker' smart constructor.
data DeploymentCircuitBreaker = DeploymentCircuitBreaker'
  { enable ::
      Lude.Bool,
    rollback :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentCircuitBreaker' with the minimum fields required to make a request.
--
-- * 'enable' - Whether to enable the deployment circuit breaker logic for the service.
-- * 'rollback' - Whether to enable Amazon ECS to roll back the service if a service deployment fails. If rollback is enabled, when a service deployment fails, the service is rolled back to the last deployment that completed successfully.
mkDeploymentCircuitBreaker ::
  -- | 'enable'
  Lude.Bool ->
  -- | 'rollback'
  Lude.Bool ->
  DeploymentCircuitBreaker
mkDeploymentCircuitBreaker pEnable_ pRollback_ =
  DeploymentCircuitBreaker'
    { enable = pEnable_,
      rollback = pRollback_
    }

-- | Whether to enable the deployment circuit breaker logic for the service.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbEnable :: Lens.Lens' DeploymentCircuitBreaker Lude.Bool
dcbEnable = Lens.lens (enable :: DeploymentCircuitBreaker -> Lude.Bool) (\s a -> s {enable = a} :: DeploymentCircuitBreaker)
{-# DEPRECATED dcbEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | Whether to enable Amazon ECS to roll back the service if a service deployment fails. If rollback is enabled, when a service deployment fails, the service is rolled back to the last deployment that completed successfully.
--
-- /Note:/ Consider using 'rollback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbRollback :: Lens.Lens' DeploymentCircuitBreaker Lude.Bool
dcbRollback = Lens.lens (rollback :: DeploymentCircuitBreaker -> Lude.Bool) (\s a -> s {rollback = a} :: DeploymentCircuitBreaker)
{-# DEPRECATED dcbRollback "Use generic-lens or generic-optics with 'rollback' instead." #-}

instance Lude.FromJSON DeploymentCircuitBreaker where
  parseJSON =
    Lude.withObject
      "DeploymentCircuitBreaker"
      ( \x ->
          DeploymentCircuitBreaker'
            Lude.<$> (x Lude..: "enable") Lude.<*> (x Lude..: "rollback")
      )

instance Lude.ToJSON DeploymentCircuitBreaker where
  toJSON DeploymentCircuitBreaker' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("enable" Lude..= enable),
            Lude.Just ("rollback" Lude..= rollback)
          ]
      )
