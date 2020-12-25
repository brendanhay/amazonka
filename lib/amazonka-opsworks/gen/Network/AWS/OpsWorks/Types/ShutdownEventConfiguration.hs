{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
  ( ShutdownEventConfiguration (..),

    -- * Smart constructor
    mkShutdownEventConfiguration,

    -- * Lenses
    secDelayUntilElbConnectionsDrained,
    secExecutionTimeout,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Shutdown event configuration.
--
-- /See:/ 'mkShutdownEventConfiguration' smart constructor.
data ShutdownEventConfiguration = ShutdownEventConfiguration'
  { -- | Whether to enable Elastic Load Balancing connection draining. For more information, see <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
    delayUntilElbConnectionsDrained :: Core.Maybe Core.Bool,
    -- | The time, in seconds, that AWS OpsWorks Stacks will wait after triggering a Shutdown event before shutting down an instance.
    executionTimeout :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ShutdownEventConfiguration' value with any optional fields omitted.
mkShutdownEventConfiguration ::
  ShutdownEventConfiguration
mkShutdownEventConfiguration =
  ShutdownEventConfiguration'
    { delayUntilElbConnectionsDrained =
        Core.Nothing,
      executionTimeout = Core.Nothing
    }

-- | Whether to enable Elastic Load Balancing connection draining. For more information, see <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
--
-- /Note:/ Consider using 'delayUntilElbConnectionsDrained' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secDelayUntilElbConnectionsDrained :: Lens.Lens' ShutdownEventConfiguration (Core.Maybe Core.Bool)
secDelayUntilElbConnectionsDrained = Lens.field @"delayUntilElbConnectionsDrained"
{-# DEPRECATED secDelayUntilElbConnectionsDrained "Use generic-lens or generic-optics with 'delayUntilElbConnectionsDrained' instead." #-}

-- | The time, in seconds, that AWS OpsWorks Stacks will wait after triggering a Shutdown event before shutting down an instance.
--
-- /Note:/ Consider using 'executionTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secExecutionTimeout :: Lens.Lens' ShutdownEventConfiguration (Core.Maybe Core.Int)
secExecutionTimeout = Lens.field @"executionTimeout"
{-# DEPRECATED secExecutionTimeout "Use generic-lens or generic-optics with 'executionTimeout' instead." #-}

instance Core.FromJSON ShutdownEventConfiguration where
  toJSON ShutdownEventConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("DelayUntilElbConnectionsDrained" Core..=)
              Core.<$> delayUntilElbConnectionsDrained,
            ("ExecutionTimeout" Core..=) Core.<$> executionTimeout
          ]
      )

instance Core.FromJSON ShutdownEventConfiguration where
  parseJSON =
    Core.withObject "ShutdownEventConfiguration" Core.$
      \x ->
        ShutdownEventConfiguration'
          Core.<$> (x Core..:? "DelayUntilElbConnectionsDrained")
          Core.<*> (x Core..:? "ExecutionTimeout")
