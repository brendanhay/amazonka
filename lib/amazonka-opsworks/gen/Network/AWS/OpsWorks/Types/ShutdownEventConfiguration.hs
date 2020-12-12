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
    secExecutionTimeout,
    secDelayUntilElbConnectionsDrained,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Shutdown event configuration.
--
-- /See:/ 'mkShutdownEventConfiguration' smart constructor.
data ShutdownEventConfiguration = ShutdownEventConfiguration'
  { executionTimeout ::
      Lude.Maybe Lude.Int,
    delayUntilElbConnectionsDrained ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShutdownEventConfiguration' with the minimum fields required to make a request.
--
-- * 'delayUntilElbConnectionsDrained' - Whether to enable Elastic Load Balancing connection draining. For more information, see <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
-- * 'executionTimeout' - The time, in seconds, that AWS OpsWorks Stacks will wait after triggering a Shutdown event before shutting down an instance.
mkShutdownEventConfiguration ::
  ShutdownEventConfiguration
mkShutdownEventConfiguration =
  ShutdownEventConfiguration'
    { executionTimeout = Lude.Nothing,
      delayUntilElbConnectionsDrained = Lude.Nothing
    }

-- | The time, in seconds, that AWS OpsWorks Stacks will wait after triggering a Shutdown event before shutting down an instance.
--
-- /Note:/ Consider using 'executionTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secExecutionTimeout :: Lens.Lens' ShutdownEventConfiguration (Lude.Maybe Lude.Int)
secExecutionTimeout = Lens.lens (executionTimeout :: ShutdownEventConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {executionTimeout = a} :: ShutdownEventConfiguration)
{-# DEPRECATED secExecutionTimeout "Use generic-lens or generic-optics with 'executionTimeout' instead." #-}

-- | Whether to enable Elastic Load Balancing connection draining. For more information, see <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
--
-- /Note:/ Consider using 'delayUntilElbConnectionsDrained' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secDelayUntilElbConnectionsDrained :: Lens.Lens' ShutdownEventConfiguration (Lude.Maybe Lude.Bool)
secDelayUntilElbConnectionsDrained = Lens.lens (delayUntilElbConnectionsDrained :: ShutdownEventConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {delayUntilElbConnectionsDrained = a} :: ShutdownEventConfiguration)
{-# DEPRECATED secDelayUntilElbConnectionsDrained "Use generic-lens or generic-optics with 'delayUntilElbConnectionsDrained' instead." #-}

instance Lude.FromJSON ShutdownEventConfiguration where
  parseJSON =
    Lude.withObject
      "ShutdownEventConfiguration"
      ( \x ->
          ShutdownEventConfiguration'
            Lude.<$> (x Lude..:? "ExecutionTimeout")
            Lude.<*> (x Lude..:? "DelayUntilElbConnectionsDrained")
      )

instance Lude.ToJSON ShutdownEventConfiguration where
  toJSON ShutdownEventConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExecutionTimeout" Lude..=) Lude.<$> executionTimeout,
            ("DelayUntilElbConnectionsDrained" Lude..=)
              Lude.<$> delayUntilElbConnectionsDrained
          ]
      )
