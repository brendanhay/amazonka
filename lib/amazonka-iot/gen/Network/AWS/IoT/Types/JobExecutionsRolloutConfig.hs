{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionsRolloutConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionsRolloutConfig
  ( JobExecutionsRolloutConfig (..),

    -- * Smart constructor
    mkJobExecutionsRolloutConfig,

    -- * Lenses
    jercExponentialRate,
    jercMaximumPerMinute,
  )
where

import qualified Network.AWS.IoT.Types.ExponentialRolloutRate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Allows you to create a staged rollout of a job.
--
-- /See:/ 'mkJobExecutionsRolloutConfig' smart constructor.
data JobExecutionsRolloutConfig = JobExecutionsRolloutConfig'
  { -- | The rate of increase for a job rollout. This parameter allows you to define an exponential rate for a job rollout.
    exponentialRate :: Core.Maybe Types.ExponentialRolloutRate,
    -- | The maximum number of things that will be notified of a pending job, per minute. This parameter allows you to create a staged rollout.
    maximumPerMinute :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobExecutionsRolloutConfig' value with any optional fields omitted.
mkJobExecutionsRolloutConfig ::
  JobExecutionsRolloutConfig
mkJobExecutionsRolloutConfig =
  JobExecutionsRolloutConfig'
    { exponentialRate = Core.Nothing,
      maximumPerMinute = Core.Nothing
    }

-- | The rate of increase for a job rollout. This parameter allows you to define an exponential rate for a job rollout.
--
-- /Note:/ Consider using 'exponentialRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jercExponentialRate :: Lens.Lens' JobExecutionsRolloutConfig (Core.Maybe Types.ExponentialRolloutRate)
jercExponentialRate = Lens.field @"exponentialRate"
{-# DEPRECATED jercExponentialRate "Use generic-lens or generic-optics with 'exponentialRate' instead." #-}

-- | The maximum number of things that will be notified of a pending job, per minute. This parameter allows you to create a staged rollout.
--
-- /Note:/ Consider using 'maximumPerMinute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jercMaximumPerMinute :: Lens.Lens' JobExecutionsRolloutConfig (Core.Maybe Core.Natural)
jercMaximumPerMinute = Lens.field @"maximumPerMinute"
{-# DEPRECATED jercMaximumPerMinute "Use generic-lens or generic-optics with 'maximumPerMinute' instead." #-}

instance Core.FromJSON JobExecutionsRolloutConfig where
  toJSON JobExecutionsRolloutConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("exponentialRate" Core..=) Core.<$> exponentialRate,
            ("maximumPerMinute" Core..=) Core.<$> maximumPerMinute
          ]
      )

instance Core.FromJSON JobExecutionsRolloutConfig where
  parseJSON =
    Core.withObject "JobExecutionsRolloutConfig" Core.$
      \x ->
        JobExecutionsRolloutConfig'
          Core.<$> (x Core..:? "exponentialRate")
          Core.<*> (x Core..:? "maximumPerMinute")
