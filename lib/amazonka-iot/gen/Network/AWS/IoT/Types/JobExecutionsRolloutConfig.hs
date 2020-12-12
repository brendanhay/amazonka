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

import Network.AWS.IoT.Types.ExponentialRolloutRate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Allows you to create a staged rollout of a job.
--
-- /See:/ 'mkJobExecutionsRolloutConfig' smart constructor.
data JobExecutionsRolloutConfig = JobExecutionsRolloutConfig'
  { exponentialRate ::
      Lude.Maybe ExponentialRolloutRate,
    maximumPerMinute ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobExecutionsRolloutConfig' with the minimum fields required to make a request.
--
-- * 'exponentialRate' - The rate of increase for a job rollout. This parameter allows you to define an exponential rate for a job rollout.
-- * 'maximumPerMinute' - The maximum number of things that will be notified of a pending job, per minute. This parameter allows you to create a staged rollout.
mkJobExecutionsRolloutConfig ::
  JobExecutionsRolloutConfig
mkJobExecutionsRolloutConfig =
  JobExecutionsRolloutConfig'
    { exponentialRate = Lude.Nothing,
      maximumPerMinute = Lude.Nothing
    }

-- | The rate of increase for a job rollout. This parameter allows you to define an exponential rate for a job rollout.
--
-- /Note:/ Consider using 'exponentialRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jercExponentialRate :: Lens.Lens' JobExecutionsRolloutConfig (Lude.Maybe ExponentialRolloutRate)
jercExponentialRate = Lens.lens (exponentialRate :: JobExecutionsRolloutConfig -> Lude.Maybe ExponentialRolloutRate) (\s a -> s {exponentialRate = a} :: JobExecutionsRolloutConfig)
{-# DEPRECATED jercExponentialRate "Use generic-lens or generic-optics with 'exponentialRate' instead." #-}

-- | The maximum number of things that will be notified of a pending job, per minute. This parameter allows you to create a staged rollout.
--
-- /Note:/ Consider using 'maximumPerMinute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jercMaximumPerMinute :: Lens.Lens' JobExecutionsRolloutConfig (Lude.Maybe Lude.Natural)
jercMaximumPerMinute = Lens.lens (maximumPerMinute :: JobExecutionsRolloutConfig -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPerMinute = a} :: JobExecutionsRolloutConfig)
{-# DEPRECATED jercMaximumPerMinute "Use generic-lens or generic-optics with 'maximumPerMinute' instead." #-}

instance Lude.FromJSON JobExecutionsRolloutConfig where
  parseJSON =
    Lude.withObject
      "JobExecutionsRolloutConfig"
      ( \x ->
          JobExecutionsRolloutConfig'
            Lude.<$> (x Lude..:? "exponentialRate")
            Lude.<*> (x Lude..:? "maximumPerMinute")
      )

instance Lude.ToJSON JobExecutionsRolloutConfig where
  toJSON JobExecutionsRolloutConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("exponentialRate" Lude..=) Lude.<$> exponentialRate,
            ("maximumPerMinute" Lude..=) Lude.<$> maximumPerMinute
          ]
      )
