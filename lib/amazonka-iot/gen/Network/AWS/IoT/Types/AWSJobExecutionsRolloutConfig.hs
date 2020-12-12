{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobExecutionsRolloutConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobExecutionsRolloutConfig
  ( AWSJobExecutionsRolloutConfig (..),

    -- * Smart constructor
    mkAWSJobExecutionsRolloutConfig,

    -- * Lenses
    ajercExponentialRate,
    ajercMaximumPerMinute,
  )
where

import Network.AWS.IoT.Types.AWSJobExponentialRolloutRate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration for the rollout of OTA updates.
--
-- /See:/ 'mkAWSJobExecutionsRolloutConfig' smart constructor.
data AWSJobExecutionsRolloutConfig = AWSJobExecutionsRolloutConfig'
  { exponentialRate ::
      Lude.Maybe
        AWSJobExponentialRolloutRate,
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

-- | Creates a value of 'AWSJobExecutionsRolloutConfig' with the minimum fields required to make a request.
--
-- * 'exponentialRate' - The rate of increase for a job rollout. This parameter allows you to define an exponential rate increase for a job rollout.
-- * 'maximumPerMinute' - The maximum number of OTA update job executions started per minute.
mkAWSJobExecutionsRolloutConfig ::
  AWSJobExecutionsRolloutConfig
mkAWSJobExecutionsRolloutConfig =
  AWSJobExecutionsRolloutConfig'
    { exponentialRate = Lude.Nothing,
      maximumPerMinute = Lude.Nothing
    }

-- | The rate of increase for a job rollout. This parameter allows you to define an exponential rate increase for a job rollout.
--
-- /Note:/ Consider using 'exponentialRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajercExponentialRate :: Lens.Lens' AWSJobExecutionsRolloutConfig (Lude.Maybe AWSJobExponentialRolloutRate)
ajercExponentialRate = Lens.lens (exponentialRate :: AWSJobExecutionsRolloutConfig -> Lude.Maybe AWSJobExponentialRolloutRate) (\s a -> s {exponentialRate = a} :: AWSJobExecutionsRolloutConfig)
{-# DEPRECATED ajercExponentialRate "Use generic-lens or generic-optics with 'exponentialRate' instead." #-}

-- | The maximum number of OTA update job executions started per minute.
--
-- /Note:/ Consider using 'maximumPerMinute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajercMaximumPerMinute :: Lens.Lens' AWSJobExecutionsRolloutConfig (Lude.Maybe Lude.Natural)
ajercMaximumPerMinute = Lens.lens (maximumPerMinute :: AWSJobExecutionsRolloutConfig -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPerMinute = a} :: AWSJobExecutionsRolloutConfig)
{-# DEPRECATED ajercMaximumPerMinute "Use generic-lens or generic-optics with 'maximumPerMinute' instead." #-}

instance Lude.FromJSON AWSJobExecutionsRolloutConfig where
  parseJSON =
    Lude.withObject
      "AWSJobExecutionsRolloutConfig"
      ( \x ->
          AWSJobExecutionsRolloutConfig'
            Lude.<$> (x Lude..:? "exponentialRate")
            Lude.<*> (x Lude..:? "maximumPerMinute")
      )

instance Lude.ToJSON AWSJobExecutionsRolloutConfig where
  toJSON AWSJobExecutionsRolloutConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("exponentialRate" Lude..=) Lude.<$> exponentialRate,
            ("maximumPerMinute" Lude..=) Lude.<$> maximumPerMinute
          ]
      )
