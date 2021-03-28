{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AwsJobExecutionsRolloutConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AwsJobExecutionsRolloutConfig
  ( AwsJobExecutionsRolloutConfig (..)
  -- * Smart constructor
  , mkAwsJobExecutionsRolloutConfig
  -- * Lenses
  , ajercExponentialRate
  , ajercMaximumPerMinute
  ) where

import qualified Network.AWS.IoT.Types.AwsJobExponentialRolloutRate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration for the rollout of OTA updates.
--
-- /See:/ 'mkAwsJobExecutionsRolloutConfig' smart constructor.
data AwsJobExecutionsRolloutConfig = AwsJobExecutionsRolloutConfig'
  { exponentialRate :: Core.Maybe Types.AwsJobExponentialRolloutRate
    -- ^ The rate of increase for a job rollout. This parameter allows you to define an exponential rate increase for a job rollout.
  , maximumPerMinute :: Core.Maybe Core.Natural
    -- ^ The maximum number of OTA update job executions started per minute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsJobExecutionsRolloutConfig' value with any optional fields omitted.
mkAwsJobExecutionsRolloutConfig
    :: AwsJobExecutionsRolloutConfig
mkAwsJobExecutionsRolloutConfig
  = AwsJobExecutionsRolloutConfig'{exponentialRate = Core.Nothing,
                                   maximumPerMinute = Core.Nothing}

-- | The rate of increase for a job rollout. This parameter allows you to define an exponential rate increase for a job rollout.
--
-- /Note:/ Consider using 'exponentialRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajercExponentialRate :: Lens.Lens' AwsJobExecutionsRolloutConfig (Core.Maybe Types.AwsJobExponentialRolloutRate)
ajercExponentialRate = Lens.field @"exponentialRate"
{-# INLINEABLE ajercExponentialRate #-}
{-# DEPRECATED exponentialRate "Use generic-lens or generic-optics with 'exponentialRate' instead"  #-}

-- | The maximum number of OTA update job executions started per minute.
--
-- /Note:/ Consider using 'maximumPerMinute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ajercMaximumPerMinute :: Lens.Lens' AwsJobExecutionsRolloutConfig (Core.Maybe Core.Natural)
ajercMaximumPerMinute = Lens.field @"maximumPerMinute"
{-# INLINEABLE ajercMaximumPerMinute #-}
{-# DEPRECATED maximumPerMinute "Use generic-lens or generic-optics with 'maximumPerMinute' instead"  #-}

instance Core.FromJSON AwsJobExecutionsRolloutConfig where
        toJSON AwsJobExecutionsRolloutConfig{..}
          = Core.object
              (Core.catMaybes
                 [("exponentialRate" Core..=) Core.<$> exponentialRate,
                  ("maximumPerMinute" Core..=) Core.<$> maximumPerMinute])

instance Core.FromJSON AwsJobExecutionsRolloutConfig where
        parseJSON
          = Core.withObject "AwsJobExecutionsRolloutConfig" Core.$
              \ x ->
                AwsJobExecutionsRolloutConfig' Core.<$>
                  (x Core..:? "exponentialRate") Core.<*>
                    x Core..:? "maximumPerMinute"
