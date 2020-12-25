{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.BehaviorCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BehaviorCriteria
  ( BehaviorCriteria (..),

    -- * Smart constructor
    mkBehaviorCriteria,

    -- * Lenses
    bcComparisonOperator,
    bcConsecutiveDatapointsToAlarm,
    bcConsecutiveDatapointsToClear,
    bcDurationSeconds,
    bcStatisticalThreshold,
    bcValue,
  )
where

import qualified Network.AWS.IoT.Types.ComparisonOperator as Types
import qualified Network.AWS.IoT.Types.MetricValue as Types
import qualified Network.AWS.IoT.Types.StatisticalThreshold as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The criteria by which the behavior is determined to be normal.
--
-- /See:/ 'mkBehaviorCriteria' smart constructor.
data BehaviorCriteria = BehaviorCriteria'
  { -- | The operator that relates the thing measured (@metric@ ) to the criteria (containing a @value@ or @statisticalThreshold@ ).
    comparisonOperator :: Core.Maybe Types.ComparisonOperator,
    -- | If a device is in violation of the behavior for the specified number of consecutive datapoints, an alarm occurs. If not specified, the default is 1.
    consecutiveDatapointsToAlarm :: Core.Maybe Core.Natural,
    -- | If an alarm has occurred and the offending device is no longer in violation of the behavior for the specified number of consecutive datapoints, the alarm is cleared. If not specified, the default is 1.
    consecutiveDatapointsToClear :: Core.Maybe Core.Natural,
    -- | Use this to specify the time duration over which the behavior is evaluated, for those criteria which have a time dimension (for example, @NUM_MESSAGES_SENT@ ). For a @statisticalThreshhold@ metric comparison, measurements from all devices are accumulated over this time duration before being used to calculate percentiles, and later, measurements from an individual device are also accumulated over this time duration before being given a percentile rank.
    durationSeconds :: Core.Maybe Core.Int,
    -- | A statistical ranking (percentile) which indicates a threshold value by which a behavior is determined to be in compliance or in violation of the behavior.
    statisticalThreshold :: Core.Maybe Types.StatisticalThreshold,
    -- | The value to be compared with the @metric@ .
    value :: Core.Maybe Types.MetricValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BehaviorCriteria' value with any optional fields omitted.
mkBehaviorCriteria ::
  BehaviorCriteria
mkBehaviorCriteria =
  BehaviorCriteria'
    { comparisonOperator = Core.Nothing,
      consecutiveDatapointsToAlarm = Core.Nothing,
      consecutiveDatapointsToClear = Core.Nothing,
      durationSeconds = Core.Nothing,
      statisticalThreshold = Core.Nothing,
      value = Core.Nothing
    }

-- | The operator that relates the thing measured (@metric@ ) to the criteria (containing a @value@ or @statisticalThreshold@ ).
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcComparisonOperator :: Lens.Lens' BehaviorCriteria (Core.Maybe Types.ComparisonOperator)
bcComparisonOperator = Lens.field @"comparisonOperator"
{-# DEPRECATED bcComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | If a device is in violation of the behavior for the specified number of consecutive datapoints, an alarm occurs. If not specified, the default is 1.
--
-- /Note:/ Consider using 'consecutiveDatapointsToAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcConsecutiveDatapointsToAlarm :: Lens.Lens' BehaviorCriteria (Core.Maybe Core.Natural)
bcConsecutiveDatapointsToAlarm = Lens.field @"consecutiveDatapointsToAlarm"
{-# DEPRECATED bcConsecutiveDatapointsToAlarm "Use generic-lens or generic-optics with 'consecutiveDatapointsToAlarm' instead." #-}

-- | If an alarm has occurred and the offending device is no longer in violation of the behavior for the specified number of consecutive datapoints, the alarm is cleared. If not specified, the default is 1.
--
-- /Note:/ Consider using 'consecutiveDatapointsToClear' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcConsecutiveDatapointsToClear :: Lens.Lens' BehaviorCriteria (Core.Maybe Core.Natural)
bcConsecutiveDatapointsToClear = Lens.field @"consecutiveDatapointsToClear"
{-# DEPRECATED bcConsecutiveDatapointsToClear "Use generic-lens or generic-optics with 'consecutiveDatapointsToClear' instead." #-}

-- | Use this to specify the time duration over which the behavior is evaluated, for those criteria which have a time dimension (for example, @NUM_MESSAGES_SENT@ ). For a @statisticalThreshhold@ metric comparison, measurements from all devices are accumulated over this time duration before being used to calculate percentiles, and later, measurements from an individual device are also accumulated over this time duration before being given a percentile rank.
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcDurationSeconds :: Lens.Lens' BehaviorCriteria (Core.Maybe Core.Int)
bcDurationSeconds = Lens.field @"durationSeconds"
{-# DEPRECATED bcDurationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead." #-}

-- | A statistical ranking (percentile) which indicates a threshold value by which a behavior is determined to be in compliance or in violation of the behavior.
--
-- /Note:/ Consider using 'statisticalThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcStatisticalThreshold :: Lens.Lens' BehaviorCriteria (Core.Maybe Types.StatisticalThreshold)
bcStatisticalThreshold = Lens.field @"statisticalThreshold"
{-# DEPRECATED bcStatisticalThreshold "Use generic-lens or generic-optics with 'statisticalThreshold' instead." #-}

-- | The value to be compared with the @metric@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcValue :: Lens.Lens' BehaviorCriteria (Core.Maybe Types.MetricValue)
bcValue = Lens.field @"value"
{-# DEPRECATED bcValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON BehaviorCriteria where
  toJSON BehaviorCriteria {..} =
    Core.object
      ( Core.catMaybes
          [ ("comparisonOperator" Core..=) Core.<$> comparisonOperator,
            ("consecutiveDatapointsToAlarm" Core..=)
              Core.<$> consecutiveDatapointsToAlarm,
            ("consecutiveDatapointsToClear" Core..=)
              Core.<$> consecutiveDatapointsToClear,
            ("durationSeconds" Core..=) Core.<$> durationSeconds,
            ("statisticalThreshold" Core..=) Core.<$> statisticalThreshold,
            ("value" Core..=) Core.<$> value
          ]
      )

instance Core.FromJSON BehaviorCriteria where
  parseJSON =
    Core.withObject "BehaviorCriteria" Core.$
      \x ->
        BehaviorCriteria'
          Core.<$> (x Core..:? "comparisonOperator")
          Core.<*> (x Core..:? "consecutiveDatapointsToAlarm")
          Core.<*> (x Core..:? "consecutiveDatapointsToClear")
          Core.<*> (x Core..:? "durationSeconds")
          Core.<*> (x Core..:? "statisticalThreshold")
          Core.<*> (x Core..:? "value")
