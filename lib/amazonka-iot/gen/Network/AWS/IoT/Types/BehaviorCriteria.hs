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
    bcValue,
    bcConsecutiveDatapointsToAlarm,
    bcComparisonOperator,
    bcStatisticalThreshold,
    bcDurationSeconds,
    bcConsecutiveDatapointsToClear,
  )
where

import Network.AWS.IoT.Types.ComparisonOperator
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.IoT.Types.StatisticalThreshold
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The criteria by which the behavior is determined to be normal.
--
-- /See:/ 'mkBehaviorCriteria' smart constructor.
data BehaviorCriteria = BehaviorCriteria'
  { value ::
      Lude.Maybe MetricValue,
    consecutiveDatapointsToAlarm :: Lude.Maybe Lude.Natural,
    comparisonOperator :: Lude.Maybe ComparisonOperator,
    statisticalThreshold :: Lude.Maybe StatisticalThreshold,
    durationSeconds :: Lude.Maybe Lude.Int,
    consecutiveDatapointsToClear :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BehaviorCriteria' with the minimum fields required to make a request.
--
-- * 'comparisonOperator' - The operator that relates the thing measured (@metric@ ) to the criteria (containing a @value@ or @statisticalThreshold@ ).
-- * 'consecutiveDatapointsToAlarm' - If a device is in violation of the behavior for the specified number of consecutive datapoints, an alarm occurs. If not specified, the default is 1.
-- * 'consecutiveDatapointsToClear' - If an alarm has occurred and the offending device is no longer in violation of the behavior for the specified number of consecutive datapoints, the alarm is cleared. If not specified, the default is 1.
-- * 'durationSeconds' - Use this to specify the time duration over which the behavior is evaluated, for those criteria which have a time dimension (for example, @NUM_MESSAGES_SENT@ ). For a @statisticalThreshhold@ metric comparison, measurements from all devices are accumulated over this time duration before being used to calculate percentiles, and later, measurements from an individual device are also accumulated over this time duration before being given a percentile rank.
-- * 'statisticalThreshold' - A statistical ranking (percentile) which indicates a threshold value by which a behavior is determined to be in compliance or in violation of the behavior.
-- * 'value' - The value to be compared with the @metric@ .
mkBehaviorCriteria ::
  BehaviorCriteria
mkBehaviorCriteria =
  BehaviorCriteria'
    { value = Lude.Nothing,
      consecutiveDatapointsToAlarm = Lude.Nothing,
      comparisonOperator = Lude.Nothing,
      statisticalThreshold = Lude.Nothing,
      durationSeconds = Lude.Nothing,
      consecutiveDatapointsToClear = Lude.Nothing
    }

-- | The value to be compared with the @metric@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcValue :: Lens.Lens' BehaviorCriteria (Lude.Maybe MetricValue)
bcValue = Lens.lens (value :: BehaviorCriteria -> Lude.Maybe MetricValue) (\s a -> s {value = a} :: BehaviorCriteria)
{-# DEPRECATED bcValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | If a device is in violation of the behavior for the specified number of consecutive datapoints, an alarm occurs. If not specified, the default is 1.
--
-- /Note:/ Consider using 'consecutiveDatapointsToAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcConsecutiveDatapointsToAlarm :: Lens.Lens' BehaviorCriteria (Lude.Maybe Lude.Natural)
bcConsecutiveDatapointsToAlarm = Lens.lens (consecutiveDatapointsToAlarm :: BehaviorCriteria -> Lude.Maybe Lude.Natural) (\s a -> s {consecutiveDatapointsToAlarm = a} :: BehaviorCriteria)
{-# DEPRECATED bcConsecutiveDatapointsToAlarm "Use generic-lens or generic-optics with 'consecutiveDatapointsToAlarm' instead." #-}

-- | The operator that relates the thing measured (@metric@ ) to the criteria (containing a @value@ or @statisticalThreshold@ ).
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcComparisonOperator :: Lens.Lens' BehaviorCriteria (Lude.Maybe ComparisonOperator)
bcComparisonOperator = Lens.lens (comparisonOperator :: BehaviorCriteria -> Lude.Maybe ComparisonOperator) (\s a -> s {comparisonOperator = a} :: BehaviorCriteria)
{-# DEPRECATED bcComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | A statistical ranking (percentile) which indicates a threshold value by which a behavior is determined to be in compliance or in violation of the behavior.
--
-- /Note:/ Consider using 'statisticalThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcStatisticalThreshold :: Lens.Lens' BehaviorCriteria (Lude.Maybe StatisticalThreshold)
bcStatisticalThreshold = Lens.lens (statisticalThreshold :: BehaviorCriteria -> Lude.Maybe StatisticalThreshold) (\s a -> s {statisticalThreshold = a} :: BehaviorCriteria)
{-# DEPRECATED bcStatisticalThreshold "Use generic-lens or generic-optics with 'statisticalThreshold' instead." #-}

-- | Use this to specify the time duration over which the behavior is evaluated, for those criteria which have a time dimension (for example, @NUM_MESSAGES_SENT@ ). For a @statisticalThreshhold@ metric comparison, measurements from all devices are accumulated over this time duration before being used to calculate percentiles, and later, measurements from an individual device are also accumulated over this time duration before being given a percentile rank.
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcDurationSeconds :: Lens.Lens' BehaviorCriteria (Lude.Maybe Lude.Int)
bcDurationSeconds = Lens.lens (durationSeconds :: BehaviorCriteria -> Lude.Maybe Lude.Int) (\s a -> s {durationSeconds = a} :: BehaviorCriteria)
{-# DEPRECATED bcDurationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead." #-}

-- | If an alarm has occurred and the offending device is no longer in violation of the behavior for the specified number of consecutive datapoints, the alarm is cleared. If not specified, the default is 1.
--
-- /Note:/ Consider using 'consecutiveDatapointsToClear' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcConsecutiveDatapointsToClear :: Lens.Lens' BehaviorCriteria (Lude.Maybe Lude.Natural)
bcConsecutiveDatapointsToClear = Lens.lens (consecutiveDatapointsToClear :: BehaviorCriteria -> Lude.Maybe Lude.Natural) (\s a -> s {consecutiveDatapointsToClear = a} :: BehaviorCriteria)
{-# DEPRECATED bcConsecutiveDatapointsToClear "Use generic-lens or generic-optics with 'consecutiveDatapointsToClear' instead." #-}

instance Lude.FromJSON BehaviorCriteria where
  parseJSON =
    Lude.withObject
      "BehaviorCriteria"
      ( \x ->
          BehaviorCriteria'
            Lude.<$> (x Lude..:? "value")
            Lude.<*> (x Lude..:? "consecutiveDatapointsToAlarm")
            Lude.<*> (x Lude..:? "comparisonOperator")
            Lude.<*> (x Lude..:? "statisticalThreshold")
            Lude.<*> (x Lude..:? "durationSeconds")
            Lude.<*> (x Lude..:? "consecutiveDatapointsToClear")
      )

instance Lude.ToJSON BehaviorCriteria where
  toJSON BehaviorCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("value" Lude..=) Lude.<$> value,
            ("consecutiveDatapointsToAlarm" Lude..=)
              Lude.<$> consecutiveDatapointsToAlarm,
            ("comparisonOperator" Lude..=) Lude.<$> comparisonOperator,
            ("statisticalThreshold" Lude..=) Lude.<$> statisticalThreshold,
            ("durationSeconds" Lude..=) Lude.<$> durationSeconds,
            ("consecutiveDatapointsToClear" Lude..=)
              Lude.<$> consecutiveDatapointsToClear
          ]
      )
