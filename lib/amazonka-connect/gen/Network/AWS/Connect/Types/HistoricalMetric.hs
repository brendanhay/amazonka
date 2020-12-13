{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HistoricalMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetric
  ( HistoricalMetric (..),

    -- * Smart constructor
    mkHistoricalMetric,

    -- * Lenses
    hmName,
    hmThreshold,
    hmUnit,
    hmStatistic,
  )
where

import Network.AWS.Connect.Types.HistoricalMetricName
import Network.AWS.Connect.Types.Statistic
import Network.AWS.Connect.Types.Threshold
import Network.AWS.Connect.Types.Unit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a historical metric. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
-- /See:/ 'mkHistoricalMetric' smart constructor.
data HistoricalMetric = HistoricalMetric'
  { -- | The name of the metric.
    name :: Lude.Maybe HistoricalMetricName,
    -- | The threshold for the metric, used with service level metrics.
    threshold :: Lude.Maybe Threshold,
    -- | The unit for the metric.
    unit :: Lude.Maybe Unit,
    -- | The statistic for the metric.
    statistic :: Lude.Maybe Statistic
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HistoricalMetric' with the minimum fields required to make a request.
--
-- * 'name' - The name of the metric.
-- * 'threshold' - The threshold for the metric, used with service level metrics.
-- * 'unit' - The unit for the metric.
-- * 'statistic' - The statistic for the metric.
mkHistoricalMetric ::
  HistoricalMetric
mkHistoricalMetric =
  HistoricalMetric'
    { name = Lude.Nothing,
      threshold = Lude.Nothing,
      unit = Lude.Nothing,
      statistic = Lude.Nothing
    }

-- | The name of the metric.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmName :: Lens.Lens' HistoricalMetric (Lude.Maybe HistoricalMetricName)
hmName = Lens.lens (name :: HistoricalMetric -> Lude.Maybe HistoricalMetricName) (\s a -> s {name = a} :: HistoricalMetric)
{-# DEPRECATED hmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The threshold for the metric, used with service level metrics.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmThreshold :: Lens.Lens' HistoricalMetric (Lude.Maybe Threshold)
hmThreshold = Lens.lens (threshold :: HistoricalMetric -> Lude.Maybe Threshold) (\s a -> s {threshold = a} :: HistoricalMetric)
{-# DEPRECATED hmThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | The unit for the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmUnit :: Lens.Lens' HistoricalMetric (Lude.Maybe Unit)
hmUnit = Lens.lens (unit :: HistoricalMetric -> Lude.Maybe Unit) (\s a -> s {unit = a} :: HistoricalMetric)
{-# DEPRECATED hmUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The statistic for the metric.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmStatistic :: Lens.Lens' HistoricalMetric (Lude.Maybe Statistic)
hmStatistic = Lens.lens (statistic :: HistoricalMetric -> Lude.Maybe Statistic) (\s a -> s {statistic = a} :: HistoricalMetric)
{-# DEPRECATED hmStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

instance Lude.FromJSON HistoricalMetric where
  parseJSON =
    Lude.withObject
      "HistoricalMetric"
      ( \x ->
          HistoricalMetric'
            Lude.<$> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Threshold")
            Lude.<*> (x Lude..:? "Unit")
            Lude.<*> (x Lude..:? "Statistic")
      )

instance Lude.ToJSON HistoricalMetric where
  toJSON HistoricalMetric' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("Threshold" Lude..=) Lude.<$> threshold,
            ("Unit" Lude..=) Lude.<$> unit,
            ("Statistic" Lude..=) Lude.<$> statistic
          ]
      )
