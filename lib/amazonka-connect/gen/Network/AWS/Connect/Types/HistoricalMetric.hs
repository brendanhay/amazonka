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
    hmStatistic,
    hmThreshold,
    hmUnit,
  )
where

import qualified Network.AWS.Connect.Types.HistoricalMetricName as Types
import qualified Network.AWS.Connect.Types.Statistic as Types
import qualified Network.AWS.Connect.Types.Threshold as Types
import qualified Network.AWS.Connect.Types.Unit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a historical metric. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
-- /See:/ 'mkHistoricalMetric' smart constructor.
data HistoricalMetric = HistoricalMetric'
  { -- | The name of the metric.
    name :: Core.Maybe Types.HistoricalMetricName,
    -- | The statistic for the metric.
    statistic :: Core.Maybe Types.Statistic,
    -- | The threshold for the metric, used with service level metrics.
    threshold :: Core.Maybe Types.Threshold,
    -- | The unit for the metric.
    unit :: Core.Maybe Types.Unit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HistoricalMetric' value with any optional fields omitted.
mkHistoricalMetric ::
  HistoricalMetric
mkHistoricalMetric =
  HistoricalMetric'
    { name = Core.Nothing,
      statistic = Core.Nothing,
      threshold = Core.Nothing,
      unit = Core.Nothing
    }

-- | The name of the metric.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmName :: Lens.Lens' HistoricalMetric (Core.Maybe Types.HistoricalMetricName)
hmName = Lens.field @"name"
{-# DEPRECATED hmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The statistic for the metric.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmStatistic :: Lens.Lens' HistoricalMetric (Core.Maybe Types.Statistic)
hmStatistic = Lens.field @"statistic"
{-# DEPRECATED hmStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | The threshold for the metric, used with service level metrics.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmThreshold :: Lens.Lens' HistoricalMetric (Core.Maybe Types.Threshold)
hmThreshold = Lens.field @"threshold"
{-# DEPRECATED hmThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | The unit for the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmUnit :: Lens.Lens' HistoricalMetric (Core.Maybe Types.Unit)
hmUnit = Lens.field @"unit"
{-# DEPRECATED hmUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.FromJSON HistoricalMetric where
  toJSON HistoricalMetric {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Statistic" Core..=) Core.<$> statistic,
            ("Threshold" Core..=) Core.<$> threshold,
            ("Unit" Core..=) Core.<$> unit
          ]
      )

instance Core.FromJSON HistoricalMetric where
  parseJSON =
    Core.withObject "HistoricalMetric" Core.$
      \x ->
        HistoricalMetric'
          Core.<$> (x Core..:? "Name")
          Core.<*> (x Core..:? "Statistic")
          Core.<*> (x Core..:? "Threshold")
          Core.<*> (x Core..:? "Unit")
