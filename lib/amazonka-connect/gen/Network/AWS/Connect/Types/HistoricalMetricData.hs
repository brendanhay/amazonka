{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HistoricalMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetricData
  ( HistoricalMetricData (..),

    -- * Smart constructor
    mkHistoricalMetricData,

    -- * Lenses
    hmdMetric,
    hmdValue,
  )
where

import qualified Network.AWS.Connect.Types.HistoricalMetric as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the data for a historical metric.
--
-- /See:/ 'mkHistoricalMetricData' smart constructor.
data HistoricalMetricData = HistoricalMetricData'
  { -- | Information about the metric.
    metric :: Core.Maybe Types.HistoricalMetric,
    -- | The value of the metric.
    value :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HistoricalMetricData' value with any optional fields omitted.
mkHistoricalMetricData ::
  HistoricalMetricData
mkHistoricalMetricData =
  HistoricalMetricData'
    { metric = Core.Nothing,
      value = Core.Nothing
    }

-- | Information about the metric.
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmdMetric :: Lens.Lens' HistoricalMetricData (Core.Maybe Types.HistoricalMetric)
hmdMetric = Lens.field @"metric"
{-# DEPRECATED hmdMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

-- | The value of the metric.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmdValue :: Lens.Lens' HistoricalMetricData (Core.Maybe Core.Double)
hmdValue = Lens.field @"value"
{-# DEPRECATED hmdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON HistoricalMetricData where
  parseJSON =
    Core.withObject "HistoricalMetricData" Core.$
      \x ->
        HistoricalMetricData'
          Core.<$> (x Core..:? "Metric") Core.<*> (x Core..:? "Value")
