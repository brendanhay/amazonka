{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.MetricValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.MetricValue
  ( MetricValue (..),

    -- * Smart constructor
    mkMetricValue,

    -- * Lenses
    mvAmount,
    mvUnit,
  )
where

import qualified Network.AWS.CostExplorer.Types.Amount as Types
import qualified Network.AWS.CostExplorer.Types.Unit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The aggregated value for a metric.
--
-- /See:/ 'mkMetricValue' smart constructor.
data MetricValue = MetricValue'
  { -- | The actual number that represents the metric.
    amount :: Core.Maybe Types.Amount,
    -- | The unit that the metric is given in.
    unit :: Core.Maybe Types.Unit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricValue' value with any optional fields omitted.
mkMetricValue ::
  MetricValue
mkMetricValue =
  MetricValue' {amount = Core.Nothing, unit = Core.Nothing}

-- | The actual number that represents the metric.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvAmount :: Lens.Lens' MetricValue (Core.Maybe Types.Amount)
mvAmount = Lens.field @"amount"
{-# DEPRECATED mvAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The unit that the metric is given in.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvUnit :: Lens.Lens' MetricValue (Core.Maybe Types.Unit)
mvUnit = Lens.field @"unit"
{-# DEPRECATED mvUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.FromJSON MetricValue where
  parseJSON =
    Core.withObject "MetricValue" Core.$
      \x ->
        MetricValue'
          Core.<$> (x Core..:? "Amount") Core.<*> (x Core..:? "Unit")
