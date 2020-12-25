{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MetricDimension
  ( MetricDimension (..),

    -- * Smart constructor
    mkMetricDimension,

    -- * Lenses
    mdComparisonOperator,
    mdValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies metric-based criteria for including or excluding endpoints from a segment. These criteria derive from custom metrics that you define for endpoints.
--
-- /See:/ 'mkMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { -- | The operator to use when comparing metric values. Valid values are: GREATER_THAN, LESS_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN_OR_EQUAL, and EQUAL.
    comparisonOperator :: Core.Text,
    -- | The value to compare.
    value :: Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricDimension' value with any optional fields omitted.
mkMetricDimension ::
  -- | 'comparisonOperator'
  Core.Text ->
  -- | 'value'
  Core.Double ->
  MetricDimension
mkMetricDimension comparisonOperator value =
  MetricDimension' {comparisonOperator, value}

-- | The operator to use when comparing metric values. Valid values are: GREATER_THAN, LESS_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN_OR_EQUAL, and EQUAL.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdComparisonOperator :: Lens.Lens' MetricDimension Core.Text
mdComparisonOperator = Lens.field @"comparisonOperator"
{-# DEPRECATED mdComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The value to compare.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricDimension Core.Double
mdValue = Lens.field @"value"
{-# DEPRECATED mdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON MetricDimension where
  toJSON MetricDimension {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ComparisonOperator" Core..= comparisonOperator),
            Core.Just ("Value" Core..= value)
          ]
      )

instance Core.FromJSON MetricDimension where
  parseJSON =
    Core.withObject "MetricDimension" Core.$
      \x ->
        MetricDimension'
          Core.<$> (x Core..: "ComparisonOperator") Core.<*> (x Core..: "Value")
