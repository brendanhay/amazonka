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
import qualified Network.AWS.Prelude as Lude

-- | Specifies metric-based criteria for including or excluding endpoints from a segment. These criteria derive from custom metrics that you define for endpoints.
--
-- /See:/ 'mkMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { comparisonOperator ::
      Lude.Text,
    value :: Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- * 'comparisonOperator' - The operator to use when comparing metric values. Valid values are: GREATER_THAN, LESS_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN_OR_EQUAL, and EQUAL.
-- * 'value' - The value to compare.
mkMetricDimension ::
  -- | 'comparisonOperator'
  Lude.Text ->
  -- | 'value'
  Lude.Double ->
  MetricDimension
mkMetricDimension pComparisonOperator_ pValue_ =
  MetricDimension'
    { comparisonOperator = pComparisonOperator_,
      value = pValue_
    }

-- | The operator to use when comparing metric values. Valid values are: GREATER_THAN, LESS_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN_OR_EQUAL, and EQUAL.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdComparisonOperator :: Lens.Lens' MetricDimension Lude.Text
mdComparisonOperator = Lens.lens (comparisonOperator :: MetricDimension -> Lude.Text) (\s a -> s {comparisonOperator = a} :: MetricDimension)
{-# DEPRECATED mdComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The value to compare.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricDimension Lude.Double
mdValue = Lens.lens (value :: MetricDimension -> Lude.Double) (\s a -> s {value = a} :: MetricDimension)
{-# DEPRECATED mdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON MetricDimension where
  parseJSON =
    Lude.withObject
      "MetricDimension"
      ( \x ->
          MetricDimension'
            Lude.<$> (x Lude..: "ComparisonOperator") Lude.<*> (x Lude..: "Value")
      )

instance Lude.ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ComparisonOperator" Lude..= comparisonOperator),
            Lude.Just ("Value" Lude..= value)
          ]
      )
