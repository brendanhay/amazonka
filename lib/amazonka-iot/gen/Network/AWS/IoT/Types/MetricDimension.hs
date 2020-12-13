{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MetricDimension
  ( MetricDimension (..),

    -- * Smart constructor
    mkMetricDimension,

    -- * Lenses
    mdOperator,
    mdDimensionName,
  )
where

import Network.AWS.IoT.Types.DimensionValueOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The dimension of a metric.
--
-- /See:/ 'mkMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { -- | Defines how the @dimensionValues@ of a dimension are interpreted. For example, for dimension type TOPIC_FILTER, the @IN@ operator, a message will be counted only if its topic matches one of the topic filters. With @NOT_IN@ operator, a message will be counted only if it doesn't match any of the topic filters. The operator is optional: if it's not provided (is @null@ ), it will be interpreted as @IN@ .
    operator :: Lude.Maybe DimensionValueOperator,
    -- | A unique identifier for the dimension.
    dimensionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- * 'operator' - Defines how the @dimensionValues@ of a dimension are interpreted. For example, for dimension type TOPIC_FILTER, the @IN@ operator, a message will be counted only if its topic matches one of the topic filters. With @NOT_IN@ operator, a message will be counted only if it doesn't match any of the topic filters. The operator is optional: if it's not provided (is @null@ ), it will be interpreted as @IN@ .
-- * 'dimensionName' - A unique identifier for the dimension.
mkMetricDimension ::
  -- | 'dimensionName'
  Lude.Text ->
  MetricDimension
mkMetricDimension pDimensionName_ =
  MetricDimension'
    { operator = Lude.Nothing,
      dimensionName = pDimensionName_
    }

-- | Defines how the @dimensionValues@ of a dimension are interpreted. For example, for dimension type TOPIC_FILTER, the @IN@ operator, a message will be counted only if its topic matches one of the topic filters. With @NOT_IN@ operator, a message will be counted only if it doesn't match any of the topic filters. The operator is optional: if it's not provided (is @null@ ), it will be interpreted as @IN@ .
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdOperator :: Lens.Lens' MetricDimension (Lude.Maybe DimensionValueOperator)
mdOperator = Lens.lens (operator :: MetricDimension -> Lude.Maybe DimensionValueOperator) (\s a -> s {operator = a} :: MetricDimension)
{-# DEPRECATED mdOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

-- | A unique identifier for the dimension.
--
-- /Note:/ Consider using 'dimensionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdDimensionName :: Lens.Lens' MetricDimension Lude.Text
mdDimensionName = Lens.lens (dimensionName :: MetricDimension -> Lude.Text) (\s a -> s {dimensionName = a} :: MetricDimension)
{-# DEPRECATED mdDimensionName "Use generic-lens or generic-optics with 'dimensionName' instead." #-}

instance Lude.FromJSON MetricDimension where
  parseJSON =
    Lude.withObject
      "MetricDimension"
      ( \x ->
          MetricDimension'
            Lude.<$> (x Lude..:? "operator") Lude.<*> (x Lude..: "dimensionName")
      )

instance Lude.ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("operator" Lude..=) Lude.<$> operator,
            Lude.Just ("dimensionName" Lude..= dimensionName)
          ]
      )
