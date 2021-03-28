{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.MetricDimension
  ( MetricDimension (..)
  -- * Smart constructor
  , mkMetricDimension
  -- * Lenses
  , mdDimensionName
  , mdOperator
  ) where

import qualified Network.AWS.IoT.Types.DimensionName as Types
import qualified Network.AWS.IoT.Types.DimensionValueOperator as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The dimension of a metric.
--
-- /See:/ 'mkMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { dimensionName :: Types.DimensionName
    -- ^ A unique identifier for the dimension.
  , operator :: Core.Maybe Types.DimensionValueOperator
    -- ^ Defines how the @dimensionValues@ of a dimension are interpreted. For example, for dimension type TOPIC_FILTER, the @IN@ operator, a message will be counted only if its topic matches one of the topic filters. With @NOT_IN@ operator, a message will be counted only if it doesn't match any of the topic filters. The operator is optional: if it's not provided (is @null@ ), it will be interpreted as @IN@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricDimension' value with any optional fields omitted.
mkMetricDimension
    :: Types.DimensionName -- ^ 'dimensionName'
    -> MetricDimension
mkMetricDimension dimensionName
  = MetricDimension'{dimensionName, operator = Core.Nothing}

-- | A unique identifier for the dimension.
--
-- /Note:/ Consider using 'dimensionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdDimensionName :: Lens.Lens' MetricDimension Types.DimensionName
mdDimensionName = Lens.field @"dimensionName"
{-# INLINEABLE mdDimensionName #-}
{-# DEPRECATED dimensionName "Use generic-lens or generic-optics with 'dimensionName' instead"  #-}

-- | Defines how the @dimensionValues@ of a dimension are interpreted. For example, for dimension type TOPIC_FILTER, the @IN@ operator, a message will be counted only if its topic matches one of the topic filters. With @NOT_IN@ operator, a message will be counted only if it doesn't match any of the topic filters. The operator is optional: if it's not provided (is @null@ ), it will be interpreted as @IN@ .
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdOperator :: Lens.Lens' MetricDimension (Core.Maybe Types.DimensionValueOperator)
mdOperator = Lens.field @"operator"
{-# INLINEABLE mdOperator #-}
{-# DEPRECATED operator "Use generic-lens or generic-optics with 'operator' instead"  #-}

instance Core.FromJSON MetricDimension where
        toJSON MetricDimension{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("dimensionName" Core..= dimensionName),
                  ("operator" Core..=) Core.<$> operator])

instance Core.FromJSON MetricDimension where
        parseJSON
          = Core.withObject "MetricDimension" Core.$
              \ x ->
                MetricDimension' Core.<$>
                  (x Core..: "dimensionName") Core.<*> x Core..:? "operator"
