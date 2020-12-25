{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.MetricDimension
  ( MetricDimension (..),

    -- * Smart constructor
    mkMetricDimension,

    -- * Lenses
    mdName,
    mdValue,
  )
where

import qualified Network.AWS.ApplicationAutoScaling.Types.MetricDimensionName as Types
import qualified Network.AWS.ApplicationAutoScaling.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the dimension names and values associated with a metric.
--
-- /See:/ 'mkMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { -- | The name of the dimension.
    name :: Types.MetricDimensionName,
    -- | The value of the dimension.
    value :: Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricDimension' value with any optional fields omitted.
mkMetricDimension ::
  -- | 'name'
  Types.MetricDimensionName ->
  -- | 'value'
  Types.Value ->
  MetricDimension
mkMetricDimension name value = MetricDimension' {name, value}

-- | The name of the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdName :: Lens.Lens' MetricDimension Types.MetricDimensionName
mdName = Lens.field @"name"
{-# DEPRECATED mdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the dimension.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricDimension Types.Value
mdValue = Lens.field @"value"
{-# DEPRECATED mdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON MetricDimension where
  toJSON MetricDimension {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Value" Core..= value)
          ]
      )

instance Core.FromJSON MetricDimension where
  parseJSON =
    Core.withObject "MetricDimension" Core.$
      \x ->
        MetricDimension'
          Core.<$> (x Core..: "Name") Core.<*> (x Core..: "Value")
