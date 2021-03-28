{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.MetricDimension
  ( MetricDimension (..)
  -- * Smart constructor
  , mkMetricDimension
  -- * Lenses
  , mdName
  , mdValue
  ) where

import qualified Network.AWS.AutoScaling.Types.MetricDimensionName as Types
import qualified Network.AWS.AutoScaling.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the dimension of a metric.
--
-- /See:/ 'mkMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { name :: Types.MetricDimensionName
    -- ^ The name of the dimension.
  , value :: Types.Value
    -- ^ The value of the dimension.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricDimension' value with any optional fields omitted.
mkMetricDimension
    :: Types.MetricDimensionName -- ^ 'name'
    -> Types.Value -- ^ 'value'
    -> MetricDimension
mkMetricDimension name value = MetricDimension'{name, value}

-- | The name of the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdName :: Lens.Lens' MetricDimension Types.MetricDimensionName
mdName = Lens.field @"name"
{-# INLINEABLE mdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of the dimension.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdValue :: Lens.Lens' MetricDimension Types.Value
mdValue = Lens.field @"value"
{-# INLINEABLE mdValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery MetricDimension where
        toQuery MetricDimension{..}
          = Core.toQueryPair "Name" name Core.<>
              Core.toQueryPair "Value" value

instance Core.FromXML MetricDimension where
        parseXML x
          = MetricDimension' Core.<$>
              (x Core..@ "Name") Core.<*> x Core..@ "Value"
