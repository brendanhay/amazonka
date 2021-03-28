{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.CurrentMetricData
  ( CurrentMetricData (..)
  -- * Smart constructor
  , mkCurrentMetricData
  -- * Lenses
  , cmdMetric
  , cmdValue
  ) where

import qualified Network.AWS.Connect.Types.CurrentMetric as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the data for a real-time metric.
--
-- /See:/ 'mkCurrentMetricData' smart constructor.
data CurrentMetricData = CurrentMetricData'
  { metric :: Core.Maybe Types.CurrentMetric
    -- ^ Information about the metric.
  , value :: Core.Maybe Core.Double
    -- ^ The value of the metric.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CurrentMetricData' value with any optional fields omitted.
mkCurrentMetricData
    :: CurrentMetricData
mkCurrentMetricData
  = CurrentMetricData'{metric = Core.Nothing, value = Core.Nothing}

-- | Information about the metric.
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdMetric :: Lens.Lens' CurrentMetricData (Core.Maybe Types.CurrentMetric)
cmdMetric = Lens.field @"metric"
{-# INLINEABLE cmdMetric #-}
{-# DEPRECATED metric "Use generic-lens or generic-optics with 'metric' instead"  #-}

-- | The value of the metric.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdValue :: Lens.Lens' CurrentMetricData (Core.Maybe Core.Double)
cmdValue = Lens.field @"value"
{-# INLINEABLE cmdValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON CurrentMetricData where
        parseJSON
          = Core.withObject "CurrentMetricData" Core.$
              \ x ->
                CurrentMetricData' Core.<$>
                  (x Core..:? "Metric") Core.<*> x Core..:? "Value"
