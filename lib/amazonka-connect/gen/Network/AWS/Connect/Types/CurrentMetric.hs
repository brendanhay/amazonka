{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.CurrentMetric
  ( CurrentMetric (..)
  -- * Smart constructor
  , mkCurrentMetric
  -- * Lenses
  , cmName
  , cmUnit
  ) where

import qualified Network.AWS.Connect.Types.CurrentMetricName as Types
import qualified Network.AWS.Connect.Types.Unit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a real-time metric. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
-- /See:/ 'mkCurrentMetric' smart constructor.
data CurrentMetric = CurrentMetric'
  { name :: Core.Maybe Types.CurrentMetricName
    -- ^ The name of the metric.
  , unit :: Core.Maybe Types.Unit
    -- ^ The unit for the metric.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CurrentMetric' value with any optional fields omitted.
mkCurrentMetric
    :: CurrentMetric
mkCurrentMetric
  = CurrentMetric'{name = Core.Nothing, unit = Core.Nothing}

-- | The name of the metric.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmName :: Lens.Lens' CurrentMetric (Core.Maybe Types.CurrentMetricName)
cmName = Lens.field @"name"
{-# INLINEABLE cmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The unit for the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmUnit :: Lens.Lens' CurrentMetric (Core.Maybe Types.Unit)
cmUnit = Lens.field @"unit"
{-# INLINEABLE cmUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.FromJSON CurrentMetric where
        toJSON CurrentMetric{..}
          = Core.object
              (Core.catMaybes
                 [("Name" Core..=) Core.<$> name, ("Unit" Core..=) Core.<$> unit])

instance Core.FromJSON CurrentMetric where
        parseJSON
          = Core.withObject "CurrentMetric" Core.$
              \ x ->
                CurrentMetric' Core.<$>
                  (x Core..:? "Name") Core.<*> x Core..:? "Unit"
