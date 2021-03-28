{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HistoricalMetricResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.HistoricalMetricResult
  ( HistoricalMetricResult (..)
  -- * Smart constructor
  , mkHistoricalMetricResult
  -- * Lenses
  , hmrCollections
  , hmrDimensions
  ) where

import qualified Network.AWS.Connect.Types.Dimensions as Types
import qualified Network.AWS.Connect.Types.HistoricalMetricData as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the historical metrics retrieved.
--
-- /See:/ 'mkHistoricalMetricResult' smart constructor.
data HistoricalMetricResult = HistoricalMetricResult'
  { collections :: Core.Maybe [Types.HistoricalMetricData]
    -- ^ The set of metrics.
  , dimensions :: Core.Maybe Types.Dimensions
    -- ^ The dimension for the metrics.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HistoricalMetricResult' value with any optional fields omitted.
mkHistoricalMetricResult
    :: HistoricalMetricResult
mkHistoricalMetricResult
  = HistoricalMetricResult'{collections = Core.Nothing,
                            dimensions = Core.Nothing}

-- | The set of metrics.
--
-- /Note:/ Consider using 'collections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmrCollections :: Lens.Lens' HistoricalMetricResult (Core.Maybe [Types.HistoricalMetricData])
hmrCollections = Lens.field @"collections"
{-# INLINEABLE hmrCollections #-}
{-# DEPRECATED collections "Use generic-lens or generic-optics with 'collections' instead"  #-}

-- | The dimension for the metrics.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmrDimensions :: Lens.Lens' HistoricalMetricResult (Core.Maybe Types.Dimensions)
hmrDimensions = Lens.field @"dimensions"
{-# INLINEABLE hmrDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

instance Core.FromJSON HistoricalMetricResult where
        parseJSON
          = Core.withObject "HistoricalMetricResult" Core.$
              \ x ->
                HistoricalMetricResult' Core.<$>
                  (x Core..:? "Collections") Core.<*> x Core..:? "Dimensions"
