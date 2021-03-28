{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.HistogramEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.HistogramEntry
  ( HistogramEntry (..)
  -- * Smart constructor
  , mkHistogramEntry
  -- * Lenses
  , heCount
  , heValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An entry in a histogram for a statistic. A histogram maps the range of observed values on the X axis, and the prevalence of each value on the Y axis.
--
-- /See:/ 'mkHistogramEntry' smart constructor.
data HistogramEntry = HistogramEntry'
  { count :: Core.Maybe Core.Int
    -- ^ The prevalence of the entry.
  , value :: Core.Maybe Core.Double
    -- ^ The value of the entry.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HistogramEntry' value with any optional fields omitted.
mkHistogramEntry
    :: HistogramEntry
mkHistogramEntry
  = HistogramEntry'{count = Core.Nothing, value = Core.Nothing}

-- | The prevalence of the entry.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heCount :: Lens.Lens' HistogramEntry (Core.Maybe Core.Int)
heCount = Lens.field @"count"
{-# INLINEABLE heCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The value of the entry.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heValue :: Lens.Lens' HistogramEntry (Core.Maybe Core.Double)
heValue = Lens.field @"value"
{-# INLINEABLE heValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON HistogramEntry where
        parseJSON
          = Core.withObject "HistogramEntry" Core.$
              \ x ->
                HistogramEntry' Core.<$>
                  (x Core..:? "Count") Core.<*> x Core..:? "Value"
