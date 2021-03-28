{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Latency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Latency
  ( Latency (..)
  -- * Smart constructor
  , mkLatency
  -- * Lenses
  , lP10
  , lP50
  , lP75
  , lP85
  , lP90
  , lP95
  , lP99
  , lP999
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the average latency for the slowest X percent of requests over the last 10 seconds.
--
-- /See:/ 'mkLatency' smart constructor.
data Latency = Latency'
  { p10 :: Core.Maybe Core.Double
    -- ^ The average latency for the slowest 90 percent of requests over the last 10 seconds.
  , p50 :: Core.Maybe Core.Double
    -- ^ The average latency for the slowest 50 percent of requests over the last 10 seconds.
  , p75 :: Core.Maybe Core.Double
    -- ^ The average latency for the slowest 25 percent of requests over the last 10 seconds.
  , p85 :: Core.Maybe Core.Double
    -- ^ The average latency for the slowest 15 percent of requests over the last 10 seconds.
  , p90 :: Core.Maybe Core.Double
    -- ^ The average latency for the slowest 10 percent of requests over the last 10 seconds.
  , p95 :: Core.Maybe Core.Double
    -- ^ The average latency for the slowest 5 percent of requests over the last 10 seconds.
  , p99 :: Core.Maybe Core.Double
    -- ^ The average latency for the slowest 1 percent of requests over the last 10 seconds.
  , p999 :: Core.Maybe Core.Double
    -- ^ The average latency for the slowest 0.1 percent of requests over the last 10 seconds.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Latency' value with any optional fields omitted.
mkLatency
    :: Latency
mkLatency
  = Latency'{p10 = Core.Nothing, p50 = Core.Nothing,
             p75 = Core.Nothing, p85 = Core.Nothing, p90 = Core.Nothing,
             p95 = Core.Nothing, p99 = Core.Nothing, p999 = Core.Nothing}

-- | The average latency for the slowest 90 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p10' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP10 :: Lens.Lens' Latency (Core.Maybe Core.Double)
lP10 = Lens.field @"p10"
{-# INLINEABLE lP10 #-}
{-# DEPRECATED p10 "Use generic-lens or generic-optics with 'p10' instead"  #-}

-- | The average latency for the slowest 50 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p50' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP50 :: Lens.Lens' Latency (Core.Maybe Core.Double)
lP50 = Lens.field @"p50"
{-# INLINEABLE lP50 #-}
{-# DEPRECATED p50 "Use generic-lens or generic-optics with 'p50' instead"  #-}

-- | The average latency for the slowest 25 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p75' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP75 :: Lens.Lens' Latency (Core.Maybe Core.Double)
lP75 = Lens.field @"p75"
{-# INLINEABLE lP75 #-}
{-# DEPRECATED p75 "Use generic-lens or generic-optics with 'p75' instead"  #-}

-- | The average latency for the slowest 15 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p85' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP85 :: Lens.Lens' Latency (Core.Maybe Core.Double)
lP85 = Lens.field @"p85"
{-# INLINEABLE lP85 #-}
{-# DEPRECATED p85 "Use generic-lens or generic-optics with 'p85' instead"  #-}

-- | The average latency for the slowest 10 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p90' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP90 :: Lens.Lens' Latency (Core.Maybe Core.Double)
lP90 = Lens.field @"p90"
{-# INLINEABLE lP90 #-}
{-# DEPRECATED p90 "Use generic-lens or generic-optics with 'p90' instead"  #-}

-- | The average latency for the slowest 5 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p95' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP95 :: Lens.Lens' Latency (Core.Maybe Core.Double)
lP95 = Lens.field @"p95"
{-# INLINEABLE lP95 #-}
{-# DEPRECATED p95 "Use generic-lens or generic-optics with 'p95' instead"  #-}

-- | The average latency for the slowest 1 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p99' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP99 :: Lens.Lens' Latency (Core.Maybe Core.Double)
lP99 = Lens.field @"p99"
{-# INLINEABLE lP99 #-}
{-# DEPRECATED p99 "Use generic-lens or generic-optics with 'p99' instead"  #-}

-- | The average latency for the slowest 0.1 percent of requests over the last 10 seconds.
--
-- /Note:/ Consider using 'p999' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lP999 :: Lens.Lens' Latency (Core.Maybe Core.Double)
lP999 = Lens.field @"p999"
{-# INLINEABLE lP999 #-}
{-# DEPRECATED p999 "Use generic-lens or generic-optics with 'p999' instead"  #-}

instance Core.FromXML Latency where
        parseXML x
          = Latency' Core.<$>
              (x Core..@? "P10") Core.<*> x Core..@? "P50" Core.<*>
                x Core..@? "P75"
                Core.<*> x Core..@? "P85"
                Core.<*> x Core..@? "P90"
                Core.<*> x Core..@? "P95"
                Core.<*> x Core..@? "P99"
                Core.<*> x Core..@? "P999"
