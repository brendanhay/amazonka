{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Edge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.Edge
  ( Edge (..)
  -- * Smart constructor
  , mkEdge
  -- * Lenses
  , eAliases
  , eEndTime
  , eReferenceId
  , eResponseTimeHistogram
  , eStartTime
  , eSummaryStatistics
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.Alias as Types
import qualified Network.AWS.XRay.Types.EdgeStatistics as Types
import qualified Network.AWS.XRay.Types.HistogramEntry as Types

-- | Information about a connection between two services.
--
-- /See:/ 'mkEdge' smart constructor.
data Edge = Edge'
  { aliases :: Core.Maybe [Types.Alias]
    -- ^ Aliases for the edge.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The end time of the last segment on the edge.
  , referenceId :: Core.Maybe Core.Int
    -- ^ Identifier of the edge. Unique within a service map.
  , responseTimeHistogram :: Core.Maybe [Types.HistogramEntry]
    -- ^ A histogram that maps the spread of client response times on an edge.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The start time of the first segment on the edge.
  , summaryStatistics :: Core.Maybe Types.EdgeStatistics
    -- ^ Response statistics for segments on the edge.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Edge' value with any optional fields omitted.
mkEdge
    :: Edge
mkEdge
  = Edge'{aliases = Core.Nothing, endTime = Core.Nothing,
          referenceId = Core.Nothing, responseTimeHistogram = Core.Nothing,
          startTime = Core.Nothing, summaryStatistics = Core.Nothing}

-- | Aliases for the edge.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAliases :: Lens.Lens' Edge (Core.Maybe [Types.Alias])
eAliases = Lens.field @"aliases"
{-# INLINEABLE eAliases #-}
{-# DEPRECATED aliases "Use generic-lens or generic-optics with 'aliases' instead"  #-}

-- | The end time of the last segment on the edge.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndTime :: Lens.Lens' Edge (Core.Maybe Core.NominalDiffTime)
eEndTime = Lens.field @"endTime"
{-# INLINEABLE eEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Identifier of the edge. Unique within a service map.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eReferenceId :: Lens.Lens' Edge (Core.Maybe Core.Int)
eReferenceId = Lens.field @"referenceId"
{-# INLINEABLE eReferenceId #-}
{-# DEPRECATED referenceId "Use generic-lens or generic-optics with 'referenceId' instead"  #-}

-- | A histogram that maps the spread of client response times on an edge.
--
-- /Note:/ Consider using 'responseTimeHistogram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eResponseTimeHistogram :: Lens.Lens' Edge (Core.Maybe [Types.HistogramEntry])
eResponseTimeHistogram = Lens.field @"responseTimeHistogram"
{-# INLINEABLE eResponseTimeHistogram #-}
{-# DEPRECATED responseTimeHistogram "Use generic-lens or generic-optics with 'responseTimeHistogram' instead"  #-}

-- | The start time of the first segment on the edge.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStartTime :: Lens.Lens' Edge (Core.Maybe Core.NominalDiffTime)
eStartTime = Lens.field @"startTime"
{-# INLINEABLE eStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | Response statistics for segments on the edge.
--
-- /Note:/ Consider using 'summaryStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSummaryStatistics :: Lens.Lens' Edge (Core.Maybe Types.EdgeStatistics)
eSummaryStatistics = Lens.field @"summaryStatistics"
{-# INLINEABLE eSummaryStatistics #-}
{-# DEPRECATED summaryStatistics "Use generic-lens or generic-optics with 'summaryStatistics' instead"  #-}

instance Core.FromJSON Edge where
        parseJSON
          = Core.withObject "Edge" Core.$
              \ x ->
                Edge' Core.<$>
                  (x Core..:? "Aliases") Core.<*> x Core..:? "EndTime" Core.<*>
                    x Core..:? "ReferenceId"
                    Core.<*> x Core..:? "ResponseTimeHistogram"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "SummaryStatistics"
