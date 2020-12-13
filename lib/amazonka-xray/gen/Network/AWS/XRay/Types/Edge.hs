{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Edge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Edge
  ( Edge (..),

    -- * Smart constructor
    mkEdge,

    -- * Lenses
    eStartTime,
    eAliases,
    eResponseTimeHistogram,
    eReferenceId,
    eEndTime,
    eSummaryStatistics,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.Alias
import Network.AWS.XRay.Types.EdgeStatistics
import Network.AWS.XRay.Types.HistogramEntry

-- | Information about a connection between two services.
--
-- /See:/ 'mkEdge' smart constructor.
data Edge = Edge'
  { -- | The start time of the first segment on the edge.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | Aliases for the edge.
    aliases :: Lude.Maybe [Alias],
    -- | A histogram that maps the spread of client response times on an edge.
    responseTimeHistogram :: Lude.Maybe [HistogramEntry],
    -- | Identifier of the edge. Unique within a service map.
    referenceId :: Lude.Maybe Lude.Int,
    -- | The end time of the last segment on the edge.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | Response statistics for segments on the edge.
    summaryStatistics :: Lude.Maybe EdgeStatistics
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Edge' with the minimum fields required to make a request.
--
-- * 'startTime' - The start time of the first segment on the edge.
-- * 'aliases' - Aliases for the edge.
-- * 'responseTimeHistogram' - A histogram that maps the spread of client response times on an edge.
-- * 'referenceId' - Identifier of the edge. Unique within a service map.
-- * 'endTime' - The end time of the last segment on the edge.
-- * 'summaryStatistics' - Response statistics for segments on the edge.
mkEdge ::
  Edge
mkEdge =
  Edge'
    { startTime = Lude.Nothing,
      aliases = Lude.Nothing,
      responseTimeHistogram = Lude.Nothing,
      referenceId = Lude.Nothing,
      endTime = Lude.Nothing,
      summaryStatistics = Lude.Nothing
    }

-- | The start time of the first segment on the edge.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStartTime :: Lens.Lens' Edge (Lude.Maybe Lude.Timestamp)
eStartTime = Lens.lens (startTime :: Edge -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: Edge)
{-# DEPRECATED eStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Aliases for the edge.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAliases :: Lens.Lens' Edge (Lude.Maybe [Alias])
eAliases = Lens.lens (aliases :: Edge -> Lude.Maybe [Alias]) (\s a -> s {aliases = a} :: Edge)
{-# DEPRECATED eAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | A histogram that maps the spread of client response times on an edge.
--
-- /Note:/ Consider using 'responseTimeHistogram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eResponseTimeHistogram :: Lens.Lens' Edge (Lude.Maybe [HistogramEntry])
eResponseTimeHistogram = Lens.lens (responseTimeHistogram :: Edge -> Lude.Maybe [HistogramEntry]) (\s a -> s {responseTimeHistogram = a} :: Edge)
{-# DEPRECATED eResponseTimeHistogram "Use generic-lens or generic-optics with 'responseTimeHistogram' instead." #-}

-- | Identifier of the edge. Unique within a service map.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eReferenceId :: Lens.Lens' Edge (Lude.Maybe Lude.Int)
eReferenceId = Lens.lens (referenceId :: Edge -> Lude.Maybe Lude.Int) (\s a -> s {referenceId = a} :: Edge)
{-# DEPRECATED eReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

-- | The end time of the last segment on the edge.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndTime :: Lens.Lens' Edge (Lude.Maybe Lude.Timestamp)
eEndTime = Lens.lens (endTime :: Edge -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: Edge)
{-# DEPRECATED eEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Response statistics for segments on the edge.
--
-- /Note:/ Consider using 'summaryStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSummaryStatistics :: Lens.Lens' Edge (Lude.Maybe EdgeStatistics)
eSummaryStatistics = Lens.lens (summaryStatistics :: Edge -> Lude.Maybe EdgeStatistics) (\s a -> s {summaryStatistics = a} :: Edge)
{-# DEPRECATED eSummaryStatistics "Use generic-lens or generic-optics with 'summaryStatistics' instead." #-}

instance Lude.FromJSON Edge where
  parseJSON =
    Lude.withObject
      "Edge"
      ( \x ->
          Edge'
            Lude.<$> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "Aliases" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResponseTimeHistogram" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ReferenceId")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "SummaryStatistics")
      )
