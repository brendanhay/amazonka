{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Edge
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Edge where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.Alias
import Network.AWS.XRay.Types.EdgeStatistics
import Network.AWS.XRay.Types.HistogramEntry

-- | Information about a connection between two services.
--
-- /See:/ 'newEdge' smart constructor.
data Edge = Edge'
  { -- | Response statistics for segments on the edge.
    summaryStatistics :: Core.Maybe EdgeStatistics,
    -- | A histogram that maps the spread of client response times on an edge.
    responseTimeHistogram :: Core.Maybe [HistogramEntry],
    -- | Identifier of the edge. Unique within a service map.
    referenceId :: Core.Maybe Core.Int,
    -- | The start time of the first segment on the edge.
    startTime :: Core.Maybe Core.POSIX,
    -- | The end time of the last segment on the edge.
    endTime :: Core.Maybe Core.POSIX,
    -- | Aliases for the edge.
    aliases :: Core.Maybe [Alias]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Edge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summaryStatistics', 'edge_summaryStatistics' - Response statistics for segments on the edge.
--
-- 'responseTimeHistogram', 'edge_responseTimeHistogram' - A histogram that maps the spread of client response times on an edge.
--
-- 'referenceId', 'edge_referenceId' - Identifier of the edge. Unique within a service map.
--
-- 'startTime', 'edge_startTime' - The start time of the first segment on the edge.
--
-- 'endTime', 'edge_endTime' - The end time of the last segment on the edge.
--
-- 'aliases', 'edge_aliases' - Aliases for the edge.
newEdge ::
  Edge
newEdge =
  Edge'
    { summaryStatistics = Core.Nothing,
      responseTimeHistogram = Core.Nothing,
      referenceId = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      aliases = Core.Nothing
    }

-- | Response statistics for segments on the edge.
edge_summaryStatistics :: Lens.Lens' Edge (Core.Maybe EdgeStatistics)
edge_summaryStatistics = Lens.lens (\Edge' {summaryStatistics} -> summaryStatistics) (\s@Edge' {} a -> s {summaryStatistics = a} :: Edge)

-- | A histogram that maps the spread of client response times on an edge.
edge_responseTimeHistogram :: Lens.Lens' Edge (Core.Maybe [HistogramEntry])
edge_responseTimeHistogram = Lens.lens (\Edge' {responseTimeHistogram} -> responseTimeHistogram) (\s@Edge' {} a -> s {responseTimeHistogram = a} :: Edge) Core.. Lens.mapping Lens._Coerce

-- | Identifier of the edge. Unique within a service map.
edge_referenceId :: Lens.Lens' Edge (Core.Maybe Core.Int)
edge_referenceId = Lens.lens (\Edge' {referenceId} -> referenceId) (\s@Edge' {} a -> s {referenceId = a} :: Edge)

-- | The start time of the first segment on the edge.
edge_startTime :: Lens.Lens' Edge (Core.Maybe Core.UTCTime)
edge_startTime = Lens.lens (\Edge' {startTime} -> startTime) (\s@Edge' {} a -> s {startTime = a} :: Edge) Core.. Lens.mapping Core._Time

-- | The end time of the last segment on the edge.
edge_endTime :: Lens.Lens' Edge (Core.Maybe Core.UTCTime)
edge_endTime = Lens.lens (\Edge' {endTime} -> endTime) (\s@Edge' {} a -> s {endTime = a} :: Edge) Core.. Lens.mapping Core._Time

-- | Aliases for the edge.
edge_aliases :: Lens.Lens' Edge (Core.Maybe [Alias])
edge_aliases = Lens.lens (\Edge' {aliases} -> aliases) (\s@Edge' {} a -> s {aliases = a} :: Edge) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Edge where
  parseJSON =
    Core.withObject
      "Edge"
      ( \x ->
          Edge'
            Core.<$> (x Core..:? "SummaryStatistics")
            Core.<*> ( x Core..:? "ResponseTimeHistogram"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ReferenceId")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "Aliases" Core..!= Core.mempty)
      )

instance Core.Hashable Edge

instance Core.NFData Edge
