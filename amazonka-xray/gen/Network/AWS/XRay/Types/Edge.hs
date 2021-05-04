{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.XRay.Types.Alias
import Network.AWS.XRay.Types.EdgeStatistics
import Network.AWS.XRay.Types.HistogramEntry

-- | Information about a connection between two services.
--
-- /See:/ 'newEdge' smart constructor.
data Edge = Edge'
  { -- | Response statistics for segments on the edge.
    summaryStatistics :: Prelude.Maybe EdgeStatistics,
    -- | A histogram that maps the spread of client response times on an edge.
    responseTimeHistogram :: Prelude.Maybe [HistogramEntry],
    -- | Identifier of the edge. Unique within a service map.
    referenceId :: Prelude.Maybe Prelude.Int,
    -- | The start time of the first segment on the edge.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The end time of the last segment on the edge.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | Aliases for the edge.
    aliases :: Prelude.Maybe [Alias]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { summaryStatistics = Prelude.Nothing,
      responseTimeHistogram = Prelude.Nothing,
      referenceId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      aliases = Prelude.Nothing
    }

-- | Response statistics for segments on the edge.
edge_summaryStatistics :: Lens.Lens' Edge (Prelude.Maybe EdgeStatistics)
edge_summaryStatistics = Lens.lens (\Edge' {summaryStatistics} -> summaryStatistics) (\s@Edge' {} a -> s {summaryStatistics = a} :: Edge)

-- | A histogram that maps the spread of client response times on an edge.
edge_responseTimeHistogram :: Lens.Lens' Edge (Prelude.Maybe [HistogramEntry])
edge_responseTimeHistogram = Lens.lens (\Edge' {responseTimeHistogram} -> responseTimeHistogram) (\s@Edge' {} a -> s {responseTimeHistogram = a} :: Edge) Prelude.. Lens.mapping Prelude._Coerce

-- | Identifier of the edge. Unique within a service map.
edge_referenceId :: Lens.Lens' Edge (Prelude.Maybe Prelude.Int)
edge_referenceId = Lens.lens (\Edge' {referenceId} -> referenceId) (\s@Edge' {} a -> s {referenceId = a} :: Edge)

-- | The start time of the first segment on the edge.
edge_startTime :: Lens.Lens' Edge (Prelude.Maybe Prelude.UTCTime)
edge_startTime = Lens.lens (\Edge' {startTime} -> startTime) (\s@Edge' {} a -> s {startTime = a} :: Edge) Prelude.. Lens.mapping Prelude._Time

-- | The end time of the last segment on the edge.
edge_endTime :: Lens.Lens' Edge (Prelude.Maybe Prelude.UTCTime)
edge_endTime = Lens.lens (\Edge' {endTime} -> endTime) (\s@Edge' {} a -> s {endTime = a} :: Edge) Prelude.. Lens.mapping Prelude._Time

-- | Aliases for the edge.
edge_aliases :: Lens.Lens' Edge (Prelude.Maybe [Alias])
edge_aliases = Lens.lens (\Edge' {aliases} -> aliases) (\s@Edge' {} a -> s {aliases = a} :: Edge) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Edge where
  parseJSON =
    Prelude.withObject
      "Edge"
      ( \x ->
          Edge'
            Prelude.<$> (x Prelude..:? "SummaryStatistics")
            Prelude.<*> ( x Prelude..:? "ResponseTimeHistogram"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ReferenceId")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "Aliases" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable Edge

instance Prelude.NFData Edge
