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
-- Module      : Amazonka.Glue.Types.Segment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Segment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a non-overlapping region of a table\'s partitions, allowing
-- multiple requests to be run in parallel.
--
-- /See:/ 'newSegment' smart constructor.
data Segment = Segment'
  { -- | The zero-based index number of the segment. For example, if the total
    -- number of segments is 4, @SegmentNumber@ values range from 0 through 3.
    segmentNumber :: Prelude.Natural,
    -- | The total number of segments.
    totalSegments :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Segment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentNumber', 'segment_segmentNumber' - The zero-based index number of the segment. For example, if the total
-- number of segments is 4, @SegmentNumber@ values range from 0 through 3.
--
-- 'totalSegments', 'segment_totalSegments' - The total number of segments.
newSegment ::
  -- | 'segmentNumber'
  Prelude.Natural ->
  -- | 'totalSegments'
  Prelude.Natural ->
  Segment
newSegment pSegmentNumber_ pTotalSegments_ =
  Segment'
    { segmentNumber = pSegmentNumber_,
      totalSegments = pTotalSegments_
    }

-- | The zero-based index number of the segment. For example, if the total
-- number of segments is 4, @SegmentNumber@ values range from 0 through 3.
segment_segmentNumber :: Lens.Lens' Segment Prelude.Natural
segment_segmentNumber = Lens.lens (\Segment' {segmentNumber} -> segmentNumber) (\s@Segment' {} a -> s {segmentNumber = a} :: Segment)

-- | The total number of segments.
segment_totalSegments :: Lens.Lens' Segment Prelude.Natural
segment_totalSegments = Lens.lens (\Segment' {totalSegments} -> totalSegments) (\s@Segment' {} a -> s {totalSegments = a} :: Segment)

instance Prelude.Hashable Segment where
  hashWithSalt _salt Segment' {..} =
    _salt
      `Prelude.hashWithSalt` segmentNumber
      `Prelude.hashWithSalt` totalSegments

instance Prelude.NFData Segment where
  rnf Segment' {..} =
    Prelude.rnf segmentNumber `Prelude.seq`
      Prelude.rnf totalSegments

instance Data.ToJSON Segment where
  toJSON Segment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SegmentNumber" Data..= segmentNumber),
            Prelude.Just
              ("TotalSegments" Data..= totalSegments)
          ]
      )
