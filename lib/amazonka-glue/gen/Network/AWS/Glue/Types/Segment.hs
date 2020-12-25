{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Segment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Segment
  ( Segment (..),

    -- * Smart constructor
    mkSegment,

    -- * Lenses
    sSegmentNumber,
    sTotalSegments,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines a non-overlapping region of a table's partitions, allowing multiple requests to be executed in parallel.
--
-- /See:/ 'mkSegment' smart constructor.
data Segment = Segment'
  { -- | The zero-based index number of the segment. For example, if the total number of segments is 4, @SegmentNumber@ values range from 0 through 3.
    segmentNumber :: Core.Natural,
    -- | The total number of segments.
    totalSegments :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Segment' value with any optional fields omitted.
mkSegment ::
  -- | 'segmentNumber'
  Core.Natural ->
  -- | 'totalSegments'
  Core.Natural ->
  Segment
mkSegment segmentNumber totalSegments =
  Segment' {segmentNumber, totalSegments}

-- | The zero-based index number of the segment. For example, if the total number of segments is 4, @SegmentNumber@ values range from 0 through 3.
--
-- /Note:/ Consider using 'segmentNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSegmentNumber :: Lens.Lens' Segment Core.Natural
sSegmentNumber = Lens.field @"segmentNumber"
{-# DEPRECATED sSegmentNumber "Use generic-lens or generic-optics with 'segmentNumber' instead." #-}

-- | The total number of segments.
--
-- /Note:/ Consider using 'totalSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTotalSegments :: Lens.Lens' Segment Core.Natural
sTotalSegments = Lens.field @"totalSegments"
{-# DEPRECATED sTotalSegments "Use generic-lens or generic-optics with 'totalSegments' instead." #-}

instance Core.FromJSON Segment where
  toJSON Segment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SegmentNumber" Core..= segmentNumber),
            Core.Just ("TotalSegments" Core..= totalSegments)
          ]
      )
