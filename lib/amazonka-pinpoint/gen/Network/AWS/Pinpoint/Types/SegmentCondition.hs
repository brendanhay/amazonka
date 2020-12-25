{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentCondition
  ( SegmentCondition (..),

    -- * Smart constructor
    mkSegmentCondition,

    -- * Lenses
    scSegmentId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a segment to associate with an activity in a journey.
--
-- /See:/ 'mkSegmentCondition' smart constructor.
newtype SegmentCondition = SegmentCondition'
  { -- | The unique identifier for the segment to associate with the activity.
    segmentId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentCondition' value with any optional fields omitted.
mkSegmentCondition ::
  -- | 'segmentId'
  Core.Text ->
  SegmentCondition
mkSegmentCondition segmentId = SegmentCondition' {segmentId}

-- | The unique identifier for the segment to associate with the activity.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSegmentId :: Lens.Lens' SegmentCondition Core.Text
scSegmentId = Lens.field @"segmentId"
{-# DEPRECATED scSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

instance Core.FromJSON SegmentCondition where
  toJSON SegmentCondition {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SegmentId" Core..= segmentId)])

instance Core.FromJSON SegmentCondition where
  parseJSON =
    Core.withObject "SegmentCondition" Core.$
      \x -> SegmentCondition' Core.<$> (x Core..: "SegmentId")
