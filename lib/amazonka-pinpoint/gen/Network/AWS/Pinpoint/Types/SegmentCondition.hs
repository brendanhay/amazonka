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
import qualified Network.AWS.Prelude as Lude

-- | Specifies a segment to associate with an activity in a journey.
--
-- /See:/ 'mkSegmentCondition' smart constructor.
newtype SegmentCondition = SegmentCondition'
  { -- | The unique identifier for the segment to associate with the activity.
    segmentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentCondition' with the minimum fields required to make a request.
--
-- * 'segmentId' - The unique identifier for the segment to associate with the activity.
mkSegmentCondition ::
  -- | 'segmentId'
  Lude.Text ->
  SegmentCondition
mkSegmentCondition pSegmentId_ =
  SegmentCondition' {segmentId = pSegmentId_}

-- | The unique identifier for the segment to associate with the activity.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSegmentId :: Lens.Lens' SegmentCondition Lude.Text
scSegmentId = Lens.lens (segmentId :: SegmentCondition -> Lude.Text) (\s a -> s {segmentId = a} :: SegmentCondition)
{-# DEPRECATED scSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

instance Lude.FromJSON SegmentCondition where
  parseJSON =
    Lude.withObject
      "SegmentCondition"
      (\x -> SegmentCondition' Lude.<$> (x Lude..: "SegmentId"))

instance Lude.ToJSON SegmentCondition where
  toJSON SegmentCondition' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SegmentId" Lude..= segmentId)])
