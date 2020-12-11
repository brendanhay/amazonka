-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SimpleCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SimpleCondition
  ( SimpleCondition (..),

    -- * Smart constructor
    mkSimpleCondition,

    -- * Lenses
    scSegmentDimensions,
    scEventCondition,
    scSegmentCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventCondition
import Network.AWS.Pinpoint.Types.SegmentCondition
import Network.AWS.Pinpoint.Types.SegmentDimensions
import qualified Network.AWS.Prelude as Lude

-- | Specifies a condition to evaluate for an activity in a journey.
--
-- /See:/ 'mkSimpleCondition' smart constructor.
data SimpleCondition = SimpleCondition'
  { segmentDimensions ::
      Lude.Maybe SegmentDimensions,
    eventCondition :: Lude.Maybe EventCondition,
    segmentCondition :: Lude.Maybe SegmentCondition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SimpleCondition' with the minimum fields required to make a request.
--
-- * 'eventCondition' - The dimension settings for the event that's associated with the activity.
-- * 'segmentCondition' - The segment that's associated with the activity.
-- * 'segmentDimensions' - The dimension settings for the segment that's associated with the activity.
mkSimpleCondition ::
  SimpleCondition
mkSimpleCondition =
  SimpleCondition'
    { segmentDimensions = Lude.Nothing,
      eventCondition = Lude.Nothing,
      segmentCondition = Lude.Nothing
    }

-- | The dimension settings for the segment that's associated with the activity.
--
-- /Note:/ Consider using 'segmentDimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSegmentDimensions :: Lens.Lens' SimpleCondition (Lude.Maybe SegmentDimensions)
scSegmentDimensions = Lens.lens (segmentDimensions :: SimpleCondition -> Lude.Maybe SegmentDimensions) (\s a -> s {segmentDimensions = a} :: SimpleCondition)
{-# DEPRECATED scSegmentDimensions "Use generic-lens or generic-optics with 'segmentDimensions' instead." #-}

-- | The dimension settings for the event that's associated with the activity.
--
-- /Note:/ Consider using 'eventCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scEventCondition :: Lens.Lens' SimpleCondition (Lude.Maybe EventCondition)
scEventCondition = Lens.lens (eventCondition :: SimpleCondition -> Lude.Maybe EventCondition) (\s a -> s {eventCondition = a} :: SimpleCondition)
{-# DEPRECATED scEventCondition "Use generic-lens or generic-optics with 'eventCondition' instead." #-}

-- | The segment that's associated with the activity.
--
-- /Note:/ Consider using 'segmentCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSegmentCondition :: Lens.Lens' SimpleCondition (Lude.Maybe SegmentCondition)
scSegmentCondition = Lens.lens (segmentCondition :: SimpleCondition -> Lude.Maybe SegmentCondition) (\s a -> s {segmentCondition = a} :: SimpleCondition)
{-# DEPRECATED scSegmentCondition "Use generic-lens or generic-optics with 'segmentCondition' instead." #-}

instance Lude.FromJSON SimpleCondition where
  parseJSON =
    Lude.withObject
      "SimpleCondition"
      ( \x ->
          SimpleCondition'
            Lude.<$> (x Lude..:? "segmentDimensions")
            Lude.<*> (x Lude..:? "EventCondition")
            Lude.<*> (x Lude..:? "SegmentCondition")
      )

instance Lude.ToJSON SimpleCondition where
  toJSON SimpleCondition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("segmentDimensions" Lude..=) Lude.<$> segmentDimensions,
            ("EventCondition" Lude..=) Lude.<$> eventCondition,
            ("SegmentCondition" Lude..=) Lude.<$> segmentCondition
          ]
      )
