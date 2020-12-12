{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventStartCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventStartCondition
  ( EventStartCondition (..),

    -- * Smart constructor
    mkEventStartCondition,

    -- * Lenses
    escEventFilter,
    escSegmentId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventFilter
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for an event that causes a journey activity to start.
--
-- /See:/ 'mkEventStartCondition' smart constructor.
data EventStartCondition = EventStartCondition'
  { eventFilter ::
      Lude.Maybe EventFilter,
    segmentId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventStartCondition' with the minimum fields required to make a request.
--
-- * 'eventFilter' - Undocumented field.
-- * 'segmentId' - Undocumented field.
mkEventStartCondition ::
  EventStartCondition
mkEventStartCondition =
  EventStartCondition'
    { eventFilter = Lude.Nothing,
      segmentId = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escEventFilter :: Lens.Lens' EventStartCondition (Lude.Maybe EventFilter)
escEventFilter = Lens.lens (eventFilter :: EventStartCondition -> Lude.Maybe EventFilter) (\s a -> s {eventFilter = a} :: EventStartCondition)
{-# DEPRECATED escEventFilter "Use generic-lens or generic-optics with 'eventFilter' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escSegmentId :: Lens.Lens' EventStartCondition (Lude.Maybe Lude.Text)
escSegmentId = Lens.lens (segmentId :: EventStartCondition -> Lude.Maybe Lude.Text) (\s a -> s {segmentId = a} :: EventStartCondition)
{-# DEPRECATED escSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

instance Lude.FromJSON EventStartCondition where
  parseJSON =
    Lude.withObject
      "EventStartCondition"
      ( \x ->
          EventStartCondition'
            Lude.<$> (x Lude..:? "EventFilter") Lude.<*> (x Lude..:? "SegmentId")
      )

instance Lude.ToJSON EventStartCondition where
  toJSON EventStartCondition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventFilter" Lude..=) Lude.<$> eventFilter,
            ("SegmentId" Lude..=) Lude.<$> segmentId
          ]
      )
