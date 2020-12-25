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
import qualified Network.AWS.Pinpoint.Types.EventFilter as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for an event that causes a journey activity to start.
--
-- /See:/ 'mkEventStartCondition' smart constructor.
data EventStartCondition = EventStartCondition'
  { eventFilter :: Core.Maybe Types.EventFilter,
    segmentId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventStartCondition' value with any optional fields omitted.
mkEventStartCondition ::
  EventStartCondition
mkEventStartCondition =
  EventStartCondition'
    { eventFilter = Core.Nothing,
      segmentId = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escEventFilter :: Lens.Lens' EventStartCondition (Core.Maybe Types.EventFilter)
escEventFilter = Lens.field @"eventFilter"
{-# DEPRECATED escEventFilter "Use generic-lens or generic-optics with 'eventFilter' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escSegmentId :: Lens.Lens' EventStartCondition (Core.Maybe Core.Text)
escSegmentId = Lens.field @"segmentId"
{-# DEPRECATED escSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

instance Core.FromJSON EventStartCondition where
  toJSON EventStartCondition {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventFilter" Core..=) Core.<$> eventFilter,
            ("SegmentId" Core..=) Core.<$> segmentId
          ]
      )

instance Core.FromJSON EventStartCondition where
  parseJSON =
    Core.withObject "EventStartCondition" Core.$
      \x ->
        EventStartCondition'
          Core.<$> (x Core..:? "EventFilter") Core.<*> (x Core..:? "SegmentId")
