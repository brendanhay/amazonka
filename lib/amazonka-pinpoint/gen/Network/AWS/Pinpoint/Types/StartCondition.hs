{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.StartCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.StartCondition
  ( StartCondition (..),

    -- * Smart constructor
    mkStartCondition,

    -- * Lenses
    scDescription,
    scEventStartCondition,
    scSegmentStartCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EventStartCondition as Types
import qualified Network.AWS.Pinpoint.Types.SegmentCondition as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the conditions for the first activity in a journey. This activity and its conditions determine which users are participants in a journey.
--
-- /See:/ 'mkStartCondition' smart constructor.
data StartCondition = StartCondition'
  { -- | The custom description of the condition.
    description :: Core.Maybe Core.Text,
    eventStartCondition :: Core.Maybe Types.EventStartCondition,
    -- | The segment that's associated with the first activity in the journey. This segment determines which users are participants in the journey.
    segmentStartCondition :: Core.Maybe Types.SegmentCondition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartCondition' value with any optional fields omitted.
mkStartCondition ::
  StartCondition
mkStartCondition =
  StartCondition'
    { description = Core.Nothing,
      eventStartCondition = Core.Nothing,
      segmentStartCondition = Core.Nothing
    }

-- | The custom description of the condition.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDescription :: Lens.Lens' StartCondition (Core.Maybe Core.Text)
scDescription = Lens.field @"description"
{-# DEPRECATED scDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventStartCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scEventStartCondition :: Lens.Lens' StartCondition (Core.Maybe Types.EventStartCondition)
scEventStartCondition = Lens.field @"eventStartCondition"
{-# DEPRECATED scEventStartCondition "Use generic-lens or generic-optics with 'eventStartCondition' instead." #-}

-- | The segment that's associated with the first activity in the journey. This segment determines which users are participants in the journey.
--
-- /Note:/ Consider using 'segmentStartCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSegmentStartCondition :: Lens.Lens' StartCondition (Core.Maybe Types.SegmentCondition)
scSegmentStartCondition = Lens.field @"segmentStartCondition"
{-# DEPRECATED scSegmentStartCondition "Use generic-lens or generic-optics with 'segmentStartCondition' instead." #-}

instance Core.FromJSON StartCondition where
  toJSON StartCondition {..} =
    Core.object
      ( Core.catMaybes
          [ ("Description" Core..=) Core.<$> description,
            ("EventStartCondition" Core..=) Core.<$> eventStartCondition,
            ("SegmentStartCondition" Core..=) Core.<$> segmentStartCondition
          ]
      )

instance Core.FromJSON StartCondition where
  parseJSON =
    Core.withObject "StartCondition" Core.$
      \x ->
        StartCondition'
          Core.<$> (x Core..:? "Description")
          Core.<*> (x Core..:? "EventStartCondition")
          Core.<*> (x Core..:? "SegmentStartCondition")
