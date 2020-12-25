{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventCondition
  ( EventCondition (..),

    -- * Smart constructor
    mkEventCondition,

    -- * Lenses
    ecDimensions,
    ecMessageActivity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EventDimensions as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the conditions to evaluate for an event that applies to an activity in a journey.
--
-- /See:/ 'mkEventCondition' smart constructor.
data EventCondition = EventCondition'
  { -- | The dimensions for the event filter to use for the activity.
    dimensions :: Core.Maybe Types.EventDimensions,
    -- | The message identifier (message_id) for the message to use when determining whether message events meet the condition.
    messageActivity :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventCondition' value with any optional fields omitted.
mkEventCondition ::
  EventCondition
mkEventCondition =
  EventCondition'
    { dimensions = Core.Nothing,
      messageActivity = Core.Nothing
    }

-- | The dimensions for the event filter to use for the activity.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecDimensions :: Lens.Lens' EventCondition (Core.Maybe Types.EventDimensions)
ecDimensions = Lens.field @"dimensions"
{-# DEPRECATED ecDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The message identifier (message_id) for the message to use when determining whether message events meet the condition.
--
-- /Note:/ Consider using 'messageActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecMessageActivity :: Lens.Lens' EventCondition (Core.Maybe Core.Text)
ecMessageActivity = Lens.field @"messageActivity"
{-# DEPRECATED ecMessageActivity "Use generic-lens or generic-optics with 'messageActivity' instead." #-}

instance Core.FromJSON EventCondition where
  toJSON EventCondition {..} =
    Core.object
      ( Core.catMaybes
          [ ("Dimensions" Core..=) Core.<$> dimensions,
            ("MessageActivity" Core..=) Core.<$> messageActivity
          ]
      )

instance Core.FromJSON EventCondition where
  parseJSON =
    Core.withObject "EventCondition" Core.$
      \x ->
        EventCondition'
          Core.<$> (x Core..:? "Dimensions") Core.<*> (x Core..:? "MessageActivity")
