{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventDetails
  ( EventDetails (..),

    -- * Smart constructor
    mkEventDetails,

    -- * Lenses
    edEvent,
    edEventDescription,
    edEventMetadata,
  )
where

import qualified Network.AWS.AWSHealth.Types.Event as Types
import qualified Network.AWS.AWSHealth.Types.EventDescription as Types
import qualified Network.AWS.AWSHealth.Types.MetadataKey as Types
import qualified Network.AWS.AWSHealth.Types.MetadataValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detailed information about an event. A combination of an <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> object, an <https://docs.aws.amazon.com/health/latest/APIReference/API_EventDescription.html EventDescription> object, and additional metadata about the event. Returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails> operation.
--
-- /See:/ 'mkEventDetails' smart constructor.
data EventDetails = EventDetails'
  { -- | Summary information about the event.
    event :: Core.Maybe Types.Event,
    -- | The most recent description of the event.
    eventDescription :: Core.Maybe Types.EventDescription,
    -- | Additional metadata about the event.
    eventMetadata :: Core.Maybe (Core.HashMap Types.MetadataKey Types.MetadataValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EventDetails' value with any optional fields omitted.
mkEventDetails ::
  EventDetails
mkEventDetails =
  EventDetails'
    { event = Core.Nothing,
      eventDescription = Core.Nothing,
      eventMetadata = Core.Nothing
    }

-- | Summary information about the event.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEvent :: Lens.Lens' EventDetails (Core.Maybe Types.Event)
edEvent = Lens.field @"event"
{-# DEPRECATED edEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | The most recent description of the event.
--
-- /Note:/ Consider using 'eventDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEventDescription :: Lens.Lens' EventDetails (Core.Maybe Types.EventDescription)
edEventDescription = Lens.field @"eventDescription"
{-# DEPRECATED edEventDescription "Use generic-lens or generic-optics with 'eventDescription' instead." #-}

-- | Additional metadata about the event.
--
-- /Note:/ Consider using 'eventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEventMetadata :: Lens.Lens' EventDetails (Core.Maybe (Core.HashMap Types.MetadataKey Types.MetadataValue))
edEventMetadata = Lens.field @"eventMetadata"
{-# DEPRECATED edEventMetadata "Use generic-lens or generic-optics with 'eventMetadata' instead." #-}

instance Core.FromJSON EventDetails where
  parseJSON =
    Core.withObject "EventDetails" Core.$
      \x ->
        EventDetails'
          Core.<$> (x Core..:? "event")
          Core.<*> (x Core..:? "eventDescription")
          Core.<*> (x Core..:? "eventMetadata")
