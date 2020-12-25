{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eDate,
    eEventCategories,
    eEventId,
    eMessage,
    eSeverity,
    eSourceIdentifier,
    eSourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.EventId as Types
import qualified Network.AWS.Redshift.Types.Message as Types
import qualified Network.AWS.Redshift.Types.Severity as Types
import qualified Network.AWS.Redshift.Types.SourceIdentifier as Types
import qualified Network.AWS.Redshift.Types.SourceType as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes an event.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { -- | The date and time of the event.
    date :: Core.Maybe Core.UTCTime,
    -- | A list of the event categories.
    --
    -- Values: Configuration, Management, Monitoring, Security
    eventCategories :: Core.Maybe [Types.String],
    -- | The identifier of the event.
    eventId :: Core.Maybe Types.EventId,
    -- | The text of this event.
    message :: Core.Maybe Types.Message,
    -- | The severity of the event.
    --
    -- Values: ERROR, INFO
    severity :: Core.Maybe Types.Severity,
    -- | The identifier for the source of the event.
    sourceIdentifier :: Core.Maybe Types.SourceIdentifier,
    -- | The source type for this event.
    sourceType :: Core.Maybe Types.SourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent ::
  Event
mkEvent =
  Event'
    { date = Core.Nothing,
      eventCategories = Core.Nothing,
      eventId = Core.Nothing,
      message = Core.Nothing,
      severity = Core.Nothing,
      sourceIdentifier = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | The date and time of the event.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Core.Maybe Core.UTCTime)
eDate = Lens.field @"date"
{-# DEPRECATED eDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventCategories :: Lens.Lens' Event (Core.Maybe [Types.String])
eEventCategories = Lens.field @"eventCategories"
{-# DEPRECATED eEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | The identifier of the event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventId :: Lens.Lens' Event (Core.Maybe Types.EventId)
eEventId = Lens.field @"eventId"
{-# DEPRECATED eEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

-- | The text of this event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Core.Maybe Types.Message)
eMessage = Lens.field @"message"
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The severity of the event.
--
-- Values: ERROR, INFO
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSeverity :: Lens.Lens' Event (Core.Maybe Types.Severity)
eSeverity = Lens.field @"severity"
{-# DEPRECATED eSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The identifier for the source of the event.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceIdentifier :: Lens.Lens' Event (Core.Maybe Types.SourceIdentifier)
eSourceIdentifier = Lens.field @"sourceIdentifier"
{-# DEPRECATED eSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | The source type for this event.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Core.Maybe Types.SourceType)
eSourceType = Lens.field @"sourceType"
{-# DEPRECATED eSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

instance Core.FromXML Event where
  parseXML x =
    Event'
      Core.<$> (x Core..@? "Date")
      Core.<*> ( x Core..@? "EventCategories"
                   Core..<@> Core.parseXMLList "EventCategory"
               )
      Core.<*> (x Core..@? "EventId")
      Core.<*> (x Core..@? "Message")
      Core.<*> (x Core..@? "Severity")
      Core.<*> (x Core..@? "SourceIdentifier")
      Core.<*> (x Core..@? "SourceType")
