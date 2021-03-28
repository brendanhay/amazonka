{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.Event
  ( Event (..)
  -- * Smart constructor
  , mkEvent
  -- * Lenses
  , eDate
  , eEventCategories
  , eEventId
  , eMessage
  , eSeverity
  , eSourceIdentifier
  , eSourceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.SourceType as Types

-- | Describes an event.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { date :: Core.Maybe Core.UTCTime
    -- ^ The date and time of the event.
  , eventCategories :: Core.Maybe [Core.Text]
    -- ^ A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security
  , eventId :: Core.Maybe Core.Text
    -- ^ The identifier of the event.
  , message :: Core.Maybe Core.Text
    -- ^ The text of this event.
  , severity :: Core.Maybe Core.Text
    -- ^ The severity of the event.
--
-- Values: ERROR, INFO
  , sourceIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier for the source of the event.
  , sourceType :: Core.Maybe Types.SourceType
    -- ^ The source type for this event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent
    :: Event
mkEvent
  = Event'{date = Core.Nothing, eventCategories = Core.Nothing,
           eventId = Core.Nothing, message = Core.Nothing,
           severity = Core.Nothing, sourceIdentifier = Core.Nothing,
           sourceType = Core.Nothing}

-- | The date and time of the event.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Core.Maybe Core.UTCTime)
eDate = Lens.field @"date"
{-# INLINEABLE eDate #-}
{-# DEPRECATED date "Use generic-lens or generic-optics with 'date' instead"  #-}

-- | A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventCategories :: Lens.Lens' Event (Core.Maybe [Core.Text])
eEventCategories = Lens.field @"eventCategories"
{-# INLINEABLE eEventCategories #-}
{-# DEPRECATED eventCategories "Use generic-lens or generic-optics with 'eventCategories' instead"  #-}

-- | The identifier of the event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventId :: Lens.Lens' Event (Core.Maybe Core.Text)
eEventId = Lens.field @"eventId"
{-# INLINEABLE eEventId #-}
{-# DEPRECATED eventId "Use generic-lens or generic-optics with 'eventId' instead"  #-}

-- | The text of this event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Core.Maybe Core.Text)
eMessage = Lens.field @"message"
{-# INLINEABLE eMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The severity of the event.
--
-- Values: ERROR, INFO
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSeverity :: Lens.Lens' Event (Core.Maybe Core.Text)
eSeverity = Lens.field @"severity"
{-# INLINEABLE eSeverity #-}
{-# DEPRECATED severity "Use generic-lens or generic-optics with 'severity' instead"  #-}

-- | The identifier for the source of the event.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceIdentifier :: Lens.Lens' Event (Core.Maybe Core.Text)
eSourceIdentifier = Lens.field @"sourceIdentifier"
{-# INLINEABLE eSourceIdentifier #-}
{-# DEPRECATED sourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead"  #-}

-- | The source type for this event.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Core.Maybe Types.SourceType)
eSourceType = Lens.field @"sourceType"
{-# INLINEABLE eSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

instance Core.FromXML Event where
        parseXML x
          = Event' Core.<$>
              (x Core..@? "Date") Core.<*>
                x Core..@? "EventCategories" Core..<@>
                  Core.parseXMLList "EventCategory"
                Core.<*> x Core..@? "EventId"
                Core.<*> x Core..@? "Message"
                Core.<*> x Core..@? "Severity"
                Core.<*> x Core..@? "SourceIdentifier"
                Core.<*> x Core..@? "SourceType"
