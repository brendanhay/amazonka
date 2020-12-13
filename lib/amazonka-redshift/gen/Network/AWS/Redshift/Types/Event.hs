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
    eSourceType,
    eSeverity,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,
    eEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SourceType

-- | Describes an event.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { -- | The source type for this event.
    sourceType :: Lude.Maybe SourceType,
    -- | The severity of the event.
    --
    -- Values: ERROR, INFO
    severity :: Lude.Maybe Lude.Text,
    -- | The identifier for the source of the event.
    sourceIdentifier :: Lude.Maybe Lude.Text,
    -- | The date and time of the event.
    date :: Lude.Maybe Lude.DateTime,
    -- | A list of the event categories.
    --
    -- Values: Configuration, Management, Monitoring, Security
    eventCategories :: Lude.Maybe [Lude.Text],
    -- | The text of this event.
    message :: Lude.Maybe Lude.Text,
    -- | The identifier of the event.
    eventId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- * 'sourceType' - The source type for this event.
-- * 'severity' - The severity of the event.
--
-- Values: ERROR, INFO
-- * 'sourceIdentifier' - The identifier for the source of the event.
-- * 'date' - The date and time of the event.
-- * 'eventCategories' - A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security
-- * 'message' - The text of this event.
-- * 'eventId' - The identifier of the event.
mkEvent ::
  Event
mkEvent =
  Event'
    { sourceType = Lude.Nothing,
      severity = Lude.Nothing,
      sourceIdentifier = Lude.Nothing,
      date = Lude.Nothing,
      eventCategories = Lude.Nothing,
      message = Lude.Nothing,
      eventId = Lude.Nothing
    }

-- | The source type for this event.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Lude.Maybe SourceType)
eSourceType = Lens.lens (sourceType :: Event -> Lude.Maybe SourceType) (\s a -> s {sourceType = a} :: Event)
{-# DEPRECATED eSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The severity of the event.
--
-- Values: ERROR, INFO
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSeverity :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eSeverity = Lens.lens (severity :: Event -> Lude.Maybe Lude.Text) (\s a -> s {severity = a} :: Event)
{-# DEPRECATED eSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The identifier for the source of the event.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceIdentifier :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eSourceIdentifier = Lens.lens (sourceIdentifier :: Event -> Lude.Maybe Lude.Text) (\s a -> s {sourceIdentifier = a} :: Event)
{-# DEPRECATED eSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | The date and time of the event.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Lude.Maybe Lude.DateTime)
eDate = Lens.lens (date :: Event -> Lude.Maybe Lude.DateTime) (\s a -> s {date = a} :: Event)
{-# DEPRECATED eDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventCategories :: Lens.Lens' Event (Lude.Maybe [Lude.Text])
eEventCategories = Lens.lens (eventCategories :: Event -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategories = a} :: Event)
{-# DEPRECATED eEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | The text of this event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eMessage = Lens.lens (message :: Event -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Event)
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The identifier of the event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventId :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eEventId = Lens.lens (eventId :: Event -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: Event)
{-# DEPRECATED eEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.FromXML Event where
  parseXML x =
    Event'
      Lude.<$> (x Lude..@? "SourceType")
      Lude.<*> (x Lude..@? "Severity")
      Lude.<*> (x Lude..@? "SourceIdentifier")
      Lude.<*> (x Lude..@? "Date")
      Lude.<*> ( x Lude..@? "EventCategories" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EventCategory")
               )
      Lude.<*> (x Lude..@? "Message")
      Lude.<*> (x Lude..@? "EventId")
