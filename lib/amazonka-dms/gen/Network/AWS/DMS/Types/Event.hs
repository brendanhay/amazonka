{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eSourceType,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,
  )
where

import Network.AWS.DMS.Types.SourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an identifiable significant activity that affects a replication instance or task. This object can provide the message, the available event categories, the date and source of the event, and the AWS DMS resource type.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | endpoint | replication-task
    sourceType :: Lude.Maybe SourceType,
    -- | The identifier of an event source.
    sourceIdentifier :: Lude.Maybe Lude.Text,
    -- | The date of the event.
    date :: Lude.Maybe Lude.Timestamp,
    -- | The event categories available for the specified source type.
    eventCategories :: Lude.Maybe [Lude.Text],
    -- | The event message.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- * 'sourceType' - The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | endpoint | replication-task
-- * 'sourceIdentifier' - The identifier of an event source.
-- * 'date' - The date of the event.
-- * 'eventCategories' - The event categories available for the specified source type.
-- * 'message' - The event message.
mkEvent ::
  Event
mkEvent =
  Event'
    { sourceType = Lude.Nothing,
      sourceIdentifier = Lude.Nothing,
      date = Lude.Nothing,
      eventCategories = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | endpoint | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Lude.Maybe SourceType)
eSourceType = Lens.lens (sourceType :: Event -> Lude.Maybe SourceType) (\s a -> s {sourceType = a} :: Event)
{-# DEPRECATED eSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The identifier of an event source.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceIdentifier :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eSourceIdentifier = Lens.lens (sourceIdentifier :: Event -> Lude.Maybe Lude.Text) (\s a -> s {sourceIdentifier = a} :: Event)
{-# DEPRECATED eSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | The date of the event.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Lude.Maybe Lude.Timestamp)
eDate = Lens.lens (date :: Event -> Lude.Maybe Lude.Timestamp) (\s a -> s {date = a} :: Event)
{-# DEPRECATED eDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | The event categories available for the specified source type.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventCategories :: Lens.Lens' Event (Lude.Maybe [Lude.Text])
eEventCategories = Lens.lens (eventCategories :: Event -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategories = a} :: Event)
{-# DEPRECATED eEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | The event message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eMessage = Lens.lens (message :: Event -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Event)
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON Event where
  parseJSON =
    Lude.withObject
      "Event"
      ( \x ->
          Event'
            Lude.<$> (x Lude..:? "SourceType")
            Lude.<*> (x Lude..:? "SourceIdentifier")
            Lude.<*> (x Lude..:? "Date")
            Lude.<*> (x Lude..:? "EventCategories" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Message")
      )
