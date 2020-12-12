{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eSourceType,
    eSourceARN,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.SourceType

-- | This data type is used as a response element in the @DescribeEvents@ action.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { sourceType :: Lude.Maybe SourceType,
    sourceARN :: Lude.Maybe Lude.Text,
    sourceIdentifier :: Lude.Maybe Lude.Text,
    date :: Lude.Maybe Lude.DateTime,
    eventCategories :: Lude.Maybe [Lude.Text],
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- * 'date' - Specifies the date and time of the event.
-- * 'eventCategories' - Specifies the category for the event.
-- * 'message' - Provides the text of this event.
-- * 'sourceARN' - The Amazon Resource Name (ARN) for the event.
-- * 'sourceIdentifier' - Provides the identifier for the source of the event.
-- * 'sourceType' - Specifies the source type for this event.
mkEvent ::
  Event
mkEvent =
  Event'
    { sourceType = Lude.Nothing,
      sourceARN = Lude.Nothing,
      sourceIdentifier = Lude.Nothing,
      date = Lude.Nothing,
      eventCategories = Lude.Nothing,
      message = Lude.Nothing
    }

-- | Specifies the source type for this event.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Lude.Maybe SourceType)
eSourceType = Lens.lens (sourceType :: Event -> Lude.Maybe SourceType) (\s a -> s {sourceType = a} :: Event)
{-# DEPRECATED eSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The Amazon Resource Name (ARN) for the event.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceARN :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eSourceARN = Lens.lens (sourceARN :: Event -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: Event)
{-# DEPRECATED eSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | Provides the identifier for the source of the event.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceIdentifier :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eSourceIdentifier = Lens.lens (sourceIdentifier :: Event -> Lude.Maybe Lude.Text) (\s a -> s {sourceIdentifier = a} :: Event)
{-# DEPRECATED eSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | Specifies the date and time of the event.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Lude.Maybe Lude.DateTime)
eDate = Lens.lens (date :: Event -> Lude.Maybe Lude.DateTime) (\s a -> s {date = a} :: Event)
{-# DEPRECATED eDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | Specifies the category for the event.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventCategories :: Lens.Lens' Event (Lude.Maybe [Lude.Text])
eEventCategories = Lens.lens (eventCategories :: Event -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategories = a} :: Event)
{-# DEPRECATED eEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | Provides the text of this event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eMessage = Lens.lens (message :: Event -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Event)
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML Event where
  parseXML x =
    Event'
      Lude.<$> (x Lude..@? "SourceType")
      Lude.<*> (x Lude..@? "SourceArn")
      Lude.<*> (x Lude..@? "SourceIdentifier")
      Lude.<*> (x Lude..@? "Date")
      Lude.<*> ( x Lude..@? "EventCategories" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EventCategory")
               )
      Lude.<*> (x Lude..@? "Message")
