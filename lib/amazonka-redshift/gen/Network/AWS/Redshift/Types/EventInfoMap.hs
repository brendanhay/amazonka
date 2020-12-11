-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.EventInfoMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.EventInfoMap
  ( EventInfoMap (..),

    -- * Smart constructor
    mkEventInfoMap,

    -- * Lenses
    eimEventDescription,
    eimSeverity,
    eimEventCategories,
    eimEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes event information.
--
-- /See:/ 'mkEventInfoMap' smart constructor.
data EventInfoMap = EventInfoMap'
  { eventDescription ::
      Lude.Maybe Lude.Text,
    severity :: Lude.Maybe Lude.Text,
    eventCategories :: Lude.Maybe [Lude.Text],
    eventId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventInfoMap' with the minimum fields required to make a request.
--
-- * 'eventCategories' - The category of an Amazon Redshift event.
-- * 'eventDescription' - The description of an Amazon Redshift event.
-- * 'eventId' - The identifier of an Amazon Redshift event.
-- * 'severity' - The severity of the event.
--
-- Values: ERROR, INFO
mkEventInfoMap ::
  EventInfoMap
mkEventInfoMap =
  EventInfoMap'
    { eventDescription = Lude.Nothing,
      severity = Lude.Nothing,
      eventCategories = Lude.Nothing,
      eventId = Lude.Nothing
    }

-- | The description of an Amazon Redshift event.
--
-- /Note:/ Consider using 'eventDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eimEventDescription :: Lens.Lens' EventInfoMap (Lude.Maybe Lude.Text)
eimEventDescription = Lens.lens (eventDescription :: EventInfoMap -> Lude.Maybe Lude.Text) (\s a -> s {eventDescription = a} :: EventInfoMap)
{-# DEPRECATED eimEventDescription "Use generic-lens or generic-optics with 'eventDescription' instead." #-}

-- | The severity of the event.
--
-- Values: ERROR, INFO
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eimSeverity :: Lens.Lens' EventInfoMap (Lude.Maybe Lude.Text)
eimSeverity = Lens.lens (severity :: EventInfoMap -> Lude.Maybe Lude.Text) (\s a -> s {severity = a} :: EventInfoMap)
{-# DEPRECATED eimSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The category of an Amazon Redshift event.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eimEventCategories :: Lens.Lens' EventInfoMap (Lude.Maybe [Lude.Text])
eimEventCategories = Lens.lens (eventCategories :: EventInfoMap -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategories = a} :: EventInfoMap)
{-# DEPRECATED eimEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | The identifier of an Amazon Redshift event.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eimEventId :: Lens.Lens' EventInfoMap (Lude.Maybe Lude.Text)
eimEventId = Lens.lens (eventId :: EventInfoMap -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: EventInfoMap)
{-# DEPRECATED eimEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.FromXML EventInfoMap where
  parseXML x =
    EventInfoMap'
      Lude.<$> (x Lude..@? "EventDescription")
      Lude.<*> (x Lude..@? "Severity")
      Lude.<*> ( x Lude..@? "EventCategories" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EventCategory")
               )
      Lude.<*> (x Lude..@? "EventId")
