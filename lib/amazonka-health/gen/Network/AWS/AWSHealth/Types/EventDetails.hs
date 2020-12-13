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

import Network.AWS.AWSHealth.Types.Event
import Network.AWS.AWSHealth.Types.EventDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detailed information about an event. A combination of an <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> object, an <https://docs.aws.amazon.com/health/latest/APIReference/API_EventDescription.html EventDescription> object, and additional metadata about the event. Returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails> operation.
--
-- /See:/ 'mkEventDetails' smart constructor.
data EventDetails = EventDetails'
  { -- | Summary information about the event.
    event :: Lude.Maybe Event,
    -- | The most recent description of the event.
    eventDescription :: Lude.Maybe EventDescription,
    -- | Additional metadata about the event.
    eventMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventDetails' with the minimum fields required to make a request.
--
-- * 'event' - Summary information about the event.
-- * 'eventDescription' - The most recent description of the event.
-- * 'eventMetadata' - Additional metadata about the event.
mkEventDetails ::
  EventDetails
mkEventDetails =
  EventDetails'
    { event = Lude.Nothing,
      eventDescription = Lude.Nothing,
      eventMetadata = Lude.Nothing
    }

-- | Summary information about the event.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEvent :: Lens.Lens' EventDetails (Lude.Maybe Event)
edEvent = Lens.lens (event :: EventDetails -> Lude.Maybe Event) (\s a -> s {event = a} :: EventDetails)
{-# DEPRECATED edEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | The most recent description of the event.
--
-- /Note:/ Consider using 'eventDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEventDescription :: Lens.Lens' EventDetails (Lude.Maybe EventDescription)
edEventDescription = Lens.lens (eventDescription :: EventDetails -> Lude.Maybe EventDescription) (\s a -> s {eventDescription = a} :: EventDetails)
{-# DEPRECATED edEventDescription "Use generic-lens or generic-optics with 'eventDescription' instead." #-}

-- | Additional metadata about the event.
--
-- /Note:/ Consider using 'eventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEventMetadata :: Lens.Lens' EventDetails (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
edEventMetadata = Lens.lens (eventMetadata :: EventDetails -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {eventMetadata = a} :: EventDetails)
{-# DEPRECATED edEventMetadata "Use generic-lens or generic-optics with 'eventMetadata' instead." #-}

instance Lude.FromJSON EventDetails where
  parseJSON =
    Lude.withObject
      "EventDetails"
      ( \x ->
          EventDetails'
            Lude.<$> (x Lude..:? "event")
            Lude.<*> (x Lude..:? "eventDescription")
            Lude.<*> (x Lude..:? "eventMetadata" Lude..!= Lude.mempty)
      )
