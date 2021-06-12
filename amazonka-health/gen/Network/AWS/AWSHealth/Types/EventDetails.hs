{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventDetails where

import Network.AWS.AWSHealth.Types.Event
import Network.AWS.AWSHealth.Types.EventDescription
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Detailed information about an event. A combination of an
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event>
-- object, an
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EventDescription.html EventDescription>
-- object, and additional metadata about the event. Returned by the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails>
-- operation.
--
-- /See:/ 'newEventDetails' smart constructor.
data EventDetails = EventDetails'
  { -- | Additional metadata about the event.
    eventMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The most recent description of the event.
    eventDescription :: Core.Maybe EventDescription,
    -- | Summary information about the event.
    event :: Core.Maybe Event
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventMetadata', 'eventDetails_eventMetadata' - Additional metadata about the event.
--
-- 'eventDescription', 'eventDetails_eventDescription' - The most recent description of the event.
--
-- 'event', 'eventDetails_event' - Summary information about the event.
newEventDetails ::
  EventDetails
newEventDetails =
  EventDetails'
    { eventMetadata = Core.Nothing,
      eventDescription = Core.Nothing,
      event = Core.Nothing
    }

-- | Additional metadata about the event.
eventDetails_eventMetadata :: Lens.Lens' EventDetails (Core.Maybe (Core.HashMap Core.Text Core.Text))
eventDetails_eventMetadata = Lens.lens (\EventDetails' {eventMetadata} -> eventMetadata) (\s@EventDetails' {} a -> s {eventMetadata = a} :: EventDetails) Core.. Lens.mapping Lens._Coerce

-- | The most recent description of the event.
eventDetails_eventDescription :: Lens.Lens' EventDetails (Core.Maybe EventDescription)
eventDetails_eventDescription = Lens.lens (\EventDetails' {eventDescription} -> eventDescription) (\s@EventDetails' {} a -> s {eventDescription = a} :: EventDetails)

-- | Summary information about the event.
eventDetails_event :: Lens.Lens' EventDetails (Core.Maybe Event)
eventDetails_event = Lens.lens (\EventDetails' {event} -> event) (\s@EventDetails' {} a -> s {event = a} :: EventDetails)

instance Core.FromJSON EventDetails where
  parseJSON =
    Core.withObject
      "EventDetails"
      ( \x ->
          EventDetails'
            Core.<$> (x Core..:? "eventMetadata" Core..!= Core.mempty)
            Core.<*> (x Core..:? "eventDescription")
            Core.<*> (x Core..:? "event")
      )

instance Core.Hashable EventDetails

instance Core.NFData EventDetails
