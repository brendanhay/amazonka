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
-- Module      : Amazonka.AWSHealth.Types.EventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EventDetails where

import Amazonka.AWSHealth.Types.Event
import Amazonka.AWSHealth.Types.EventDescription
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | Summary information about the event.
    event :: Prelude.Maybe Event,
    -- | The most recent description of the event.
    eventDescription :: Prelude.Maybe EventDescription,
    -- | Additional metadata about the event.
    eventMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'event', 'eventDetails_event' - Summary information about the event.
--
-- 'eventDescription', 'eventDetails_eventDescription' - The most recent description of the event.
--
-- 'eventMetadata', 'eventDetails_eventMetadata' - Additional metadata about the event.
newEventDetails ::
  EventDetails
newEventDetails =
  EventDetails'
    { event = Prelude.Nothing,
      eventDescription = Prelude.Nothing,
      eventMetadata = Prelude.Nothing
    }

-- | Summary information about the event.
eventDetails_event :: Lens.Lens' EventDetails (Prelude.Maybe Event)
eventDetails_event = Lens.lens (\EventDetails' {event} -> event) (\s@EventDetails' {} a -> s {event = a} :: EventDetails)

-- | The most recent description of the event.
eventDetails_eventDescription :: Lens.Lens' EventDetails (Prelude.Maybe EventDescription)
eventDetails_eventDescription = Lens.lens (\EventDetails' {eventDescription} -> eventDescription) (\s@EventDetails' {} a -> s {eventDescription = a} :: EventDetails)

-- | Additional metadata about the event.
eventDetails_eventMetadata :: Lens.Lens' EventDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
eventDetails_eventMetadata = Lens.lens (\EventDetails' {eventMetadata} -> eventMetadata) (\s@EventDetails' {} a -> s {eventMetadata = a} :: EventDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EventDetails where
  parseJSON =
    Data.withObject
      "EventDetails"
      ( \x ->
          EventDetails'
            Prelude.<$> (x Data..:? "event")
            Prelude.<*> (x Data..:? "eventDescription")
            Prelude.<*> (x Data..:? "eventMetadata" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EventDetails where
  hashWithSalt _salt EventDetails' {..} =
    _salt `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` eventDescription
      `Prelude.hashWithSalt` eventMetadata

instance Prelude.NFData EventDetails where
  rnf EventDetails' {..} =
    Prelude.rnf event
      `Prelude.seq` Prelude.rnf eventDescription
      `Prelude.seq` Prelude.rnf eventMetadata
