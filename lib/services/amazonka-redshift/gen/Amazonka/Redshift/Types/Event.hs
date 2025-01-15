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
-- Module      : Amazonka.Redshift.Types.Event
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.SourceType

-- | Describes an event.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The date and time of the event.
    date :: Prelude.Maybe Data.ISO8601,
    -- | A list of the event categories.
    --
    -- Values: Configuration, Management, Monitoring, Security, Pending
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the event.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The text of this event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The severity of the event.
    --
    -- Values: ERROR, INFO
    severity :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the source of the event.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The source type for this event.
    sourceType :: Prelude.Maybe SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'date', 'event_date' - The date and time of the event.
--
-- 'eventCategories', 'event_eventCategories' - A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security, Pending
--
-- 'eventId', 'event_eventId' - The identifier of the event.
--
-- 'message', 'event_message' - The text of this event.
--
-- 'severity', 'event_severity' - The severity of the event.
--
-- Values: ERROR, INFO
--
-- 'sourceIdentifier', 'event_sourceIdentifier' - The identifier for the source of the event.
--
-- 'sourceType', 'event_sourceType' - The source type for this event.
newEvent ::
  Event
newEvent =
  Event'
    { date = Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      eventId = Prelude.Nothing,
      message = Prelude.Nothing,
      severity = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The date and time of the event.
event_date :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Prelude.. Lens.mapping Data._Time

-- | A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security, Pending
event_eventCategories :: Lens.Lens' Event (Prelude.Maybe [Prelude.Text])
event_eventCategories = Lens.lens (\Event' {eventCategories} -> eventCategories) (\s@Event' {} a -> s {eventCategories = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the event.
event_eventId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventId = Lens.lens (\Event' {eventId} -> eventId) (\s@Event' {} a -> s {eventId = a} :: Event)

-- | The text of this event.
event_message :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | The severity of the event.
--
-- Values: ERROR, INFO
event_severity :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_severity = Lens.lens (\Event' {severity} -> severity) (\s@Event' {} a -> s {severity = a} :: Event)

-- | The identifier for the source of the event.
event_sourceIdentifier :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceIdentifier = Lens.lens (\Event' {sourceIdentifier} -> sourceIdentifier) (\s@Event' {} a -> s {sourceIdentifier = a} :: Event)

-- | The source type for this event.
event_sourceType :: Lens.Lens' Event (Prelude.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

instance Data.FromXML Event where
  parseXML x =
    Event'
      Prelude.<$> (x Data..@? "Date")
      Prelude.<*> ( x Data..@? "EventCategories" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "EventCategory")
                  )
      Prelude.<*> (x Data..@? "EventId")
      Prelude.<*> (x Data..@? "Message")
      Prelude.<*> (x Data..@? "Severity")
      Prelude.<*> (x Data..@? "SourceIdentifier")
      Prelude.<*> (x Data..@? "SourceType")

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` eventCategories
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` sourceIdentifier
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf date `Prelude.seq`
      Prelude.rnf eventCategories `Prelude.seq`
        Prelude.rnf eventId `Prelude.seq`
          Prelude.rnf message `Prelude.seq`
            Prelude.rnf severity `Prelude.seq`
              Prelude.rnf sourceIdentifier `Prelude.seq`
                Prelude.rnf sourceType
