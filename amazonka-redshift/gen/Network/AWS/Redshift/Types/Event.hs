{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Redshift.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Event where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SourceType

-- | Describes an event.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The identifier of the event.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The severity of the event.
    --
    -- Values: ERROR, INFO
    severity :: Prelude.Maybe Prelude.Text,
    -- | The text of this event.
    message :: Prelude.Maybe Prelude.Text,
    -- | A list of the event categories.
    --
    -- Values: Configuration, Management, Monitoring, Security
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The date and time of the event.
    date :: Prelude.Maybe Prelude.ISO8601,
    -- | The identifier for the source of the event.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The source type for this event.
    sourceType :: Prelude.Maybe SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventId', 'event_eventId' - The identifier of the event.
--
-- 'severity', 'event_severity' - The severity of the event.
--
-- Values: ERROR, INFO
--
-- 'message', 'event_message' - The text of this event.
--
-- 'eventCategories', 'event_eventCategories' - A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security
--
-- 'date', 'event_date' - The date and time of the event.
--
-- 'sourceIdentifier', 'event_sourceIdentifier' - The identifier for the source of the event.
--
-- 'sourceType', 'event_sourceType' - The source type for this event.
newEvent ::
  Event
newEvent =
  Event'
    { eventId = Prelude.Nothing,
      severity = Prelude.Nothing,
      message = Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      date = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The identifier of the event.
event_eventId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventId = Lens.lens (\Event' {eventId} -> eventId) (\s@Event' {} a -> s {eventId = a} :: Event)

-- | The severity of the event.
--
-- Values: ERROR, INFO
event_severity :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_severity = Lens.lens (\Event' {severity} -> severity) (\s@Event' {} a -> s {severity = a} :: Event)

-- | The text of this event.
event_message :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security
event_eventCategories :: Lens.Lens' Event (Prelude.Maybe [Prelude.Text])
event_eventCategories = Lens.lens (\Event' {eventCategories} -> eventCategories) (\s@Event' {} a -> s {eventCategories = a} :: Event) Prelude.. Lens.mapping Prelude._Coerce

-- | The date and time of the event.
event_date :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Prelude.. Lens.mapping Prelude._Time

-- | The identifier for the source of the event.
event_sourceIdentifier :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceIdentifier = Lens.lens (\Event' {sourceIdentifier} -> sourceIdentifier) (\s@Event' {} a -> s {sourceIdentifier = a} :: Event)

-- | The source type for this event.
event_sourceType :: Lens.Lens' Event (Prelude.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

instance Prelude.FromXML Event where
  parseXML x =
    Event'
      Prelude.<$> (x Prelude..@? "EventId")
      Prelude.<*> (x Prelude..@? "Severity")
      Prelude.<*> (x Prelude..@? "Message")
      Prelude.<*> ( x Prelude..@? "EventCategories"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "EventCategory")
                  )
      Prelude.<*> (x Prelude..@? "Date")
      Prelude.<*> (x Prelude..@? "SourceIdentifier")
      Prelude.<*> (x Prelude..@? "SourceType")

instance Prelude.Hashable Event

instance Prelude.NFData Event
