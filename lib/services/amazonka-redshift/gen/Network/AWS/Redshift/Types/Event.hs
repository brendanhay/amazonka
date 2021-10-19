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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SourceType

-- | Describes an event.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The source type for this event.
    sourceType :: Prelude.Maybe SourceType,
    -- | The severity of the event.
    --
    -- Values: ERROR, INFO
    severity :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the source of the event.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the event.
    date :: Prelude.Maybe Core.ISO8601,
    -- | A list of the event categories.
    --
    -- Values: Configuration, Management, Monitoring, Security, Pending
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The text of this event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the event.
    eventId :: Prelude.Maybe Prelude.Text
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
-- 'sourceType', 'event_sourceType' - The source type for this event.
--
-- 'severity', 'event_severity' - The severity of the event.
--
-- Values: ERROR, INFO
--
-- 'sourceIdentifier', 'event_sourceIdentifier' - The identifier for the source of the event.
--
-- 'date', 'event_date' - The date and time of the event.
--
-- 'eventCategories', 'event_eventCategories' - A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security, Pending
--
-- 'message', 'event_message' - The text of this event.
--
-- 'eventId', 'event_eventId' - The identifier of the event.
newEvent ::
  Event
newEvent =
  Event'
    { sourceType = Prelude.Nothing,
      severity = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      date = Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      message = Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | The source type for this event.
event_sourceType :: Lens.Lens' Event (Prelude.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

-- | The severity of the event.
--
-- Values: ERROR, INFO
event_severity :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_severity = Lens.lens (\Event' {severity} -> severity) (\s@Event' {} a -> s {severity = a} :: Event)

-- | The identifier for the source of the event.
event_sourceIdentifier :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceIdentifier = Lens.lens (\Event' {sourceIdentifier} -> sourceIdentifier) (\s@Event' {} a -> s {sourceIdentifier = a} :: Event)

-- | The date and time of the event.
event_date :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Prelude.. Lens.mapping Core._Time

-- | A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security, Pending
event_eventCategories :: Lens.Lens' Event (Prelude.Maybe [Prelude.Text])
event_eventCategories = Lens.lens (\Event' {eventCategories} -> eventCategories) (\s@Event' {} a -> s {eventCategories = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | The text of this event.
event_message :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | The identifier of the event.
event_eventId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventId = Lens.lens (\Event' {eventId} -> eventId) (\s@Event' {} a -> s {eventId = a} :: Event)

instance Core.FromXML Event where
  parseXML x =
    Event'
      Prelude.<$> (x Core..@? "SourceType")
      Prelude.<*> (x Core..@? "Severity")
      Prelude.<*> (x Core..@? "SourceIdentifier")
      Prelude.<*> (x Core..@? "Date")
      Prelude.<*> ( x Core..@? "EventCategories" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "EventCategory")
                  )
      Prelude.<*> (x Core..@? "Message")
      Prelude.<*> (x Core..@? "EventId")

instance Prelude.Hashable Event

instance Prelude.NFData Event
