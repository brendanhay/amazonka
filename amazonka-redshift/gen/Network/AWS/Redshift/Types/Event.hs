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
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SourceType

-- | Describes an event.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The identifier of the event.
    eventId :: Core.Maybe Core.Text,
    -- | The severity of the event.
    --
    -- Values: ERROR, INFO
    severity :: Core.Maybe Core.Text,
    -- | The text of this event.
    message :: Core.Maybe Core.Text,
    -- | A list of the event categories.
    --
    -- Values: Configuration, Management, Monitoring, Security
    eventCategories :: Core.Maybe [Core.Text],
    -- | The date and time of the event.
    date :: Core.Maybe Core.ISO8601,
    -- | The identifier for the source of the event.
    sourceIdentifier :: Core.Maybe Core.Text,
    -- | The source type for this event.
    sourceType :: Core.Maybe SourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { eventId = Core.Nothing,
      severity = Core.Nothing,
      message = Core.Nothing,
      eventCategories = Core.Nothing,
      date = Core.Nothing,
      sourceIdentifier = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | The identifier of the event.
event_eventId :: Lens.Lens' Event (Core.Maybe Core.Text)
event_eventId = Lens.lens (\Event' {eventId} -> eventId) (\s@Event' {} a -> s {eventId = a} :: Event)

-- | The severity of the event.
--
-- Values: ERROR, INFO
event_severity :: Lens.Lens' Event (Core.Maybe Core.Text)
event_severity = Lens.lens (\Event' {severity} -> severity) (\s@Event' {} a -> s {severity = a} :: Event)

-- | The text of this event.
event_message :: Lens.Lens' Event (Core.Maybe Core.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | A list of the event categories.
--
-- Values: Configuration, Management, Monitoring, Security
event_eventCategories :: Lens.Lens' Event (Core.Maybe [Core.Text])
event_eventCategories = Lens.lens (\Event' {eventCategories} -> eventCategories) (\s@Event' {} a -> s {eventCategories = a} :: Event) Core.. Lens.mapping Lens._Coerce

-- | The date and time of the event.
event_date :: Lens.Lens' Event (Core.Maybe Core.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Core.. Lens.mapping Core._Time

-- | The identifier for the source of the event.
event_sourceIdentifier :: Lens.Lens' Event (Core.Maybe Core.Text)
event_sourceIdentifier = Lens.lens (\Event' {sourceIdentifier} -> sourceIdentifier) (\s@Event' {} a -> s {sourceIdentifier = a} :: Event)

-- | The source type for this event.
event_sourceType :: Lens.Lens' Event (Core.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

instance Core.FromXML Event where
  parseXML x =
    Event'
      Core.<$> (x Core..@? "EventId")
      Core.<*> (x Core..@? "Severity")
      Core.<*> (x Core..@? "Message")
      Core.<*> ( x Core..@? "EventCategories" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "EventCategory")
               )
      Core.<*> (x Core..@? "Date")
      Core.<*> (x Core..@? "SourceIdentifier")
      Core.<*> (x Core..@? "SourceType")

instance Core.Hashable Event

instance Core.NFData Event
