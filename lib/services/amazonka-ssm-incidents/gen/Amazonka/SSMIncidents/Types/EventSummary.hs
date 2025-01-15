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
-- Module      : Amazonka.SSMIncidents.Types.EventSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.EventSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.EventReference

-- | Details about a timeline event during an incident.
--
-- /See:/ 'newEventSummary' smart constructor.
data EventSummary = EventSummary'
  { -- | A list of references in a @TimelineEvent@.
    eventReferences :: Prelude.Maybe [EventReference],
    -- | The timeline event ID.
    eventId :: Prelude.Text,
    -- | The time that the event occurred.
    eventTime :: Data.POSIX,
    -- | The type of event. The timeline event must be @Custom Event@.
    eventType :: Prelude.Text,
    -- | The time that the timeline event was last updated.
    eventUpdatedTime :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the incident that the event happened
    -- during.
    incidentRecordArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventReferences', 'eventSummary_eventReferences' - A list of references in a @TimelineEvent@.
--
-- 'eventId', 'eventSummary_eventId' - The timeline event ID.
--
-- 'eventTime', 'eventSummary_eventTime' - The time that the event occurred.
--
-- 'eventType', 'eventSummary_eventType' - The type of event. The timeline event must be @Custom Event@.
--
-- 'eventUpdatedTime', 'eventSummary_eventUpdatedTime' - The time that the timeline event was last updated.
--
-- 'incidentRecordArn', 'eventSummary_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident that the event happened
-- during.
newEventSummary ::
  -- | 'eventId'
  Prelude.Text ->
  -- | 'eventTime'
  Prelude.UTCTime ->
  -- | 'eventType'
  Prelude.Text ->
  -- | 'eventUpdatedTime'
  Prelude.UTCTime ->
  -- | 'incidentRecordArn'
  Prelude.Text ->
  EventSummary
newEventSummary
  pEventId_
  pEventTime_
  pEventType_
  pEventUpdatedTime_
  pIncidentRecordArn_ =
    EventSummary'
      { eventReferences = Prelude.Nothing,
        eventId = pEventId_,
        eventTime = Data._Time Lens.# pEventTime_,
        eventType = pEventType_,
        eventUpdatedTime =
          Data._Time Lens.# pEventUpdatedTime_,
        incidentRecordArn = pIncidentRecordArn_
      }

-- | A list of references in a @TimelineEvent@.
eventSummary_eventReferences :: Lens.Lens' EventSummary (Prelude.Maybe [EventReference])
eventSummary_eventReferences = Lens.lens (\EventSummary' {eventReferences} -> eventReferences) (\s@EventSummary' {} a -> s {eventReferences = a} :: EventSummary) Prelude.. Lens.mapping Lens.coerced

-- | The timeline event ID.
eventSummary_eventId :: Lens.Lens' EventSummary Prelude.Text
eventSummary_eventId = Lens.lens (\EventSummary' {eventId} -> eventId) (\s@EventSummary' {} a -> s {eventId = a} :: EventSummary)

-- | The time that the event occurred.
eventSummary_eventTime :: Lens.Lens' EventSummary Prelude.UTCTime
eventSummary_eventTime = Lens.lens (\EventSummary' {eventTime} -> eventTime) (\s@EventSummary' {} a -> s {eventTime = a} :: EventSummary) Prelude.. Data._Time

-- | The type of event. The timeline event must be @Custom Event@.
eventSummary_eventType :: Lens.Lens' EventSummary Prelude.Text
eventSummary_eventType = Lens.lens (\EventSummary' {eventType} -> eventType) (\s@EventSummary' {} a -> s {eventType = a} :: EventSummary)

-- | The time that the timeline event was last updated.
eventSummary_eventUpdatedTime :: Lens.Lens' EventSummary Prelude.UTCTime
eventSummary_eventUpdatedTime = Lens.lens (\EventSummary' {eventUpdatedTime} -> eventUpdatedTime) (\s@EventSummary' {} a -> s {eventUpdatedTime = a} :: EventSummary) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the incident that the event happened
-- during.
eventSummary_incidentRecordArn :: Lens.Lens' EventSummary Prelude.Text
eventSummary_incidentRecordArn = Lens.lens (\EventSummary' {incidentRecordArn} -> incidentRecordArn) (\s@EventSummary' {} a -> s {incidentRecordArn = a} :: EventSummary)

instance Data.FromJSON EventSummary where
  parseJSON =
    Data.withObject
      "EventSummary"
      ( \x ->
          EventSummary'
            Prelude.<$> ( x
                            Data..:? "eventReferences"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "eventId")
            Prelude.<*> (x Data..: "eventTime")
            Prelude.<*> (x Data..: "eventType")
            Prelude.<*> (x Data..: "eventUpdatedTime")
            Prelude.<*> (x Data..: "incidentRecordArn")
      )

instance Prelude.Hashable EventSummary where
  hashWithSalt _salt EventSummary' {..} =
    _salt
      `Prelude.hashWithSalt` eventReferences
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` eventUpdatedTime
      `Prelude.hashWithSalt` incidentRecordArn

instance Prelude.NFData EventSummary where
  rnf EventSummary' {..} =
    Prelude.rnf eventReferences `Prelude.seq`
      Prelude.rnf eventId `Prelude.seq`
        Prelude.rnf eventTime `Prelude.seq`
          Prelude.rnf eventType `Prelude.seq`
            Prelude.rnf eventUpdatedTime `Prelude.seq`
              Prelude.rnf incidentRecordArn
