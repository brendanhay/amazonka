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
-- Module      : Amazonka.FraudDetector.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.Event where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types.Entity
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The event details.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The timestamp that defines when the event under evaluation occurred. The
    -- timestamp must be specified using ISO 8601 standard in UTC.
    eventTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The event entities.
    entities :: Prelude.Maybe [Core.Sensitive Entity],
    -- | The timestamp associated with the label to update. The timestamp must be
    -- specified using ISO 8601 standard in UTC.
    labelTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The event type.
    eventTypeName :: Prelude.Maybe Prelude.Text,
    -- | Names of the event type\'s variables you defined in Amazon Fraud
    -- Detector to represent data elements and their corresponding values for
    -- the event you are sending for evaluation.
    eventVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Core.Sensitive Prelude.Text)),
    -- | The label associated with the event.
    currentLabel :: Prelude.Maybe Prelude.Text,
    -- | The event ID.
    eventId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTimestamp', 'event_eventTimestamp' - The timestamp that defines when the event under evaluation occurred. The
-- timestamp must be specified using ISO 8601 standard in UTC.
--
-- 'entities', 'event_entities' - The event entities.
--
-- 'labelTimestamp', 'event_labelTimestamp' - The timestamp associated with the label to update. The timestamp must be
-- specified using ISO 8601 standard in UTC.
--
-- 'eventTypeName', 'event_eventTypeName' - The event type.
--
-- 'eventVariables', 'event_eventVariables' - Names of the event type\'s variables you defined in Amazon Fraud
-- Detector to represent data elements and their corresponding values for
-- the event you are sending for evaluation.
--
-- 'currentLabel', 'event_currentLabel' - The label associated with the event.
--
-- 'eventId', 'event_eventId' - The event ID.
newEvent ::
  Event
newEvent =
  Event'
    { eventTimestamp = Prelude.Nothing,
      entities = Prelude.Nothing,
      labelTimestamp = Prelude.Nothing,
      eventTypeName = Prelude.Nothing,
      eventVariables = Prelude.Nothing,
      currentLabel = Prelude.Nothing,
      eventId = Prelude.Nothing
    }

-- | The timestamp that defines when the event under evaluation occurred. The
-- timestamp must be specified using ISO 8601 standard in UTC.
event_eventTimestamp :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventTimestamp = Lens.lens (\Event' {eventTimestamp} -> eventTimestamp) (\s@Event' {} a -> s {eventTimestamp = a} :: Event)

-- | The event entities.
event_entities :: Lens.Lens' Event (Prelude.Maybe [Entity])
event_entities = Lens.lens (\Event' {entities} -> entities) (\s@Event' {} a -> s {entities = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp associated with the label to update. The timestamp must be
-- specified using ISO 8601 standard in UTC.
event_labelTimestamp :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_labelTimestamp = Lens.lens (\Event' {labelTimestamp} -> labelTimestamp) (\s@Event' {} a -> s {labelTimestamp = a} :: Event)

-- | The event type.
event_eventTypeName :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventTypeName = Lens.lens (\Event' {eventTypeName} -> eventTypeName) (\s@Event' {} a -> s {eventTypeName = a} :: Event)

-- | Names of the event type\'s variables you defined in Amazon Fraud
-- Detector to represent data elements and their corresponding values for
-- the event you are sending for evaluation.
event_eventVariables :: Lens.Lens' Event (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
event_eventVariables = Lens.lens (\Event' {eventVariables} -> eventVariables) (\s@Event' {} a -> s {eventVariables = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | The label associated with the event.
event_currentLabel :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_currentLabel = Lens.lens (\Event' {currentLabel} -> currentLabel) (\s@Event' {} a -> s {currentLabel = a} :: Event)

-- | The event ID.
event_eventId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventId = Lens.lens (\Event' {eventId} -> eventId) (\s@Event' {} a -> s {eventId = a} :: Event)

instance Core.FromJSON Event where
  parseJSON =
    Core.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Core..:? "eventTimestamp")
            Prelude.<*> (x Core..:? "entities" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "labelTimestamp")
            Prelude.<*> (x Core..:? "eventTypeName")
            Prelude.<*> (x Core..:? "eventVariables" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "currentLabel")
            Prelude.<*> (x Core..:? "eventId")
      )

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt `Prelude.hashWithSalt` eventTimestamp
      `Prelude.hashWithSalt` entities
      `Prelude.hashWithSalt` labelTimestamp
      `Prelude.hashWithSalt` eventTypeName
      `Prelude.hashWithSalt` eventVariables
      `Prelude.hashWithSalt` currentLabel
      `Prelude.hashWithSalt` eventId

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf eventTimestamp
      `Prelude.seq` Prelude.rnf entities
      `Prelude.seq` Prelude.rnf labelTimestamp
      `Prelude.seq` Prelude.rnf eventTypeName
      `Prelude.seq` Prelude.rnf eventVariables
      `Prelude.seq` Prelude.rnf currentLabel
      `Prelude.seq` Prelude.rnf eventId
