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
-- Module      : Amazonka.DMS.Types.Event
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.SourceType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an identifiable significant activity that affects a
-- replication instance or task. This object can provide the message, the
-- available event categories, the date and source of the event, and the
-- DMS resource type.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The date of the event.
    date :: Prelude.Maybe Data.POSIX,
    -- | The event categories available for the specified source type.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The event message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The identifier of an event source.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The type of DMS resource that generates events.
    --
    -- Valid values: replication-instance | endpoint | replication-task
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
-- 'date', 'event_date' - The date of the event.
--
-- 'eventCategories', 'event_eventCategories' - The event categories available for the specified source type.
--
-- 'message', 'event_message' - The event message.
--
-- 'sourceIdentifier', 'event_sourceIdentifier' - The identifier of an event source.
--
-- 'sourceType', 'event_sourceType' - The type of DMS resource that generates events.
--
-- Valid values: replication-instance | endpoint | replication-task
newEvent ::
  Event
newEvent =
  Event'
    { date = Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      message = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The date of the event.
event_date :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Prelude.. Lens.mapping Data._Time

-- | The event categories available for the specified source type.
event_eventCategories :: Lens.Lens' Event (Prelude.Maybe [Prelude.Text])
event_eventCategories = Lens.lens (\Event' {eventCategories} -> eventCategories) (\s@Event' {} a -> s {eventCategories = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | The event message.
event_message :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | The identifier of an event source.
event_sourceIdentifier :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceIdentifier = Lens.lens (\Event' {sourceIdentifier} -> sourceIdentifier) (\s@Event' {} a -> s {sourceIdentifier = a} :: Event)

-- | The type of DMS resource that generates events.
--
-- Valid values: replication-instance | endpoint | replication-task
event_sourceType :: Lens.Lens' Event (Prelude.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

instance Data.FromJSON Event where
  parseJSON =
    Data.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Data..:? "Date")
            Prelude.<*> ( x
                            Data..:? "EventCategories"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "SourceIdentifier")
            Prelude.<*> (x Data..:? "SourceType")
      )

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` eventCategories
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` sourceIdentifier
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf date
      `Prelude.seq` Prelude.rnf eventCategories
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf sourceIdentifier
      `Prelude.seq` Prelude.rnf sourceType
