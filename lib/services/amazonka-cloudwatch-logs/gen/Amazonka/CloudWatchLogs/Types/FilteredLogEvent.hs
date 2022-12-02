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
-- Module      : Amazonka.CloudWatchLogs.Types.FilteredLogEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.FilteredLogEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a matched event.
--
-- /See:/ 'newFilteredLogEvent' smart constructor.
data FilteredLogEvent = FilteredLogEvent'
  { -- | The data contained in the log event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time the event occurred, expressed as the number of milliseconds
    -- after Jan 1, 1970 00:00:00 UTC.
    timestamp :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the event.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | The time the event was ingested, expressed as the number of milliseconds
    -- after Jan 1, 1970 00:00:00 UTC.
    ingestionTime :: Prelude.Maybe Prelude.Natural,
    -- | The name of the log stream to which this event belongs.
    logStreamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilteredLogEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'filteredLogEvent_message' - The data contained in the log event.
--
-- 'timestamp', 'filteredLogEvent_timestamp' - The time the event occurred, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
--
-- 'eventId', 'filteredLogEvent_eventId' - The ID of the event.
--
-- 'ingestionTime', 'filteredLogEvent_ingestionTime' - The time the event was ingested, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
--
-- 'logStreamName', 'filteredLogEvent_logStreamName' - The name of the log stream to which this event belongs.
newFilteredLogEvent ::
  FilteredLogEvent
newFilteredLogEvent =
  FilteredLogEvent'
    { message = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      eventId = Prelude.Nothing,
      ingestionTime = Prelude.Nothing,
      logStreamName = Prelude.Nothing
    }

-- | The data contained in the log event.
filteredLogEvent_message :: Lens.Lens' FilteredLogEvent (Prelude.Maybe Prelude.Text)
filteredLogEvent_message = Lens.lens (\FilteredLogEvent' {message} -> message) (\s@FilteredLogEvent' {} a -> s {message = a} :: FilteredLogEvent)

-- | The time the event occurred, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
filteredLogEvent_timestamp :: Lens.Lens' FilteredLogEvent (Prelude.Maybe Prelude.Natural)
filteredLogEvent_timestamp = Lens.lens (\FilteredLogEvent' {timestamp} -> timestamp) (\s@FilteredLogEvent' {} a -> s {timestamp = a} :: FilteredLogEvent)

-- | The ID of the event.
filteredLogEvent_eventId :: Lens.Lens' FilteredLogEvent (Prelude.Maybe Prelude.Text)
filteredLogEvent_eventId = Lens.lens (\FilteredLogEvent' {eventId} -> eventId) (\s@FilteredLogEvent' {} a -> s {eventId = a} :: FilteredLogEvent)

-- | The time the event was ingested, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
filteredLogEvent_ingestionTime :: Lens.Lens' FilteredLogEvent (Prelude.Maybe Prelude.Natural)
filteredLogEvent_ingestionTime = Lens.lens (\FilteredLogEvent' {ingestionTime} -> ingestionTime) (\s@FilteredLogEvent' {} a -> s {ingestionTime = a} :: FilteredLogEvent)

-- | The name of the log stream to which this event belongs.
filteredLogEvent_logStreamName :: Lens.Lens' FilteredLogEvent (Prelude.Maybe Prelude.Text)
filteredLogEvent_logStreamName = Lens.lens (\FilteredLogEvent' {logStreamName} -> logStreamName) (\s@FilteredLogEvent' {} a -> s {logStreamName = a} :: FilteredLogEvent)

instance Data.FromJSON FilteredLogEvent where
  parseJSON =
    Data.withObject
      "FilteredLogEvent"
      ( \x ->
          FilteredLogEvent'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "timestamp")
            Prelude.<*> (x Data..:? "eventId")
            Prelude.<*> (x Data..:? "ingestionTime")
            Prelude.<*> (x Data..:? "logStreamName")
      )

instance Prelude.Hashable FilteredLogEvent where
  hashWithSalt _salt FilteredLogEvent' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` ingestionTime
      `Prelude.hashWithSalt` logStreamName

instance Prelude.NFData FilteredLogEvent where
  rnf FilteredLogEvent' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf ingestionTime
      `Prelude.seq` Prelude.rnf logStreamName
