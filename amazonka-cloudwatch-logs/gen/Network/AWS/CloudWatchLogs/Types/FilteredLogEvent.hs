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
-- Module      : Network.AWS.CloudWatchLogs.Types.FilteredLogEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.FilteredLogEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a matched event.
--
-- /See:/ 'newFilteredLogEvent' smart constructor.
data FilteredLogEvent = FilteredLogEvent'
  { -- | The name of the log stream to which this event belongs.
    logStreamName :: Core.Maybe Core.Text,
    -- | The ID of the event.
    eventId :: Core.Maybe Core.Text,
    -- | The data contained in the log event.
    message :: Core.Maybe Core.Text,
    -- | The time the event was ingested, expressed as the number of milliseconds
    -- after Jan 1, 1970 00:00:00 UTC.
    ingestionTime :: Core.Maybe Core.Natural,
    -- | The time the event occurred, expressed as the number of milliseconds
    -- after Jan 1, 1970 00:00:00 UTC.
    timestamp :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FilteredLogEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamName', 'filteredLogEvent_logStreamName' - The name of the log stream to which this event belongs.
--
-- 'eventId', 'filteredLogEvent_eventId' - The ID of the event.
--
-- 'message', 'filteredLogEvent_message' - The data contained in the log event.
--
-- 'ingestionTime', 'filteredLogEvent_ingestionTime' - The time the event was ingested, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
--
-- 'timestamp', 'filteredLogEvent_timestamp' - The time the event occurred, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
newFilteredLogEvent ::
  FilteredLogEvent
newFilteredLogEvent =
  FilteredLogEvent'
    { logStreamName = Core.Nothing,
      eventId = Core.Nothing,
      message = Core.Nothing,
      ingestionTime = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | The name of the log stream to which this event belongs.
filteredLogEvent_logStreamName :: Lens.Lens' FilteredLogEvent (Core.Maybe Core.Text)
filteredLogEvent_logStreamName = Lens.lens (\FilteredLogEvent' {logStreamName} -> logStreamName) (\s@FilteredLogEvent' {} a -> s {logStreamName = a} :: FilteredLogEvent)

-- | The ID of the event.
filteredLogEvent_eventId :: Lens.Lens' FilteredLogEvent (Core.Maybe Core.Text)
filteredLogEvent_eventId = Lens.lens (\FilteredLogEvent' {eventId} -> eventId) (\s@FilteredLogEvent' {} a -> s {eventId = a} :: FilteredLogEvent)

-- | The data contained in the log event.
filteredLogEvent_message :: Lens.Lens' FilteredLogEvent (Core.Maybe Core.Text)
filteredLogEvent_message = Lens.lens (\FilteredLogEvent' {message} -> message) (\s@FilteredLogEvent' {} a -> s {message = a} :: FilteredLogEvent)

-- | The time the event was ingested, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
filteredLogEvent_ingestionTime :: Lens.Lens' FilteredLogEvent (Core.Maybe Core.Natural)
filteredLogEvent_ingestionTime = Lens.lens (\FilteredLogEvent' {ingestionTime} -> ingestionTime) (\s@FilteredLogEvent' {} a -> s {ingestionTime = a} :: FilteredLogEvent)

-- | The time the event occurred, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
filteredLogEvent_timestamp :: Lens.Lens' FilteredLogEvent (Core.Maybe Core.Natural)
filteredLogEvent_timestamp = Lens.lens (\FilteredLogEvent' {timestamp} -> timestamp) (\s@FilteredLogEvent' {} a -> s {timestamp = a} :: FilteredLogEvent)

instance Core.FromJSON FilteredLogEvent where
  parseJSON =
    Core.withObject
      "FilteredLogEvent"
      ( \x ->
          FilteredLogEvent'
            Core.<$> (x Core..:? "logStreamName")
            Core.<*> (x Core..:? "eventId")
            Core.<*> (x Core..:? "message")
            Core.<*> (x Core..:? "ingestionTime")
            Core.<*> (x Core..:? "timestamp")
      )

instance Core.Hashable FilteredLogEvent

instance Core.NFData FilteredLogEvent
