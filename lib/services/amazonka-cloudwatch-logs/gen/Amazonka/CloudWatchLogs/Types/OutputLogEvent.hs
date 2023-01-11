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
-- Module      : Amazonka.CloudWatchLogs.Types.OutputLogEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.OutputLogEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a log event.
--
-- /See:/ 'newOutputLogEvent' smart constructor.
data OutputLogEvent = OutputLogEvent'
  { -- | The time the event was ingested, expressed as the number of milliseconds
    -- after @Jan 1, 1970 00:00:00 UTC@.
    ingestionTime :: Prelude.Maybe Prelude.Natural,
    -- | The data contained in the log event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time the event occurred, expressed as the number of milliseconds
    -- after @Jan 1, 1970 00:00:00 UTC@.
    timestamp :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputLogEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestionTime', 'outputLogEvent_ingestionTime' - The time the event was ingested, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@.
--
-- 'message', 'outputLogEvent_message' - The data contained in the log event.
--
-- 'timestamp', 'outputLogEvent_timestamp' - The time the event occurred, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@.
newOutputLogEvent ::
  OutputLogEvent
newOutputLogEvent =
  OutputLogEvent'
    { ingestionTime = Prelude.Nothing,
      message = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The time the event was ingested, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@.
outputLogEvent_ingestionTime :: Lens.Lens' OutputLogEvent (Prelude.Maybe Prelude.Natural)
outputLogEvent_ingestionTime = Lens.lens (\OutputLogEvent' {ingestionTime} -> ingestionTime) (\s@OutputLogEvent' {} a -> s {ingestionTime = a} :: OutputLogEvent)

-- | The data contained in the log event.
outputLogEvent_message :: Lens.Lens' OutputLogEvent (Prelude.Maybe Prelude.Text)
outputLogEvent_message = Lens.lens (\OutputLogEvent' {message} -> message) (\s@OutputLogEvent' {} a -> s {message = a} :: OutputLogEvent)

-- | The time the event occurred, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@.
outputLogEvent_timestamp :: Lens.Lens' OutputLogEvent (Prelude.Maybe Prelude.Natural)
outputLogEvent_timestamp = Lens.lens (\OutputLogEvent' {timestamp} -> timestamp) (\s@OutputLogEvent' {} a -> s {timestamp = a} :: OutputLogEvent)

instance Data.FromJSON OutputLogEvent where
  parseJSON =
    Data.withObject
      "OutputLogEvent"
      ( \x ->
          OutputLogEvent'
            Prelude.<$> (x Data..:? "ingestionTime")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "timestamp")
      )

instance Prelude.Hashable OutputLogEvent where
  hashWithSalt _salt OutputLogEvent' {..} =
    _salt `Prelude.hashWithSalt` ingestionTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData OutputLogEvent where
  rnf OutputLogEvent' {..} =
    Prelude.rnf ingestionTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf timestamp
