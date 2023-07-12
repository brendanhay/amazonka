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
-- Module      : Amazonka.DrS.Types.JobLog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.JobLog where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.JobLogEvent
import Amazonka.DrS.Types.JobLogEventData
import qualified Amazonka.Prelude as Prelude

-- | A log outputted by a Job.
--
-- /See:/ 'newJobLog' smart constructor.
data JobLog = JobLog'
  { -- | The event represents the type of a log.
    event :: Prelude.Maybe JobLogEvent,
    -- | Metadata associated with a Job log.
    eventData :: Prelude.Maybe JobLogEventData,
    -- | The date and time the log was taken.
    logDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'event', 'jobLog_event' - The event represents the type of a log.
--
-- 'eventData', 'jobLog_eventData' - Metadata associated with a Job log.
--
-- 'logDateTime', 'jobLog_logDateTime' - The date and time the log was taken.
newJobLog ::
  JobLog
newJobLog =
  JobLog'
    { event = Prelude.Nothing,
      eventData = Prelude.Nothing,
      logDateTime = Prelude.Nothing
    }

-- | The event represents the type of a log.
jobLog_event :: Lens.Lens' JobLog (Prelude.Maybe JobLogEvent)
jobLog_event = Lens.lens (\JobLog' {event} -> event) (\s@JobLog' {} a -> s {event = a} :: JobLog)

-- | Metadata associated with a Job log.
jobLog_eventData :: Lens.Lens' JobLog (Prelude.Maybe JobLogEventData)
jobLog_eventData = Lens.lens (\JobLog' {eventData} -> eventData) (\s@JobLog' {} a -> s {eventData = a} :: JobLog)

-- | The date and time the log was taken.
jobLog_logDateTime :: Lens.Lens' JobLog (Prelude.Maybe Prelude.Text)
jobLog_logDateTime = Lens.lens (\JobLog' {logDateTime} -> logDateTime) (\s@JobLog' {} a -> s {logDateTime = a} :: JobLog)

instance Data.FromJSON JobLog where
  parseJSON =
    Data.withObject
      "JobLog"
      ( \x ->
          JobLog'
            Prelude.<$> (x Data..:? "event")
            Prelude.<*> (x Data..:? "eventData")
            Prelude.<*> (x Data..:? "logDateTime")
      )

instance Prelude.Hashable JobLog where
  hashWithSalt _salt JobLog' {..} =
    _salt
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` eventData
      `Prelude.hashWithSalt` logDateTime

instance Prelude.NFData JobLog where
  rnf JobLog' {..} =
    Prelude.rnf event
      `Prelude.seq` Prelude.rnf eventData
      `Prelude.seq` Prelude.rnf logDateTime
