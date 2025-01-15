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
-- Module      : Amazonka.MGN.Types.JobLog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.JobLog where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.JobLogEvent
import Amazonka.MGN.Types.JobLogEventData
import qualified Amazonka.Prelude as Prelude

-- | Job log.
--
-- /See:/ 'newJobLog' smart constructor.
data JobLog = JobLog'
  { -- | Job log event.
    event :: Prelude.Maybe JobLogEvent,
    -- | Job event data
    eventData :: Prelude.Maybe JobLogEventData,
    -- | Job log event date and time.
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
-- 'event', 'jobLog_event' - Job log event.
--
-- 'eventData', 'jobLog_eventData' - Job event data
--
-- 'logDateTime', 'jobLog_logDateTime' - Job log event date and time.
newJobLog ::
  JobLog
newJobLog =
  JobLog'
    { event = Prelude.Nothing,
      eventData = Prelude.Nothing,
      logDateTime = Prelude.Nothing
    }

-- | Job log event.
jobLog_event :: Lens.Lens' JobLog (Prelude.Maybe JobLogEvent)
jobLog_event = Lens.lens (\JobLog' {event} -> event) (\s@JobLog' {} a -> s {event = a} :: JobLog)

-- | Job event data
jobLog_eventData :: Lens.Lens' JobLog (Prelude.Maybe JobLogEventData)
jobLog_eventData = Lens.lens (\JobLog' {eventData} -> eventData) (\s@JobLog' {} a -> s {eventData = a} :: JobLog)

-- | Job log event date and time.
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
    Prelude.rnf event `Prelude.seq`
      Prelude.rnf eventData `Prelude.seq`
        Prelude.rnf logDateTime
