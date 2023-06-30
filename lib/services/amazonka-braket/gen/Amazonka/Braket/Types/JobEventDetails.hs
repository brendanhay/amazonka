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
-- Module      : Amazonka.Braket.Types.JobEventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.JobEventDetails where

import Amazonka.Braket.Types.JobEventType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the type and time events occurred related to the Amazon
-- Braket job.
--
-- /See:/ 'newJobEventDetails' smart constructor.
data JobEventDetails = JobEventDetails'
  { -- | The type of event that occurred related to the Amazon Braket job.
    eventType :: Prelude.Maybe JobEventType,
    -- | A message describing the event that occurred related to the Amazon
    -- Braket job.
    message :: Prelude.Maybe Prelude.Text,
    -- | TThe type of event that occurred related to the Amazon Braket job.
    timeOfEvent :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventType', 'jobEventDetails_eventType' - The type of event that occurred related to the Amazon Braket job.
--
-- 'message', 'jobEventDetails_message' - A message describing the event that occurred related to the Amazon
-- Braket job.
--
-- 'timeOfEvent', 'jobEventDetails_timeOfEvent' - TThe type of event that occurred related to the Amazon Braket job.
newJobEventDetails ::
  JobEventDetails
newJobEventDetails =
  JobEventDetails'
    { eventType = Prelude.Nothing,
      message = Prelude.Nothing,
      timeOfEvent = Prelude.Nothing
    }

-- | The type of event that occurred related to the Amazon Braket job.
jobEventDetails_eventType :: Lens.Lens' JobEventDetails (Prelude.Maybe JobEventType)
jobEventDetails_eventType = Lens.lens (\JobEventDetails' {eventType} -> eventType) (\s@JobEventDetails' {} a -> s {eventType = a} :: JobEventDetails)

-- | A message describing the event that occurred related to the Amazon
-- Braket job.
jobEventDetails_message :: Lens.Lens' JobEventDetails (Prelude.Maybe Prelude.Text)
jobEventDetails_message = Lens.lens (\JobEventDetails' {message} -> message) (\s@JobEventDetails' {} a -> s {message = a} :: JobEventDetails)

-- | TThe type of event that occurred related to the Amazon Braket job.
jobEventDetails_timeOfEvent :: Lens.Lens' JobEventDetails (Prelude.Maybe Prelude.UTCTime)
jobEventDetails_timeOfEvent = Lens.lens (\JobEventDetails' {timeOfEvent} -> timeOfEvent) (\s@JobEventDetails' {} a -> s {timeOfEvent = a} :: JobEventDetails) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON JobEventDetails where
  parseJSON =
    Data.withObject
      "JobEventDetails"
      ( \x ->
          JobEventDetails'
            Prelude.<$> (x Data..:? "eventType")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "timeOfEvent")
      )

instance Prelude.Hashable JobEventDetails where
  hashWithSalt _salt JobEventDetails' {..} =
    _salt
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` timeOfEvent

instance Prelude.NFData JobEventDetails where
  rnf JobEventDetails' {..} =
    Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf timeOfEvent
