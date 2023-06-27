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
-- Module      : Amazonka.Pinpoint.Types.JourneyRunResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyRunResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.JourneyRunStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information from a specified run of a journey.
--
-- /See:/ 'newJourneyRunResponse' smart constructor.
data JourneyRunResponse = JourneyRunResponse'
  { -- | The current status of the journey run.
    status :: JourneyRunStatus,
    -- | The last time the journey run was updated, in ISO 8601 format..
    lastUpdateTime :: Prelude.Text,
    -- | The time when the journey run was created or scheduled, in ISO 8601
    -- format.
    creationTime :: Prelude.Text,
    -- | The unique identifier for the run.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'journeyRunResponse_status' - The current status of the journey run.
--
-- 'lastUpdateTime', 'journeyRunResponse_lastUpdateTime' - The last time the journey run was updated, in ISO 8601 format..
--
-- 'creationTime', 'journeyRunResponse_creationTime' - The time when the journey run was created or scheduled, in ISO 8601
-- format.
--
-- 'runId', 'journeyRunResponse_runId' - The unique identifier for the run.
newJourneyRunResponse ::
  -- | 'status'
  JourneyRunStatus ->
  -- | 'lastUpdateTime'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  JourneyRunResponse
newJourneyRunResponse
  pStatus_
  pLastUpdateTime_
  pCreationTime_
  pRunId_ =
    JourneyRunResponse'
      { status = pStatus_,
        lastUpdateTime = pLastUpdateTime_,
        creationTime = pCreationTime_,
        runId = pRunId_
      }

-- | The current status of the journey run.
journeyRunResponse_status :: Lens.Lens' JourneyRunResponse JourneyRunStatus
journeyRunResponse_status = Lens.lens (\JourneyRunResponse' {status} -> status) (\s@JourneyRunResponse' {} a -> s {status = a} :: JourneyRunResponse)

-- | The last time the journey run was updated, in ISO 8601 format..
journeyRunResponse_lastUpdateTime :: Lens.Lens' JourneyRunResponse Prelude.Text
journeyRunResponse_lastUpdateTime = Lens.lens (\JourneyRunResponse' {lastUpdateTime} -> lastUpdateTime) (\s@JourneyRunResponse' {} a -> s {lastUpdateTime = a} :: JourneyRunResponse)

-- | The time when the journey run was created or scheduled, in ISO 8601
-- format.
journeyRunResponse_creationTime :: Lens.Lens' JourneyRunResponse Prelude.Text
journeyRunResponse_creationTime = Lens.lens (\JourneyRunResponse' {creationTime} -> creationTime) (\s@JourneyRunResponse' {} a -> s {creationTime = a} :: JourneyRunResponse)

-- | The unique identifier for the run.
journeyRunResponse_runId :: Lens.Lens' JourneyRunResponse Prelude.Text
journeyRunResponse_runId = Lens.lens (\JourneyRunResponse' {runId} -> runId) (\s@JourneyRunResponse' {} a -> s {runId = a} :: JourneyRunResponse)

instance Data.FromJSON JourneyRunResponse where
  parseJSON =
    Data.withObject
      "JourneyRunResponse"
      ( \x ->
          JourneyRunResponse'
            Prelude.<$> (x Data..: "Status")
            Prelude.<*> (x Data..: "LastUpdateTime")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "RunId")
      )

instance Prelude.Hashable JourneyRunResponse where
  hashWithSalt _salt JourneyRunResponse' {..} =
    _salt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` runId

instance Prelude.NFData JourneyRunResponse where
  rnf JourneyRunResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf runId
