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
-- Module      : Amazonka.IVSRealtime.Types.StageSessionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types.StageSessionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a stage session.
--
-- /See:/ 'newStageSessionSummary' smart constructor.
data StageSessionSummary = StageSessionSummary'
  { -- | ISO 8601 timestamp (returned as a string) when the stage session ended.
    -- This is null if the stage is active.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | ID of the session within the stage.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | ISO 8601 timestamp (returned as a string) when this stage session began.
    startTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StageSessionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'stageSessionSummary_endTime' - ISO 8601 timestamp (returned as a string) when the stage session ended.
-- This is null if the stage is active.
--
-- 'sessionId', 'stageSessionSummary_sessionId' - ID of the session within the stage.
--
-- 'startTime', 'stageSessionSummary_startTime' - ISO 8601 timestamp (returned as a string) when this stage session began.
newStageSessionSummary ::
  StageSessionSummary
newStageSessionSummary =
  StageSessionSummary'
    { endTime = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | ISO 8601 timestamp (returned as a string) when the stage session ended.
-- This is null if the stage is active.
stageSessionSummary_endTime :: Lens.Lens' StageSessionSummary (Prelude.Maybe Prelude.UTCTime)
stageSessionSummary_endTime = Lens.lens (\StageSessionSummary' {endTime} -> endTime) (\s@StageSessionSummary' {} a -> s {endTime = a} :: StageSessionSummary) Prelude.. Lens.mapping Data._Time

-- | ID of the session within the stage.
stageSessionSummary_sessionId :: Lens.Lens' StageSessionSummary (Prelude.Maybe Prelude.Text)
stageSessionSummary_sessionId = Lens.lens (\StageSessionSummary' {sessionId} -> sessionId) (\s@StageSessionSummary' {} a -> s {sessionId = a} :: StageSessionSummary)

-- | ISO 8601 timestamp (returned as a string) when this stage session began.
stageSessionSummary_startTime :: Lens.Lens' StageSessionSummary (Prelude.Maybe Prelude.UTCTime)
stageSessionSummary_startTime = Lens.lens (\StageSessionSummary' {startTime} -> startTime) (\s@StageSessionSummary' {} a -> s {startTime = a} :: StageSessionSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON StageSessionSummary where
  parseJSON =
    Data.withObject
      "StageSessionSummary"
      ( \x ->
          StageSessionSummary'
            Prelude.<$> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "sessionId")
            Prelude.<*> (x Data..:? "startTime")
      )

instance Prelude.Hashable StageSessionSummary where
  hashWithSalt _salt StageSessionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData StageSessionSummary where
  rnf StageSessionSummary' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf startTime
