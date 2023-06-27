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
-- Module      : Amazonka.IVSRealtime.Types.StageSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSRealtime.Types.StageSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A stage session begins when the first participant joins a stage and ends
-- after the last participant leaves the stage. A stage session helps with
-- debugging stages by grouping events and participants into shorter
-- periods of time (i.e., a session), which is helpful when stages are used
-- over long periods of time.
--
-- /See:/ 'newStageSession' smart constructor.
data StageSession = StageSession'
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
-- Create a value of 'StageSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'stageSession_endTime' - ISO 8601 timestamp (returned as a string) when the stage session ended.
-- This is null if the stage is active.
--
-- 'sessionId', 'stageSession_sessionId' - ID of the session within the stage.
--
-- 'startTime', 'stageSession_startTime' - ISO 8601 timestamp (returned as a string) when this stage session began.
newStageSession ::
  StageSession
newStageSession =
  StageSession'
    { endTime = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | ISO 8601 timestamp (returned as a string) when the stage session ended.
-- This is null if the stage is active.
stageSession_endTime :: Lens.Lens' StageSession (Prelude.Maybe Prelude.UTCTime)
stageSession_endTime = Lens.lens (\StageSession' {endTime} -> endTime) (\s@StageSession' {} a -> s {endTime = a} :: StageSession) Prelude.. Lens.mapping Data._Time

-- | ID of the session within the stage.
stageSession_sessionId :: Lens.Lens' StageSession (Prelude.Maybe Prelude.Text)
stageSession_sessionId = Lens.lens (\StageSession' {sessionId} -> sessionId) (\s@StageSession' {} a -> s {sessionId = a} :: StageSession)

-- | ISO 8601 timestamp (returned as a string) when this stage session began.
stageSession_startTime :: Lens.Lens' StageSession (Prelude.Maybe Prelude.UTCTime)
stageSession_startTime = Lens.lens (\StageSession' {startTime} -> startTime) (\s@StageSession' {} a -> s {startTime = a} :: StageSession) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON StageSession where
  parseJSON =
    Data.withObject
      "StageSession"
      ( \x ->
          StageSession'
            Prelude.<$> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "sessionId")
            Prelude.<*> (x Data..:? "startTime")
      )

instance Prelude.Hashable StageSession where
  hashWithSalt _salt StageSession' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData StageSession where
  rnf StageSession' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf startTime
