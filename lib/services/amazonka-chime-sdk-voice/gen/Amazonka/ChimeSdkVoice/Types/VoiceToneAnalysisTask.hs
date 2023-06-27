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
-- Module      : Amazonka.ChimeSdkVoice.Types.VoiceToneAnalysisTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.VoiceToneAnalysisTask where

import Amazonka.ChimeSdkVoice.Types.CallDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of an asynchronous request to perform voice tone
-- analysis on a Voice Connector call.
--
-- /See:/ 'newVoiceToneAnalysisTask' smart constructor.
data VoiceToneAnalysisTask = VoiceToneAnalysisTask'
  { -- | The call details of a voice tone analysis task.
    callDetails :: Prelude.Maybe CallDetails,
    -- | The time at which a voice tone analysis task was created.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The time at which a voice tone analysis task started.
    startedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The status of a voice tone analysis task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The time at which a voice tone analysis task was updated.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the voice tone analysis task.
    voiceToneAnalysisTaskId :: Prelude.Maybe Prelude.Text,
    -- | The status of a voice tone analysis task, @IN_QUEUE@, @IN_PROGRESS@,
    -- @PARTIAL_SUCCESS@, @SUCCEEDED@, @FAILED@, or @STOPPED@.
    voiceToneAnalysisTaskStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceToneAnalysisTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callDetails', 'voiceToneAnalysisTask_callDetails' - The call details of a voice tone analysis task.
--
-- 'createdTimestamp', 'voiceToneAnalysisTask_createdTimestamp' - The time at which a voice tone analysis task was created.
--
-- 'startedTimestamp', 'voiceToneAnalysisTask_startedTimestamp' - The time at which a voice tone analysis task started.
--
-- 'statusMessage', 'voiceToneAnalysisTask_statusMessage' - The status of a voice tone analysis task.
--
-- 'updatedTimestamp', 'voiceToneAnalysisTask_updatedTimestamp' - The time at which a voice tone analysis task was updated.
--
-- 'voiceToneAnalysisTaskId', 'voiceToneAnalysisTask_voiceToneAnalysisTaskId' - The ID of the voice tone analysis task.
--
-- 'voiceToneAnalysisTaskStatus', 'voiceToneAnalysisTask_voiceToneAnalysisTaskStatus' - The status of a voice tone analysis task, @IN_QUEUE@, @IN_PROGRESS@,
-- @PARTIAL_SUCCESS@, @SUCCEEDED@, @FAILED@, or @STOPPED@.
newVoiceToneAnalysisTask ::
  VoiceToneAnalysisTask
newVoiceToneAnalysisTask =
  VoiceToneAnalysisTask'
    { callDetails =
        Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      startedTimestamp = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      voiceToneAnalysisTaskId = Prelude.Nothing,
      voiceToneAnalysisTaskStatus = Prelude.Nothing
    }

-- | The call details of a voice tone analysis task.
voiceToneAnalysisTask_callDetails :: Lens.Lens' VoiceToneAnalysisTask (Prelude.Maybe CallDetails)
voiceToneAnalysisTask_callDetails = Lens.lens (\VoiceToneAnalysisTask' {callDetails} -> callDetails) (\s@VoiceToneAnalysisTask' {} a -> s {callDetails = a} :: VoiceToneAnalysisTask)

-- | The time at which a voice tone analysis task was created.
voiceToneAnalysisTask_createdTimestamp :: Lens.Lens' VoiceToneAnalysisTask (Prelude.Maybe Prelude.UTCTime)
voiceToneAnalysisTask_createdTimestamp = Lens.lens (\VoiceToneAnalysisTask' {createdTimestamp} -> createdTimestamp) (\s@VoiceToneAnalysisTask' {} a -> s {createdTimestamp = a} :: VoiceToneAnalysisTask) Prelude.. Lens.mapping Data._Time

-- | The time at which a voice tone analysis task started.
voiceToneAnalysisTask_startedTimestamp :: Lens.Lens' VoiceToneAnalysisTask (Prelude.Maybe Prelude.UTCTime)
voiceToneAnalysisTask_startedTimestamp = Lens.lens (\VoiceToneAnalysisTask' {startedTimestamp} -> startedTimestamp) (\s@VoiceToneAnalysisTask' {} a -> s {startedTimestamp = a} :: VoiceToneAnalysisTask) Prelude.. Lens.mapping Data._Time

-- | The status of a voice tone analysis task.
voiceToneAnalysisTask_statusMessage :: Lens.Lens' VoiceToneAnalysisTask (Prelude.Maybe Prelude.Text)
voiceToneAnalysisTask_statusMessage = Lens.lens (\VoiceToneAnalysisTask' {statusMessage} -> statusMessage) (\s@VoiceToneAnalysisTask' {} a -> s {statusMessage = a} :: VoiceToneAnalysisTask)

-- | The time at which a voice tone analysis task was updated.
voiceToneAnalysisTask_updatedTimestamp :: Lens.Lens' VoiceToneAnalysisTask (Prelude.Maybe Prelude.UTCTime)
voiceToneAnalysisTask_updatedTimestamp = Lens.lens (\VoiceToneAnalysisTask' {updatedTimestamp} -> updatedTimestamp) (\s@VoiceToneAnalysisTask' {} a -> s {updatedTimestamp = a} :: VoiceToneAnalysisTask) Prelude.. Lens.mapping Data._Time

-- | The ID of the voice tone analysis task.
voiceToneAnalysisTask_voiceToneAnalysisTaskId :: Lens.Lens' VoiceToneAnalysisTask (Prelude.Maybe Prelude.Text)
voiceToneAnalysisTask_voiceToneAnalysisTaskId = Lens.lens (\VoiceToneAnalysisTask' {voiceToneAnalysisTaskId} -> voiceToneAnalysisTaskId) (\s@VoiceToneAnalysisTask' {} a -> s {voiceToneAnalysisTaskId = a} :: VoiceToneAnalysisTask)

-- | The status of a voice tone analysis task, @IN_QUEUE@, @IN_PROGRESS@,
-- @PARTIAL_SUCCESS@, @SUCCEEDED@, @FAILED@, or @STOPPED@.
voiceToneAnalysisTask_voiceToneAnalysisTaskStatus :: Lens.Lens' VoiceToneAnalysisTask (Prelude.Maybe Prelude.Text)
voiceToneAnalysisTask_voiceToneAnalysisTaskStatus = Lens.lens (\VoiceToneAnalysisTask' {voiceToneAnalysisTaskStatus} -> voiceToneAnalysisTaskStatus) (\s@VoiceToneAnalysisTask' {} a -> s {voiceToneAnalysisTaskStatus = a} :: VoiceToneAnalysisTask)

instance Data.FromJSON VoiceToneAnalysisTask where
  parseJSON =
    Data.withObject
      "VoiceToneAnalysisTask"
      ( \x ->
          VoiceToneAnalysisTask'
            Prelude.<$> (x Data..:? "CallDetails")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "StartedTimestamp")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
            Prelude.<*> (x Data..:? "VoiceToneAnalysisTaskId")
            Prelude.<*> (x Data..:? "VoiceToneAnalysisTaskStatus")
      )

instance Prelude.Hashable VoiceToneAnalysisTask where
  hashWithSalt _salt VoiceToneAnalysisTask' {..} =
    _salt
      `Prelude.hashWithSalt` callDetails
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` startedTimestamp
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` voiceToneAnalysisTaskId
      `Prelude.hashWithSalt` voiceToneAnalysisTaskStatus

instance Prelude.NFData VoiceToneAnalysisTask where
  rnf VoiceToneAnalysisTask' {..} =
    Prelude.rnf callDetails
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf startedTimestamp
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf voiceToneAnalysisTaskId
      `Prelude.seq` Prelude.rnf voiceToneAnalysisTaskStatus
