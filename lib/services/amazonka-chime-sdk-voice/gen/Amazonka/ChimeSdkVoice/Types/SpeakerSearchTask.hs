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
-- Module      : Amazonka.ChimeSdkVoice.Types.SpeakerSearchTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.SpeakerSearchTask where

import Amazonka.ChimeSdkVoice.Types.CallDetails
import Amazonka.ChimeSdkVoice.Types.SpeakerSearchDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of an asynchronous request to perform speaker search
-- analysis on a Voice Connector call.
--
-- /See:/ 'newSpeakerSearchTask' smart constructor.
data SpeakerSearchTask = SpeakerSearchTask'
  { -- | The call details of a speaker search task.
    callDetails :: Prelude.Maybe CallDetails,
    -- | The time at which a speaker search task was created.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The details of a speaker search task.
    speakerSearchDetails :: Prelude.Maybe SpeakerSearchDetails,
    -- | The speaker search task ID.
    speakerSearchTaskId :: Prelude.Maybe Prelude.Text,
    -- | The status of the speaker search task, @IN_QUEUE@, @IN_PROGRESS@,
    -- @PARTIAL_SUCCESS@, @SUCCEEDED@, @FAILED@, or @STOPPED@.
    speakerSearchTaskStatus :: Prelude.Maybe Prelude.Text,
    -- | The time at which the speaker search task began.
    startedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | A detailed message about the status of a speaker search.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The time at which a speaker search task was updated.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpeakerSearchTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callDetails', 'speakerSearchTask_callDetails' - The call details of a speaker search task.
--
-- 'createdTimestamp', 'speakerSearchTask_createdTimestamp' - The time at which a speaker search task was created.
--
-- 'speakerSearchDetails', 'speakerSearchTask_speakerSearchDetails' - The details of a speaker search task.
--
-- 'speakerSearchTaskId', 'speakerSearchTask_speakerSearchTaskId' - The speaker search task ID.
--
-- 'speakerSearchTaskStatus', 'speakerSearchTask_speakerSearchTaskStatus' - The status of the speaker search task, @IN_QUEUE@, @IN_PROGRESS@,
-- @PARTIAL_SUCCESS@, @SUCCEEDED@, @FAILED@, or @STOPPED@.
--
-- 'startedTimestamp', 'speakerSearchTask_startedTimestamp' - The time at which the speaker search task began.
--
-- 'statusMessage', 'speakerSearchTask_statusMessage' - A detailed message about the status of a speaker search.
--
-- 'updatedTimestamp', 'speakerSearchTask_updatedTimestamp' - The time at which a speaker search task was updated.
newSpeakerSearchTask ::
  SpeakerSearchTask
newSpeakerSearchTask =
  SpeakerSearchTask'
    { callDetails = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      speakerSearchDetails = Prelude.Nothing,
      speakerSearchTaskId = Prelude.Nothing,
      speakerSearchTaskStatus = Prelude.Nothing,
      startedTimestamp = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | The call details of a speaker search task.
speakerSearchTask_callDetails :: Lens.Lens' SpeakerSearchTask (Prelude.Maybe CallDetails)
speakerSearchTask_callDetails = Lens.lens (\SpeakerSearchTask' {callDetails} -> callDetails) (\s@SpeakerSearchTask' {} a -> s {callDetails = a} :: SpeakerSearchTask)

-- | The time at which a speaker search task was created.
speakerSearchTask_createdTimestamp :: Lens.Lens' SpeakerSearchTask (Prelude.Maybe Prelude.UTCTime)
speakerSearchTask_createdTimestamp = Lens.lens (\SpeakerSearchTask' {createdTimestamp} -> createdTimestamp) (\s@SpeakerSearchTask' {} a -> s {createdTimestamp = a} :: SpeakerSearchTask) Prelude.. Lens.mapping Data._Time

-- | The details of a speaker search task.
speakerSearchTask_speakerSearchDetails :: Lens.Lens' SpeakerSearchTask (Prelude.Maybe SpeakerSearchDetails)
speakerSearchTask_speakerSearchDetails = Lens.lens (\SpeakerSearchTask' {speakerSearchDetails} -> speakerSearchDetails) (\s@SpeakerSearchTask' {} a -> s {speakerSearchDetails = a} :: SpeakerSearchTask)

-- | The speaker search task ID.
speakerSearchTask_speakerSearchTaskId :: Lens.Lens' SpeakerSearchTask (Prelude.Maybe Prelude.Text)
speakerSearchTask_speakerSearchTaskId = Lens.lens (\SpeakerSearchTask' {speakerSearchTaskId} -> speakerSearchTaskId) (\s@SpeakerSearchTask' {} a -> s {speakerSearchTaskId = a} :: SpeakerSearchTask)

-- | The status of the speaker search task, @IN_QUEUE@, @IN_PROGRESS@,
-- @PARTIAL_SUCCESS@, @SUCCEEDED@, @FAILED@, or @STOPPED@.
speakerSearchTask_speakerSearchTaskStatus :: Lens.Lens' SpeakerSearchTask (Prelude.Maybe Prelude.Text)
speakerSearchTask_speakerSearchTaskStatus = Lens.lens (\SpeakerSearchTask' {speakerSearchTaskStatus} -> speakerSearchTaskStatus) (\s@SpeakerSearchTask' {} a -> s {speakerSearchTaskStatus = a} :: SpeakerSearchTask)

-- | The time at which the speaker search task began.
speakerSearchTask_startedTimestamp :: Lens.Lens' SpeakerSearchTask (Prelude.Maybe Prelude.UTCTime)
speakerSearchTask_startedTimestamp = Lens.lens (\SpeakerSearchTask' {startedTimestamp} -> startedTimestamp) (\s@SpeakerSearchTask' {} a -> s {startedTimestamp = a} :: SpeakerSearchTask) Prelude.. Lens.mapping Data._Time

-- | A detailed message about the status of a speaker search.
speakerSearchTask_statusMessage :: Lens.Lens' SpeakerSearchTask (Prelude.Maybe Prelude.Text)
speakerSearchTask_statusMessage = Lens.lens (\SpeakerSearchTask' {statusMessage} -> statusMessage) (\s@SpeakerSearchTask' {} a -> s {statusMessage = a} :: SpeakerSearchTask)

-- | The time at which a speaker search task was updated.
speakerSearchTask_updatedTimestamp :: Lens.Lens' SpeakerSearchTask (Prelude.Maybe Prelude.UTCTime)
speakerSearchTask_updatedTimestamp = Lens.lens (\SpeakerSearchTask' {updatedTimestamp} -> updatedTimestamp) (\s@SpeakerSearchTask' {} a -> s {updatedTimestamp = a} :: SpeakerSearchTask) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON SpeakerSearchTask where
  parseJSON =
    Data.withObject
      "SpeakerSearchTask"
      ( \x ->
          SpeakerSearchTask'
            Prelude.<$> (x Data..:? "CallDetails")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "SpeakerSearchDetails")
            Prelude.<*> (x Data..:? "SpeakerSearchTaskId")
            Prelude.<*> (x Data..:? "SpeakerSearchTaskStatus")
            Prelude.<*> (x Data..:? "StartedTimestamp")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance Prelude.Hashable SpeakerSearchTask where
  hashWithSalt _salt SpeakerSearchTask' {..} =
    _salt
      `Prelude.hashWithSalt` callDetails
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` speakerSearchDetails
      `Prelude.hashWithSalt` speakerSearchTaskId
      `Prelude.hashWithSalt` speakerSearchTaskStatus
      `Prelude.hashWithSalt` startedTimestamp
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` updatedTimestamp

instance Prelude.NFData SpeakerSearchTask where
  rnf SpeakerSearchTask' {..} =
    Prelude.rnf callDetails
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf speakerSearchDetails
      `Prelude.seq` Prelude.rnf speakerSearchTaskId
      `Prelude.seq` Prelude.rnf speakerSearchTaskStatus
      `Prelude.seq` Prelude.rnf startedTimestamp
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf updatedTimestamp
