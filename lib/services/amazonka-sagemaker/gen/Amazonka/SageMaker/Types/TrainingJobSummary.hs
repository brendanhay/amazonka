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
-- Module      : Amazonka.SageMaker.Types.TrainingJobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrainingJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrainingJobStatus
import Amazonka.SageMaker.Types.WarmPoolStatus

-- | Provides summary information about a training job.
--
-- /See:/ 'newTrainingJobSummary' smart constructor.
data TrainingJobSummary = TrainingJobSummary'
  { -- | Timestamp when the training job was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A timestamp that shows when the training job ended. This field is set
    -- only if the training job has one of the terminal statuses (@Completed@,
    -- @Failed@, or @Stopped@).
    trainingEndTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the warm pool associated with the training job.
    warmPoolStatus :: Prelude.Maybe WarmPoolStatus,
    -- | The name of the training job that you want a summary for.
    trainingJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Prelude.Text,
    -- | A timestamp that shows when the training job was created.
    creationTime :: Data.POSIX,
    -- | The status of the training job.
    trainingJobStatus :: TrainingJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'trainingJobSummary_lastModifiedTime' - Timestamp when the training job was last modified.
--
-- 'trainingEndTime', 'trainingJobSummary_trainingEndTime' - A timestamp that shows when the training job ended. This field is set
-- only if the training job has one of the terminal statuses (@Completed@,
-- @Failed@, or @Stopped@).
--
-- 'warmPoolStatus', 'trainingJobSummary_warmPoolStatus' - The status of the warm pool associated with the training job.
--
-- 'trainingJobName', 'trainingJobSummary_trainingJobName' - The name of the training job that you want a summary for.
--
-- 'trainingJobArn', 'trainingJobSummary_trainingJobArn' - The Amazon Resource Name (ARN) of the training job.
--
-- 'creationTime', 'trainingJobSummary_creationTime' - A timestamp that shows when the training job was created.
--
-- 'trainingJobStatus', 'trainingJobSummary_trainingJobStatus' - The status of the training job.
newTrainingJobSummary ::
  -- | 'trainingJobName'
  Prelude.Text ->
  -- | 'trainingJobArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'trainingJobStatus'
  TrainingJobStatus ->
  TrainingJobSummary
newTrainingJobSummary
  pTrainingJobName_
  pTrainingJobArn_
  pCreationTime_
  pTrainingJobStatus_ =
    TrainingJobSummary'
      { lastModifiedTime =
          Prelude.Nothing,
        trainingEndTime = Prelude.Nothing,
        warmPoolStatus = Prelude.Nothing,
        trainingJobName = pTrainingJobName_,
        trainingJobArn = pTrainingJobArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        trainingJobStatus = pTrainingJobStatus_
      }

-- | Timestamp when the training job was last modified.
trainingJobSummary_lastModifiedTime :: Lens.Lens' TrainingJobSummary (Prelude.Maybe Prelude.UTCTime)
trainingJobSummary_lastModifiedTime = Lens.lens (\TrainingJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@TrainingJobSummary' {} a -> s {lastModifiedTime = a} :: TrainingJobSummary) Prelude.. Lens.mapping Data._Time

-- | A timestamp that shows when the training job ended. This field is set
-- only if the training job has one of the terminal statuses (@Completed@,
-- @Failed@, or @Stopped@).
trainingJobSummary_trainingEndTime :: Lens.Lens' TrainingJobSummary (Prelude.Maybe Prelude.UTCTime)
trainingJobSummary_trainingEndTime = Lens.lens (\TrainingJobSummary' {trainingEndTime} -> trainingEndTime) (\s@TrainingJobSummary' {} a -> s {trainingEndTime = a} :: TrainingJobSummary) Prelude.. Lens.mapping Data._Time

-- | The status of the warm pool associated with the training job.
trainingJobSummary_warmPoolStatus :: Lens.Lens' TrainingJobSummary (Prelude.Maybe WarmPoolStatus)
trainingJobSummary_warmPoolStatus = Lens.lens (\TrainingJobSummary' {warmPoolStatus} -> warmPoolStatus) (\s@TrainingJobSummary' {} a -> s {warmPoolStatus = a} :: TrainingJobSummary)

-- | The name of the training job that you want a summary for.
trainingJobSummary_trainingJobName :: Lens.Lens' TrainingJobSummary Prelude.Text
trainingJobSummary_trainingJobName = Lens.lens (\TrainingJobSummary' {trainingJobName} -> trainingJobName) (\s@TrainingJobSummary' {} a -> s {trainingJobName = a} :: TrainingJobSummary)

-- | The Amazon Resource Name (ARN) of the training job.
trainingJobSummary_trainingJobArn :: Lens.Lens' TrainingJobSummary Prelude.Text
trainingJobSummary_trainingJobArn = Lens.lens (\TrainingJobSummary' {trainingJobArn} -> trainingJobArn) (\s@TrainingJobSummary' {} a -> s {trainingJobArn = a} :: TrainingJobSummary)

-- | A timestamp that shows when the training job was created.
trainingJobSummary_creationTime :: Lens.Lens' TrainingJobSummary Prelude.UTCTime
trainingJobSummary_creationTime = Lens.lens (\TrainingJobSummary' {creationTime} -> creationTime) (\s@TrainingJobSummary' {} a -> s {creationTime = a} :: TrainingJobSummary) Prelude.. Data._Time

-- | The status of the training job.
trainingJobSummary_trainingJobStatus :: Lens.Lens' TrainingJobSummary TrainingJobStatus
trainingJobSummary_trainingJobStatus = Lens.lens (\TrainingJobSummary' {trainingJobStatus} -> trainingJobStatus) (\s@TrainingJobSummary' {} a -> s {trainingJobStatus = a} :: TrainingJobSummary)

instance Data.FromJSON TrainingJobSummary where
  parseJSON =
    Data.withObject
      "TrainingJobSummary"
      ( \x ->
          TrainingJobSummary'
            Prelude.<$> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "TrainingEndTime")
            Prelude.<*> (x Data..:? "WarmPoolStatus")
            Prelude.<*> (x Data..: "TrainingJobName")
            Prelude.<*> (x Data..: "TrainingJobArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "TrainingJobStatus")
      )

instance Prelude.Hashable TrainingJobSummary where
  hashWithSalt _salt TrainingJobSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` trainingEndTime
      `Prelude.hashWithSalt` warmPoolStatus
      `Prelude.hashWithSalt` trainingJobName
      `Prelude.hashWithSalt` trainingJobArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` trainingJobStatus

instance Prelude.NFData TrainingJobSummary where
  rnf TrainingJobSummary' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf trainingEndTime
      `Prelude.seq` Prelude.rnf warmPoolStatus
      `Prelude.seq` Prelude.rnf trainingJobName
      `Prelude.seq` Prelude.rnf trainingJobArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf trainingJobStatus
