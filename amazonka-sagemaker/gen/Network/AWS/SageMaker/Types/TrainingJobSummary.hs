{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.TrainingJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.TrainingJobStatus

-- | Provides summary information about a training job.
--
-- /See:/ 'newTrainingJobSummary' smart constructor.
data TrainingJobSummary = TrainingJobSummary'
  { -- | Timestamp when the training job was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | A timestamp that shows when the training job ended. This field is set
    -- only if the training job has one of the terminal statuses (@Completed@,
    -- @Failed@, or @Stopped@).
    trainingEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the training job that you want a summary for.
    trainingJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Prelude.Text,
    -- | A timestamp that shows when the training job was created.
    creationTime :: Prelude.POSIX,
    -- | The status of the training job.
    trainingJobStatus :: TrainingJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        trainingJobName = pTrainingJobName_,
        trainingJobArn = pTrainingJobArn_,
        creationTime = Prelude._Time Lens.# pCreationTime_,
        trainingJobStatus = pTrainingJobStatus_
      }

-- | Timestamp when the training job was last modified.
trainingJobSummary_lastModifiedTime :: Lens.Lens' TrainingJobSummary (Prelude.Maybe Prelude.UTCTime)
trainingJobSummary_lastModifiedTime = Lens.lens (\TrainingJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@TrainingJobSummary' {} a -> s {lastModifiedTime = a} :: TrainingJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | A timestamp that shows when the training job ended. This field is set
-- only if the training job has one of the terminal statuses (@Completed@,
-- @Failed@, or @Stopped@).
trainingJobSummary_trainingEndTime :: Lens.Lens' TrainingJobSummary (Prelude.Maybe Prelude.UTCTime)
trainingJobSummary_trainingEndTime = Lens.lens (\TrainingJobSummary' {trainingEndTime} -> trainingEndTime) (\s@TrainingJobSummary' {} a -> s {trainingEndTime = a} :: TrainingJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the training job that you want a summary for.
trainingJobSummary_trainingJobName :: Lens.Lens' TrainingJobSummary Prelude.Text
trainingJobSummary_trainingJobName = Lens.lens (\TrainingJobSummary' {trainingJobName} -> trainingJobName) (\s@TrainingJobSummary' {} a -> s {trainingJobName = a} :: TrainingJobSummary)

-- | The Amazon Resource Name (ARN) of the training job.
trainingJobSummary_trainingJobArn :: Lens.Lens' TrainingJobSummary Prelude.Text
trainingJobSummary_trainingJobArn = Lens.lens (\TrainingJobSummary' {trainingJobArn} -> trainingJobArn) (\s@TrainingJobSummary' {} a -> s {trainingJobArn = a} :: TrainingJobSummary)

-- | A timestamp that shows when the training job was created.
trainingJobSummary_creationTime :: Lens.Lens' TrainingJobSummary Prelude.UTCTime
trainingJobSummary_creationTime = Lens.lens (\TrainingJobSummary' {creationTime} -> creationTime) (\s@TrainingJobSummary' {} a -> s {creationTime = a} :: TrainingJobSummary) Prelude.. Prelude._Time

-- | The status of the training job.
trainingJobSummary_trainingJobStatus :: Lens.Lens' TrainingJobSummary TrainingJobStatus
trainingJobSummary_trainingJobStatus = Lens.lens (\TrainingJobSummary' {trainingJobStatus} -> trainingJobStatus) (\s@TrainingJobSummary' {} a -> s {trainingJobStatus = a} :: TrainingJobSummary)

instance Prelude.FromJSON TrainingJobSummary where
  parseJSON =
    Prelude.withObject
      "TrainingJobSummary"
      ( \x ->
          TrainingJobSummary'
            Prelude.<$> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "TrainingEndTime")
            Prelude.<*> (x Prelude..: "TrainingJobName")
            Prelude.<*> (x Prelude..: "TrainingJobArn")
            Prelude.<*> (x Prelude..: "CreationTime")
            Prelude.<*> (x Prelude..: "TrainingJobStatus")
      )

instance Prelude.Hashable TrainingJobSummary

instance Prelude.NFData TrainingJobSummary
