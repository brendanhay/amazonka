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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.TrainingJobStatus

-- | Provides summary information about a training job.
--
-- /See:/ 'newTrainingJobSummary' smart constructor.
data TrainingJobSummary = TrainingJobSummary'
  { -- | Timestamp when the training job was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | A timestamp that shows when the training job ended. This field is set
    -- only if the training job has one of the terminal statuses (@Completed@,
    -- @Failed@, or @Stopped@).
    trainingEndTime :: Core.Maybe Core.POSIX,
    -- | The name of the training job that you want a summary for.
    trainingJobName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Core.Text,
    -- | A timestamp that shows when the training job was created.
    creationTime :: Core.POSIX,
    -- | The status of the training job.
    trainingJobStatus :: TrainingJobStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'trainingJobArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
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
          Core.Nothing,
        trainingEndTime = Core.Nothing,
        trainingJobName = pTrainingJobName_,
        trainingJobArn = pTrainingJobArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        trainingJobStatus = pTrainingJobStatus_
      }

-- | Timestamp when the training job was last modified.
trainingJobSummary_lastModifiedTime :: Lens.Lens' TrainingJobSummary (Core.Maybe Core.UTCTime)
trainingJobSummary_lastModifiedTime = Lens.lens (\TrainingJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@TrainingJobSummary' {} a -> s {lastModifiedTime = a} :: TrainingJobSummary) Core.. Lens.mapping Core._Time

-- | A timestamp that shows when the training job ended. This field is set
-- only if the training job has one of the terminal statuses (@Completed@,
-- @Failed@, or @Stopped@).
trainingJobSummary_trainingEndTime :: Lens.Lens' TrainingJobSummary (Core.Maybe Core.UTCTime)
trainingJobSummary_trainingEndTime = Lens.lens (\TrainingJobSummary' {trainingEndTime} -> trainingEndTime) (\s@TrainingJobSummary' {} a -> s {trainingEndTime = a} :: TrainingJobSummary) Core.. Lens.mapping Core._Time

-- | The name of the training job that you want a summary for.
trainingJobSummary_trainingJobName :: Lens.Lens' TrainingJobSummary Core.Text
trainingJobSummary_trainingJobName = Lens.lens (\TrainingJobSummary' {trainingJobName} -> trainingJobName) (\s@TrainingJobSummary' {} a -> s {trainingJobName = a} :: TrainingJobSummary)

-- | The Amazon Resource Name (ARN) of the training job.
trainingJobSummary_trainingJobArn :: Lens.Lens' TrainingJobSummary Core.Text
trainingJobSummary_trainingJobArn = Lens.lens (\TrainingJobSummary' {trainingJobArn} -> trainingJobArn) (\s@TrainingJobSummary' {} a -> s {trainingJobArn = a} :: TrainingJobSummary)

-- | A timestamp that shows when the training job was created.
trainingJobSummary_creationTime :: Lens.Lens' TrainingJobSummary Core.UTCTime
trainingJobSummary_creationTime = Lens.lens (\TrainingJobSummary' {creationTime} -> creationTime) (\s@TrainingJobSummary' {} a -> s {creationTime = a} :: TrainingJobSummary) Core.. Core._Time

-- | The status of the training job.
trainingJobSummary_trainingJobStatus :: Lens.Lens' TrainingJobSummary TrainingJobStatus
trainingJobSummary_trainingJobStatus = Lens.lens (\TrainingJobSummary' {trainingJobStatus} -> trainingJobStatus) (\s@TrainingJobSummary' {} a -> s {trainingJobStatus = a} :: TrainingJobSummary)

instance Core.FromJSON TrainingJobSummary where
  parseJSON =
    Core.withObject
      "TrainingJobSummary"
      ( \x ->
          TrainingJobSummary'
            Core.<$> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "TrainingEndTime")
            Core.<*> (x Core..: "TrainingJobName")
            Core.<*> (x Core..: "TrainingJobArn")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "TrainingJobStatus")
      )

instance Core.Hashable TrainingJobSummary

instance Core.NFData TrainingJobSummary
