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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobForWorkteamSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobForWorkteamSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.LabelCountersForWorkteam

-- | Provides summary information for a work team.
--
-- /See:/ 'newLabelingJobForWorkteamSummary' smart constructor.
data LabelingJobForWorkteamSummary = LabelingJobForWorkteamSummary'
  { -- | Provides information about the progress of a labeling job.
    labelCounters :: Core.Maybe LabelCountersForWorkteam,
    -- | The name of the labeling job that the work team is assigned to.
    labelingJobName :: Core.Maybe Core.Text,
    -- | The configured number of workers per data object.
    numberOfHumanWorkersPerDataObject :: Core.Maybe Core.Natural,
    -- | A unique identifier for a labeling job. You can use this to refer to a
    -- specific labeling job.
    jobReferenceCode :: Core.Text,
    workRequesterAccountId :: Core.Text,
    -- | The date and time that the labeling job was created.
    creationTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LabelingJobForWorkteamSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelCounters', 'labelingJobForWorkteamSummary_labelCounters' - Provides information about the progress of a labeling job.
--
-- 'labelingJobName', 'labelingJobForWorkteamSummary_labelingJobName' - The name of the labeling job that the work team is assigned to.
--
-- 'numberOfHumanWorkersPerDataObject', 'labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject' - The configured number of workers per data object.
--
-- 'jobReferenceCode', 'labelingJobForWorkteamSummary_jobReferenceCode' - A unique identifier for a labeling job. You can use this to refer to a
-- specific labeling job.
--
-- 'workRequesterAccountId', 'labelingJobForWorkteamSummary_workRequesterAccountId' -
--
-- 'creationTime', 'labelingJobForWorkteamSummary_creationTime' - The date and time that the labeling job was created.
newLabelingJobForWorkteamSummary ::
  -- | 'jobReferenceCode'
  Core.Text ->
  -- | 'workRequesterAccountId'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  LabelingJobForWorkteamSummary
newLabelingJobForWorkteamSummary
  pJobReferenceCode_
  pWorkRequesterAccountId_
  pCreationTime_ =
    LabelingJobForWorkteamSummary'
      { labelCounters =
          Core.Nothing,
        labelingJobName = Core.Nothing,
        numberOfHumanWorkersPerDataObject =
          Core.Nothing,
        jobReferenceCode = pJobReferenceCode_,
        workRequesterAccountId =
          pWorkRequesterAccountId_,
        creationTime =
          Core._Time Lens.# pCreationTime_
      }

-- | Provides information about the progress of a labeling job.
labelingJobForWorkteamSummary_labelCounters :: Lens.Lens' LabelingJobForWorkteamSummary (Core.Maybe LabelCountersForWorkteam)
labelingJobForWorkteamSummary_labelCounters = Lens.lens (\LabelingJobForWorkteamSummary' {labelCounters} -> labelCounters) (\s@LabelingJobForWorkteamSummary' {} a -> s {labelCounters = a} :: LabelingJobForWorkteamSummary)

-- | The name of the labeling job that the work team is assigned to.
labelingJobForWorkteamSummary_labelingJobName :: Lens.Lens' LabelingJobForWorkteamSummary (Core.Maybe Core.Text)
labelingJobForWorkteamSummary_labelingJobName = Lens.lens (\LabelingJobForWorkteamSummary' {labelingJobName} -> labelingJobName) (\s@LabelingJobForWorkteamSummary' {} a -> s {labelingJobName = a} :: LabelingJobForWorkteamSummary)

-- | The configured number of workers per data object.
labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject :: Lens.Lens' LabelingJobForWorkteamSummary (Core.Maybe Core.Natural)
labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject = Lens.lens (\LabelingJobForWorkteamSummary' {numberOfHumanWorkersPerDataObject} -> numberOfHumanWorkersPerDataObject) (\s@LabelingJobForWorkteamSummary' {} a -> s {numberOfHumanWorkersPerDataObject = a} :: LabelingJobForWorkteamSummary)

-- | A unique identifier for a labeling job. You can use this to refer to a
-- specific labeling job.
labelingJobForWorkteamSummary_jobReferenceCode :: Lens.Lens' LabelingJobForWorkteamSummary Core.Text
labelingJobForWorkteamSummary_jobReferenceCode = Lens.lens (\LabelingJobForWorkteamSummary' {jobReferenceCode} -> jobReferenceCode) (\s@LabelingJobForWorkteamSummary' {} a -> s {jobReferenceCode = a} :: LabelingJobForWorkteamSummary)

-- |
labelingJobForWorkteamSummary_workRequesterAccountId :: Lens.Lens' LabelingJobForWorkteamSummary Core.Text
labelingJobForWorkteamSummary_workRequesterAccountId = Lens.lens (\LabelingJobForWorkteamSummary' {workRequesterAccountId} -> workRequesterAccountId) (\s@LabelingJobForWorkteamSummary' {} a -> s {workRequesterAccountId = a} :: LabelingJobForWorkteamSummary)

-- | The date and time that the labeling job was created.
labelingJobForWorkteamSummary_creationTime :: Lens.Lens' LabelingJobForWorkteamSummary Core.UTCTime
labelingJobForWorkteamSummary_creationTime = Lens.lens (\LabelingJobForWorkteamSummary' {creationTime} -> creationTime) (\s@LabelingJobForWorkteamSummary' {} a -> s {creationTime = a} :: LabelingJobForWorkteamSummary) Core.. Core._Time

instance Core.FromJSON LabelingJobForWorkteamSummary where
  parseJSON =
    Core.withObject
      "LabelingJobForWorkteamSummary"
      ( \x ->
          LabelingJobForWorkteamSummary'
            Core.<$> (x Core..:? "LabelCounters")
            Core.<*> (x Core..:? "LabelingJobName")
            Core.<*> (x Core..:? "NumberOfHumanWorkersPerDataObject")
            Core.<*> (x Core..: "JobReferenceCode")
            Core.<*> (x Core..: "WorkRequesterAccountId")
            Core.<*> (x Core..: "CreationTime")
      )

instance Core.Hashable LabelingJobForWorkteamSummary

instance Core.NFData LabelingJobForWorkteamSummary
