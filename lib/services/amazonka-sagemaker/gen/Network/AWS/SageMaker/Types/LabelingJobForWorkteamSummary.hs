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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.LabelCountersForWorkteam

-- | Provides summary information for a work team.
--
-- /See:/ 'newLabelingJobForWorkteamSummary' smart constructor.
data LabelingJobForWorkteamSummary = LabelingJobForWorkteamSummary'
  { -- | The configured number of workers per data object.
    numberOfHumanWorkersPerDataObject :: Prelude.Maybe Prelude.Natural,
    -- | Provides information about the progress of a labeling job.
    labelCounters :: Prelude.Maybe LabelCountersForWorkteam,
    -- | The name of the labeling job that the work team is assigned to.
    labelingJobName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a labeling job. You can use this to refer to a
    -- specific labeling job.
    jobReferenceCode :: Prelude.Text,
    -- | The Amazon Web Services account ID of the account used to start the
    -- labeling job.
    workRequesterAccountId :: Prelude.Text,
    -- | The date and time that the labeling job was created.
    creationTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobForWorkteamSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfHumanWorkersPerDataObject', 'labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject' - The configured number of workers per data object.
--
-- 'labelCounters', 'labelingJobForWorkteamSummary_labelCounters' - Provides information about the progress of a labeling job.
--
-- 'labelingJobName', 'labelingJobForWorkteamSummary_labelingJobName' - The name of the labeling job that the work team is assigned to.
--
-- 'jobReferenceCode', 'labelingJobForWorkteamSummary_jobReferenceCode' - A unique identifier for a labeling job. You can use this to refer to a
-- specific labeling job.
--
-- 'workRequesterAccountId', 'labelingJobForWorkteamSummary_workRequesterAccountId' - The Amazon Web Services account ID of the account used to start the
-- labeling job.
--
-- 'creationTime', 'labelingJobForWorkteamSummary_creationTime' - The date and time that the labeling job was created.
newLabelingJobForWorkteamSummary ::
  -- | 'jobReferenceCode'
  Prelude.Text ->
  -- | 'workRequesterAccountId'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  LabelingJobForWorkteamSummary
newLabelingJobForWorkteamSummary
  pJobReferenceCode_
  pWorkRequesterAccountId_
  pCreationTime_ =
    LabelingJobForWorkteamSummary'
      { numberOfHumanWorkersPerDataObject =
          Prelude.Nothing,
        labelCounters = Prelude.Nothing,
        labelingJobName = Prelude.Nothing,
        jobReferenceCode = pJobReferenceCode_,
        workRequesterAccountId =
          pWorkRequesterAccountId_,
        creationTime =
          Core._Time Lens.# pCreationTime_
      }

-- | The configured number of workers per data object.
labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject :: Lens.Lens' LabelingJobForWorkteamSummary (Prelude.Maybe Prelude.Natural)
labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject = Lens.lens (\LabelingJobForWorkteamSummary' {numberOfHumanWorkersPerDataObject} -> numberOfHumanWorkersPerDataObject) (\s@LabelingJobForWorkteamSummary' {} a -> s {numberOfHumanWorkersPerDataObject = a} :: LabelingJobForWorkteamSummary)

-- | Provides information about the progress of a labeling job.
labelingJobForWorkteamSummary_labelCounters :: Lens.Lens' LabelingJobForWorkteamSummary (Prelude.Maybe LabelCountersForWorkteam)
labelingJobForWorkteamSummary_labelCounters = Lens.lens (\LabelingJobForWorkteamSummary' {labelCounters} -> labelCounters) (\s@LabelingJobForWorkteamSummary' {} a -> s {labelCounters = a} :: LabelingJobForWorkteamSummary)

-- | The name of the labeling job that the work team is assigned to.
labelingJobForWorkteamSummary_labelingJobName :: Lens.Lens' LabelingJobForWorkteamSummary (Prelude.Maybe Prelude.Text)
labelingJobForWorkteamSummary_labelingJobName = Lens.lens (\LabelingJobForWorkteamSummary' {labelingJobName} -> labelingJobName) (\s@LabelingJobForWorkteamSummary' {} a -> s {labelingJobName = a} :: LabelingJobForWorkteamSummary)

-- | A unique identifier for a labeling job. You can use this to refer to a
-- specific labeling job.
labelingJobForWorkteamSummary_jobReferenceCode :: Lens.Lens' LabelingJobForWorkteamSummary Prelude.Text
labelingJobForWorkteamSummary_jobReferenceCode = Lens.lens (\LabelingJobForWorkteamSummary' {jobReferenceCode} -> jobReferenceCode) (\s@LabelingJobForWorkteamSummary' {} a -> s {jobReferenceCode = a} :: LabelingJobForWorkteamSummary)

-- | The Amazon Web Services account ID of the account used to start the
-- labeling job.
labelingJobForWorkteamSummary_workRequesterAccountId :: Lens.Lens' LabelingJobForWorkteamSummary Prelude.Text
labelingJobForWorkteamSummary_workRequesterAccountId = Lens.lens (\LabelingJobForWorkteamSummary' {workRequesterAccountId} -> workRequesterAccountId) (\s@LabelingJobForWorkteamSummary' {} a -> s {workRequesterAccountId = a} :: LabelingJobForWorkteamSummary)

-- | The date and time that the labeling job was created.
labelingJobForWorkteamSummary_creationTime :: Lens.Lens' LabelingJobForWorkteamSummary Prelude.UTCTime
labelingJobForWorkteamSummary_creationTime = Lens.lens (\LabelingJobForWorkteamSummary' {creationTime} -> creationTime) (\s@LabelingJobForWorkteamSummary' {} a -> s {creationTime = a} :: LabelingJobForWorkteamSummary) Prelude.. Core._Time

instance Core.FromJSON LabelingJobForWorkteamSummary where
  parseJSON =
    Core.withObject
      "LabelingJobForWorkteamSummary"
      ( \x ->
          LabelingJobForWorkteamSummary'
            Prelude.<$> (x Core..:? "NumberOfHumanWorkersPerDataObject")
            Prelude.<*> (x Core..:? "LabelCounters")
            Prelude.<*> (x Core..:? "LabelingJobName")
            Prelude.<*> (x Core..: "JobReferenceCode")
            Prelude.<*> (x Core..: "WorkRequesterAccountId")
            Prelude.<*> (x Core..: "CreationTime")
      )

instance
  Prelude.Hashable
    LabelingJobForWorkteamSummary

instance Prelude.NFData LabelingJobForWorkteamSummary
