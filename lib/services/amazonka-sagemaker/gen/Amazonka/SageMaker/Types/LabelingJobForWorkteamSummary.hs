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
-- Module      : Amazonka.SageMaker.Types.LabelingJobForWorkteamSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobForWorkteamSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.LabelCountersForWorkteam

-- | Provides summary information for a work team.
--
-- /See:/ 'newLabelingJobForWorkteamSummary' smart constructor.
data LabelingJobForWorkteamSummary = LabelingJobForWorkteamSummary'
  { -- | Provides information about the progress of a labeling job.
    labelCounters :: Prelude.Maybe LabelCountersForWorkteam,
    -- | The name of the labeling job that the work team is assigned to.
    labelingJobName :: Prelude.Maybe Prelude.Text,
    -- | The configured number of workers per data object.
    numberOfHumanWorkersPerDataObject :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for a labeling job. You can use this to refer to a
    -- specific labeling job.
    jobReferenceCode :: Prelude.Text,
    -- | The Amazon Web Services account ID of the account used to start the
    -- labeling job.
    workRequesterAccountId :: Prelude.Text,
    -- | The date and time that the labeling job was created.
    creationTime :: Data.POSIX
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
-- 'labelCounters', 'labelingJobForWorkteamSummary_labelCounters' - Provides information about the progress of a labeling job.
--
-- 'labelingJobName', 'labelingJobForWorkteamSummary_labelingJobName' - The name of the labeling job that the work team is assigned to.
--
-- 'numberOfHumanWorkersPerDataObject', 'labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject' - The configured number of workers per data object.
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
      { labelCounters =
          Prelude.Nothing,
        labelingJobName = Prelude.Nothing,
        numberOfHumanWorkersPerDataObject =
          Prelude.Nothing,
        jobReferenceCode = pJobReferenceCode_,
        workRequesterAccountId =
          pWorkRequesterAccountId_,
        creationTime =
          Data._Time Lens.# pCreationTime_
      }

-- | Provides information about the progress of a labeling job.
labelingJobForWorkteamSummary_labelCounters :: Lens.Lens' LabelingJobForWorkteamSummary (Prelude.Maybe LabelCountersForWorkteam)
labelingJobForWorkteamSummary_labelCounters = Lens.lens (\LabelingJobForWorkteamSummary' {labelCounters} -> labelCounters) (\s@LabelingJobForWorkteamSummary' {} a -> s {labelCounters = a} :: LabelingJobForWorkteamSummary)

-- | The name of the labeling job that the work team is assigned to.
labelingJobForWorkteamSummary_labelingJobName :: Lens.Lens' LabelingJobForWorkteamSummary (Prelude.Maybe Prelude.Text)
labelingJobForWorkteamSummary_labelingJobName = Lens.lens (\LabelingJobForWorkteamSummary' {labelingJobName} -> labelingJobName) (\s@LabelingJobForWorkteamSummary' {} a -> s {labelingJobName = a} :: LabelingJobForWorkteamSummary)

-- | The configured number of workers per data object.
labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject :: Lens.Lens' LabelingJobForWorkteamSummary (Prelude.Maybe Prelude.Natural)
labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject = Lens.lens (\LabelingJobForWorkteamSummary' {numberOfHumanWorkersPerDataObject} -> numberOfHumanWorkersPerDataObject) (\s@LabelingJobForWorkteamSummary' {} a -> s {numberOfHumanWorkersPerDataObject = a} :: LabelingJobForWorkteamSummary)

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
labelingJobForWorkteamSummary_creationTime = Lens.lens (\LabelingJobForWorkteamSummary' {creationTime} -> creationTime) (\s@LabelingJobForWorkteamSummary' {} a -> s {creationTime = a} :: LabelingJobForWorkteamSummary) Prelude.. Data._Time

instance Data.FromJSON LabelingJobForWorkteamSummary where
  parseJSON =
    Data.withObject
      "LabelingJobForWorkteamSummary"
      ( \x ->
          LabelingJobForWorkteamSummary'
            Prelude.<$> (x Data..:? "LabelCounters")
            Prelude.<*> (x Data..:? "LabelingJobName")
            Prelude.<*> (x Data..:? "NumberOfHumanWorkersPerDataObject")
            Prelude.<*> (x Data..: "JobReferenceCode")
            Prelude.<*> (x Data..: "WorkRequesterAccountId")
            Prelude.<*> (x Data..: "CreationTime")
      )

instance
  Prelude.Hashable
    LabelingJobForWorkteamSummary
  where
  hashWithSalt _salt LabelingJobForWorkteamSummary' {..} =
    _salt
      `Prelude.hashWithSalt` labelCounters
      `Prelude.hashWithSalt` labelingJobName
      `Prelude.hashWithSalt` numberOfHumanWorkersPerDataObject
      `Prelude.hashWithSalt` jobReferenceCode
      `Prelude.hashWithSalt` workRequesterAccountId
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData LabelingJobForWorkteamSummary where
  rnf LabelingJobForWorkteamSummary' {..} =
    Prelude.rnf labelCounters
      `Prelude.seq` Prelude.rnf labelingJobName
      `Prelude.seq` Prelude.rnf numberOfHumanWorkersPerDataObject
      `Prelude.seq` Prelude.rnf jobReferenceCode
      `Prelude.seq` Prelude.rnf workRequesterAccountId
      `Prelude.seq` Prelude.rnf creationTime
