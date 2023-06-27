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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HyperParameterTuningJobStatus
import Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyType
import Amazonka.SageMaker.Types.ObjectiveStatusCounters
import Amazonka.SageMaker.Types.ResourceLimits
import Amazonka.SageMaker.Types.TrainingJobStatusCounters

-- | Provides summary information about a hyperparameter tuning job.
--
-- /See:/ 'newHyperParameterTuningJobSummary' smart constructor.
data HyperParameterTuningJobSummary = HyperParameterTuningJobSummary'
  { -- | The date and time that the tuning job ended.
    hyperParameterTuningEndTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the tuning job was modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ResourceLimits.html ResourceLimits>
    -- object that specifies the maximum number of training jobs and parallel
    -- training jobs allowed for this tuning job.
    resourceLimits :: Prelude.Maybe ResourceLimits,
    -- | The name of the tuning job.
    hyperParameterTuningJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the tuning job.
    hyperParameterTuningJobArn :: Prelude.Text,
    -- | The status of the tuning job.
    hyperParameterTuningJobStatus :: HyperParameterTuningJobStatus,
    -- | Specifies the search strategy hyperparameter tuning uses to choose which
    -- hyperparameters to evaluate at each iteration.
    strategy :: HyperParameterTuningJobStrategyType,
    -- | The date and time that the tuning job was created.
    creationTime :: Data.POSIX,
    -- | The
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_TrainingJobStatusCounters.html TrainingJobStatusCounters>
    -- object that specifies the numbers of training jobs, categorized by
    -- status, that this tuning job launched.
    trainingJobStatusCounters :: TrainingJobStatusCounters,
    -- | The
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ObjectiveStatusCounters.html ObjectiveStatusCounters>
    -- object that specifies the numbers of training jobs, categorized by
    -- objective metric status, that this tuning job launched.
    objectiveStatusCounters :: ObjectiveStatusCounters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hyperParameterTuningEndTime', 'hyperParameterTuningJobSummary_hyperParameterTuningEndTime' - The date and time that the tuning job ended.
--
-- 'lastModifiedTime', 'hyperParameterTuningJobSummary_lastModifiedTime' - The date and time that the tuning job was modified.
--
-- 'resourceLimits', 'hyperParameterTuningJobSummary_resourceLimits' - The
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ResourceLimits.html ResourceLimits>
-- object that specifies the maximum number of training jobs and parallel
-- training jobs allowed for this tuning job.
--
-- 'hyperParameterTuningJobName', 'hyperParameterTuningJobSummary_hyperParameterTuningJobName' - The name of the tuning job.
--
-- 'hyperParameterTuningJobArn', 'hyperParameterTuningJobSummary_hyperParameterTuningJobArn' - The Amazon Resource Name (ARN) of the tuning job.
--
-- 'hyperParameterTuningJobStatus', 'hyperParameterTuningJobSummary_hyperParameterTuningJobStatus' - The status of the tuning job.
--
-- 'strategy', 'hyperParameterTuningJobSummary_strategy' - Specifies the search strategy hyperparameter tuning uses to choose which
-- hyperparameters to evaluate at each iteration.
--
-- 'creationTime', 'hyperParameterTuningJobSummary_creationTime' - The date and time that the tuning job was created.
--
-- 'trainingJobStatusCounters', 'hyperParameterTuningJobSummary_trainingJobStatusCounters' - The
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_TrainingJobStatusCounters.html TrainingJobStatusCounters>
-- object that specifies the numbers of training jobs, categorized by
-- status, that this tuning job launched.
--
-- 'objectiveStatusCounters', 'hyperParameterTuningJobSummary_objectiveStatusCounters' - The
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ObjectiveStatusCounters.html ObjectiveStatusCounters>
-- object that specifies the numbers of training jobs, categorized by
-- objective metric status, that this tuning job launched.
newHyperParameterTuningJobSummary ::
  -- | 'hyperParameterTuningJobName'
  Prelude.Text ->
  -- | 'hyperParameterTuningJobArn'
  Prelude.Text ->
  -- | 'hyperParameterTuningJobStatus'
  HyperParameterTuningJobStatus ->
  -- | 'strategy'
  HyperParameterTuningJobStrategyType ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'trainingJobStatusCounters'
  TrainingJobStatusCounters ->
  -- | 'objectiveStatusCounters'
  ObjectiveStatusCounters ->
  HyperParameterTuningJobSummary
newHyperParameterTuningJobSummary
  pHyperParameterTuningJobName_
  pHyperParameterTuningJobArn_
  pHyperParameterTuningJobStatus_
  pStrategy_
  pCreationTime_
  pTrainingJobStatusCounters_
  pObjectiveStatusCounters_ =
    HyperParameterTuningJobSummary'
      { hyperParameterTuningEndTime =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        resourceLimits = Prelude.Nothing,
        hyperParameterTuningJobName =
          pHyperParameterTuningJobName_,
        hyperParameterTuningJobArn =
          pHyperParameterTuningJobArn_,
        hyperParameterTuningJobStatus =
          pHyperParameterTuningJobStatus_,
        strategy = pStrategy_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        trainingJobStatusCounters =
          pTrainingJobStatusCounters_,
        objectiveStatusCounters =
          pObjectiveStatusCounters_
      }

-- | The date and time that the tuning job ended.
hyperParameterTuningJobSummary_hyperParameterTuningEndTime :: Lens.Lens' HyperParameterTuningJobSummary (Prelude.Maybe Prelude.UTCTime)
hyperParameterTuningJobSummary_hyperParameterTuningEndTime = Lens.lens (\HyperParameterTuningJobSummary' {hyperParameterTuningEndTime} -> hyperParameterTuningEndTime) (\s@HyperParameterTuningJobSummary' {} a -> s {hyperParameterTuningEndTime = a} :: HyperParameterTuningJobSummary) Prelude.. Lens.mapping Data._Time

-- | The date and time that the tuning job was modified.
hyperParameterTuningJobSummary_lastModifiedTime :: Lens.Lens' HyperParameterTuningJobSummary (Prelude.Maybe Prelude.UTCTime)
hyperParameterTuningJobSummary_lastModifiedTime = Lens.lens (\HyperParameterTuningJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@HyperParameterTuningJobSummary' {} a -> s {lastModifiedTime = a} :: HyperParameterTuningJobSummary) Prelude.. Lens.mapping Data._Time

-- | The
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ResourceLimits.html ResourceLimits>
-- object that specifies the maximum number of training jobs and parallel
-- training jobs allowed for this tuning job.
hyperParameterTuningJobSummary_resourceLimits :: Lens.Lens' HyperParameterTuningJobSummary (Prelude.Maybe ResourceLimits)
hyperParameterTuningJobSummary_resourceLimits = Lens.lens (\HyperParameterTuningJobSummary' {resourceLimits} -> resourceLimits) (\s@HyperParameterTuningJobSummary' {} a -> s {resourceLimits = a} :: HyperParameterTuningJobSummary)

-- | The name of the tuning job.
hyperParameterTuningJobSummary_hyperParameterTuningJobName :: Lens.Lens' HyperParameterTuningJobSummary Prelude.Text
hyperParameterTuningJobSummary_hyperParameterTuningJobName = Lens.lens (\HyperParameterTuningJobSummary' {hyperParameterTuningJobName} -> hyperParameterTuningJobName) (\s@HyperParameterTuningJobSummary' {} a -> s {hyperParameterTuningJobName = a} :: HyperParameterTuningJobSummary)

-- | The Amazon Resource Name (ARN) of the tuning job.
hyperParameterTuningJobSummary_hyperParameterTuningJobArn :: Lens.Lens' HyperParameterTuningJobSummary Prelude.Text
hyperParameterTuningJobSummary_hyperParameterTuningJobArn = Lens.lens (\HyperParameterTuningJobSummary' {hyperParameterTuningJobArn} -> hyperParameterTuningJobArn) (\s@HyperParameterTuningJobSummary' {} a -> s {hyperParameterTuningJobArn = a} :: HyperParameterTuningJobSummary)

-- | The status of the tuning job.
hyperParameterTuningJobSummary_hyperParameterTuningJobStatus :: Lens.Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStatus
hyperParameterTuningJobSummary_hyperParameterTuningJobStatus = Lens.lens (\HyperParameterTuningJobSummary' {hyperParameterTuningJobStatus} -> hyperParameterTuningJobStatus) (\s@HyperParameterTuningJobSummary' {} a -> s {hyperParameterTuningJobStatus = a} :: HyperParameterTuningJobSummary)

-- | Specifies the search strategy hyperparameter tuning uses to choose which
-- hyperparameters to evaluate at each iteration.
hyperParameterTuningJobSummary_strategy :: Lens.Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStrategyType
hyperParameterTuningJobSummary_strategy = Lens.lens (\HyperParameterTuningJobSummary' {strategy} -> strategy) (\s@HyperParameterTuningJobSummary' {} a -> s {strategy = a} :: HyperParameterTuningJobSummary)

-- | The date and time that the tuning job was created.
hyperParameterTuningJobSummary_creationTime :: Lens.Lens' HyperParameterTuningJobSummary Prelude.UTCTime
hyperParameterTuningJobSummary_creationTime = Lens.lens (\HyperParameterTuningJobSummary' {creationTime} -> creationTime) (\s@HyperParameterTuningJobSummary' {} a -> s {creationTime = a} :: HyperParameterTuningJobSummary) Prelude.. Data._Time

-- | The
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_TrainingJobStatusCounters.html TrainingJobStatusCounters>
-- object that specifies the numbers of training jobs, categorized by
-- status, that this tuning job launched.
hyperParameterTuningJobSummary_trainingJobStatusCounters :: Lens.Lens' HyperParameterTuningJobSummary TrainingJobStatusCounters
hyperParameterTuningJobSummary_trainingJobStatusCounters = Lens.lens (\HyperParameterTuningJobSummary' {trainingJobStatusCounters} -> trainingJobStatusCounters) (\s@HyperParameterTuningJobSummary' {} a -> s {trainingJobStatusCounters = a} :: HyperParameterTuningJobSummary)

-- | The
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ObjectiveStatusCounters.html ObjectiveStatusCounters>
-- object that specifies the numbers of training jobs, categorized by
-- objective metric status, that this tuning job launched.
hyperParameterTuningJobSummary_objectiveStatusCounters :: Lens.Lens' HyperParameterTuningJobSummary ObjectiveStatusCounters
hyperParameterTuningJobSummary_objectiveStatusCounters = Lens.lens (\HyperParameterTuningJobSummary' {objectiveStatusCounters} -> objectiveStatusCounters) (\s@HyperParameterTuningJobSummary' {} a -> s {objectiveStatusCounters = a} :: HyperParameterTuningJobSummary)

instance Data.FromJSON HyperParameterTuningJobSummary where
  parseJSON =
    Data.withObject
      "HyperParameterTuningJobSummary"
      ( \x ->
          HyperParameterTuningJobSummary'
            Prelude.<$> (x Data..:? "HyperParameterTuningEndTime")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "ResourceLimits")
            Prelude.<*> (x Data..: "HyperParameterTuningJobName")
            Prelude.<*> (x Data..: "HyperParameterTuningJobArn")
            Prelude.<*> (x Data..: "HyperParameterTuningJobStatus")
            Prelude.<*> (x Data..: "Strategy")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "TrainingJobStatusCounters")
            Prelude.<*> (x Data..: "ObjectiveStatusCounters")
      )

instance
  Prelude.Hashable
    HyperParameterTuningJobSummary
  where
  hashWithSalt
    _salt
    HyperParameterTuningJobSummary' {..} =
      _salt
        `Prelude.hashWithSalt` hyperParameterTuningEndTime
        `Prelude.hashWithSalt` lastModifiedTime
        `Prelude.hashWithSalt` resourceLimits
        `Prelude.hashWithSalt` hyperParameterTuningJobName
        `Prelude.hashWithSalt` hyperParameterTuningJobArn
        `Prelude.hashWithSalt` hyperParameterTuningJobStatus
        `Prelude.hashWithSalt` strategy
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` trainingJobStatusCounters
        `Prelude.hashWithSalt` objectiveStatusCounters

instance
  Prelude.NFData
    HyperParameterTuningJobSummary
  where
  rnf HyperParameterTuningJobSummary' {..} =
    Prelude.rnf hyperParameterTuningEndTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf resourceLimits
      `Prelude.seq` Prelude.rnf hyperParameterTuningJobName
      `Prelude.seq` Prelude.rnf hyperParameterTuningJobArn
      `Prelude.seq` Prelude.rnf hyperParameterTuningJobStatus
      `Prelude.seq` Prelude.rnf strategy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf trainingJobStatusCounters
      `Prelude.seq` Prelude.rnf objectiveStatusCounters
