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
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
import Network.AWS.SageMaker.Types.ObjectiveStatusCounters
import Network.AWS.SageMaker.Types.ResourceLimits
import Network.AWS.SageMaker.Types.TrainingJobStatusCounters

-- | Provides summary information about a hyperparameter tuning job.
--
-- /See:/ 'newHyperParameterTuningJobSummary' smart constructor.
data HyperParameterTuningJobSummary = HyperParameterTuningJobSummary'
  { -- | The ResourceLimits object that specifies the maximum number of training
    -- jobs and parallel training jobs allowed for this tuning job.
    resourceLimits :: Prelude.Maybe ResourceLimits,
    -- | The date and time that the tuning job ended.
    hyperParameterTuningEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time that the tuning job was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the tuning job.
    hyperParameterTuningJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the tuning job.
    hyperParameterTuningJobArn :: Prelude.Text,
    -- | The status of the tuning job.
    hyperParameterTuningJobStatus :: HyperParameterTuningJobStatus,
    -- | Specifies the search strategy hyperparameter tuning uses to choose which
    -- hyperparameters to use for each iteration. Currently, the only valid
    -- value is Bayesian.
    strategy :: HyperParameterTuningJobStrategyType,
    -- | The date and time that the tuning job was created.
    creationTime :: Prelude.POSIX,
    -- | The TrainingJobStatusCounters object that specifies the numbers of
    -- training jobs, categorized by status, that this tuning job launched.
    trainingJobStatusCounters :: TrainingJobStatusCounters,
    -- | The ObjectiveStatusCounters object that specifies the numbers of
    -- training jobs, categorized by objective metric status, that this tuning
    -- job launched.
    objectiveStatusCounters :: ObjectiveStatusCounters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceLimits', 'hyperParameterTuningJobSummary_resourceLimits' - The ResourceLimits object that specifies the maximum number of training
-- jobs and parallel training jobs allowed for this tuning job.
--
-- 'hyperParameterTuningEndTime', 'hyperParameterTuningJobSummary_hyperParameterTuningEndTime' - The date and time that the tuning job ended.
--
-- 'lastModifiedTime', 'hyperParameterTuningJobSummary_lastModifiedTime' - The date and time that the tuning job was modified.
--
-- 'hyperParameterTuningJobName', 'hyperParameterTuningJobSummary_hyperParameterTuningJobName' - The name of the tuning job.
--
-- 'hyperParameterTuningJobArn', 'hyperParameterTuningJobSummary_hyperParameterTuningJobArn' - The Amazon Resource Name (ARN) of the tuning job.
--
-- 'hyperParameterTuningJobStatus', 'hyperParameterTuningJobSummary_hyperParameterTuningJobStatus' - The status of the tuning job.
--
-- 'strategy', 'hyperParameterTuningJobSummary_strategy' - Specifies the search strategy hyperparameter tuning uses to choose which
-- hyperparameters to use for each iteration. Currently, the only valid
-- value is Bayesian.
--
-- 'creationTime', 'hyperParameterTuningJobSummary_creationTime' - The date and time that the tuning job was created.
--
-- 'trainingJobStatusCounters', 'hyperParameterTuningJobSummary_trainingJobStatusCounters' - The TrainingJobStatusCounters object that specifies the numbers of
-- training jobs, categorized by status, that this tuning job launched.
--
-- 'objectiveStatusCounters', 'hyperParameterTuningJobSummary_objectiveStatusCounters' - The ObjectiveStatusCounters object that specifies the numbers of
-- training jobs, categorized by objective metric status, that this tuning
-- job launched.
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
      { resourceLimits =
          Prelude.Nothing,
        hyperParameterTuningEndTime =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        hyperParameterTuningJobName =
          pHyperParameterTuningJobName_,
        hyperParameterTuningJobArn =
          pHyperParameterTuningJobArn_,
        hyperParameterTuningJobStatus =
          pHyperParameterTuningJobStatus_,
        strategy = pStrategy_,
        creationTime =
          Prelude._Time Lens.# pCreationTime_,
        trainingJobStatusCounters =
          pTrainingJobStatusCounters_,
        objectiveStatusCounters =
          pObjectiveStatusCounters_
      }

-- | The ResourceLimits object that specifies the maximum number of training
-- jobs and parallel training jobs allowed for this tuning job.
hyperParameterTuningJobSummary_resourceLimits :: Lens.Lens' HyperParameterTuningJobSummary (Prelude.Maybe ResourceLimits)
hyperParameterTuningJobSummary_resourceLimits = Lens.lens (\HyperParameterTuningJobSummary' {resourceLimits} -> resourceLimits) (\s@HyperParameterTuningJobSummary' {} a -> s {resourceLimits = a} :: HyperParameterTuningJobSummary)

-- | The date and time that the tuning job ended.
hyperParameterTuningJobSummary_hyperParameterTuningEndTime :: Lens.Lens' HyperParameterTuningJobSummary (Prelude.Maybe Prelude.UTCTime)
hyperParameterTuningJobSummary_hyperParameterTuningEndTime = Lens.lens (\HyperParameterTuningJobSummary' {hyperParameterTuningEndTime} -> hyperParameterTuningEndTime) (\s@HyperParameterTuningJobSummary' {} a -> s {hyperParameterTuningEndTime = a} :: HyperParameterTuningJobSummary) Prelude.. Lens.mapping Prelude._Time

-- | The date and time that the tuning job was modified.
hyperParameterTuningJobSummary_lastModifiedTime :: Lens.Lens' HyperParameterTuningJobSummary (Prelude.Maybe Prelude.UTCTime)
hyperParameterTuningJobSummary_lastModifiedTime = Lens.lens (\HyperParameterTuningJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@HyperParameterTuningJobSummary' {} a -> s {lastModifiedTime = a} :: HyperParameterTuningJobSummary) Prelude.. Lens.mapping Prelude._Time

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
-- hyperparameters to use for each iteration. Currently, the only valid
-- value is Bayesian.
hyperParameterTuningJobSummary_strategy :: Lens.Lens' HyperParameterTuningJobSummary HyperParameterTuningJobStrategyType
hyperParameterTuningJobSummary_strategy = Lens.lens (\HyperParameterTuningJobSummary' {strategy} -> strategy) (\s@HyperParameterTuningJobSummary' {} a -> s {strategy = a} :: HyperParameterTuningJobSummary)

-- | The date and time that the tuning job was created.
hyperParameterTuningJobSummary_creationTime :: Lens.Lens' HyperParameterTuningJobSummary Prelude.UTCTime
hyperParameterTuningJobSummary_creationTime = Lens.lens (\HyperParameterTuningJobSummary' {creationTime} -> creationTime) (\s@HyperParameterTuningJobSummary' {} a -> s {creationTime = a} :: HyperParameterTuningJobSummary) Prelude.. Prelude._Time

-- | The TrainingJobStatusCounters object that specifies the numbers of
-- training jobs, categorized by status, that this tuning job launched.
hyperParameterTuningJobSummary_trainingJobStatusCounters :: Lens.Lens' HyperParameterTuningJobSummary TrainingJobStatusCounters
hyperParameterTuningJobSummary_trainingJobStatusCounters = Lens.lens (\HyperParameterTuningJobSummary' {trainingJobStatusCounters} -> trainingJobStatusCounters) (\s@HyperParameterTuningJobSummary' {} a -> s {trainingJobStatusCounters = a} :: HyperParameterTuningJobSummary)

-- | The ObjectiveStatusCounters object that specifies the numbers of
-- training jobs, categorized by objective metric status, that this tuning
-- job launched.
hyperParameterTuningJobSummary_objectiveStatusCounters :: Lens.Lens' HyperParameterTuningJobSummary ObjectiveStatusCounters
hyperParameterTuningJobSummary_objectiveStatusCounters = Lens.lens (\HyperParameterTuningJobSummary' {objectiveStatusCounters} -> objectiveStatusCounters) (\s@HyperParameterTuningJobSummary' {} a -> s {objectiveStatusCounters = a} :: HyperParameterTuningJobSummary)

instance
  Prelude.FromJSON
    HyperParameterTuningJobSummary
  where
  parseJSON =
    Prelude.withObject
      "HyperParameterTuningJobSummary"
      ( \x ->
          HyperParameterTuningJobSummary'
            Prelude.<$> (x Prelude..:? "ResourceLimits")
            Prelude.<*> (x Prelude..:? "HyperParameterTuningEndTime")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..: "HyperParameterTuningJobName")
            Prelude.<*> (x Prelude..: "HyperParameterTuningJobArn")
            Prelude.<*> (x Prelude..: "HyperParameterTuningJobStatus")
            Prelude.<*> (x Prelude..: "Strategy")
            Prelude.<*> (x Prelude..: "CreationTime")
            Prelude.<*> (x Prelude..: "TrainingJobStatusCounters")
            Prelude.<*> (x Prelude..: "ObjectiveStatusCounters")
      )

instance
  Prelude.Hashable
    HyperParameterTuningJobSummary

instance
  Prelude.NFData
    HyperParameterTuningJobSummary
