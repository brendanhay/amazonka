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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningJobSearchEntity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobSearchEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HyperParameterTrainingJobDefinition
import Amazonka.SageMaker.Types.HyperParameterTrainingJobSummary
import Amazonka.SageMaker.Types.HyperParameterTuningJobConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobStatus
import Amazonka.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
import Amazonka.SageMaker.Types.ObjectiveStatusCounters
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.TrainingJobStatusCounters

-- | An entity returned by the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_SearchRecord.html SearchRecord>
-- API containing the properties of a hyperparameter tuning job.
--
-- /See:/ 'newHyperParameterTuningJobSearchEntity' smart constructor.
data HyperParameterTuningJobSearchEntity = HyperParameterTuningJobSearchEntity'
  { bestTrainingJob :: Prelude.Maybe HyperParameterTrainingJobSummary,
    -- | The time that a hyperparameter tuning job was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The error that was created when a hyperparameter tuning job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The time that a hyperparameter tuning job ended.
    hyperParameterTuningEndTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of a hyperparameter tuning job.
    hyperParameterTuningJobArn :: Prelude.Maybe Prelude.Text,
    hyperParameterTuningJobConfig :: Prelude.Maybe HyperParameterTuningJobConfig,
    -- | The name of a hyperparameter tuning job.
    hyperParameterTuningJobName :: Prelude.Maybe Prelude.Text,
    -- | The status of a hyperparameter tuning job.
    hyperParameterTuningJobStatus :: Prelude.Maybe HyperParameterTuningJobStatus,
    -- | The time that a hyperparameter tuning job was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    objectiveStatusCounters :: Prelude.Maybe ObjectiveStatusCounters,
    overallBestTrainingJob :: Prelude.Maybe HyperParameterTrainingJobSummary,
    -- | The tags associated with a hyperparameter tuning job. For more
    -- information see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>.
    tags :: Prelude.Maybe [Tag],
    trainingJobDefinition :: Prelude.Maybe HyperParameterTrainingJobDefinition,
    -- | The job definitions included in a hyperparameter tuning job.
    trainingJobDefinitions :: Prelude.Maybe (Prelude.NonEmpty HyperParameterTrainingJobDefinition),
    trainingJobStatusCounters :: Prelude.Maybe TrainingJobStatusCounters,
    warmStartConfig :: Prelude.Maybe HyperParameterTuningJobWarmStartConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningJobSearchEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bestTrainingJob', 'hyperParameterTuningJobSearchEntity_bestTrainingJob' - Undocumented member.
--
-- 'creationTime', 'hyperParameterTuningJobSearchEntity_creationTime' - The time that a hyperparameter tuning job was created.
--
-- 'failureReason', 'hyperParameterTuningJobSearchEntity_failureReason' - The error that was created when a hyperparameter tuning job failed.
--
-- 'hyperParameterTuningEndTime', 'hyperParameterTuningJobSearchEntity_hyperParameterTuningEndTime' - The time that a hyperparameter tuning job ended.
--
-- 'hyperParameterTuningJobArn', 'hyperParameterTuningJobSearchEntity_hyperParameterTuningJobArn' - The Amazon Resource Name (ARN) of a hyperparameter tuning job.
--
-- 'hyperParameterTuningJobConfig', 'hyperParameterTuningJobSearchEntity_hyperParameterTuningJobConfig' - Undocumented member.
--
-- 'hyperParameterTuningJobName', 'hyperParameterTuningJobSearchEntity_hyperParameterTuningJobName' - The name of a hyperparameter tuning job.
--
-- 'hyperParameterTuningJobStatus', 'hyperParameterTuningJobSearchEntity_hyperParameterTuningJobStatus' - The status of a hyperparameter tuning job.
--
-- 'lastModifiedTime', 'hyperParameterTuningJobSearchEntity_lastModifiedTime' - The time that a hyperparameter tuning job was last modified.
--
-- 'objectiveStatusCounters', 'hyperParameterTuningJobSearchEntity_objectiveStatusCounters' - Undocumented member.
--
-- 'overallBestTrainingJob', 'hyperParameterTuningJobSearchEntity_overallBestTrainingJob' - Undocumented member.
--
-- 'tags', 'hyperParameterTuningJobSearchEntity_tags' - The tags associated with a hyperparameter tuning job. For more
-- information see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>.
--
-- 'trainingJobDefinition', 'hyperParameterTuningJobSearchEntity_trainingJobDefinition' - Undocumented member.
--
-- 'trainingJobDefinitions', 'hyperParameterTuningJobSearchEntity_trainingJobDefinitions' - The job definitions included in a hyperparameter tuning job.
--
-- 'trainingJobStatusCounters', 'hyperParameterTuningJobSearchEntity_trainingJobStatusCounters' - Undocumented member.
--
-- 'warmStartConfig', 'hyperParameterTuningJobSearchEntity_warmStartConfig' - Undocumented member.
newHyperParameterTuningJobSearchEntity ::
  HyperParameterTuningJobSearchEntity
newHyperParameterTuningJobSearchEntity =
  HyperParameterTuningJobSearchEntity'
    { bestTrainingJob =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      hyperParameterTuningEndTime =
        Prelude.Nothing,
      hyperParameterTuningJobArn =
        Prelude.Nothing,
      hyperParameterTuningJobConfig =
        Prelude.Nothing,
      hyperParameterTuningJobName =
        Prelude.Nothing,
      hyperParameterTuningJobStatus =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      objectiveStatusCounters =
        Prelude.Nothing,
      overallBestTrainingJob =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      trainingJobDefinition =
        Prelude.Nothing,
      trainingJobDefinitions =
        Prelude.Nothing,
      trainingJobStatusCounters =
        Prelude.Nothing,
      warmStartConfig = Prelude.Nothing
    }

-- | Undocumented member.
hyperParameterTuningJobSearchEntity_bestTrainingJob :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe HyperParameterTrainingJobSummary)
hyperParameterTuningJobSearchEntity_bestTrainingJob = Lens.lens (\HyperParameterTuningJobSearchEntity' {bestTrainingJob} -> bestTrainingJob) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {bestTrainingJob = a} :: HyperParameterTuningJobSearchEntity)

-- | The time that a hyperparameter tuning job was created.
hyperParameterTuningJobSearchEntity_creationTime :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe Prelude.UTCTime)
hyperParameterTuningJobSearchEntity_creationTime = Lens.lens (\HyperParameterTuningJobSearchEntity' {creationTime} -> creationTime) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {creationTime = a} :: HyperParameterTuningJobSearchEntity) Prelude.. Lens.mapping Data._Time

-- | The error that was created when a hyperparameter tuning job failed.
hyperParameterTuningJobSearchEntity_failureReason :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe Prelude.Text)
hyperParameterTuningJobSearchEntity_failureReason = Lens.lens (\HyperParameterTuningJobSearchEntity' {failureReason} -> failureReason) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {failureReason = a} :: HyperParameterTuningJobSearchEntity)

-- | The time that a hyperparameter tuning job ended.
hyperParameterTuningJobSearchEntity_hyperParameterTuningEndTime :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe Prelude.UTCTime)
hyperParameterTuningJobSearchEntity_hyperParameterTuningEndTime = Lens.lens (\HyperParameterTuningJobSearchEntity' {hyperParameterTuningEndTime} -> hyperParameterTuningEndTime) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {hyperParameterTuningEndTime = a} :: HyperParameterTuningJobSearchEntity) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of a hyperparameter tuning job.
hyperParameterTuningJobSearchEntity_hyperParameterTuningJobArn :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe Prelude.Text)
hyperParameterTuningJobSearchEntity_hyperParameterTuningJobArn = Lens.lens (\HyperParameterTuningJobSearchEntity' {hyperParameterTuningJobArn} -> hyperParameterTuningJobArn) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {hyperParameterTuningJobArn = a} :: HyperParameterTuningJobSearchEntity)

-- | Undocumented member.
hyperParameterTuningJobSearchEntity_hyperParameterTuningJobConfig :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe HyperParameterTuningJobConfig)
hyperParameterTuningJobSearchEntity_hyperParameterTuningJobConfig = Lens.lens (\HyperParameterTuningJobSearchEntity' {hyperParameterTuningJobConfig} -> hyperParameterTuningJobConfig) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {hyperParameterTuningJobConfig = a} :: HyperParameterTuningJobSearchEntity)

-- | The name of a hyperparameter tuning job.
hyperParameterTuningJobSearchEntity_hyperParameterTuningJobName :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe Prelude.Text)
hyperParameterTuningJobSearchEntity_hyperParameterTuningJobName = Lens.lens (\HyperParameterTuningJobSearchEntity' {hyperParameterTuningJobName} -> hyperParameterTuningJobName) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {hyperParameterTuningJobName = a} :: HyperParameterTuningJobSearchEntity)

-- | The status of a hyperparameter tuning job.
hyperParameterTuningJobSearchEntity_hyperParameterTuningJobStatus :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe HyperParameterTuningJobStatus)
hyperParameterTuningJobSearchEntity_hyperParameterTuningJobStatus = Lens.lens (\HyperParameterTuningJobSearchEntity' {hyperParameterTuningJobStatus} -> hyperParameterTuningJobStatus) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {hyperParameterTuningJobStatus = a} :: HyperParameterTuningJobSearchEntity)

-- | The time that a hyperparameter tuning job was last modified.
hyperParameterTuningJobSearchEntity_lastModifiedTime :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe Prelude.UTCTime)
hyperParameterTuningJobSearchEntity_lastModifiedTime = Lens.lens (\HyperParameterTuningJobSearchEntity' {lastModifiedTime} -> lastModifiedTime) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {lastModifiedTime = a} :: HyperParameterTuningJobSearchEntity) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
hyperParameterTuningJobSearchEntity_objectiveStatusCounters :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe ObjectiveStatusCounters)
hyperParameterTuningJobSearchEntity_objectiveStatusCounters = Lens.lens (\HyperParameterTuningJobSearchEntity' {objectiveStatusCounters} -> objectiveStatusCounters) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {objectiveStatusCounters = a} :: HyperParameterTuningJobSearchEntity)

-- | Undocumented member.
hyperParameterTuningJobSearchEntity_overallBestTrainingJob :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe HyperParameterTrainingJobSummary)
hyperParameterTuningJobSearchEntity_overallBestTrainingJob = Lens.lens (\HyperParameterTuningJobSearchEntity' {overallBestTrainingJob} -> overallBestTrainingJob) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {overallBestTrainingJob = a} :: HyperParameterTuningJobSearchEntity)

-- | The tags associated with a hyperparameter tuning job. For more
-- information see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>.
hyperParameterTuningJobSearchEntity_tags :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe [Tag])
hyperParameterTuningJobSearchEntity_tags = Lens.lens (\HyperParameterTuningJobSearchEntity' {tags} -> tags) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {tags = a} :: HyperParameterTuningJobSearchEntity) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
hyperParameterTuningJobSearchEntity_trainingJobDefinition :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe HyperParameterTrainingJobDefinition)
hyperParameterTuningJobSearchEntity_trainingJobDefinition = Lens.lens (\HyperParameterTuningJobSearchEntity' {trainingJobDefinition} -> trainingJobDefinition) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {trainingJobDefinition = a} :: HyperParameterTuningJobSearchEntity)

-- | The job definitions included in a hyperparameter tuning job.
hyperParameterTuningJobSearchEntity_trainingJobDefinitions :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe (Prelude.NonEmpty HyperParameterTrainingJobDefinition))
hyperParameterTuningJobSearchEntity_trainingJobDefinitions = Lens.lens (\HyperParameterTuningJobSearchEntity' {trainingJobDefinitions} -> trainingJobDefinitions) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {trainingJobDefinitions = a} :: HyperParameterTuningJobSearchEntity) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
hyperParameterTuningJobSearchEntity_trainingJobStatusCounters :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe TrainingJobStatusCounters)
hyperParameterTuningJobSearchEntity_trainingJobStatusCounters = Lens.lens (\HyperParameterTuningJobSearchEntity' {trainingJobStatusCounters} -> trainingJobStatusCounters) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {trainingJobStatusCounters = a} :: HyperParameterTuningJobSearchEntity)

-- | Undocumented member.
hyperParameterTuningJobSearchEntity_warmStartConfig :: Lens.Lens' HyperParameterTuningJobSearchEntity (Prelude.Maybe HyperParameterTuningJobWarmStartConfig)
hyperParameterTuningJobSearchEntity_warmStartConfig = Lens.lens (\HyperParameterTuningJobSearchEntity' {warmStartConfig} -> warmStartConfig) (\s@HyperParameterTuningJobSearchEntity' {} a -> s {warmStartConfig = a} :: HyperParameterTuningJobSearchEntity)

instance
  Data.FromJSON
    HyperParameterTuningJobSearchEntity
  where
  parseJSON =
    Data.withObject
      "HyperParameterTuningJobSearchEntity"
      ( \x ->
          HyperParameterTuningJobSearchEntity'
            Prelude.<$> (x Data..:? "BestTrainingJob")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "HyperParameterTuningEndTime")
            Prelude.<*> (x Data..:? "HyperParameterTuningJobArn")
            Prelude.<*> (x Data..:? "HyperParameterTuningJobConfig")
            Prelude.<*> (x Data..:? "HyperParameterTuningJobName")
            Prelude.<*> (x Data..:? "HyperParameterTuningJobStatus")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "ObjectiveStatusCounters")
            Prelude.<*> (x Data..:? "OverallBestTrainingJob")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TrainingJobDefinition")
            Prelude.<*> (x Data..:? "TrainingJobDefinitions")
            Prelude.<*> (x Data..:? "TrainingJobStatusCounters")
            Prelude.<*> (x Data..:? "WarmStartConfig")
      )

instance
  Prelude.Hashable
    HyperParameterTuningJobSearchEntity
  where
  hashWithSalt
    _salt
    HyperParameterTuningJobSearchEntity' {..} =
      _salt `Prelude.hashWithSalt` bestTrainingJob
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` hyperParameterTuningEndTime
        `Prelude.hashWithSalt` hyperParameterTuningJobArn
        `Prelude.hashWithSalt` hyperParameterTuningJobConfig
        `Prelude.hashWithSalt` hyperParameterTuningJobName
        `Prelude.hashWithSalt` hyperParameterTuningJobStatus
        `Prelude.hashWithSalt` lastModifiedTime
        `Prelude.hashWithSalt` objectiveStatusCounters
        `Prelude.hashWithSalt` overallBestTrainingJob
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` trainingJobDefinition
        `Prelude.hashWithSalt` trainingJobDefinitions
        `Prelude.hashWithSalt` trainingJobStatusCounters
        `Prelude.hashWithSalt` warmStartConfig

instance
  Prelude.NFData
    HyperParameterTuningJobSearchEntity
  where
  rnf HyperParameterTuningJobSearchEntity' {..} =
    Prelude.rnf bestTrainingJob
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf hyperParameterTuningEndTime
      `Prelude.seq` Prelude.rnf hyperParameterTuningJobArn
      `Prelude.seq` Prelude.rnf hyperParameterTuningJobConfig
      `Prelude.seq` Prelude.rnf hyperParameterTuningJobName
      `Prelude.seq` Prelude.rnf hyperParameterTuningJobStatus
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf objectiveStatusCounters
      `Prelude.seq` Prelude.rnf overallBestTrainingJob
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trainingJobDefinition
      `Prelude.seq` Prelude.rnf trainingJobDefinitions
      `Prelude.seq` Prelude.rnf trainingJobStatusCounters
      `Prelude.seq` Prelude.rnf warmStartConfig
