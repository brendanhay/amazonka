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
-- Module      : Amazonka.Rekognition.Types.ProjectVersionDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ProjectVersionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.EvaluationResult
import Amazonka.Rekognition.Types.GroundTruthManifest
import Amazonka.Rekognition.Types.OutputConfig
import Amazonka.Rekognition.Types.ProjectVersionStatus
import Amazonka.Rekognition.Types.TestingDataResult
import Amazonka.Rekognition.Types.TrainingDataResult

-- | A description of a version of an Amazon Rekognition Custom Labels model.
--
-- /See:/ 'newProjectVersionDescription' smart constructor.
data ProjectVersionDescription = ProjectVersionDescription'
  { -- | The Unix date and time that training of the model ended.
    trainingEndTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The minimum number of inference units used by the model. For more
    -- information, see StartProjectVersion.
    minInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | If the model version was copied from a different project,
    -- @SourceProjectVersionArn@ contains the ARN of the source model version.
    sourceProjectVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the model version.
    status :: Prelude.Maybe ProjectVersionStatus,
    -- | Contains information about the testing results.
    testingDataResult :: Prelude.Maybe TestingDataResult,
    -- | The Unix datetime for the date and time that training started.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The location of the summary manifest. The summary manifest provides
    -- aggregate data validation results for the training and test datasets.
    manifestSummary :: Prelude.Maybe GroundTruthManifest,
    -- | Contains information about the training results.
    trainingDataResult :: Prelude.Maybe TrainingDataResult,
    -- | The training results. @EvaluationResult@ is only returned if training is
    -- successful.
    evaluationResult :: Prelude.Maybe EvaluationResult,
    -- | The identifer for the AWS Key Management Service key (AWS KMS key) that
    -- was used to encrypt the model during training.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model version.
    projectVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of inference units Amazon Rekognition Custom Labels
    -- uses to auto-scale the model. For more information, see
    -- StartProjectVersion.
    maxInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | The duration, in seconds, that you were billed for a successful training
    -- of the model version. This value is only returned if the model version
    -- has been successfully trained.
    billableTrainingTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A descriptive message for an error or warning that occurred.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The location where training results are saved.
    outputConfig :: Prelude.Maybe OutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectVersionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trainingEndTimestamp', 'projectVersionDescription_trainingEndTimestamp' - The Unix date and time that training of the model ended.
--
-- 'minInferenceUnits', 'projectVersionDescription_minInferenceUnits' - The minimum number of inference units used by the model. For more
-- information, see StartProjectVersion.
--
-- 'sourceProjectVersionArn', 'projectVersionDescription_sourceProjectVersionArn' - If the model version was copied from a different project,
-- @SourceProjectVersionArn@ contains the ARN of the source model version.
--
-- 'status', 'projectVersionDescription_status' - The current status of the model version.
--
-- 'testingDataResult', 'projectVersionDescription_testingDataResult' - Contains information about the testing results.
--
-- 'creationTimestamp', 'projectVersionDescription_creationTimestamp' - The Unix datetime for the date and time that training started.
--
-- 'manifestSummary', 'projectVersionDescription_manifestSummary' - The location of the summary manifest. The summary manifest provides
-- aggregate data validation results for the training and test datasets.
--
-- 'trainingDataResult', 'projectVersionDescription_trainingDataResult' - Contains information about the training results.
--
-- 'evaluationResult', 'projectVersionDescription_evaluationResult' - The training results. @EvaluationResult@ is only returned if training is
-- successful.
--
-- 'kmsKeyId', 'projectVersionDescription_kmsKeyId' - The identifer for the AWS Key Management Service key (AWS KMS key) that
-- was used to encrypt the model during training.
--
-- 'projectVersionArn', 'projectVersionDescription_projectVersionArn' - The Amazon Resource Name (ARN) of the model version.
--
-- 'maxInferenceUnits', 'projectVersionDescription_maxInferenceUnits' - The maximum number of inference units Amazon Rekognition Custom Labels
-- uses to auto-scale the model. For more information, see
-- StartProjectVersion.
--
-- 'billableTrainingTimeInSeconds', 'projectVersionDescription_billableTrainingTimeInSeconds' - The duration, in seconds, that you were billed for a successful training
-- of the model version. This value is only returned if the model version
-- has been successfully trained.
--
-- 'statusMessage', 'projectVersionDescription_statusMessage' - A descriptive message for an error or warning that occurred.
--
-- 'outputConfig', 'projectVersionDescription_outputConfig' - The location where training results are saved.
newProjectVersionDescription ::
  ProjectVersionDescription
newProjectVersionDescription =
  ProjectVersionDescription'
    { trainingEndTimestamp =
        Prelude.Nothing,
      minInferenceUnits = Prelude.Nothing,
      sourceProjectVersionArn = Prelude.Nothing,
      status = Prelude.Nothing,
      testingDataResult = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      manifestSummary = Prelude.Nothing,
      trainingDataResult = Prelude.Nothing,
      evaluationResult = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      projectVersionArn = Prelude.Nothing,
      maxInferenceUnits = Prelude.Nothing,
      billableTrainingTimeInSeconds = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      outputConfig = Prelude.Nothing
    }

-- | The Unix date and time that training of the model ended.
projectVersionDescription_trainingEndTimestamp :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.UTCTime)
projectVersionDescription_trainingEndTimestamp = Lens.lens (\ProjectVersionDescription' {trainingEndTimestamp} -> trainingEndTimestamp) (\s@ProjectVersionDescription' {} a -> s {trainingEndTimestamp = a} :: ProjectVersionDescription) Prelude.. Lens.mapping Core._Time

-- | The minimum number of inference units used by the model. For more
-- information, see StartProjectVersion.
projectVersionDescription_minInferenceUnits :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Natural)
projectVersionDescription_minInferenceUnits = Lens.lens (\ProjectVersionDescription' {minInferenceUnits} -> minInferenceUnits) (\s@ProjectVersionDescription' {} a -> s {minInferenceUnits = a} :: ProjectVersionDescription)

-- | If the model version was copied from a different project,
-- @SourceProjectVersionArn@ contains the ARN of the source model version.
projectVersionDescription_sourceProjectVersionArn :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Text)
projectVersionDescription_sourceProjectVersionArn = Lens.lens (\ProjectVersionDescription' {sourceProjectVersionArn} -> sourceProjectVersionArn) (\s@ProjectVersionDescription' {} a -> s {sourceProjectVersionArn = a} :: ProjectVersionDescription)

-- | The current status of the model version.
projectVersionDescription_status :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe ProjectVersionStatus)
projectVersionDescription_status = Lens.lens (\ProjectVersionDescription' {status} -> status) (\s@ProjectVersionDescription' {} a -> s {status = a} :: ProjectVersionDescription)

-- | Contains information about the testing results.
projectVersionDescription_testingDataResult :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe TestingDataResult)
projectVersionDescription_testingDataResult = Lens.lens (\ProjectVersionDescription' {testingDataResult} -> testingDataResult) (\s@ProjectVersionDescription' {} a -> s {testingDataResult = a} :: ProjectVersionDescription)

-- | The Unix datetime for the date and time that training started.
projectVersionDescription_creationTimestamp :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.UTCTime)
projectVersionDescription_creationTimestamp = Lens.lens (\ProjectVersionDescription' {creationTimestamp} -> creationTimestamp) (\s@ProjectVersionDescription' {} a -> s {creationTimestamp = a} :: ProjectVersionDescription) Prelude.. Lens.mapping Core._Time

-- | The location of the summary manifest. The summary manifest provides
-- aggregate data validation results for the training and test datasets.
projectVersionDescription_manifestSummary :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe GroundTruthManifest)
projectVersionDescription_manifestSummary = Lens.lens (\ProjectVersionDescription' {manifestSummary} -> manifestSummary) (\s@ProjectVersionDescription' {} a -> s {manifestSummary = a} :: ProjectVersionDescription)

-- | Contains information about the training results.
projectVersionDescription_trainingDataResult :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe TrainingDataResult)
projectVersionDescription_trainingDataResult = Lens.lens (\ProjectVersionDescription' {trainingDataResult} -> trainingDataResult) (\s@ProjectVersionDescription' {} a -> s {trainingDataResult = a} :: ProjectVersionDescription)

-- | The training results. @EvaluationResult@ is only returned if training is
-- successful.
projectVersionDescription_evaluationResult :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe EvaluationResult)
projectVersionDescription_evaluationResult = Lens.lens (\ProjectVersionDescription' {evaluationResult} -> evaluationResult) (\s@ProjectVersionDescription' {} a -> s {evaluationResult = a} :: ProjectVersionDescription)

-- | The identifer for the AWS Key Management Service key (AWS KMS key) that
-- was used to encrypt the model during training.
projectVersionDescription_kmsKeyId :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Text)
projectVersionDescription_kmsKeyId = Lens.lens (\ProjectVersionDescription' {kmsKeyId} -> kmsKeyId) (\s@ProjectVersionDescription' {} a -> s {kmsKeyId = a} :: ProjectVersionDescription)

-- | The Amazon Resource Name (ARN) of the model version.
projectVersionDescription_projectVersionArn :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Text)
projectVersionDescription_projectVersionArn = Lens.lens (\ProjectVersionDescription' {projectVersionArn} -> projectVersionArn) (\s@ProjectVersionDescription' {} a -> s {projectVersionArn = a} :: ProjectVersionDescription)

-- | The maximum number of inference units Amazon Rekognition Custom Labels
-- uses to auto-scale the model. For more information, see
-- StartProjectVersion.
projectVersionDescription_maxInferenceUnits :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Natural)
projectVersionDescription_maxInferenceUnits = Lens.lens (\ProjectVersionDescription' {maxInferenceUnits} -> maxInferenceUnits) (\s@ProjectVersionDescription' {} a -> s {maxInferenceUnits = a} :: ProjectVersionDescription)

-- | The duration, in seconds, that you were billed for a successful training
-- of the model version. This value is only returned if the model version
-- has been successfully trained.
projectVersionDescription_billableTrainingTimeInSeconds :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Natural)
projectVersionDescription_billableTrainingTimeInSeconds = Lens.lens (\ProjectVersionDescription' {billableTrainingTimeInSeconds} -> billableTrainingTimeInSeconds) (\s@ProjectVersionDescription' {} a -> s {billableTrainingTimeInSeconds = a} :: ProjectVersionDescription)

-- | A descriptive message for an error or warning that occurred.
projectVersionDescription_statusMessage :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Text)
projectVersionDescription_statusMessage = Lens.lens (\ProjectVersionDescription' {statusMessage} -> statusMessage) (\s@ProjectVersionDescription' {} a -> s {statusMessage = a} :: ProjectVersionDescription)

-- | The location where training results are saved.
projectVersionDescription_outputConfig :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe OutputConfig)
projectVersionDescription_outputConfig = Lens.lens (\ProjectVersionDescription' {outputConfig} -> outputConfig) (\s@ProjectVersionDescription' {} a -> s {outputConfig = a} :: ProjectVersionDescription)

instance Core.FromJSON ProjectVersionDescription where
  parseJSON =
    Core.withObject
      "ProjectVersionDescription"
      ( \x ->
          ProjectVersionDescription'
            Prelude.<$> (x Core..:? "TrainingEndTimestamp")
            Prelude.<*> (x Core..:? "MinInferenceUnits")
            Prelude.<*> (x Core..:? "SourceProjectVersionArn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "TestingDataResult")
            Prelude.<*> (x Core..:? "CreationTimestamp")
            Prelude.<*> (x Core..:? "ManifestSummary")
            Prelude.<*> (x Core..:? "TrainingDataResult")
            Prelude.<*> (x Core..:? "EvaluationResult")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "ProjectVersionArn")
            Prelude.<*> (x Core..:? "MaxInferenceUnits")
            Prelude.<*> (x Core..:? "BillableTrainingTimeInSeconds")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "OutputConfig")
      )

instance Prelude.Hashable ProjectVersionDescription where
  hashWithSalt _salt ProjectVersionDescription' {..} =
    _salt `Prelude.hashWithSalt` trainingEndTimestamp
      `Prelude.hashWithSalt` minInferenceUnits
      `Prelude.hashWithSalt` sourceProjectVersionArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` testingDataResult
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` manifestSummary
      `Prelude.hashWithSalt` trainingDataResult
      `Prelude.hashWithSalt` evaluationResult
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` projectVersionArn
      `Prelude.hashWithSalt` maxInferenceUnits
      `Prelude.hashWithSalt` billableTrainingTimeInSeconds
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData ProjectVersionDescription where
  rnf ProjectVersionDescription' {..} =
    Prelude.rnf trainingEndTimestamp
      `Prelude.seq` Prelude.rnf minInferenceUnits
      `Prelude.seq` Prelude.rnf sourceProjectVersionArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf testingDataResult
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf manifestSummary
      `Prelude.seq` Prelude.rnf trainingDataResult
      `Prelude.seq` Prelude.rnf evaluationResult
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf projectVersionArn
      `Prelude.seq` Prelude.rnf maxInferenceUnits
      `Prelude.seq` Prelude.rnf billableTrainingTimeInSeconds
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf outputConfig
