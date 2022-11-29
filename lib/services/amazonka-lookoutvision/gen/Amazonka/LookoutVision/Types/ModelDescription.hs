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
-- Module      : Amazonka.LookoutVision.Types.ModelDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ModelDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutVision.Types.ModelPerformance
import Amazonka.LookoutVision.Types.ModelStatus
import Amazonka.LookoutVision.Types.OutputConfig
import Amazonka.LookoutVision.Types.OutputS3Object
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Lookout for Vision model.
--
-- /See:/ 'newModelDescription' smart constructor.
data ModelDescription = ModelDescription'
  { -- | The S3 location where Amazon Lookout for Vision saves the manifest file
    -- that was used to test the trained model and generate the performance
    -- scores.
    evaluationManifest :: Prelude.Maybe OutputS3Object,
    -- | The minimum number of inference units used by the model. For more
    -- information, see StartModel
    minInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | The version of the model
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The unix timestamp for the date and time that the evaluation ended.
    evaluationEndTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The status of the model.
    status :: Prelude.Maybe ModelStatus,
    -- | The description for the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unix timestamp for the date and time that the model was created.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Performance metrics for the model. Created during training.
    performance :: Prelude.Maybe ModelPerformance,
    -- | The Amazon Resource Name (ARN) of the model.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The S3 location where Amazon Lookout for Vision saves the performance
    -- metrics.
    evaluationResult :: Prelude.Maybe OutputS3Object,
    -- | The identifer for the AWS Key Management Service (AWS KMS) key that was
    -- used to encrypt the model during training.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of inference units Amazon Lookout for Vision uses to
    -- auto-scale the model. For more information, see StartModel.
    maxInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | The status message for the model.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The S3 location where Amazon Lookout for Vision saves model training
    -- files.
    outputConfig :: Prelude.Maybe OutputConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationManifest', 'modelDescription_evaluationManifest' - The S3 location where Amazon Lookout for Vision saves the manifest file
-- that was used to test the trained model and generate the performance
-- scores.
--
-- 'minInferenceUnits', 'modelDescription_minInferenceUnits' - The minimum number of inference units used by the model. For more
-- information, see StartModel
--
-- 'modelVersion', 'modelDescription_modelVersion' - The version of the model
--
-- 'evaluationEndTimestamp', 'modelDescription_evaluationEndTimestamp' - The unix timestamp for the date and time that the evaluation ended.
--
-- 'status', 'modelDescription_status' - The status of the model.
--
-- 'description', 'modelDescription_description' - The description for the model.
--
-- 'creationTimestamp', 'modelDescription_creationTimestamp' - The unix timestamp for the date and time that the model was created.
--
-- 'performance', 'modelDescription_performance' - Performance metrics for the model. Created during training.
--
-- 'modelArn', 'modelDescription_modelArn' - The Amazon Resource Name (ARN) of the model.
--
-- 'evaluationResult', 'modelDescription_evaluationResult' - The S3 location where Amazon Lookout for Vision saves the performance
-- metrics.
--
-- 'kmsKeyId', 'modelDescription_kmsKeyId' - The identifer for the AWS Key Management Service (AWS KMS) key that was
-- used to encrypt the model during training.
--
-- 'maxInferenceUnits', 'modelDescription_maxInferenceUnits' - The maximum number of inference units Amazon Lookout for Vision uses to
-- auto-scale the model. For more information, see StartModel.
--
-- 'statusMessage', 'modelDescription_statusMessage' - The status message for the model.
--
-- 'outputConfig', 'modelDescription_outputConfig' - The S3 location where Amazon Lookout for Vision saves model training
-- files.
newModelDescription ::
  ModelDescription
newModelDescription =
  ModelDescription'
    { evaluationManifest =
        Prelude.Nothing,
      minInferenceUnits = Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      evaluationEndTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      performance = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      evaluationResult = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      maxInferenceUnits = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      outputConfig = Prelude.Nothing
    }

-- | The S3 location where Amazon Lookout for Vision saves the manifest file
-- that was used to test the trained model and generate the performance
-- scores.
modelDescription_evaluationManifest :: Lens.Lens' ModelDescription (Prelude.Maybe OutputS3Object)
modelDescription_evaluationManifest = Lens.lens (\ModelDescription' {evaluationManifest} -> evaluationManifest) (\s@ModelDescription' {} a -> s {evaluationManifest = a} :: ModelDescription)

-- | The minimum number of inference units used by the model. For more
-- information, see StartModel
modelDescription_minInferenceUnits :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Natural)
modelDescription_minInferenceUnits = Lens.lens (\ModelDescription' {minInferenceUnits} -> minInferenceUnits) (\s@ModelDescription' {} a -> s {minInferenceUnits = a} :: ModelDescription)

-- | The version of the model
modelDescription_modelVersion :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Text)
modelDescription_modelVersion = Lens.lens (\ModelDescription' {modelVersion} -> modelVersion) (\s@ModelDescription' {} a -> s {modelVersion = a} :: ModelDescription)

-- | The unix timestamp for the date and time that the evaluation ended.
modelDescription_evaluationEndTimestamp :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.UTCTime)
modelDescription_evaluationEndTimestamp = Lens.lens (\ModelDescription' {evaluationEndTimestamp} -> evaluationEndTimestamp) (\s@ModelDescription' {} a -> s {evaluationEndTimestamp = a} :: ModelDescription) Prelude.. Lens.mapping Core._Time

-- | The status of the model.
modelDescription_status :: Lens.Lens' ModelDescription (Prelude.Maybe ModelStatus)
modelDescription_status = Lens.lens (\ModelDescription' {status} -> status) (\s@ModelDescription' {} a -> s {status = a} :: ModelDescription)

-- | The description for the model.
modelDescription_description :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Text)
modelDescription_description = Lens.lens (\ModelDescription' {description} -> description) (\s@ModelDescription' {} a -> s {description = a} :: ModelDescription)

-- | The unix timestamp for the date and time that the model was created.
modelDescription_creationTimestamp :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.UTCTime)
modelDescription_creationTimestamp = Lens.lens (\ModelDescription' {creationTimestamp} -> creationTimestamp) (\s@ModelDescription' {} a -> s {creationTimestamp = a} :: ModelDescription) Prelude.. Lens.mapping Core._Time

-- | Performance metrics for the model. Created during training.
modelDescription_performance :: Lens.Lens' ModelDescription (Prelude.Maybe ModelPerformance)
modelDescription_performance = Lens.lens (\ModelDescription' {performance} -> performance) (\s@ModelDescription' {} a -> s {performance = a} :: ModelDescription)

-- | The Amazon Resource Name (ARN) of the model.
modelDescription_modelArn :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Text)
modelDescription_modelArn = Lens.lens (\ModelDescription' {modelArn} -> modelArn) (\s@ModelDescription' {} a -> s {modelArn = a} :: ModelDescription)

-- | The S3 location where Amazon Lookout for Vision saves the performance
-- metrics.
modelDescription_evaluationResult :: Lens.Lens' ModelDescription (Prelude.Maybe OutputS3Object)
modelDescription_evaluationResult = Lens.lens (\ModelDescription' {evaluationResult} -> evaluationResult) (\s@ModelDescription' {} a -> s {evaluationResult = a} :: ModelDescription)

-- | The identifer for the AWS Key Management Service (AWS KMS) key that was
-- used to encrypt the model during training.
modelDescription_kmsKeyId :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Text)
modelDescription_kmsKeyId = Lens.lens (\ModelDescription' {kmsKeyId} -> kmsKeyId) (\s@ModelDescription' {} a -> s {kmsKeyId = a} :: ModelDescription)

-- | The maximum number of inference units Amazon Lookout for Vision uses to
-- auto-scale the model. For more information, see StartModel.
modelDescription_maxInferenceUnits :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Natural)
modelDescription_maxInferenceUnits = Lens.lens (\ModelDescription' {maxInferenceUnits} -> maxInferenceUnits) (\s@ModelDescription' {} a -> s {maxInferenceUnits = a} :: ModelDescription)

-- | The status message for the model.
modelDescription_statusMessage :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Text)
modelDescription_statusMessage = Lens.lens (\ModelDescription' {statusMessage} -> statusMessage) (\s@ModelDescription' {} a -> s {statusMessage = a} :: ModelDescription)

-- | The S3 location where Amazon Lookout for Vision saves model training
-- files.
modelDescription_outputConfig :: Lens.Lens' ModelDescription (Prelude.Maybe OutputConfig)
modelDescription_outputConfig = Lens.lens (\ModelDescription' {outputConfig} -> outputConfig) (\s@ModelDescription' {} a -> s {outputConfig = a} :: ModelDescription)

instance Core.FromJSON ModelDescription where
  parseJSON =
    Core.withObject
      "ModelDescription"
      ( \x ->
          ModelDescription'
            Prelude.<$> (x Core..:? "EvaluationManifest")
            Prelude.<*> (x Core..:? "MinInferenceUnits")
            Prelude.<*> (x Core..:? "ModelVersion")
            Prelude.<*> (x Core..:? "EvaluationEndTimestamp")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "CreationTimestamp")
            Prelude.<*> (x Core..:? "Performance")
            Prelude.<*> (x Core..:? "ModelArn")
            Prelude.<*> (x Core..:? "EvaluationResult")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "MaxInferenceUnits")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "OutputConfig")
      )

instance Prelude.Hashable ModelDescription where
  hashWithSalt _salt ModelDescription' {..} =
    _salt `Prelude.hashWithSalt` evaluationManifest
      `Prelude.hashWithSalt` minInferenceUnits
      `Prelude.hashWithSalt` modelVersion
      `Prelude.hashWithSalt` evaluationEndTimestamp
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` performance
      `Prelude.hashWithSalt` modelArn
      `Prelude.hashWithSalt` evaluationResult
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` maxInferenceUnits
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` outputConfig

instance Prelude.NFData ModelDescription where
  rnf ModelDescription' {..} =
    Prelude.rnf evaluationManifest
      `Prelude.seq` Prelude.rnf minInferenceUnits
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf evaluationEndTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf performance
      `Prelude.seq` Prelude.rnf modelArn
      `Prelude.seq` Prelude.rnf evaluationResult
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf maxInferenceUnits
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf outputConfig
