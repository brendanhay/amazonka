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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.ModelDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutVision.Types.ModelPerformance
import Amazonka.LookoutVision.Types.ModelStatus
import Amazonka.LookoutVision.Types.OutputConfig
import Amazonka.LookoutVision.Types.OutputS3Object
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Lookout for Vision model.
--
-- /See:/ 'newModelDescription' smart constructor.
data ModelDescription = ModelDescription'
  { -- | The status of the model.
    status :: Prelude.Maybe ModelStatus,
    -- | The S3 location where Amazon Lookout for Vision saves the performance
    -- metrics.
    evaluationResult :: Prelude.Maybe OutputS3Object,
    -- | The unix timestamp for the date and time that the evaluation ended.
    evaluationEndTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the model.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | Performance metrics for the model. Created during training.
    performance :: Prelude.Maybe ModelPerformance,
    -- | The identifer for the AWS Key Management Service (AWS KMS) key that was
    -- used to encrypt the model during training.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The status message for the model.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The unix timestamp for the date and time that the model was created.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The S3 location where Amazon Lookout for Vision saves model training
    -- files.
    outputConfig :: Prelude.Maybe OutputConfig,
    -- | The version of the model
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The description for the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The S3 location where Amazon Lookout for Vision saves the manifest file
    -- that was used to test the trained model and generate the performance
    -- scores.
    evaluationManifest :: Prelude.Maybe OutputS3Object
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
-- 'status', 'modelDescription_status' - The status of the model.
--
-- 'evaluationResult', 'modelDescription_evaluationResult' - The S3 location where Amazon Lookout for Vision saves the performance
-- metrics.
--
-- 'evaluationEndTimestamp', 'modelDescription_evaluationEndTimestamp' - The unix timestamp for the date and time that the evaluation ended.
--
-- 'modelArn', 'modelDescription_modelArn' - The Amazon Resource Name (ARN) of the model.
--
-- 'performance', 'modelDescription_performance' - Performance metrics for the model. Created during training.
--
-- 'kmsKeyId', 'modelDescription_kmsKeyId' - The identifer for the AWS Key Management Service (AWS KMS) key that was
-- used to encrypt the model during training.
--
-- 'statusMessage', 'modelDescription_statusMessage' - The status message for the model.
--
-- 'creationTimestamp', 'modelDescription_creationTimestamp' - The unix timestamp for the date and time that the model was created.
--
-- 'outputConfig', 'modelDescription_outputConfig' - The S3 location where Amazon Lookout for Vision saves model training
-- files.
--
-- 'modelVersion', 'modelDescription_modelVersion' - The version of the model
--
-- 'description', 'modelDescription_description' - The description for the model.
--
-- 'evaluationManifest', 'modelDescription_evaluationManifest' - The S3 location where Amazon Lookout for Vision saves the manifest file
-- that was used to test the trained model and generate the performance
-- scores.
newModelDescription ::
  ModelDescription
newModelDescription =
  ModelDescription'
    { status = Prelude.Nothing,
      evaluationResult = Prelude.Nothing,
      evaluationEndTimestamp = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      performance = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      outputConfig = Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      description = Prelude.Nothing,
      evaluationManifest = Prelude.Nothing
    }

-- | The status of the model.
modelDescription_status :: Lens.Lens' ModelDescription (Prelude.Maybe ModelStatus)
modelDescription_status = Lens.lens (\ModelDescription' {status} -> status) (\s@ModelDescription' {} a -> s {status = a} :: ModelDescription)

-- | The S3 location where Amazon Lookout for Vision saves the performance
-- metrics.
modelDescription_evaluationResult :: Lens.Lens' ModelDescription (Prelude.Maybe OutputS3Object)
modelDescription_evaluationResult = Lens.lens (\ModelDescription' {evaluationResult} -> evaluationResult) (\s@ModelDescription' {} a -> s {evaluationResult = a} :: ModelDescription)

-- | The unix timestamp for the date and time that the evaluation ended.
modelDescription_evaluationEndTimestamp :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.UTCTime)
modelDescription_evaluationEndTimestamp = Lens.lens (\ModelDescription' {evaluationEndTimestamp} -> evaluationEndTimestamp) (\s@ModelDescription' {} a -> s {evaluationEndTimestamp = a} :: ModelDescription) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the model.
modelDescription_modelArn :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Text)
modelDescription_modelArn = Lens.lens (\ModelDescription' {modelArn} -> modelArn) (\s@ModelDescription' {} a -> s {modelArn = a} :: ModelDescription)

-- | Performance metrics for the model. Created during training.
modelDescription_performance :: Lens.Lens' ModelDescription (Prelude.Maybe ModelPerformance)
modelDescription_performance = Lens.lens (\ModelDescription' {performance} -> performance) (\s@ModelDescription' {} a -> s {performance = a} :: ModelDescription)

-- | The identifer for the AWS Key Management Service (AWS KMS) key that was
-- used to encrypt the model during training.
modelDescription_kmsKeyId :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Text)
modelDescription_kmsKeyId = Lens.lens (\ModelDescription' {kmsKeyId} -> kmsKeyId) (\s@ModelDescription' {} a -> s {kmsKeyId = a} :: ModelDescription)

-- | The status message for the model.
modelDescription_statusMessage :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Text)
modelDescription_statusMessage = Lens.lens (\ModelDescription' {statusMessage} -> statusMessage) (\s@ModelDescription' {} a -> s {statusMessage = a} :: ModelDescription)

-- | The unix timestamp for the date and time that the model was created.
modelDescription_creationTimestamp :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.UTCTime)
modelDescription_creationTimestamp = Lens.lens (\ModelDescription' {creationTimestamp} -> creationTimestamp) (\s@ModelDescription' {} a -> s {creationTimestamp = a} :: ModelDescription) Prelude.. Lens.mapping Core._Time

-- | The S3 location where Amazon Lookout for Vision saves model training
-- files.
modelDescription_outputConfig :: Lens.Lens' ModelDescription (Prelude.Maybe OutputConfig)
modelDescription_outputConfig = Lens.lens (\ModelDescription' {outputConfig} -> outputConfig) (\s@ModelDescription' {} a -> s {outputConfig = a} :: ModelDescription)

-- | The version of the model
modelDescription_modelVersion :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Text)
modelDescription_modelVersion = Lens.lens (\ModelDescription' {modelVersion} -> modelVersion) (\s@ModelDescription' {} a -> s {modelVersion = a} :: ModelDescription)

-- | The description for the model.
modelDescription_description :: Lens.Lens' ModelDescription (Prelude.Maybe Prelude.Text)
modelDescription_description = Lens.lens (\ModelDescription' {description} -> description) (\s@ModelDescription' {} a -> s {description = a} :: ModelDescription)

-- | The S3 location where Amazon Lookout for Vision saves the manifest file
-- that was used to test the trained model and generate the performance
-- scores.
modelDescription_evaluationManifest :: Lens.Lens' ModelDescription (Prelude.Maybe OutputS3Object)
modelDescription_evaluationManifest = Lens.lens (\ModelDescription' {evaluationManifest} -> evaluationManifest) (\s@ModelDescription' {} a -> s {evaluationManifest = a} :: ModelDescription)

instance Core.FromJSON ModelDescription where
  parseJSON =
    Core.withObject
      "ModelDescription"
      ( \x ->
          ModelDescription'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "EvaluationResult")
            Prelude.<*> (x Core..:? "EvaluationEndTimestamp")
            Prelude.<*> (x Core..:? "ModelArn")
            Prelude.<*> (x Core..:? "Performance")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "CreationTimestamp")
            Prelude.<*> (x Core..:? "OutputConfig")
            Prelude.<*> (x Core..:? "ModelVersion")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "EvaluationManifest")
      )

instance Prelude.Hashable ModelDescription

instance Prelude.NFData ModelDescription
