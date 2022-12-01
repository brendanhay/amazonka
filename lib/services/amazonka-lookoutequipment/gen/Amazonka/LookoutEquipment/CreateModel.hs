{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutEquipment.CreateModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an ML model for data inference.
--
-- A machine-learning (ML) model is a mathematical model that finds
-- patterns in your data. In Amazon Lookout for Equipment, the model learns
-- the patterns of normal behavior and detects abnormal behavior that could
-- be potential equipment failure (or maintenance events). The models are
-- made by analyzing normal data and abnormalities in machine behavior that
-- have already occurred.
--
-- Your model is trained using a portion of the data from your dataset and
-- uses that data to learn patterns of normal behavior and abnormal
-- patterns that lead to equipment failure. Another portion of the data is
-- used to evaluate the model\'s accuracy.
module Amazonka.LookoutEquipment.CreateModel
  ( -- * Creating a Request
    CreateModel (..),
    newCreateModel,

    -- * Request Lenses
    createModel_tags,
    createModel_serverSideKmsKeyId,
    createModel_roleArn,
    createModel_dataPreProcessingConfiguration,
    createModel_datasetSchema,
    createModel_labelsInputConfiguration,
    createModel_trainingDataStartTime,
    createModel_evaluationDataStartTime,
    createModel_trainingDataEndTime,
    createModel_evaluationDataEndTime,
    createModel_offCondition,
    createModel_modelName,
    createModel_datasetName,
    createModel_clientToken,

    -- * Destructuring the Response
    CreateModelResponse (..),
    newCreateModelResponse,

    -- * Response Lenses
    createModelResponse_status,
    createModelResponse_modelArn,
    createModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateModel' smart constructor.
data CreateModel = CreateModel'
  { -- | Any tags associated with the ML model being created.
    tags :: Prelude.Maybe [Tag],
    -- | Provides the identifier of the KMS key used to encrypt model data by
    -- Amazon Lookout for Equipment.
    serverSideKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a role with permission to access the
    -- data source being used to create the ML model.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration is the @TargetSamplingRate@, which is the sampling
    -- rate of the data after post processing by Amazon Lookout for Equipment.
    -- For example, if you provide data that has been collected at a 1 second
    -- level and you want the system to resample the data at a 1 minute rate
    -- before training, the @TargetSamplingRate@ is 1 minute.
    --
    -- When providing a value for the @TargetSamplingRate@, you must attach the
    -- prefix \"PT\" to the rate you want. The value for a 1 second rate is
    -- therefore /PT1S/, the value for a 15 minute rate is /PT15M/, and the
    -- value for a 1 hour rate is /PT1H/
    dataPreProcessingConfiguration :: Prelude.Maybe DataPreProcessingConfiguration,
    -- | The data schema for the ML model being created.
    datasetSchema :: Prelude.Maybe DatasetSchema,
    -- | The input configuration for the labels being used for the ML model
    -- that\'s being created.
    labelsInputConfiguration :: Prelude.Maybe LabelsInputConfiguration,
    -- | Indicates the time reference in the dataset that should be used to begin
    -- the subset of training data for the ML model.
    trainingDataStartTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates the time reference in the dataset that should be used to begin
    -- the subset of evaluation data for the ML model.
    evaluationDataStartTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates the time reference in the dataset that should be used to end
    -- the subset of training data for the ML model.
    trainingDataEndTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates the time reference in the dataset that should be used to end
    -- the subset of evaluation data for the ML model.
    evaluationDataEndTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates that the asset associated with this sensor has been shut off.
    -- As long as this condition is met, Lookout for Equipment will not use
    -- data from this asset for training, evaluation, or inference.
    offCondition :: Prelude.Maybe Prelude.Text,
    -- | The name for the ML model to be created.
    modelName :: Prelude.Text,
    -- | The name of the dataset for the ML model being created.
    datasetName :: Prelude.Text,
    -- | A unique identifier for the request. If you do not set the client
    -- request token, Amazon Lookout for Equipment generates one.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createModel_tags' - Any tags associated with the ML model being created.
--
-- 'serverSideKmsKeyId', 'createModel_serverSideKmsKeyId' - Provides the identifier of the KMS key used to encrypt model data by
-- Amazon Lookout for Equipment.
--
-- 'roleArn', 'createModel_roleArn' - The Amazon Resource Name (ARN) of a role with permission to access the
-- data source being used to create the ML model.
--
-- 'dataPreProcessingConfiguration', 'createModel_dataPreProcessingConfiguration' - The configuration is the @TargetSamplingRate@, which is the sampling
-- rate of the data after post processing by Amazon Lookout for Equipment.
-- For example, if you provide data that has been collected at a 1 second
-- level and you want the system to resample the data at a 1 minute rate
-- before training, the @TargetSamplingRate@ is 1 minute.
--
-- When providing a value for the @TargetSamplingRate@, you must attach the
-- prefix \"PT\" to the rate you want. The value for a 1 second rate is
-- therefore /PT1S/, the value for a 15 minute rate is /PT15M/, and the
-- value for a 1 hour rate is /PT1H/
--
-- 'datasetSchema', 'createModel_datasetSchema' - The data schema for the ML model being created.
--
-- 'labelsInputConfiguration', 'createModel_labelsInputConfiguration' - The input configuration for the labels being used for the ML model
-- that\'s being created.
--
-- 'trainingDataStartTime', 'createModel_trainingDataStartTime' - Indicates the time reference in the dataset that should be used to begin
-- the subset of training data for the ML model.
--
-- 'evaluationDataStartTime', 'createModel_evaluationDataStartTime' - Indicates the time reference in the dataset that should be used to begin
-- the subset of evaluation data for the ML model.
--
-- 'trainingDataEndTime', 'createModel_trainingDataEndTime' - Indicates the time reference in the dataset that should be used to end
-- the subset of training data for the ML model.
--
-- 'evaluationDataEndTime', 'createModel_evaluationDataEndTime' - Indicates the time reference in the dataset that should be used to end
-- the subset of evaluation data for the ML model.
--
-- 'offCondition', 'createModel_offCondition' - Indicates that the asset associated with this sensor has been shut off.
-- As long as this condition is met, Lookout for Equipment will not use
-- data from this asset for training, evaluation, or inference.
--
-- 'modelName', 'createModel_modelName' - The name for the ML model to be created.
--
-- 'datasetName', 'createModel_datasetName' - The name of the dataset for the ML model being created.
--
-- 'clientToken', 'createModel_clientToken' - A unique identifier for the request. If you do not set the client
-- request token, Amazon Lookout for Equipment generates one.
newCreateModel ::
  -- | 'modelName'
  Prelude.Text ->
  -- | 'datasetName'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateModel
newCreateModel
  pModelName_
  pDatasetName_
  pClientToken_ =
    CreateModel'
      { tags = Prelude.Nothing,
        serverSideKmsKeyId = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        dataPreProcessingConfiguration = Prelude.Nothing,
        datasetSchema = Prelude.Nothing,
        labelsInputConfiguration = Prelude.Nothing,
        trainingDataStartTime = Prelude.Nothing,
        evaluationDataStartTime = Prelude.Nothing,
        trainingDataEndTime = Prelude.Nothing,
        evaluationDataEndTime = Prelude.Nothing,
        offCondition = Prelude.Nothing,
        modelName = pModelName_,
        datasetName = pDatasetName_,
        clientToken = pClientToken_
      }

-- | Any tags associated with the ML model being created.
createModel_tags :: Lens.Lens' CreateModel (Prelude.Maybe [Tag])
createModel_tags = Lens.lens (\CreateModel' {tags} -> tags) (\s@CreateModel' {} a -> s {tags = a} :: CreateModel) Prelude.. Lens.mapping Lens.coerced

-- | Provides the identifier of the KMS key used to encrypt model data by
-- Amazon Lookout for Equipment.
createModel_serverSideKmsKeyId :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.Text)
createModel_serverSideKmsKeyId = Lens.lens (\CreateModel' {serverSideKmsKeyId} -> serverSideKmsKeyId) (\s@CreateModel' {} a -> s {serverSideKmsKeyId = a} :: CreateModel)

-- | The Amazon Resource Name (ARN) of a role with permission to access the
-- data source being used to create the ML model.
createModel_roleArn :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.Text)
createModel_roleArn = Lens.lens (\CreateModel' {roleArn} -> roleArn) (\s@CreateModel' {} a -> s {roleArn = a} :: CreateModel)

-- | The configuration is the @TargetSamplingRate@, which is the sampling
-- rate of the data after post processing by Amazon Lookout for Equipment.
-- For example, if you provide data that has been collected at a 1 second
-- level and you want the system to resample the data at a 1 minute rate
-- before training, the @TargetSamplingRate@ is 1 minute.
--
-- When providing a value for the @TargetSamplingRate@, you must attach the
-- prefix \"PT\" to the rate you want. The value for a 1 second rate is
-- therefore /PT1S/, the value for a 15 minute rate is /PT15M/, and the
-- value for a 1 hour rate is /PT1H/
createModel_dataPreProcessingConfiguration :: Lens.Lens' CreateModel (Prelude.Maybe DataPreProcessingConfiguration)
createModel_dataPreProcessingConfiguration = Lens.lens (\CreateModel' {dataPreProcessingConfiguration} -> dataPreProcessingConfiguration) (\s@CreateModel' {} a -> s {dataPreProcessingConfiguration = a} :: CreateModel)

-- | The data schema for the ML model being created.
createModel_datasetSchema :: Lens.Lens' CreateModel (Prelude.Maybe DatasetSchema)
createModel_datasetSchema = Lens.lens (\CreateModel' {datasetSchema} -> datasetSchema) (\s@CreateModel' {} a -> s {datasetSchema = a} :: CreateModel)

-- | The input configuration for the labels being used for the ML model
-- that\'s being created.
createModel_labelsInputConfiguration :: Lens.Lens' CreateModel (Prelude.Maybe LabelsInputConfiguration)
createModel_labelsInputConfiguration = Lens.lens (\CreateModel' {labelsInputConfiguration} -> labelsInputConfiguration) (\s@CreateModel' {} a -> s {labelsInputConfiguration = a} :: CreateModel)

-- | Indicates the time reference in the dataset that should be used to begin
-- the subset of training data for the ML model.
createModel_trainingDataStartTime :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.UTCTime)
createModel_trainingDataStartTime = Lens.lens (\CreateModel' {trainingDataStartTime} -> trainingDataStartTime) (\s@CreateModel' {} a -> s {trainingDataStartTime = a} :: CreateModel) Prelude.. Lens.mapping Core._Time

-- | Indicates the time reference in the dataset that should be used to begin
-- the subset of evaluation data for the ML model.
createModel_evaluationDataStartTime :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.UTCTime)
createModel_evaluationDataStartTime = Lens.lens (\CreateModel' {evaluationDataStartTime} -> evaluationDataStartTime) (\s@CreateModel' {} a -> s {evaluationDataStartTime = a} :: CreateModel) Prelude.. Lens.mapping Core._Time

-- | Indicates the time reference in the dataset that should be used to end
-- the subset of training data for the ML model.
createModel_trainingDataEndTime :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.UTCTime)
createModel_trainingDataEndTime = Lens.lens (\CreateModel' {trainingDataEndTime} -> trainingDataEndTime) (\s@CreateModel' {} a -> s {trainingDataEndTime = a} :: CreateModel) Prelude.. Lens.mapping Core._Time

-- | Indicates the time reference in the dataset that should be used to end
-- the subset of evaluation data for the ML model.
createModel_evaluationDataEndTime :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.UTCTime)
createModel_evaluationDataEndTime = Lens.lens (\CreateModel' {evaluationDataEndTime} -> evaluationDataEndTime) (\s@CreateModel' {} a -> s {evaluationDataEndTime = a} :: CreateModel) Prelude.. Lens.mapping Core._Time

-- | Indicates that the asset associated with this sensor has been shut off.
-- As long as this condition is met, Lookout for Equipment will not use
-- data from this asset for training, evaluation, or inference.
createModel_offCondition :: Lens.Lens' CreateModel (Prelude.Maybe Prelude.Text)
createModel_offCondition = Lens.lens (\CreateModel' {offCondition} -> offCondition) (\s@CreateModel' {} a -> s {offCondition = a} :: CreateModel)

-- | The name for the ML model to be created.
createModel_modelName :: Lens.Lens' CreateModel Prelude.Text
createModel_modelName = Lens.lens (\CreateModel' {modelName} -> modelName) (\s@CreateModel' {} a -> s {modelName = a} :: CreateModel)

-- | The name of the dataset for the ML model being created.
createModel_datasetName :: Lens.Lens' CreateModel Prelude.Text
createModel_datasetName = Lens.lens (\CreateModel' {datasetName} -> datasetName) (\s@CreateModel' {} a -> s {datasetName = a} :: CreateModel)

-- | A unique identifier for the request. If you do not set the client
-- request token, Amazon Lookout for Equipment generates one.
createModel_clientToken :: Lens.Lens' CreateModel Prelude.Text
createModel_clientToken = Lens.lens (\CreateModel' {clientToken} -> clientToken) (\s@CreateModel' {} a -> s {clientToken = a} :: CreateModel)

instance Core.AWSRequest CreateModel where
  type AWSResponse CreateModel = CreateModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "ModelArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateModel where
  hashWithSalt _salt CreateModel' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` serverSideKmsKeyId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` dataPreProcessingConfiguration
      `Prelude.hashWithSalt` datasetSchema
      `Prelude.hashWithSalt` labelsInputConfiguration
      `Prelude.hashWithSalt` trainingDataStartTime
      `Prelude.hashWithSalt` evaluationDataStartTime
      `Prelude.hashWithSalt` trainingDataEndTime
      `Prelude.hashWithSalt` evaluationDataEndTime
      `Prelude.hashWithSalt` offCondition
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateModel where
  rnf CreateModel' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serverSideKmsKeyId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf dataPreProcessingConfiguration
      `Prelude.seq` Prelude.rnf datasetSchema
      `Prelude.seq` Prelude.rnf labelsInputConfiguration
      `Prelude.seq` Prelude.rnf trainingDataStartTime
      `Prelude.seq` Prelude.rnf evaluationDataStartTime
      `Prelude.seq` Prelude.rnf trainingDataEndTime
      `Prelude.seq` Prelude.rnf evaluationDataEndTime
      `Prelude.seq` Prelude.rnf offCondition
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf clientToken

instance Core.ToHeaders CreateModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLookoutEquipmentFrontendService.CreateModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateModel where
  toJSON CreateModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ServerSideKmsKeyId" Core..=)
              Prelude.<$> serverSideKmsKeyId,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("DataPreProcessingConfiguration" Core..=)
              Prelude.<$> dataPreProcessingConfiguration,
            ("DatasetSchema" Core..=) Prelude.<$> datasetSchema,
            ("LabelsInputConfiguration" Core..=)
              Prelude.<$> labelsInputConfiguration,
            ("TrainingDataStartTime" Core..=)
              Prelude.<$> trainingDataStartTime,
            ("EvaluationDataStartTime" Core..=)
              Prelude.<$> evaluationDataStartTime,
            ("TrainingDataEndTime" Core..=)
              Prelude.<$> trainingDataEndTime,
            ("EvaluationDataEndTime" Core..=)
              Prelude.<$> evaluationDataEndTime,
            ("OffCondition" Core..=) Prelude.<$> offCondition,
            Prelude.Just ("ModelName" Core..= modelName),
            Prelude.Just ("DatasetName" Core..= datasetName),
            Prelude.Just ("ClientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath CreateModel where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelResponse' smart constructor.
data CreateModelResponse = CreateModelResponse'
  { -- | Indicates the status of the @CreateModel@ operation.
    status :: Prelude.Maybe ModelStatus,
    -- | The Amazon Resource Name (ARN) of the model being created.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createModelResponse_status' - Indicates the status of the @CreateModel@ operation.
--
-- 'modelArn', 'createModelResponse_modelArn' - The Amazon Resource Name (ARN) of the model being created.
--
-- 'httpStatus', 'createModelResponse_httpStatus' - The response's http status code.
newCreateModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateModelResponse
newCreateModelResponse pHttpStatus_ =
  CreateModelResponse'
    { status = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates the status of the @CreateModel@ operation.
createModelResponse_status :: Lens.Lens' CreateModelResponse (Prelude.Maybe ModelStatus)
createModelResponse_status = Lens.lens (\CreateModelResponse' {status} -> status) (\s@CreateModelResponse' {} a -> s {status = a} :: CreateModelResponse)

-- | The Amazon Resource Name (ARN) of the model being created.
createModelResponse_modelArn :: Lens.Lens' CreateModelResponse (Prelude.Maybe Prelude.Text)
createModelResponse_modelArn = Lens.lens (\CreateModelResponse' {modelArn} -> modelArn) (\s@CreateModelResponse' {} a -> s {modelArn = a} :: CreateModelResponse)

-- | The response's http status code.
createModelResponse_httpStatus :: Lens.Lens' CreateModelResponse Prelude.Int
createModelResponse_httpStatus = Lens.lens (\CreateModelResponse' {httpStatus} -> httpStatus) (\s@CreateModelResponse' {} a -> s {httpStatus = a} :: CreateModelResponse)

instance Prelude.NFData CreateModelResponse where
  rnf CreateModelResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf modelArn
      `Prelude.seq` Prelude.rnf httpStatus
