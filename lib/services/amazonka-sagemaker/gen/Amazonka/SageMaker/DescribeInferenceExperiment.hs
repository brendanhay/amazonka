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
-- Module      : Amazonka.SageMaker.DescribeInferenceExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about an inference experiment.
module Amazonka.SageMaker.DescribeInferenceExperiment
  ( -- * Creating a Request
    DescribeInferenceExperiment (..),
    newDescribeInferenceExperiment,

    -- * Request Lenses
    describeInferenceExperiment_name,

    -- * Destructuring the Response
    DescribeInferenceExperimentResponse (..),
    newDescribeInferenceExperimentResponse,

    -- * Response Lenses
    describeInferenceExperimentResponse_completionTime,
    describeInferenceExperimentResponse_creationTime,
    describeInferenceExperimentResponse_dataStorageConfig,
    describeInferenceExperimentResponse_description,
    describeInferenceExperimentResponse_kmsKey,
    describeInferenceExperimentResponse_lastModifiedTime,
    describeInferenceExperimentResponse_roleArn,
    describeInferenceExperimentResponse_schedule,
    describeInferenceExperimentResponse_shadowModeConfig,
    describeInferenceExperimentResponse_statusReason,
    describeInferenceExperimentResponse_httpStatus,
    describeInferenceExperimentResponse_arn,
    describeInferenceExperimentResponse_name,
    describeInferenceExperimentResponse_type,
    describeInferenceExperimentResponse_status,
    describeInferenceExperimentResponse_endpointMetadata,
    describeInferenceExperimentResponse_modelVariants,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeInferenceExperiment' smart constructor.
data DescribeInferenceExperiment = DescribeInferenceExperiment'
  { -- | The name of the inference experiment to describe.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInferenceExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeInferenceExperiment_name' - The name of the inference experiment to describe.
newDescribeInferenceExperiment ::
  -- | 'name'
  Prelude.Text ->
  DescribeInferenceExperiment
newDescribeInferenceExperiment pName_ =
  DescribeInferenceExperiment' {name = pName_}

-- | The name of the inference experiment to describe.
describeInferenceExperiment_name :: Lens.Lens' DescribeInferenceExperiment Prelude.Text
describeInferenceExperiment_name = Lens.lens (\DescribeInferenceExperiment' {name} -> name) (\s@DescribeInferenceExperiment' {} a -> s {name = a} :: DescribeInferenceExperiment)

instance Core.AWSRequest DescribeInferenceExperiment where
  type
    AWSResponse DescribeInferenceExperiment =
      DescribeInferenceExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInferenceExperimentResponse'
            Prelude.<$> (x Data..?> "CompletionTime")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DataStorageConfig")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "KmsKey")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "Schedule")
            Prelude.<*> (x Data..?> "ShadowModeConfig")
            Prelude.<*> (x Data..?> "StatusReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "Name")
            Prelude.<*> (x Data..:> "Type")
            Prelude.<*> (x Data..:> "Status")
            Prelude.<*> (x Data..:> "EndpointMetadata")
            Prelude.<*> (x Data..?> "ModelVariants" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeInferenceExperiment where
  hashWithSalt _salt DescribeInferenceExperiment' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeInferenceExperiment where
  rnf DescribeInferenceExperiment' {..} =
    Prelude.rnf name

instance Data.ToHeaders DescribeInferenceExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeInferenceExperiment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeInferenceExperiment where
  toJSON DescribeInferenceExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DescribeInferenceExperiment where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInferenceExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInferenceExperimentResponse' smart constructor.
data DescribeInferenceExperimentResponse = DescribeInferenceExperimentResponse'
  { -- | The timestamp at which the inference experiment was completed.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp at which you created the inference experiment.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon S3 location and configuration for storing inference request
    -- and response data.
    dataStorageConfig :: Prelude.Maybe InferenceExperimentDataStorageConfig,
    -- | The description of the inference experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that Amazon SageMaker uses to encrypt data on the storage volume
    -- attached to the ML compute instance that hosts the endpoint. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateInferenceExperiment.html CreateInferenceExperiment>.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which you last modified the inference experiment.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the IAM role that Amazon SageMaker can assume to access model
    -- artifacts and container images, and manage Amazon SageMaker Inference
    -- endpoints for model deployment.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The duration for which the inference experiment ran or will run.
    schedule :: Prelude.Maybe InferenceExperimentSchedule,
    -- | The configuration of @ShadowMode@ inference experiment type, which shows
    -- the production variant that takes all the inference requests, and the
    -- shadow variant to which Amazon SageMaker replicates a percentage of the
    -- inference requests. For the shadow variant it also shows the percentage
    -- of requests that Amazon SageMaker replicates.
    shadowModeConfig :: Prelude.Maybe ShadowModeConfig,
    -- | The error message or client-specified @Reason@ from the
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_StopInferenceExperiment.html StopInferenceExperiment>
    -- API, that explains the status of the inference experiment.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the inference experiment being described.
    arn :: Prelude.Text,
    -- | The name of the inference experiment.
    name :: Prelude.Text,
    -- | The type of the inference experiment.
    type' :: InferenceExperimentType,
    -- | The status of the inference experiment. The following are the possible
    -- statuses for an inference experiment:
    --
    -- -   @Creating@ - Amazon SageMaker is creating your experiment.
    --
    -- -   @Created@ - Amazon SageMaker has finished the creation of your
    --     experiment and will begin the experiment at the scheduled time.
    --
    -- -   @Updating@ - When you make changes to your experiment, your
    --     experiment shows as updating.
    --
    -- -   @Starting@ - Amazon SageMaker is beginning your experiment.
    --
    -- -   @Running@ - Your experiment is in progress.
    --
    -- -   @Stopping@ - Amazon SageMaker is stopping your experiment.
    --
    -- -   @Completed@ - Your experiment has completed.
    --
    -- -   @Cancelled@ - When you conclude your experiment early using the
    --     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_StopInferenceExperiment.html StopInferenceExperiment>
    --     API, or if any operation fails with an unexpected error, it shows as
    --     cancelled.
    status :: InferenceExperimentStatus,
    -- | The metadata of the endpoint on which the inference experiment ran.
    endpointMetadata :: EndpointMetadata,
    -- | An array of @ModelVariantConfigSummary@ objects. There is one for each
    -- variant in the inference experiment. Each @ModelVariantConfigSummary@
    -- object in the array describes the infrastructure configuration for
    -- deploying the corresponding variant.
    modelVariants :: [ModelVariantConfigSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInferenceExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'describeInferenceExperimentResponse_completionTime' - The timestamp at which the inference experiment was completed.
--
-- 'creationTime', 'describeInferenceExperimentResponse_creationTime' - The timestamp at which you created the inference experiment.
--
-- 'dataStorageConfig', 'describeInferenceExperimentResponse_dataStorageConfig' - The Amazon S3 location and configuration for storing inference request
-- and response data.
--
-- 'description', 'describeInferenceExperimentResponse_description' - The description of the inference experiment.
--
-- 'kmsKey', 'describeInferenceExperimentResponse_kmsKey' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance that hosts the endpoint. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateInferenceExperiment.html CreateInferenceExperiment>.
--
-- 'lastModifiedTime', 'describeInferenceExperimentResponse_lastModifiedTime' - The timestamp at which you last modified the inference experiment.
--
-- 'roleArn', 'describeInferenceExperimentResponse_roleArn' - The ARN of the IAM role that Amazon SageMaker can assume to access model
-- artifacts and container images, and manage Amazon SageMaker Inference
-- endpoints for model deployment.
--
-- 'schedule', 'describeInferenceExperimentResponse_schedule' - The duration for which the inference experiment ran or will run.
--
-- 'shadowModeConfig', 'describeInferenceExperimentResponse_shadowModeConfig' - The configuration of @ShadowMode@ inference experiment type, which shows
-- the production variant that takes all the inference requests, and the
-- shadow variant to which Amazon SageMaker replicates a percentage of the
-- inference requests. For the shadow variant it also shows the percentage
-- of requests that Amazon SageMaker replicates.
--
-- 'statusReason', 'describeInferenceExperimentResponse_statusReason' - The error message or client-specified @Reason@ from the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_StopInferenceExperiment.html StopInferenceExperiment>
-- API, that explains the status of the inference experiment.
--
-- 'httpStatus', 'describeInferenceExperimentResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'describeInferenceExperimentResponse_arn' - The ARN of the inference experiment being described.
--
-- 'name', 'describeInferenceExperimentResponse_name' - The name of the inference experiment.
--
-- 'type'', 'describeInferenceExperimentResponse_type' - The type of the inference experiment.
--
-- 'status', 'describeInferenceExperimentResponse_status' - The status of the inference experiment. The following are the possible
-- statuses for an inference experiment:
--
-- -   @Creating@ - Amazon SageMaker is creating your experiment.
--
-- -   @Created@ - Amazon SageMaker has finished the creation of your
--     experiment and will begin the experiment at the scheduled time.
--
-- -   @Updating@ - When you make changes to your experiment, your
--     experiment shows as updating.
--
-- -   @Starting@ - Amazon SageMaker is beginning your experiment.
--
-- -   @Running@ - Your experiment is in progress.
--
-- -   @Stopping@ - Amazon SageMaker is stopping your experiment.
--
-- -   @Completed@ - Your experiment has completed.
--
-- -   @Cancelled@ - When you conclude your experiment early using the
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_StopInferenceExperiment.html StopInferenceExperiment>
--     API, or if any operation fails with an unexpected error, it shows as
--     cancelled.
--
-- 'endpointMetadata', 'describeInferenceExperimentResponse_endpointMetadata' - The metadata of the endpoint on which the inference experiment ran.
--
-- 'modelVariants', 'describeInferenceExperimentResponse_modelVariants' - An array of @ModelVariantConfigSummary@ objects. There is one for each
-- variant in the inference experiment. Each @ModelVariantConfigSummary@
-- object in the array describes the infrastructure configuration for
-- deploying the corresponding variant.
newDescribeInferenceExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  InferenceExperimentType ->
  -- | 'status'
  InferenceExperimentStatus ->
  -- | 'endpointMetadata'
  EndpointMetadata ->
  DescribeInferenceExperimentResponse
newDescribeInferenceExperimentResponse
  pHttpStatus_
  pArn_
  pName_
  pType_
  pStatus_
  pEndpointMetadata_ =
    DescribeInferenceExperimentResponse'
      { completionTime =
          Prelude.Nothing,
        creationTime = Prelude.Nothing,
        dataStorageConfig = Prelude.Nothing,
        description = Prelude.Nothing,
        kmsKey = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        schedule = Prelude.Nothing,
        shadowModeConfig = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        name = pName_,
        type' = pType_,
        status = pStatus_,
        endpointMetadata = pEndpointMetadata_,
        modelVariants = Prelude.mempty
      }

-- | The timestamp at which the inference experiment was completed.
describeInferenceExperimentResponse_completionTime :: Lens.Lens' DescribeInferenceExperimentResponse (Prelude.Maybe Prelude.UTCTime)
describeInferenceExperimentResponse_completionTime = Lens.lens (\DescribeInferenceExperimentResponse' {completionTime} -> completionTime) (\s@DescribeInferenceExperimentResponse' {} a -> s {completionTime = a} :: DescribeInferenceExperimentResponse) Prelude.. Lens.mapping Data._Time

-- | The timestamp at which you created the inference experiment.
describeInferenceExperimentResponse_creationTime :: Lens.Lens' DescribeInferenceExperimentResponse (Prelude.Maybe Prelude.UTCTime)
describeInferenceExperimentResponse_creationTime = Lens.lens (\DescribeInferenceExperimentResponse' {creationTime} -> creationTime) (\s@DescribeInferenceExperimentResponse' {} a -> s {creationTime = a} :: DescribeInferenceExperimentResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon S3 location and configuration for storing inference request
-- and response data.
describeInferenceExperimentResponse_dataStorageConfig :: Lens.Lens' DescribeInferenceExperimentResponse (Prelude.Maybe InferenceExperimentDataStorageConfig)
describeInferenceExperimentResponse_dataStorageConfig = Lens.lens (\DescribeInferenceExperimentResponse' {dataStorageConfig} -> dataStorageConfig) (\s@DescribeInferenceExperimentResponse' {} a -> s {dataStorageConfig = a} :: DescribeInferenceExperimentResponse)

-- | The description of the inference experiment.
describeInferenceExperimentResponse_description :: Lens.Lens' DescribeInferenceExperimentResponse (Prelude.Maybe Prelude.Text)
describeInferenceExperimentResponse_description = Lens.lens (\DescribeInferenceExperimentResponse' {description} -> description) (\s@DescribeInferenceExperimentResponse' {} a -> s {description = a} :: DescribeInferenceExperimentResponse)

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance that hosts the endpoint. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateInferenceExperiment.html CreateInferenceExperiment>.
describeInferenceExperimentResponse_kmsKey :: Lens.Lens' DescribeInferenceExperimentResponse (Prelude.Maybe Prelude.Text)
describeInferenceExperimentResponse_kmsKey = Lens.lens (\DescribeInferenceExperimentResponse' {kmsKey} -> kmsKey) (\s@DescribeInferenceExperimentResponse' {} a -> s {kmsKey = a} :: DescribeInferenceExperimentResponse)

-- | The timestamp at which you last modified the inference experiment.
describeInferenceExperimentResponse_lastModifiedTime :: Lens.Lens' DescribeInferenceExperimentResponse (Prelude.Maybe Prelude.UTCTime)
describeInferenceExperimentResponse_lastModifiedTime = Lens.lens (\DescribeInferenceExperimentResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeInferenceExperimentResponse' {} a -> s {lastModifiedTime = a} :: DescribeInferenceExperimentResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the IAM role that Amazon SageMaker can assume to access model
-- artifacts and container images, and manage Amazon SageMaker Inference
-- endpoints for model deployment.
describeInferenceExperimentResponse_roleArn :: Lens.Lens' DescribeInferenceExperimentResponse (Prelude.Maybe Prelude.Text)
describeInferenceExperimentResponse_roleArn = Lens.lens (\DescribeInferenceExperimentResponse' {roleArn} -> roleArn) (\s@DescribeInferenceExperimentResponse' {} a -> s {roleArn = a} :: DescribeInferenceExperimentResponse)

-- | The duration for which the inference experiment ran or will run.
describeInferenceExperimentResponse_schedule :: Lens.Lens' DescribeInferenceExperimentResponse (Prelude.Maybe InferenceExperimentSchedule)
describeInferenceExperimentResponse_schedule = Lens.lens (\DescribeInferenceExperimentResponse' {schedule} -> schedule) (\s@DescribeInferenceExperimentResponse' {} a -> s {schedule = a} :: DescribeInferenceExperimentResponse)

-- | The configuration of @ShadowMode@ inference experiment type, which shows
-- the production variant that takes all the inference requests, and the
-- shadow variant to which Amazon SageMaker replicates a percentage of the
-- inference requests. For the shadow variant it also shows the percentage
-- of requests that Amazon SageMaker replicates.
describeInferenceExperimentResponse_shadowModeConfig :: Lens.Lens' DescribeInferenceExperimentResponse (Prelude.Maybe ShadowModeConfig)
describeInferenceExperimentResponse_shadowModeConfig = Lens.lens (\DescribeInferenceExperimentResponse' {shadowModeConfig} -> shadowModeConfig) (\s@DescribeInferenceExperimentResponse' {} a -> s {shadowModeConfig = a} :: DescribeInferenceExperimentResponse)

-- | The error message or client-specified @Reason@ from the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_StopInferenceExperiment.html StopInferenceExperiment>
-- API, that explains the status of the inference experiment.
describeInferenceExperimentResponse_statusReason :: Lens.Lens' DescribeInferenceExperimentResponse (Prelude.Maybe Prelude.Text)
describeInferenceExperimentResponse_statusReason = Lens.lens (\DescribeInferenceExperimentResponse' {statusReason} -> statusReason) (\s@DescribeInferenceExperimentResponse' {} a -> s {statusReason = a} :: DescribeInferenceExperimentResponse)

-- | The response's http status code.
describeInferenceExperimentResponse_httpStatus :: Lens.Lens' DescribeInferenceExperimentResponse Prelude.Int
describeInferenceExperimentResponse_httpStatus = Lens.lens (\DescribeInferenceExperimentResponse' {httpStatus} -> httpStatus) (\s@DescribeInferenceExperimentResponse' {} a -> s {httpStatus = a} :: DescribeInferenceExperimentResponse)

-- | The ARN of the inference experiment being described.
describeInferenceExperimentResponse_arn :: Lens.Lens' DescribeInferenceExperimentResponse Prelude.Text
describeInferenceExperimentResponse_arn = Lens.lens (\DescribeInferenceExperimentResponse' {arn} -> arn) (\s@DescribeInferenceExperimentResponse' {} a -> s {arn = a} :: DescribeInferenceExperimentResponse)

-- | The name of the inference experiment.
describeInferenceExperimentResponse_name :: Lens.Lens' DescribeInferenceExperimentResponse Prelude.Text
describeInferenceExperimentResponse_name = Lens.lens (\DescribeInferenceExperimentResponse' {name} -> name) (\s@DescribeInferenceExperimentResponse' {} a -> s {name = a} :: DescribeInferenceExperimentResponse)

-- | The type of the inference experiment.
describeInferenceExperimentResponse_type :: Lens.Lens' DescribeInferenceExperimentResponse InferenceExperimentType
describeInferenceExperimentResponse_type = Lens.lens (\DescribeInferenceExperimentResponse' {type'} -> type') (\s@DescribeInferenceExperimentResponse' {} a -> s {type' = a} :: DescribeInferenceExperimentResponse)

-- | The status of the inference experiment. The following are the possible
-- statuses for an inference experiment:
--
-- -   @Creating@ - Amazon SageMaker is creating your experiment.
--
-- -   @Created@ - Amazon SageMaker has finished the creation of your
--     experiment and will begin the experiment at the scheduled time.
--
-- -   @Updating@ - When you make changes to your experiment, your
--     experiment shows as updating.
--
-- -   @Starting@ - Amazon SageMaker is beginning your experiment.
--
-- -   @Running@ - Your experiment is in progress.
--
-- -   @Stopping@ - Amazon SageMaker is stopping your experiment.
--
-- -   @Completed@ - Your experiment has completed.
--
-- -   @Cancelled@ - When you conclude your experiment early using the
--     <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_StopInferenceExperiment.html StopInferenceExperiment>
--     API, or if any operation fails with an unexpected error, it shows as
--     cancelled.
describeInferenceExperimentResponse_status :: Lens.Lens' DescribeInferenceExperimentResponse InferenceExperimentStatus
describeInferenceExperimentResponse_status = Lens.lens (\DescribeInferenceExperimentResponse' {status} -> status) (\s@DescribeInferenceExperimentResponse' {} a -> s {status = a} :: DescribeInferenceExperimentResponse)

-- | The metadata of the endpoint on which the inference experiment ran.
describeInferenceExperimentResponse_endpointMetadata :: Lens.Lens' DescribeInferenceExperimentResponse EndpointMetadata
describeInferenceExperimentResponse_endpointMetadata = Lens.lens (\DescribeInferenceExperimentResponse' {endpointMetadata} -> endpointMetadata) (\s@DescribeInferenceExperimentResponse' {} a -> s {endpointMetadata = a} :: DescribeInferenceExperimentResponse)

-- | An array of @ModelVariantConfigSummary@ objects. There is one for each
-- variant in the inference experiment. Each @ModelVariantConfigSummary@
-- object in the array describes the infrastructure configuration for
-- deploying the corresponding variant.
describeInferenceExperimentResponse_modelVariants :: Lens.Lens' DescribeInferenceExperimentResponse [ModelVariantConfigSummary]
describeInferenceExperimentResponse_modelVariants = Lens.lens (\DescribeInferenceExperimentResponse' {modelVariants} -> modelVariants) (\s@DescribeInferenceExperimentResponse' {} a -> s {modelVariants = a} :: DescribeInferenceExperimentResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeInferenceExperimentResponse
  where
  rnf DescribeInferenceExperimentResponse' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataStorageConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf shadowModeConfig
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endpointMetadata
      `Prelude.seq` Prelude.rnf modelVariants
