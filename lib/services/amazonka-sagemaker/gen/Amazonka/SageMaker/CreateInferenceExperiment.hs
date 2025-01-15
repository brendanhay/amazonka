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
-- Module      : Amazonka.SageMaker.CreateInferenceExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an inference experiment using the configurations specified in
-- the request.
--
-- Use this API to setup and schedule an experiment to compare model
-- variants on a Amazon SageMaker inference endpoint. For more information
-- about inference experiments, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/shadow-tests.html Shadow tests>.
--
-- Amazon SageMaker begins your experiment at the scheduled time and routes
-- traffic to your endpoint\'s model variants based on your specified
-- configuration.
--
-- While the experiment is in progress or after it has concluded, you can
-- view metrics that compare your model variants. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/shadow-tests-view-monitor-edit.html View, monitor, and edit shadow tests>.
module Amazonka.SageMaker.CreateInferenceExperiment
  ( -- * Creating a Request
    CreateInferenceExperiment (..),
    newCreateInferenceExperiment,

    -- * Request Lenses
    createInferenceExperiment_dataStorageConfig,
    createInferenceExperiment_description,
    createInferenceExperiment_kmsKey,
    createInferenceExperiment_schedule,
    createInferenceExperiment_tags,
    createInferenceExperiment_name,
    createInferenceExperiment_type,
    createInferenceExperiment_roleArn,
    createInferenceExperiment_endpointName,
    createInferenceExperiment_modelVariants,
    createInferenceExperiment_shadowModeConfig,

    -- * Destructuring the Response
    CreateInferenceExperimentResponse (..),
    newCreateInferenceExperimentResponse,

    -- * Response Lenses
    createInferenceExperimentResponse_httpStatus,
    createInferenceExperimentResponse_inferenceExperimentArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateInferenceExperiment' smart constructor.
data CreateInferenceExperiment = CreateInferenceExperiment'
  { -- | The Amazon S3 location and configuration for storing inference request
    -- and response data.
    --
    -- This is an optional parameter that you can use for data capture. For
    -- more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-data-capture.html Capture data>.
    dataStorageConfig :: Prelude.Maybe InferenceExperimentDataStorageConfig,
    -- | A description for the inference experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that Amazon SageMaker uses to encrypt data on the storage volume
    -- attached to the ML compute instance that hosts the endpoint. The
    -- @KmsKey@ can be any of the following formats:
    --
    -- -   KMS key ID
    --
    --     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS key
    --
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   KMS key Alias
    --
    --     @\"alias\/ExampleAlias\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS key Alias
    --
    --     @\"arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias\"@
    --
    -- If you use a KMS key ID or an alias of your KMS key, the Amazon
    -- SageMaker execution role must include permissions to call @kms:Encrypt@.
    -- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
    -- KMS key for Amazon S3 for your role\'s account. Amazon SageMaker uses
    -- server-side encryption with KMS managed keys for @OutputDataConfig@. If
    -- you use a bucket policy with an @s3:PutObject@ permission that only
    -- allows objects with server-side encryption, set the condition key of
    -- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS managed Encryption Keys>
    -- in the /Amazon Simple Storage Service Developer Guide./
    --
    -- The KMS key policy must grant permission to the IAM role that you
    -- specify in your @CreateEndpoint@ and @UpdateEndpoint@ requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
    -- in the /Amazon Web Services Key Management Service Developer Guide/.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The duration for which you want the inference experiment to run. If you
    -- don\'t specify this field, the experiment automatically starts
    -- immediately upon creation and concludes after 7 days.
    schedule :: Prelude.Maybe InferenceExperimentSchedule,
    -- | Array of key-value pairs. You can use tags to categorize your Amazon Web
    -- Services resources in different ways, for example, by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/ARG/latest/userguide/tagging.html Tagging your Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the inference experiment.
    name :: Prelude.Text,
    -- | The type of the inference experiment that you want to run. The following
    -- types of experiments are possible:
    --
    -- -   @ShadowMode@: You can use this type to validate a shadow variant.
    --     For more information, see
    --     <https://docs.aws.amazon.com/sagemaker/latest/dg/shadow-tests.html Shadow tests>.
    type' :: InferenceExperimentType,
    -- | The ARN of the IAM role that Amazon SageMaker can assume to access model
    -- artifacts and container images, and manage Amazon SageMaker Inference
    -- endpoints for model deployment.
    roleArn :: Prelude.Text,
    -- | The name of the Amazon SageMaker endpoint on which you want to run the
    -- inference experiment.
    endpointName :: Prelude.Text,
    -- | An array of @ModelVariantConfig@ objects. There is one for each variant
    -- in the inference experiment. Each @ModelVariantConfig@ object in the
    -- array describes the infrastructure configuration for the corresponding
    -- variant.
    modelVariants :: Prelude.NonEmpty ModelVariantConfig,
    -- | The configuration of @ShadowMode@ inference experiment type. Use this
    -- field to specify a production variant which takes all the inference
    -- requests, and a shadow variant to which Amazon SageMaker replicates a
    -- percentage of the inference requests. For the shadow variant also
    -- specify the percentage of requests that Amazon SageMaker replicates.
    shadowModeConfig :: ShadowModeConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInferenceExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataStorageConfig', 'createInferenceExperiment_dataStorageConfig' - The Amazon S3 location and configuration for storing inference request
-- and response data.
--
-- This is an optional parameter that you can use for data capture. For
-- more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-data-capture.html Capture data>.
--
-- 'description', 'createInferenceExperiment_description' - A description for the inference experiment.
--
-- 'kmsKey', 'createInferenceExperiment_kmsKey' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance that hosts the endpoint. The
-- @KmsKey@ can be any of the following formats:
--
-- -   KMS key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS key
--
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   KMS key Alias
--
--     @\"alias\/ExampleAlias\"@
--
-- -   Amazon Resource Name (ARN) of a KMS key Alias
--
--     @\"arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias\"@
--
-- If you use a KMS key ID or an alias of your KMS key, the Amazon
-- SageMaker execution role must include permissions to call @kms:Encrypt@.
-- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
-- KMS key for Amazon S3 for your role\'s account. Amazon SageMaker uses
-- server-side encryption with KMS managed keys for @OutputDataConfig@. If
-- you use a bucket policy with an @s3:PutObject@ permission that only
-- allows objects with server-side encryption, set the condition key of
-- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateEndpoint@ and @UpdateEndpoint@ requests. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
--
-- 'schedule', 'createInferenceExperiment_schedule' - The duration for which you want the inference experiment to run. If you
-- don\'t specify this field, the experiment automatically starts
-- immediately upon creation and concludes after 7 days.
--
-- 'tags', 'createInferenceExperiment_tags' - Array of key-value pairs. You can use tags to categorize your Amazon Web
-- Services resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/tagging.html Tagging your Amazon Web Services Resources>.
--
-- 'name', 'createInferenceExperiment_name' - The name for the inference experiment.
--
-- 'type'', 'createInferenceExperiment_type' - The type of the inference experiment that you want to run. The following
-- types of experiments are possible:
--
-- -   @ShadowMode@: You can use this type to validate a shadow variant.
--     For more information, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/shadow-tests.html Shadow tests>.
--
-- 'roleArn', 'createInferenceExperiment_roleArn' - The ARN of the IAM role that Amazon SageMaker can assume to access model
-- artifacts and container images, and manage Amazon SageMaker Inference
-- endpoints for model deployment.
--
-- 'endpointName', 'createInferenceExperiment_endpointName' - The name of the Amazon SageMaker endpoint on which you want to run the
-- inference experiment.
--
-- 'modelVariants', 'createInferenceExperiment_modelVariants' - An array of @ModelVariantConfig@ objects. There is one for each variant
-- in the inference experiment. Each @ModelVariantConfig@ object in the
-- array describes the infrastructure configuration for the corresponding
-- variant.
--
-- 'shadowModeConfig', 'createInferenceExperiment_shadowModeConfig' - The configuration of @ShadowMode@ inference experiment type. Use this
-- field to specify a production variant which takes all the inference
-- requests, and a shadow variant to which Amazon SageMaker replicates a
-- percentage of the inference requests. For the shadow variant also
-- specify the percentage of requests that Amazon SageMaker replicates.
newCreateInferenceExperiment ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  InferenceExperimentType ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'modelVariants'
  Prelude.NonEmpty ModelVariantConfig ->
  -- | 'shadowModeConfig'
  ShadowModeConfig ->
  CreateInferenceExperiment
newCreateInferenceExperiment
  pName_
  pType_
  pRoleArn_
  pEndpointName_
  pModelVariants_
  pShadowModeConfig_ =
    CreateInferenceExperiment'
      { dataStorageConfig =
          Prelude.Nothing,
        description = Prelude.Nothing,
        kmsKey = Prelude.Nothing,
        schedule = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        type' = pType_,
        roleArn = pRoleArn_,
        endpointName = pEndpointName_,
        modelVariants =
          Lens.coerced Lens.# pModelVariants_,
        shadowModeConfig = pShadowModeConfig_
      }

-- | The Amazon S3 location and configuration for storing inference request
-- and response data.
--
-- This is an optional parameter that you can use for data capture. For
-- more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor-data-capture.html Capture data>.
createInferenceExperiment_dataStorageConfig :: Lens.Lens' CreateInferenceExperiment (Prelude.Maybe InferenceExperimentDataStorageConfig)
createInferenceExperiment_dataStorageConfig = Lens.lens (\CreateInferenceExperiment' {dataStorageConfig} -> dataStorageConfig) (\s@CreateInferenceExperiment' {} a -> s {dataStorageConfig = a} :: CreateInferenceExperiment)

-- | A description for the inference experiment.
createInferenceExperiment_description :: Lens.Lens' CreateInferenceExperiment (Prelude.Maybe Prelude.Text)
createInferenceExperiment_description = Lens.lens (\CreateInferenceExperiment' {description} -> description) (\s@CreateInferenceExperiment' {} a -> s {description = a} :: CreateInferenceExperiment)

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance that hosts the endpoint. The
-- @KmsKey@ can be any of the following formats:
--
-- -   KMS key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS key
--
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   KMS key Alias
--
--     @\"alias\/ExampleAlias\"@
--
-- -   Amazon Resource Name (ARN) of a KMS key Alias
--
--     @\"arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias\"@
--
-- If you use a KMS key ID or an alias of your KMS key, the Amazon
-- SageMaker execution role must include permissions to call @kms:Encrypt@.
-- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
-- KMS key for Amazon S3 for your role\'s account. Amazon SageMaker uses
-- server-side encryption with KMS managed keys for @OutputDataConfig@. If
-- you use a bucket policy with an @s3:PutObject@ permission that only
-- allows objects with server-side encryption, set the condition key of
-- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateEndpoint@ and @UpdateEndpoint@ requests. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
createInferenceExperiment_kmsKey :: Lens.Lens' CreateInferenceExperiment (Prelude.Maybe Prelude.Text)
createInferenceExperiment_kmsKey = Lens.lens (\CreateInferenceExperiment' {kmsKey} -> kmsKey) (\s@CreateInferenceExperiment' {} a -> s {kmsKey = a} :: CreateInferenceExperiment)

-- | The duration for which you want the inference experiment to run. If you
-- don\'t specify this field, the experiment automatically starts
-- immediately upon creation and concludes after 7 days.
createInferenceExperiment_schedule :: Lens.Lens' CreateInferenceExperiment (Prelude.Maybe InferenceExperimentSchedule)
createInferenceExperiment_schedule = Lens.lens (\CreateInferenceExperiment' {schedule} -> schedule) (\s@CreateInferenceExperiment' {} a -> s {schedule = a} :: CreateInferenceExperiment)

-- | Array of key-value pairs. You can use tags to categorize your Amazon Web
-- Services resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/ARG/latest/userguide/tagging.html Tagging your Amazon Web Services Resources>.
createInferenceExperiment_tags :: Lens.Lens' CreateInferenceExperiment (Prelude.Maybe [Tag])
createInferenceExperiment_tags = Lens.lens (\CreateInferenceExperiment' {tags} -> tags) (\s@CreateInferenceExperiment' {} a -> s {tags = a} :: CreateInferenceExperiment) Prelude.. Lens.mapping Lens.coerced

-- | The name for the inference experiment.
createInferenceExperiment_name :: Lens.Lens' CreateInferenceExperiment Prelude.Text
createInferenceExperiment_name = Lens.lens (\CreateInferenceExperiment' {name} -> name) (\s@CreateInferenceExperiment' {} a -> s {name = a} :: CreateInferenceExperiment)

-- | The type of the inference experiment that you want to run. The following
-- types of experiments are possible:
--
-- -   @ShadowMode@: You can use this type to validate a shadow variant.
--     For more information, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/shadow-tests.html Shadow tests>.
createInferenceExperiment_type :: Lens.Lens' CreateInferenceExperiment InferenceExperimentType
createInferenceExperiment_type = Lens.lens (\CreateInferenceExperiment' {type'} -> type') (\s@CreateInferenceExperiment' {} a -> s {type' = a} :: CreateInferenceExperiment)

-- | The ARN of the IAM role that Amazon SageMaker can assume to access model
-- artifacts and container images, and manage Amazon SageMaker Inference
-- endpoints for model deployment.
createInferenceExperiment_roleArn :: Lens.Lens' CreateInferenceExperiment Prelude.Text
createInferenceExperiment_roleArn = Lens.lens (\CreateInferenceExperiment' {roleArn} -> roleArn) (\s@CreateInferenceExperiment' {} a -> s {roleArn = a} :: CreateInferenceExperiment)

-- | The name of the Amazon SageMaker endpoint on which you want to run the
-- inference experiment.
createInferenceExperiment_endpointName :: Lens.Lens' CreateInferenceExperiment Prelude.Text
createInferenceExperiment_endpointName = Lens.lens (\CreateInferenceExperiment' {endpointName} -> endpointName) (\s@CreateInferenceExperiment' {} a -> s {endpointName = a} :: CreateInferenceExperiment)

-- | An array of @ModelVariantConfig@ objects. There is one for each variant
-- in the inference experiment. Each @ModelVariantConfig@ object in the
-- array describes the infrastructure configuration for the corresponding
-- variant.
createInferenceExperiment_modelVariants :: Lens.Lens' CreateInferenceExperiment (Prelude.NonEmpty ModelVariantConfig)
createInferenceExperiment_modelVariants = Lens.lens (\CreateInferenceExperiment' {modelVariants} -> modelVariants) (\s@CreateInferenceExperiment' {} a -> s {modelVariants = a} :: CreateInferenceExperiment) Prelude.. Lens.coerced

-- | The configuration of @ShadowMode@ inference experiment type. Use this
-- field to specify a production variant which takes all the inference
-- requests, and a shadow variant to which Amazon SageMaker replicates a
-- percentage of the inference requests. For the shadow variant also
-- specify the percentage of requests that Amazon SageMaker replicates.
createInferenceExperiment_shadowModeConfig :: Lens.Lens' CreateInferenceExperiment ShadowModeConfig
createInferenceExperiment_shadowModeConfig = Lens.lens (\CreateInferenceExperiment' {shadowModeConfig} -> shadowModeConfig) (\s@CreateInferenceExperiment' {} a -> s {shadowModeConfig = a} :: CreateInferenceExperiment)

instance Core.AWSRequest CreateInferenceExperiment where
  type
    AWSResponse CreateInferenceExperiment =
      CreateInferenceExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInferenceExperimentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InferenceExperimentArn")
      )

instance Prelude.Hashable CreateInferenceExperiment where
  hashWithSalt _salt CreateInferenceExperiment' {..} =
    _salt
      `Prelude.hashWithSalt` dataStorageConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` modelVariants
      `Prelude.hashWithSalt` shadowModeConfig

instance Prelude.NFData CreateInferenceExperiment where
  rnf CreateInferenceExperiment' {..} =
    Prelude.rnf dataStorageConfig `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf kmsKey `Prelude.seq`
          Prelude.rnf schedule `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf type' `Prelude.seq`
                  Prelude.rnf roleArn `Prelude.seq`
                    Prelude.rnf endpointName `Prelude.seq`
                      Prelude.rnf modelVariants `Prelude.seq`
                        Prelude.rnf shadowModeConfig

instance Data.ToHeaders CreateInferenceExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateInferenceExperiment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateInferenceExperiment where
  toJSON CreateInferenceExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataStorageConfig" Data..=)
              Prelude.<$> dataStorageConfig,
            ("Description" Data..=) Prelude.<$> description,
            ("KmsKey" Data..=) Prelude.<$> kmsKey,
            ("Schedule" Data..=) Prelude.<$> schedule,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("EndpointName" Data..= endpointName),
            Prelude.Just ("ModelVariants" Data..= modelVariants),
            Prelude.Just
              ("ShadowModeConfig" Data..= shadowModeConfig)
          ]
      )

instance Data.ToPath CreateInferenceExperiment where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateInferenceExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInferenceExperimentResponse' smart constructor.
data CreateInferenceExperimentResponse = CreateInferenceExperimentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN for your inference experiment.
    inferenceExperimentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInferenceExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createInferenceExperimentResponse_httpStatus' - The response's http status code.
--
-- 'inferenceExperimentArn', 'createInferenceExperimentResponse_inferenceExperimentArn' - The ARN for your inference experiment.
newCreateInferenceExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'inferenceExperimentArn'
  Prelude.Text ->
  CreateInferenceExperimentResponse
newCreateInferenceExperimentResponse
  pHttpStatus_
  pInferenceExperimentArn_ =
    CreateInferenceExperimentResponse'
      { httpStatus =
          pHttpStatus_,
        inferenceExperimentArn =
          pInferenceExperimentArn_
      }

-- | The response's http status code.
createInferenceExperimentResponse_httpStatus :: Lens.Lens' CreateInferenceExperimentResponse Prelude.Int
createInferenceExperimentResponse_httpStatus = Lens.lens (\CreateInferenceExperimentResponse' {httpStatus} -> httpStatus) (\s@CreateInferenceExperimentResponse' {} a -> s {httpStatus = a} :: CreateInferenceExperimentResponse)

-- | The ARN for your inference experiment.
createInferenceExperimentResponse_inferenceExperimentArn :: Lens.Lens' CreateInferenceExperimentResponse Prelude.Text
createInferenceExperimentResponse_inferenceExperimentArn = Lens.lens (\CreateInferenceExperimentResponse' {inferenceExperimentArn} -> inferenceExperimentArn) (\s@CreateInferenceExperimentResponse' {} a -> s {inferenceExperimentArn = a} :: CreateInferenceExperimentResponse)

instance
  Prelude.NFData
    CreateInferenceExperimentResponse
  where
  rnf CreateInferenceExperimentResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf inferenceExperimentArn
