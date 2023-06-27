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
-- Module      : Amazonka.SageMaker.CreateModelPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model package that you can use to create SageMaker models or
-- list on Amazon Web Services Marketplace, or a versioned model that is
-- part of a model group. Buyers can subscribe to model packages listed on
-- Amazon Web Services Marketplace to create models in SageMaker.
--
-- To create a model package by specifying a Docker container that contains
-- your inference code and the Amazon S3 location of your model artifacts,
-- provide values for @InferenceSpecification@. To create a model from an
-- algorithm resource that you created or subscribed to in Amazon Web
-- Services Marketplace, provide a value for
-- @SourceAlgorithmSpecification@.
--
-- There are two types of model packages:
--
-- -   Versioned - a model that is part of a model group in the model
--     registry.
--
-- -   Unversioned - a model package that is not part of a model group.
module Amazonka.SageMaker.CreateModelPackage
  ( -- * Creating a Request
    CreateModelPackage (..),
    newCreateModelPackage,

    -- * Request Lenses
    createModelPackage_additionalInferenceSpecifications,
    createModelPackage_certifyForMarketplace,
    createModelPackage_clientToken,
    createModelPackage_customerMetadataProperties,
    createModelPackage_domain,
    createModelPackage_driftCheckBaselines,
    createModelPackage_inferenceSpecification,
    createModelPackage_metadataProperties,
    createModelPackage_modelApprovalStatus,
    createModelPackage_modelMetrics,
    createModelPackage_modelPackageDescription,
    createModelPackage_modelPackageGroupName,
    createModelPackage_modelPackageName,
    createModelPackage_samplePayloadUrl,
    createModelPackage_sourceAlgorithmSpecification,
    createModelPackage_tags,
    createModelPackage_task,
    createModelPackage_validationSpecification,

    -- * Destructuring the Response
    CreateModelPackageResponse (..),
    newCreateModelPackageResponse,

    -- * Response Lenses
    createModelPackageResponse_httpStatus,
    createModelPackageResponse_modelPackageArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateModelPackage' smart constructor.
data CreateModelPackage = CreateModelPackage'
  { -- | An array of additional Inference Specification objects. Each additional
    -- Inference Specification specifies artifacts based on this model package
    -- that can be used on inference endpoints. Generally used with SageMaker
    -- Neo to store the compiled artifacts.
    additionalInferenceSpecifications :: Prelude.Maybe (Prelude.NonEmpty AdditionalInferenceSpecificationDefinition),
    -- | Whether to certify the model package for listing on Amazon Web Services
    -- Marketplace.
    --
    -- This parameter is optional for unversioned models, and does not apply to
    -- versioned models.
    certifyForMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | A unique token that guarantees that the call to this API is idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The metadata properties associated with the model package versions.
    customerMetadataProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The machine learning domain of your model package and its components.
    -- Common machine learning domains include computer vision and natural
    -- language processing.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Represents the drift check baselines that can be used when the model
    -- monitor is set using the model package. For more information, see the
    -- topic on
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/pipelines-quality-clarify-baseline-lifecycle.html#pipelines-quality-clarify-baseline-drift-detection Drift Detection against Previous Baselines in SageMaker Pipelines>
    -- in the /Amazon SageMaker Developer Guide/.
    driftCheckBaselines :: Prelude.Maybe DriftCheckBaselines,
    -- | Specifies details about inference jobs that can be run with models based
    -- on this model package, including the following:
    --
    -- -   The Amazon ECR paths of containers that contain the inference code
    --     and model artifacts.
    --
    -- -   The instance types that the model package supports for transform
    --     jobs and real-time endpoints used for inference.
    --
    -- -   The input and output content formats that the model package supports
    --     for inference.
    inferenceSpecification :: Prelude.Maybe InferenceSpecification,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | Whether the model is approved for deployment.
    --
    -- This parameter is optional for versioned models, and does not apply to
    -- unversioned models.
    --
    -- For versioned models, the value of this parameter must be set to
    -- @Approved@ to deploy the model.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
    -- | A structure that contains model metrics reports.
    modelMetrics :: Prelude.Maybe ModelMetrics,
    -- | A description of the model package.
    modelPackageDescription :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the model package group that
    -- this model version belongs to.
    --
    -- This parameter is required for versioned models, and does not apply to
    -- unversioned models.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the model package. The name must have 1 to 63 characters.
    -- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    --
    -- This parameter is required for unversioned models. It is not applicable
    -- to versioned models.
    modelPackageName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Storage Service (Amazon S3) path where the sample
    -- payload is stored. This path must point to a single gzip compressed tar
    -- archive (.tar.gz suffix). This archive can hold multiple files that are
    -- all equally used in the load test. Each file in the archive must satisfy
    -- the size constraints of the
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_runtime_InvokeEndpoint.html#API_runtime_InvokeEndpoint_RequestSyntax InvokeEndpoint>
    -- call.
    samplePayloadUrl :: Prelude.Maybe Prelude.Text,
    -- | Details about the algorithm that was used to create the model package.
    sourceAlgorithmSpecification :: Prelude.Maybe SourceAlgorithmSpecification,
    -- | A list of key value pairs associated with the model. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The machine learning task your model package accomplishes. Common
    -- machine learning tasks include object detection and image
    -- classification. The following tasks are supported by Inference
    -- Recommender: @\"IMAGE_CLASSIFICATION\"@ | @\"OBJECT_DETECTION\"@ |
    -- @\"TEXT_GENERATION\"@ |@\"IMAGE_SEGMENTATION\"@ | @\"FILL_MASK\"@ |
    -- @\"CLASSIFICATION\"@ | @\"REGRESSION\"@ | @\"OTHER\"@.
    --
    -- Specify \"OTHER\" if none of the tasks listed fit your use case.
    task :: Prelude.Maybe Prelude.Text,
    -- | Specifies configurations for one or more transform jobs that SageMaker
    -- runs to test the model package.
    validationSpecification :: Prelude.Maybe ModelPackageValidationSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInferenceSpecifications', 'createModelPackage_additionalInferenceSpecifications' - An array of additional Inference Specification objects. Each additional
-- Inference Specification specifies artifacts based on this model package
-- that can be used on inference endpoints. Generally used with SageMaker
-- Neo to store the compiled artifacts.
--
-- 'certifyForMarketplace', 'createModelPackage_certifyForMarketplace' - Whether to certify the model package for listing on Amazon Web Services
-- Marketplace.
--
-- This parameter is optional for unversioned models, and does not apply to
-- versioned models.
--
-- 'clientToken', 'createModelPackage_clientToken' - A unique token that guarantees that the call to this API is idempotent.
--
-- 'customerMetadataProperties', 'createModelPackage_customerMetadataProperties' - The metadata properties associated with the model package versions.
--
-- 'domain', 'createModelPackage_domain' - The machine learning domain of your model package and its components.
-- Common machine learning domains include computer vision and natural
-- language processing.
--
-- 'driftCheckBaselines', 'createModelPackage_driftCheckBaselines' - Represents the drift check baselines that can be used when the model
-- monitor is set using the model package. For more information, see the
-- topic on
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/pipelines-quality-clarify-baseline-lifecycle.html#pipelines-quality-clarify-baseline-drift-detection Drift Detection against Previous Baselines in SageMaker Pipelines>
-- in the /Amazon SageMaker Developer Guide/.
--
-- 'inferenceSpecification', 'createModelPackage_inferenceSpecification' - Specifies details about inference jobs that can be run with models based
-- on this model package, including the following:
--
-- -   The Amazon ECR paths of containers that contain the inference code
--     and model artifacts.
--
-- -   The instance types that the model package supports for transform
--     jobs and real-time endpoints used for inference.
--
-- -   The input and output content formats that the model package supports
--     for inference.
--
-- 'metadataProperties', 'createModelPackage_metadataProperties' - Undocumented member.
--
-- 'modelApprovalStatus', 'createModelPackage_modelApprovalStatus' - Whether the model is approved for deployment.
--
-- This parameter is optional for versioned models, and does not apply to
-- unversioned models.
--
-- For versioned models, the value of this parameter must be set to
-- @Approved@ to deploy the model.
--
-- 'modelMetrics', 'createModelPackage_modelMetrics' - A structure that contains model metrics reports.
--
-- 'modelPackageDescription', 'createModelPackage_modelPackageDescription' - A description of the model package.
--
-- 'modelPackageGroupName', 'createModelPackage_modelPackageGroupName' - The name or Amazon Resource Name (ARN) of the model package group that
-- this model version belongs to.
--
-- This parameter is required for versioned models, and does not apply to
-- unversioned models.
--
-- 'modelPackageName', 'createModelPackage_modelPackageName' - The name of the model package. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- This parameter is required for unversioned models. It is not applicable
-- to versioned models.
--
-- 'samplePayloadUrl', 'createModelPackage_samplePayloadUrl' - The Amazon Simple Storage Service (Amazon S3) path where the sample
-- payload is stored. This path must point to a single gzip compressed tar
-- archive (.tar.gz suffix). This archive can hold multiple files that are
-- all equally used in the load test. Each file in the archive must satisfy
-- the size constraints of the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_runtime_InvokeEndpoint.html#API_runtime_InvokeEndpoint_RequestSyntax InvokeEndpoint>
-- call.
--
-- 'sourceAlgorithmSpecification', 'createModelPackage_sourceAlgorithmSpecification' - Details about the algorithm that was used to create the model package.
--
-- 'tags', 'createModelPackage_tags' - A list of key value pairs associated with the model. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
--
-- 'task', 'createModelPackage_task' - The machine learning task your model package accomplishes. Common
-- machine learning tasks include object detection and image
-- classification. The following tasks are supported by Inference
-- Recommender: @\"IMAGE_CLASSIFICATION\"@ | @\"OBJECT_DETECTION\"@ |
-- @\"TEXT_GENERATION\"@ |@\"IMAGE_SEGMENTATION\"@ | @\"FILL_MASK\"@ |
-- @\"CLASSIFICATION\"@ | @\"REGRESSION\"@ | @\"OTHER\"@.
--
-- Specify \"OTHER\" if none of the tasks listed fit your use case.
--
-- 'validationSpecification', 'createModelPackage_validationSpecification' - Specifies configurations for one or more transform jobs that SageMaker
-- runs to test the model package.
newCreateModelPackage ::
  CreateModelPackage
newCreateModelPackage =
  CreateModelPackage'
    { additionalInferenceSpecifications =
        Prelude.Nothing,
      certifyForMarketplace = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      customerMetadataProperties = Prelude.Nothing,
      domain = Prelude.Nothing,
      driftCheckBaselines = Prelude.Nothing,
      inferenceSpecification = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      modelApprovalStatus = Prelude.Nothing,
      modelMetrics = Prelude.Nothing,
      modelPackageDescription = Prelude.Nothing,
      modelPackageGroupName = Prelude.Nothing,
      modelPackageName = Prelude.Nothing,
      samplePayloadUrl = Prelude.Nothing,
      sourceAlgorithmSpecification = Prelude.Nothing,
      tags = Prelude.Nothing,
      task = Prelude.Nothing,
      validationSpecification = Prelude.Nothing
    }

-- | An array of additional Inference Specification objects. Each additional
-- Inference Specification specifies artifacts based on this model package
-- that can be used on inference endpoints. Generally used with SageMaker
-- Neo to store the compiled artifacts.
createModelPackage_additionalInferenceSpecifications :: Lens.Lens' CreateModelPackage (Prelude.Maybe (Prelude.NonEmpty AdditionalInferenceSpecificationDefinition))
createModelPackage_additionalInferenceSpecifications = Lens.lens (\CreateModelPackage' {additionalInferenceSpecifications} -> additionalInferenceSpecifications) (\s@CreateModelPackage' {} a -> s {additionalInferenceSpecifications = a} :: CreateModelPackage) Prelude.. Lens.mapping Lens.coerced

-- | Whether to certify the model package for listing on Amazon Web Services
-- Marketplace.
--
-- This parameter is optional for unversioned models, and does not apply to
-- versioned models.
createModelPackage_certifyForMarketplace :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Bool)
createModelPackage_certifyForMarketplace = Lens.lens (\CreateModelPackage' {certifyForMarketplace} -> certifyForMarketplace) (\s@CreateModelPackage' {} a -> s {certifyForMarketplace = a} :: CreateModelPackage)

-- | A unique token that guarantees that the call to this API is idempotent.
createModelPackage_clientToken :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_clientToken = Lens.lens (\CreateModelPackage' {clientToken} -> clientToken) (\s@CreateModelPackage' {} a -> s {clientToken = a} :: CreateModelPackage)

-- | The metadata properties associated with the model package versions.
createModelPackage_customerMetadataProperties :: Lens.Lens' CreateModelPackage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createModelPackage_customerMetadataProperties = Lens.lens (\CreateModelPackage' {customerMetadataProperties} -> customerMetadataProperties) (\s@CreateModelPackage' {} a -> s {customerMetadataProperties = a} :: CreateModelPackage) Prelude.. Lens.mapping Lens.coerced

-- | The machine learning domain of your model package and its components.
-- Common machine learning domains include computer vision and natural
-- language processing.
createModelPackage_domain :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_domain = Lens.lens (\CreateModelPackage' {domain} -> domain) (\s@CreateModelPackage' {} a -> s {domain = a} :: CreateModelPackage)

-- | Represents the drift check baselines that can be used when the model
-- monitor is set using the model package. For more information, see the
-- topic on
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/pipelines-quality-clarify-baseline-lifecycle.html#pipelines-quality-clarify-baseline-drift-detection Drift Detection against Previous Baselines in SageMaker Pipelines>
-- in the /Amazon SageMaker Developer Guide/.
createModelPackage_driftCheckBaselines :: Lens.Lens' CreateModelPackage (Prelude.Maybe DriftCheckBaselines)
createModelPackage_driftCheckBaselines = Lens.lens (\CreateModelPackage' {driftCheckBaselines} -> driftCheckBaselines) (\s@CreateModelPackage' {} a -> s {driftCheckBaselines = a} :: CreateModelPackage)

-- | Specifies details about inference jobs that can be run with models based
-- on this model package, including the following:
--
-- -   The Amazon ECR paths of containers that contain the inference code
--     and model artifacts.
--
-- -   The instance types that the model package supports for transform
--     jobs and real-time endpoints used for inference.
--
-- -   The input and output content formats that the model package supports
--     for inference.
createModelPackage_inferenceSpecification :: Lens.Lens' CreateModelPackage (Prelude.Maybe InferenceSpecification)
createModelPackage_inferenceSpecification = Lens.lens (\CreateModelPackage' {inferenceSpecification} -> inferenceSpecification) (\s@CreateModelPackage' {} a -> s {inferenceSpecification = a} :: CreateModelPackage)

-- | Undocumented member.
createModelPackage_metadataProperties :: Lens.Lens' CreateModelPackage (Prelude.Maybe MetadataProperties)
createModelPackage_metadataProperties = Lens.lens (\CreateModelPackage' {metadataProperties} -> metadataProperties) (\s@CreateModelPackage' {} a -> s {metadataProperties = a} :: CreateModelPackage)

-- | Whether the model is approved for deployment.
--
-- This parameter is optional for versioned models, and does not apply to
-- unversioned models.
--
-- For versioned models, the value of this parameter must be set to
-- @Approved@ to deploy the model.
createModelPackage_modelApprovalStatus :: Lens.Lens' CreateModelPackage (Prelude.Maybe ModelApprovalStatus)
createModelPackage_modelApprovalStatus = Lens.lens (\CreateModelPackage' {modelApprovalStatus} -> modelApprovalStatus) (\s@CreateModelPackage' {} a -> s {modelApprovalStatus = a} :: CreateModelPackage)

-- | A structure that contains model metrics reports.
createModelPackage_modelMetrics :: Lens.Lens' CreateModelPackage (Prelude.Maybe ModelMetrics)
createModelPackage_modelMetrics = Lens.lens (\CreateModelPackage' {modelMetrics} -> modelMetrics) (\s@CreateModelPackage' {} a -> s {modelMetrics = a} :: CreateModelPackage)

-- | A description of the model package.
createModelPackage_modelPackageDescription :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_modelPackageDescription = Lens.lens (\CreateModelPackage' {modelPackageDescription} -> modelPackageDescription) (\s@CreateModelPackage' {} a -> s {modelPackageDescription = a} :: CreateModelPackage)

-- | The name or Amazon Resource Name (ARN) of the model package group that
-- this model version belongs to.
--
-- This parameter is required for versioned models, and does not apply to
-- unversioned models.
createModelPackage_modelPackageGroupName :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_modelPackageGroupName = Lens.lens (\CreateModelPackage' {modelPackageGroupName} -> modelPackageGroupName) (\s@CreateModelPackage' {} a -> s {modelPackageGroupName = a} :: CreateModelPackage)

-- | The name of the model package. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- This parameter is required for unversioned models. It is not applicable
-- to versioned models.
createModelPackage_modelPackageName :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_modelPackageName = Lens.lens (\CreateModelPackage' {modelPackageName} -> modelPackageName) (\s@CreateModelPackage' {} a -> s {modelPackageName = a} :: CreateModelPackage)

-- | The Amazon Simple Storage Service (Amazon S3) path where the sample
-- payload is stored. This path must point to a single gzip compressed tar
-- archive (.tar.gz suffix). This archive can hold multiple files that are
-- all equally used in the load test. Each file in the archive must satisfy
-- the size constraints of the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_runtime_InvokeEndpoint.html#API_runtime_InvokeEndpoint_RequestSyntax InvokeEndpoint>
-- call.
createModelPackage_samplePayloadUrl :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_samplePayloadUrl = Lens.lens (\CreateModelPackage' {samplePayloadUrl} -> samplePayloadUrl) (\s@CreateModelPackage' {} a -> s {samplePayloadUrl = a} :: CreateModelPackage)

-- | Details about the algorithm that was used to create the model package.
createModelPackage_sourceAlgorithmSpecification :: Lens.Lens' CreateModelPackage (Prelude.Maybe SourceAlgorithmSpecification)
createModelPackage_sourceAlgorithmSpecification = Lens.lens (\CreateModelPackage' {sourceAlgorithmSpecification} -> sourceAlgorithmSpecification) (\s@CreateModelPackage' {} a -> s {sourceAlgorithmSpecification = a} :: CreateModelPackage)

-- | A list of key value pairs associated with the model. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
createModelPackage_tags :: Lens.Lens' CreateModelPackage (Prelude.Maybe [Tag])
createModelPackage_tags = Lens.lens (\CreateModelPackage' {tags} -> tags) (\s@CreateModelPackage' {} a -> s {tags = a} :: CreateModelPackage) Prelude.. Lens.mapping Lens.coerced

-- | The machine learning task your model package accomplishes. Common
-- machine learning tasks include object detection and image
-- classification. The following tasks are supported by Inference
-- Recommender: @\"IMAGE_CLASSIFICATION\"@ | @\"OBJECT_DETECTION\"@ |
-- @\"TEXT_GENERATION\"@ |@\"IMAGE_SEGMENTATION\"@ | @\"FILL_MASK\"@ |
-- @\"CLASSIFICATION\"@ | @\"REGRESSION\"@ | @\"OTHER\"@.
--
-- Specify \"OTHER\" if none of the tasks listed fit your use case.
createModelPackage_task :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_task = Lens.lens (\CreateModelPackage' {task} -> task) (\s@CreateModelPackage' {} a -> s {task = a} :: CreateModelPackage)

-- | Specifies configurations for one or more transform jobs that SageMaker
-- runs to test the model package.
createModelPackage_validationSpecification :: Lens.Lens' CreateModelPackage (Prelude.Maybe ModelPackageValidationSpecification)
createModelPackage_validationSpecification = Lens.lens (\CreateModelPackage' {validationSpecification} -> validationSpecification) (\s@CreateModelPackage' {} a -> s {validationSpecification = a} :: CreateModelPackage)

instance Core.AWSRequest CreateModelPackage where
  type
    AWSResponse CreateModelPackage =
      CreateModelPackageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelPackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ModelPackageArn")
      )

instance Prelude.Hashable CreateModelPackage where
  hashWithSalt _salt CreateModelPackage' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInferenceSpecifications
      `Prelude.hashWithSalt` certifyForMarketplace
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` customerMetadataProperties
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` driftCheckBaselines
      `Prelude.hashWithSalt` inferenceSpecification
      `Prelude.hashWithSalt` metadataProperties
      `Prelude.hashWithSalt` modelApprovalStatus
      `Prelude.hashWithSalt` modelMetrics
      `Prelude.hashWithSalt` modelPackageDescription
      `Prelude.hashWithSalt` modelPackageGroupName
      `Prelude.hashWithSalt` modelPackageName
      `Prelude.hashWithSalt` samplePayloadUrl
      `Prelude.hashWithSalt` sourceAlgorithmSpecification
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` task
      `Prelude.hashWithSalt` validationSpecification

instance Prelude.NFData CreateModelPackage where
  rnf CreateModelPackage' {..} =
    Prelude.rnf additionalInferenceSpecifications
      `Prelude.seq` Prelude.rnf certifyForMarketplace
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf customerMetadataProperties
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf driftCheckBaselines
      `Prelude.seq` Prelude.rnf inferenceSpecification
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf modelApprovalStatus
      `Prelude.seq` Prelude.rnf modelMetrics
      `Prelude.seq` Prelude.rnf modelPackageDescription
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf modelPackageName
      `Prelude.seq` Prelude.rnf samplePayloadUrl
      `Prelude.seq` Prelude.rnf sourceAlgorithmSpecification
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf task
      `Prelude.seq` Prelude.rnf
        validationSpecification

instance Data.ToHeaders CreateModelPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateModelPackage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateModelPackage where
  toJSON CreateModelPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalInferenceSpecifications" Data..=)
              Prelude.<$> additionalInferenceSpecifications,
            ("CertifyForMarketplace" Data..=)
              Prelude.<$> certifyForMarketplace,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("CustomerMetadataProperties" Data..=)
              Prelude.<$> customerMetadataProperties,
            ("Domain" Data..=) Prelude.<$> domain,
            ("DriftCheckBaselines" Data..=)
              Prelude.<$> driftCheckBaselines,
            ("InferenceSpecification" Data..=)
              Prelude.<$> inferenceSpecification,
            ("MetadataProperties" Data..=)
              Prelude.<$> metadataProperties,
            ("ModelApprovalStatus" Data..=)
              Prelude.<$> modelApprovalStatus,
            ("ModelMetrics" Data..=) Prelude.<$> modelMetrics,
            ("ModelPackageDescription" Data..=)
              Prelude.<$> modelPackageDescription,
            ("ModelPackageGroupName" Data..=)
              Prelude.<$> modelPackageGroupName,
            ("ModelPackageName" Data..=)
              Prelude.<$> modelPackageName,
            ("SamplePayloadUrl" Data..=)
              Prelude.<$> samplePayloadUrl,
            ("SourceAlgorithmSpecification" Data..=)
              Prelude.<$> sourceAlgorithmSpecification,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Task" Data..=) Prelude.<$> task,
            ("ValidationSpecification" Data..=)
              Prelude.<$> validationSpecification
          ]
      )

instance Data.ToPath CreateModelPackage where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateModelPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelPackageResponse' smart constructor.
data CreateModelPackageResponse = CreateModelPackageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the new model package.
    modelPackageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateModelPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createModelPackageResponse_httpStatus' - The response's http status code.
--
-- 'modelPackageArn', 'createModelPackageResponse_modelPackageArn' - The Amazon Resource Name (ARN) of the new model package.
newCreateModelPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelPackageArn'
  Prelude.Text ->
  CreateModelPackageResponse
newCreateModelPackageResponse
  pHttpStatus_
  pModelPackageArn_ =
    CreateModelPackageResponse'
      { httpStatus =
          pHttpStatus_,
        modelPackageArn = pModelPackageArn_
      }

-- | The response's http status code.
createModelPackageResponse_httpStatus :: Lens.Lens' CreateModelPackageResponse Prelude.Int
createModelPackageResponse_httpStatus = Lens.lens (\CreateModelPackageResponse' {httpStatus} -> httpStatus) (\s@CreateModelPackageResponse' {} a -> s {httpStatus = a} :: CreateModelPackageResponse)

-- | The Amazon Resource Name (ARN) of the new model package.
createModelPackageResponse_modelPackageArn :: Lens.Lens' CreateModelPackageResponse Prelude.Text
createModelPackageResponse_modelPackageArn = Lens.lens (\CreateModelPackageResponse' {modelPackageArn} -> modelPackageArn) (\s@CreateModelPackageResponse' {} a -> s {modelPackageArn = a} :: CreateModelPackageResponse)

instance Prelude.NFData CreateModelPackageResponse where
  rnf CreateModelPackageResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelPackageArn
