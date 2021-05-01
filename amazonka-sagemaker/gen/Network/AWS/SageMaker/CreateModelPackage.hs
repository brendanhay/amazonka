{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.CreateModelPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model package that you can use to create Amazon SageMaker
-- models or list on AWS Marketplace, or a versioned model that is part of
-- a model group. Buyers can subscribe to model packages listed on AWS
-- Marketplace to create models in Amazon SageMaker.
--
-- To create a model package by specifying a Docker container that contains
-- your inference code and the Amazon S3 location of your model artifacts,
-- provide values for @InferenceSpecification@. To create a model from an
-- algorithm resource that you created or subscribed to in AWS Marketplace,
-- provide a value for @SourceAlgorithmSpecification@.
--
-- There are two types of model packages:
--
-- -   Versioned - a model that is part of a model group in the model
--     registry.
--
-- -   Unversioned - a model package that is not part of a model group.
module Network.AWS.SageMaker.CreateModelPackage
  ( -- * Creating a Request
    CreateModelPackage (..),
    newCreateModelPackage,

    -- * Request Lenses
    createModelPackage_sourceAlgorithmSpecification,
    createModelPackage_metadataProperties,
    createModelPackage_validationSpecification,
    createModelPackage_modelMetrics,
    createModelPackage_certifyForMarketplace,
    createModelPackage_modelPackageName,
    createModelPackage_modelApprovalStatus,
    createModelPackage_tags,
    createModelPackage_inferenceSpecification,
    createModelPackage_modelPackageDescription,
    createModelPackage_modelPackageGroupName,
    createModelPackage_clientToken,

    -- * Destructuring the Response
    CreateModelPackageResponse (..),
    newCreateModelPackageResponse,

    -- * Response Lenses
    createModelPackageResponse_httpStatus,
    createModelPackageResponse_modelPackageArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateModelPackage' smart constructor.
data CreateModelPackage = CreateModelPackage'
  { -- | Details about the algorithm that was used to create the model package.
    sourceAlgorithmSpecification :: Prelude.Maybe SourceAlgorithmSpecification,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | Specifies configurations for one or more transform jobs that Amazon
    -- SageMaker runs to test the model package.
    validationSpecification :: Prelude.Maybe ModelPackageValidationSpecification,
    -- | A structure that contains model metrics reports.
    modelMetrics :: Prelude.Maybe ModelMetrics,
    -- | Whether to certify the model package for listing on AWS Marketplace.
    --
    -- This parameter is optional for unversioned models, and does not apply to
    -- versioned models.
    certifyForMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | The name of the model package. The name must have 1 to 63 characters.
    -- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    --
    -- This parameter is required for unversioned models. It is not applicable
    -- to versioned models.
    modelPackageName :: Prelude.Maybe Prelude.Text,
    -- | Whether the model is approved for deployment.
    --
    -- This parameter is optional for versioned models, and does not apply to
    -- unversioned models.
    --
    -- For versioned models, the value of this parameter must be set to
    -- @Approved@ to deploy the model.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
    -- | A list of key value pairs associated with the model. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
    -- in the /AWS General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
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
    -- | A description of the model package.
    modelPackageDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the model group that this model version belongs to.
    --
    -- This parameter is required for versioned models, and does not apply to
    -- unversioned models.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | A unique token that guarantees that the call to this API is idempotent.
    clientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateModelPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceAlgorithmSpecification', 'createModelPackage_sourceAlgorithmSpecification' - Details about the algorithm that was used to create the model package.
--
-- 'metadataProperties', 'createModelPackage_metadataProperties' - Undocumented member.
--
-- 'validationSpecification', 'createModelPackage_validationSpecification' - Specifies configurations for one or more transform jobs that Amazon
-- SageMaker runs to test the model package.
--
-- 'modelMetrics', 'createModelPackage_modelMetrics' - A structure that contains model metrics reports.
--
-- 'certifyForMarketplace', 'createModelPackage_certifyForMarketplace' - Whether to certify the model package for listing on AWS Marketplace.
--
-- This parameter is optional for unversioned models, and does not apply to
-- versioned models.
--
-- 'modelPackageName', 'createModelPackage_modelPackageName' - The name of the model package. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- This parameter is required for unversioned models. It is not applicable
-- to versioned models.
--
-- 'modelApprovalStatus', 'createModelPackage_modelApprovalStatus' - Whether the model is approved for deployment.
--
-- This parameter is optional for versioned models, and does not apply to
-- unversioned models.
--
-- For versioned models, the value of this parameter must be set to
-- @Approved@ to deploy the model.
--
-- 'tags', 'createModelPackage_tags' - A list of key value pairs associated with the model. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
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
-- 'modelPackageDescription', 'createModelPackage_modelPackageDescription' - A description of the model package.
--
-- 'modelPackageGroupName', 'createModelPackage_modelPackageGroupName' - The name of the model group that this model version belongs to.
--
-- This parameter is required for versioned models, and does not apply to
-- unversioned models.
--
-- 'clientToken', 'createModelPackage_clientToken' - A unique token that guarantees that the call to this API is idempotent.
newCreateModelPackage ::
  CreateModelPackage
newCreateModelPackage =
  CreateModelPackage'
    { sourceAlgorithmSpecification =
        Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      validationSpecification = Prelude.Nothing,
      modelMetrics = Prelude.Nothing,
      certifyForMarketplace = Prelude.Nothing,
      modelPackageName = Prelude.Nothing,
      modelApprovalStatus = Prelude.Nothing,
      tags = Prelude.Nothing,
      inferenceSpecification = Prelude.Nothing,
      modelPackageDescription = Prelude.Nothing,
      modelPackageGroupName = Prelude.Nothing,
      clientToken = Prelude.Nothing
    }

-- | Details about the algorithm that was used to create the model package.
createModelPackage_sourceAlgorithmSpecification :: Lens.Lens' CreateModelPackage (Prelude.Maybe SourceAlgorithmSpecification)
createModelPackage_sourceAlgorithmSpecification = Lens.lens (\CreateModelPackage' {sourceAlgorithmSpecification} -> sourceAlgorithmSpecification) (\s@CreateModelPackage' {} a -> s {sourceAlgorithmSpecification = a} :: CreateModelPackage)

-- | Undocumented member.
createModelPackage_metadataProperties :: Lens.Lens' CreateModelPackage (Prelude.Maybe MetadataProperties)
createModelPackage_metadataProperties = Lens.lens (\CreateModelPackage' {metadataProperties} -> metadataProperties) (\s@CreateModelPackage' {} a -> s {metadataProperties = a} :: CreateModelPackage)

-- | Specifies configurations for one or more transform jobs that Amazon
-- SageMaker runs to test the model package.
createModelPackage_validationSpecification :: Lens.Lens' CreateModelPackage (Prelude.Maybe ModelPackageValidationSpecification)
createModelPackage_validationSpecification = Lens.lens (\CreateModelPackage' {validationSpecification} -> validationSpecification) (\s@CreateModelPackage' {} a -> s {validationSpecification = a} :: CreateModelPackage)

-- | A structure that contains model metrics reports.
createModelPackage_modelMetrics :: Lens.Lens' CreateModelPackage (Prelude.Maybe ModelMetrics)
createModelPackage_modelMetrics = Lens.lens (\CreateModelPackage' {modelMetrics} -> modelMetrics) (\s@CreateModelPackage' {} a -> s {modelMetrics = a} :: CreateModelPackage)

-- | Whether to certify the model package for listing on AWS Marketplace.
--
-- This parameter is optional for unversioned models, and does not apply to
-- versioned models.
createModelPackage_certifyForMarketplace :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Bool)
createModelPackage_certifyForMarketplace = Lens.lens (\CreateModelPackage' {certifyForMarketplace} -> certifyForMarketplace) (\s@CreateModelPackage' {} a -> s {certifyForMarketplace = a} :: CreateModelPackage)

-- | The name of the model package. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- This parameter is required for unversioned models. It is not applicable
-- to versioned models.
createModelPackage_modelPackageName :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_modelPackageName = Lens.lens (\CreateModelPackage' {modelPackageName} -> modelPackageName) (\s@CreateModelPackage' {} a -> s {modelPackageName = a} :: CreateModelPackage)

-- | Whether the model is approved for deployment.
--
-- This parameter is optional for versioned models, and does not apply to
-- unversioned models.
--
-- For versioned models, the value of this parameter must be set to
-- @Approved@ to deploy the model.
createModelPackage_modelApprovalStatus :: Lens.Lens' CreateModelPackage (Prelude.Maybe ModelApprovalStatus)
createModelPackage_modelApprovalStatus = Lens.lens (\CreateModelPackage' {modelApprovalStatus} -> modelApprovalStatus) (\s@CreateModelPackage' {} a -> s {modelApprovalStatus = a} :: CreateModelPackage)

-- | A list of key value pairs associated with the model. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
createModelPackage_tags :: Lens.Lens' CreateModelPackage (Prelude.Maybe [Tag])
createModelPackage_tags = Lens.lens (\CreateModelPackage' {tags} -> tags) (\s@CreateModelPackage' {} a -> s {tags = a} :: CreateModelPackage) Prelude.. Lens.mapping Prelude._Coerce

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

-- | A description of the model package.
createModelPackage_modelPackageDescription :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_modelPackageDescription = Lens.lens (\CreateModelPackage' {modelPackageDescription} -> modelPackageDescription) (\s@CreateModelPackage' {} a -> s {modelPackageDescription = a} :: CreateModelPackage)

-- | The name of the model group that this model version belongs to.
--
-- This parameter is required for versioned models, and does not apply to
-- unversioned models.
createModelPackage_modelPackageGroupName :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_modelPackageGroupName = Lens.lens (\CreateModelPackage' {modelPackageGroupName} -> modelPackageGroupName) (\s@CreateModelPackage' {} a -> s {modelPackageGroupName = a} :: CreateModelPackage)

-- | A unique token that guarantees that the call to this API is idempotent.
createModelPackage_clientToken :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_clientToken = Lens.lens (\CreateModelPackage' {clientToken} -> clientToken) (\s@CreateModelPackage' {} a -> s {clientToken = a} :: CreateModelPackage)

instance Prelude.AWSRequest CreateModelPackage where
  type
    Rs CreateModelPackage =
      CreateModelPackageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelPackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "ModelPackageArn")
      )

instance Prelude.Hashable CreateModelPackage

instance Prelude.NFData CreateModelPackage

instance Prelude.ToHeaders CreateModelPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.CreateModelPackage" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateModelPackage where
  toJSON CreateModelPackage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SourceAlgorithmSpecification" Prelude..=)
              Prelude.<$> sourceAlgorithmSpecification,
            ("MetadataProperties" Prelude..=)
              Prelude.<$> metadataProperties,
            ("ValidationSpecification" Prelude..=)
              Prelude.<$> validationSpecification,
            ("ModelMetrics" Prelude..=) Prelude.<$> modelMetrics,
            ("CertifyForMarketplace" Prelude..=)
              Prelude.<$> certifyForMarketplace,
            ("ModelPackageName" Prelude..=)
              Prelude.<$> modelPackageName,
            ("ModelApprovalStatus" Prelude..=)
              Prelude.<$> modelApprovalStatus,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("InferenceSpecification" Prelude..=)
              Prelude.<$> inferenceSpecification,
            ("ModelPackageDescription" Prelude..=)
              Prelude.<$> modelPackageDescription,
            ("ModelPackageGroupName" Prelude..=)
              Prelude.<$> modelPackageGroupName,
            ("ClientToken" Prelude..=) Prelude.<$> clientToken
          ]
      )

instance Prelude.ToPath CreateModelPackage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateModelPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateModelPackageResponse' smart constructor.
data CreateModelPackageResponse = CreateModelPackageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the new model package.
    modelPackageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateModelPackageResponse
