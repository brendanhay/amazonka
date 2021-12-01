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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a model package that you can use to create Amazon SageMaker
-- models or list on Amazon Web Services Marketplace, or a versioned model
-- that is part of a model group. Buyers can subscribe to model packages
-- listed on Amazon Web Services Marketplace to create models in Amazon
-- SageMaker.
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
    createModelPackage_metadataProperties,
    createModelPackage_modelApprovalStatus,
    createModelPackage_sourceAlgorithmSpecification,
    createModelPackage_modelPackageName,
    createModelPackage_clientToken,
    createModelPackage_modelMetrics,
    createModelPackage_modelPackageDescription,
    createModelPackage_validationSpecification,
    createModelPackage_inferenceSpecification,
    createModelPackage_certifyForMarketplace,
    createModelPackage_modelPackageGroupName,
    createModelPackage_tags,

    -- * Destructuring the Response
    CreateModelPackageResponse (..),
    newCreateModelPackageResponse,

    -- * Response Lenses
    createModelPackageResponse_httpStatus,
    createModelPackageResponse_modelPackageArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateModelPackage' smart constructor.
data CreateModelPackage = CreateModelPackage'
  { metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | Whether the model is approved for deployment.
    --
    -- This parameter is optional for versioned models, and does not apply to
    -- unversioned models.
    --
    -- For versioned models, the value of this parameter must be set to
    -- @Approved@ to deploy the model.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
    -- | Details about the algorithm that was used to create the model package.
    sourceAlgorithmSpecification :: Prelude.Maybe SourceAlgorithmSpecification,
    -- | The name of the model package. The name must have 1 to 63 characters.
    -- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    --
    -- This parameter is required for unversioned models. It is not applicable
    -- to versioned models.
    modelPackageName :: Prelude.Maybe Prelude.Text,
    -- | A unique token that guarantees that the call to this API is idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains model metrics reports.
    modelMetrics :: Prelude.Maybe ModelMetrics,
    -- | A description of the model package.
    modelPackageDescription :: Prelude.Maybe Prelude.Text,
    -- | Specifies configurations for one or more transform jobs that Amazon
    -- SageMaker runs to test the model package.
    validationSpecification :: Prelude.Maybe ModelPackageValidationSpecification,
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
    -- | Whether to certify the model package for listing on Amazon Web Services
    -- Marketplace.
    --
    -- This parameter is optional for unversioned models, and does not apply to
    -- versioned models.
    certifyForMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | The name of the model group that this model version belongs to.
    --
    -- This parameter is required for versioned models, and does not apply to
    -- unversioned models.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of key value pairs associated with the model. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag]
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
-- 'sourceAlgorithmSpecification', 'createModelPackage_sourceAlgorithmSpecification' - Details about the algorithm that was used to create the model package.
--
-- 'modelPackageName', 'createModelPackage_modelPackageName' - The name of the model package. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- This parameter is required for unversioned models. It is not applicable
-- to versioned models.
--
-- 'clientToken', 'createModelPackage_clientToken' - A unique token that guarantees that the call to this API is idempotent.
--
-- 'modelMetrics', 'createModelPackage_modelMetrics' - A structure that contains model metrics reports.
--
-- 'modelPackageDescription', 'createModelPackage_modelPackageDescription' - A description of the model package.
--
-- 'validationSpecification', 'createModelPackage_validationSpecification' - Specifies configurations for one or more transform jobs that Amazon
-- SageMaker runs to test the model package.
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
-- 'certifyForMarketplace', 'createModelPackage_certifyForMarketplace' - Whether to certify the model package for listing on Amazon Web Services
-- Marketplace.
--
-- This parameter is optional for unversioned models, and does not apply to
-- versioned models.
--
-- 'modelPackageGroupName', 'createModelPackage_modelPackageGroupName' - The name of the model group that this model version belongs to.
--
-- This parameter is required for versioned models, and does not apply to
-- unversioned models.
--
-- 'tags', 'createModelPackage_tags' - A list of key value pairs associated with the model. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
newCreateModelPackage ::
  CreateModelPackage
newCreateModelPackage =
  CreateModelPackage'
    { metadataProperties =
        Prelude.Nothing,
      modelApprovalStatus = Prelude.Nothing,
      sourceAlgorithmSpecification = Prelude.Nothing,
      modelPackageName = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      modelMetrics = Prelude.Nothing,
      modelPackageDescription = Prelude.Nothing,
      validationSpecification = Prelude.Nothing,
      inferenceSpecification = Prelude.Nothing,
      certifyForMarketplace = Prelude.Nothing,
      modelPackageGroupName = Prelude.Nothing,
      tags = Prelude.Nothing
    }

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

-- | Details about the algorithm that was used to create the model package.
createModelPackage_sourceAlgorithmSpecification :: Lens.Lens' CreateModelPackage (Prelude.Maybe SourceAlgorithmSpecification)
createModelPackage_sourceAlgorithmSpecification = Lens.lens (\CreateModelPackage' {sourceAlgorithmSpecification} -> sourceAlgorithmSpecification) (\s@CreateModelPackage' {} a -> s {sourceAlgorithmSpecification = a} :: CreateModelPackage)

-- | The name of the model package. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- This parameter is required for unversioned models. It is not applicable
-- to versioned models.
createModelPackage_modelPackageName :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_modelPackageName = Lens.lens (\CreateModelPackage' {modelPackageName} -> modelPackageName) (\s@CreateModelPackage' {} a -> s {modelPackageName = a} :: CreateModelPackage)

-- | A unique token that guarantees that the call to this API is idempotent.
createModelPackage_clientToken :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_clientToken = Lens.lens (\CreateModelPackage' {clientToken} -> clientToken) (\s@CreateModelPackage' {} a -> s {clientToken = a} :: CreateModelPackage)

-- | A structure that contains model metrics reports.
createModelPackage_modelMetrics :: Lens.Lens' CreateModelPackage (Prelude.Maybe ModelMetrics)
createModelPackage_modelMetrics = Lens.lens (\CreateModelPackage' {modelMetrics} -> modelMetrics) (\s@CreateModelPackage' {} a -> s {modelMetrics = a} :: CreateModelPackage)

-- | A description of the model package.
createModelPackage_modelPackageDescription :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_modelPackageDescription = Lens.lens (\CreateModelPackage' {modelPackageDescription} -> modelPackageDescription) (\s@CreateModelPackage' {} a -> s {modelPackageDescription = a} :: CreateModelPackage)

-- | Specifies configurations for one or more transform jobs that Amazon
-- SageMaker runs to test the model package.
createModelPackage_validationSpecification :: Lens.Lens' CreateModelPackage (Prelude.Maybe ModelPackageValidationSpecification)
createModelPackage_validationSpecification = Lens.lens (\CreateModelPackage' {validationSpecification} -> validationSpecification) (\s@CreateModelPackage' {} a -> s {validationSpecification = a} :: CreateModelPackage)

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

-- | Whether to certify the model package for listing on Amazon Web Services
-- Marketplace.
--
-- This parameter is optional for unversioned models, and does not apply to
-- versioned models.
createModelPackage_certifyForMarketplace :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Bool)
createModelPackage_certifyForMarketplace = Lens.lens (\CreateModelPackage' {certifyForMarketplace} -> certifyForMarketplace) (\s@CreateModelPackage' {} a -> s {certifyForMarketplace = a} :: CreateModelPackage)

-- | The name of the model group that this model version belongs to.
--
-- This parameter is required for versioned models, and does not apply to
-- unversioned models.
createModelPackage_modelPackageGroupName :: Lens.Lens' CreateModelPackage (Prelude.Maybe Prelude.Text)
createModelPackage_modelPackageGroupName = Lens.lens (\CreateModelPackage' {modelPackageGroupName} -> modelPackageGroupName) (\s@CreateModelPackage' {} a -> s {modelPackageGroupName = a} :: CreateModelPackage)

-- | A list of key value pairs associated with the model. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
createModelPackage_tags :: Lens.Lens' CreateModelPackage (Prelude.Maybe [Tag])
createModelPackage_tags = Lens.lens (\CreateModelPackage' {tags} -> tags) (\s@CreateModelPackage' {} a -> s {tags = a} :: CreateModelPackage) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateModelPackage where
  type
    AWSResponse CreateModelPackage =
      CreateModelPackageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateModelPackageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ModelPackageArn")
      )

instance Prelude.Hashable CreateModelPackage where
  hashWithSalt salt' CreateModelPackage' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` modelPackageGroupName
      `Prelude.hashWithSalt` certifyForMarketplace
      `Prelude.hashWithSalt` inferenceSpecification
      `Prelude.hashWithSalt` validationSpecification
      `Prelude.hashWithSalt` modelPackageDescription
      `Prelude.hashWithSalt` modelMetrics
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` modelPackageName
      `Prelude.hashWithSalt` sourceAlgorithmSpecification
      `Prelude.hashWithSalt` modelApprovalStatus
      `Prelude.hashWithSalt` metadataProperties

instance Prelude.NFData CreateModelPackage where
  rnf CreateModelPackage' {..} =
    Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf certifyForMarketplace
      `Prelude.seq` Prelude.rnf inferenceSpecification
      `Prelude.seq` Prelude.rnf validationSpecification
      `Prelude.seq` Prelude.rnf modelPackageDescription
      `Prelude.seq` Prelude.rnf modelMetrics
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf modelPackageName
      `Prelude.seq` Prelude.rnf sourceAlgorithmSpecification
      `Prelude.seq` Prelude.rnf modelApprovalStatus

instance Core.ToHeaders CreateModelPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateModelPackage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateModelPackage where
  toJSON CreateModelPackage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MetadataProperties" Core..=)
              Prelude.<$> metadataProperties,
            ("ModelApprovalStatus" Core..=)
              Prelude.<$> modelApprovalStatus,
            ("SourceAlgorithmSpecification" Core..=)
              Prelude.<$> sourceAlgorithmSpecification,
            ("ModelPackageName" Core..=)
              Prelude.<$> modelPackageName,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("ModelMetrics" Core..=) Prelude.<$> modelMetrics,
            ("ModelPackageDescription" Core..=)
              Prelude.<$> modelPackageDescription,
            ("ValidationSpecification" Core..=)
              Prelude.<$> validationSpecification,
            ("InferenceSpecification" Core..=)
              Prelude.<$> inferenceSpecification,
            ("CertifyForMarketplace" Core..=)
              Prelude.<$> certifyForMarketplace,
            ("ModelPackageGroupName" Core..=)
              Prelude.<$> modelPackageGroupName,
            ("Tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateModelPackage where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateModelPackage where
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
