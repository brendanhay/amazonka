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
-- Module      : Amazonka.SageMaker.DescribeModelPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified model package, which is used to
-- create Amazon SageMaker models or list them on Amazon Web Services
-- Marketplace.
--
-- To create models in Amazon SageMaker, buyers can subscribe to model
-- packages listed on Amazon Web Services Marketplace.
module Amazonka.SageMaker.DescribeModelPackage
  ( -- * Creating a Request
    DescribeModelPackage (..),
    newDescribeModelPackage,

    -- * Request Lenses
    describeModelPackage_modelPackageName,

    -- * Destructuring the Response
    DescribeModelPackageResponse (..),
    newDescribeModelPackageResponse,

    -- * Response Lenses
    describeModelPackageResponse_modelPackageVersion,
    describeModelPackageResponse_modelPackageGroupName,
    describeModelPackageResponse_sourceAlgorithmSpecification,
    describeModelPackageResponse_validationSpecification,
    describeModelPackageResponse_certifyForMarketplace,
    describeModelPackageResponse_inferenceSpecification,
    describeModelPackageResponse_modelApprovalStatus,
    describeModelPackageResponse_metadataProperties,
    describeModelPackageResponse_modelPackageDescription,
    describeModelPackageResponse_approvalDescription,
    describeModelPackageResponse_lastModifiedTime,
    describeModelPackageResponse_modelMetrics,
    describeModelPackageResponse_lastModifiedBy,
    describeModelPackageResponse_createdBy,
    describeModelPackageResponse_httpStatus,
    describeModelPackageResponse_modelPackageName,
    describeModelPackageResponse_modelPackageArn,
    describeModelPackageResponse_creationTime,
    describeModelPackageResponse_modelPackageStatus,
    describeModelPackageResponse_modelPackageStatusDetails,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeModelPackage' smart constructor.
data DescribeModelPackage = DescribeModelPackage'
  { -- | The name or Amazon Resource Name (ARN) of the model package to describe.
    --
    -- When you specify a name, the name must have 1 to 63 characters. Valid
    -- characters are a-z, A-Z, 0-9, and - (hyphen).
    modelPackageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageName', 'describeModelPackage_modelPackageName' - The name or Amazon Resource Name (ARN) of the model package to describe.
--
-- When you specify a name, the name must have 1 to 63 characters. Valid
-- characters are a-z, A-Z, 0-9, and - (hyphen).
newDescribeModelPackage ::
  -- | 'modelPackageName'
  Prelude.Text ->
  DescribeModelPackage
newDescribeModelPackage pModelPackageName_ =
  DescribeModelPackage'
    { modelPackageName =
        pModelPackageName_
    }

-- | The name or Amazon Resource Name (ARN) of the model package to describe.
--
-- When you specify a name, the name must have 1 to 63 characters. Valid
-- characters are a-z, A-Z, 0-9, and - (hyphen).
describeModelPackage_modelPackageName :: Lens.Lens' DescribeModelPackage Prelude.Text
describeModelPackage_modelPackageName = Lens.lens (\DescribeModelPackage' {modelPackageName} -> modelPackageName) (\s@DescribeModelPackage' {} a -> s {modelPackageName = a} :: DescribeModelPackage)

instance Core.AWSRequest DescribeModelPackage where
  type
    AWSResponse DescribeModelPackage =
      DescribeModelPackageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelPackageResponse'
            Prelude.<$> (x Core..?> "ModelPackageVersion")
            Prelude.<*> (x Core..?> "ModelPackageGroupName")
            Prelude.<*> (x Core..?> "SourceAlgorithmSpecification")
            Prelude.<*> (x Core..?> "ValidationSpecification")
            Prelude.<*> (x Core..?> "CertifyForMarketplace")
            Prelude.<*> (x Core..?> "InferenceSpecification")
            Prelude.<*> (x Core..?> "ModelApprovalStatus")
            Prelude.<*> (x Core..?> "MetadataProperties")
            Prelude.<*> (x Core..?> "ModelPackageDescription")
            Prelude.<*> (x Core..?> "ApprovalDescription")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "ModelMetrics")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ModelPackageName")
            Prelude.<*> (x Core..:> "ModelPackageArn")
            Prelude.<*> (x Core..:> "CreationTime")
            Prelude.<*> (x Core..:> "ModelPackageStatus")
            Prelude.<*> (x Core..:> "ModelPackageStatusDetails")
      )

instance Prelude.Hashable DescribeModelPackage where
  hashWithSalt _salt DescribeModelPackage' {..} =
    _salt `Prelude.hashWithSalt` modelPackageName

instance Prelude.NFData DescribeModelPackage where
  rnf DescribeModelPackage' {..} =
    Prelude.rnf modelPackageName

instance Core.ToHeaders DescribeModelPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeModelPackage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeModelPackage where
  toJSON DescribeModelPackage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ModelPackageName" Core..= modelPackageName)
          ]
      )

instance Core.ToPath DescribeModelPackage where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeModelPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelPackageResponse' smart constructor.
data DescribeModelPackageResponse = DescribeModelPackageResponse'
  { -- | The version of the model package.
    modelPackageVersion :: Prelude.Maybe Prelude.Natural,
    -- | If the model is a versioned model, the name of the model group that the
    -- versioned model belongs to.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | Details about the algorithm that was used to create the model package.
    sourceAlgorithmSpecification :: Prelude.Maybe SourceAlgorithmSpecification,
    -- | Configurations for one or more transform jobs that Amazon SageMaker runs
    -- to test the model package.
    validationSpecification :: Prelude.Maybe ModelPackageValidationSpecification,
    -- | Whether the model package is certified for listing on Amazon Web
    -- Services Marketplace.
    certifyForMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | Details about inference jobs that can be run with models based on this
    -- model package.
    inferenceSpecification :: Prelude.Maybe InferenceSpecification,
    -- | The approval status of the model package.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | A brief summary of the model package.
    modelPackageDescription :: Prelude.Maybe Prelude.Text,
    -- | A description provided for the model approval.
    approvalDescription :: Prelude.Maybe Prelude.Text,
    -- | The last time the model package was modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | Metrics for the model.
    modelMetrics :: Prelude.Maybe ModelMetrics,
    lastModifiedBy :: Prelude.Maybe UserContext,
    createdBy :: Prelude.Maybe UserContext,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the model package being described.
    modelPackageName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Prelude.Text,
    -- | A timestamp specifying when the model package was created.
    creationTime :: Core.POSIX,
    -- | The current status of the model package.
    modelPackageStatus :: ModelPackageStatus,
    -- | Details about the current status of the model package.
    modelPackageStatusDetails :: ModelPackageStatusDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageVersion', 'describeModelPackageResponse_modelPackageVersion' - The version of the model package.
--
-- 'modelPackageGroupName', 'describeModelPackageResponse_modelPackageGroupName' - If the model is a versioned model, the name of the model group that the
-- versioned model belongs to.
--
-- 'sourceAlgorithmSpecification', 'describeModelPackageResponse_sourceAlgorithmSpecification' - Details about the algorithm that was used to create the model package.
--
-- 'validationSpecification', 'describeModelPackageResponse_validationSpecification' - Configurations for one or more transform jobs that Amazon SageMaker runs
-- to test the model package.
--
-- 'certifyForMarketplace', 'describeModelPackageResponse_certifyForMarketplace' - Whether the model package is certified for listing on Amazon Web
-- Services Marketplace.
--
-- 'inferenceSpecification', 'describeModelPackageResponse_inferenceSpecification' - Details about inference jobs that can be run with models based on this
-- model package.
--
-- 'modelApprovalStatus', 'describeModelPackageResponse_modelApprovalStatus' - The approval status of the model package.
--
-- 'metadataProperties', 'describeModelPackageResponse_metadataProperties' - Undocumented member.
--
-- 'modelPackageDescription', 'describeModelPackageResponse_modelPackageDescription' - A brief summary of the model package.
--
-- 'approvalDescription', 'describeModelPackageResponse_approvalDescription' - A description provided for the model approval.
--
-- 'lastModifiedTime', 'describeModelPackageResponse_lastModifiedTime' - The last time the model package was modified.
--
-- 'modelMetrics', 'describeModelPackageResponse_modelMetrics' - Metrics for the model.
--
-- 'lastModifiedBy', 'describeModelPackageResponse_lastModifiedBy' - Undocumented member.
--
-- 'createdBy', 'describeModelPackageResponse_createdBy' - Undocumented member.
--
-- 'httpStatus', 'describeModelPackageResponse_httpStatus' - The response's http status code.
--
-- 'modelPackageName', 'describeModelPackageResponse_modelPackageName' - The name of the model package being described.
--
-- 'modelPackageArn', 'describeModelPackageResponse_modelPackageArn' - The Amazon Resource Name (ARN) of the model package.
--
-- 'creationTime', 'describeModelPackageResponse_creationTime' - A timestamp specifying when the model package was created.
--
-- 'modelPackageStatus', 'describeModelPackageResponse_modelPackageStatus' - The current status of the model package.
--
-- 'modelPackageStatusDetails', 'describeModelPackageResponse_modelPackageStatusDetails' - Details about the current status of the model package.
newDescribeModelPackageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelPackageName'
  Prelude.Text ->
  -- | 'modelPackageArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'modelPackageStatus'
  ModelPackageStatus ->
  -- | 'modelPackageStatusDetails'
  ModelPackageStatusDetails ->
  DescribeModelPackageResponse
newDescribeModelPackageResponse
  pHttpStatus_
  pModelPackageName_
  pModelPackageArn_
  pCreationTime_
  pModelPackageStatus_
  pModelPackageStatusDetails_ =
    DescribeModelPackageResponse'
      { modelPackageVersion =
          Prelude.Nothing,
        modelPackageGroupName = Prelude.Nothing,
        sourceAlgorithmSpecification =
          Prelude.Nothing,
        validationSpecification = Prelude.Nothing,
        certifyForMarketplace = Prelude.Nothing,
        inferenceSpecification = Prelude.Nothing,
        modelApprovalStatus = Prelude.Nothing,
        metadataProperties = Prelude.Nothing,
        modelPackageDescription = Prelude.Nothing,
        approvalDescription = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        modelMetrics = Prelude.Nothing,
        lastModifiedBy = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        modelPackageName = pModelPackageName_,
        modelPackageArn = pModelPackageArn_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        modelPackageStatus = pModelPackageStatus_,
        modelPackageStatusDetails =
          pModelPackageStatusDetails_
      }

-- | The version of the model package.
describeModelPackageResponse_modelPackageVersion :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.Natural)
describeModelPackageResponse_modelPackageVersion = Lens.lens (\DescribeModelPackageResponse' {modelPackageVersion} -> modelPackageVersion) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageVersion = a} :: DescribeModelPackageResponse)

-- | If the model is a versioned model, the name of the model group that the
-- versioned model belongs to.
describeModelPackageResponse_modelPackageGroupName :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.Text)
describeModelPackageResponse_modelPackageGroupName = Lens.lens (\DescribeModelPackageResponse' {modelPackageGroupName} -> modelPackageGroupName) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageGroupName = a} :: DescribeModelPackageResponse)

-- | Details about the algorithm that was used to create the model package.
describeModelPackageResponse_sourceAlgorithmSpecification :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe SourceAlgorithmSpecification)
describeModelPackageResponse_sourceAlgorithmSpecification = Lens.lens (\DescribeModelPackageResponse' {sourceAlgorithmSpecification} -> sourceAlgorithmSpecification) (\s@DescribeModelPackageResponse' {} a -> s {sourceAlgorithmSpecification = a} :: DescribeModelPackageResponse)

-- | Configurations for one or more transform jobs that Amazon SageMaker runs
-- to test the model package.
describeModelPackageResponse_validationSpecification :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe ModelPackageValidationSpecification)
describeModelPackageResponse_validationSpecification = Lens.lens (\DescribeModelPackageResponse' {validationSpecification} -> validationSpecification) (\s@DescribeModelPackageResponse' {} a -> s {validationSpecification = a} :: DescribeModelPackageResponse)

-- | Whether the model package is certified for listing on Amazon Web
-- Services Marketplace.
describeModelPackageResponse_certifyForMarketplace :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.Bool)
describeModelPackageResponse_certifyForMarketplace = Lens.lens (\DescribeModelPackageResponse' {certifyForMarketplace} -> certifyForMarketplace) (\s@DescribeModelPackageResponse' {} a -> s {certifyForMarketplace = a} :: DescribeModelPackageResponse)

-- | Details about inference jobs that can be run with models based on this
-- model package.
describeModelPackageResponse_inferenceSpecification :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe InferenceSpecification)
describeModelPackageResponse_inferenceSpecification = Lens.lens (\DescribeModelPackageResponse' {inferenceSpecification} -> inferenceSpecification) (\s@DescribeModelPackageResponse' {} a -> s {inferenceSpecification = a} :: DescribeModelPackageResponse)

-- | The approval status of the model package.
describeModelPackageResponse_modelApprovalStatus :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe ModelApprovalStatus)
describeModelPackageResponse_modelApprovalStatus = Lens.lens (\DescribeModelPackageResponse' {modelApprovalStatus} -> modelApprovalStatus) (\s@DescribeModelPackageResponse' {} a -> s {modelApprovalStatus = a} :: DescribeModelPackageResponse)

-- | Undocumented member.
describeModelPackageResponse_metadataProperties :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe MetadataProperties)
describeModelPackageResponse_metadataProperties = Lens.lens (\DescribeModelPackageResponse' {metadataProperties} -> metadataProperties) (\s@DescribeModelPackageResponse' {} a -> s {metadataProperties = a} :: DescribeModelPackageResponse)

-- | A brief summary of the model package.
describeModelPackageResponse_modelPackageDescription :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.Text)
describeModelPackageResponse_modelPackageDescription = Lens.lens (\DescribeModelPackageResponse' {modelPackageDescription} -> modelPackageDescription) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageDescription = a} :: DescribeModelPackageResponse)

-- | A description provided for the model approval.
describeModelPackageResponse_approvalDescription :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.Text)
describeModelPackageResponse_approvalDescription = Lens.lens (\DescribeModelPackageResponse' {approvalDescription} -> approvalDescription) (\s@DescribeModelPackageResponse' {} a -> s {approvalDescription = a} :: DescribeModelPackageResponse)

-- | The last time the model package was modified.
describeModelPackageResponse_lastModifiedTime :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.UTCTime)
describeModelPackageResponse_lastModifiedTime = Lens.lens (\DescribeModelPackageResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeModelPackageResponse' {} a -> s {lastModifiedTime = a} :: DescribeModelPackageResponse) Prelude.. Lens.mapping Core._Time

-- | Metrics for the model.
describeModelPackageResponse_modelMetrics :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe ModelMetrics)
describeModelPackageResponse_modelMetrics = Lens.lens (\DescribeModelPackageResponse' {modelMetrics} -> modelMetrics) (\s@DescribeModelPackageResponse' {} a -> s {modelMetrics = a} :: DescribeModelPackageResponse)

-- | Undocumented member.
describeModelPackageResponse_lastModifiedBy :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe UserContext)
describeModelPackageResponse_lastModifiedBy = Lens.lens (\DescribeModelPackageResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeModelPackageResponse' {} a -> s {lastModifiedBy = a} :: DescribeModelPackageResponse)

-- | Undocumented member.
describeModelPackageResponse_createdBy :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe UserContext)
describeModelPackageResponse_createdBy = Lens.lens (\DescribeModelPackageResponse' {createdBy} -> createdBy) (\s@DescribeModelPackageResponse' {} a -> s {createdBy = a} :: DescribeModelPackageResponse)

-- | The response's http status code.
describeModelPackageResponse_httpStatus :: Lens.Lens' DescribeModelPackageResponse Prelude.Int
describeModelPackageResponse_httpStatus = Lens.lens (\DescribeModelPackageResponse' {httpStatus} -> httpStatus) (\s@DescribeModelPackageResponse' {} a -> s {httpStatus = a} :: DescribeModelPackageResponse)

-- | The name of the model package being described.
describeModelPackageResponse_modelPackageName :: Lens.Lens' DescribeModelPackageResponse Prelude.Text
describeModelPackageResponse_modelPackageName = Lens.lens (\DescribeModelPackageResponse' {modelPackageName} -> modelPackageName) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageName = a} :: DescribeModelPackageResponse)

-- | The Amazon Resource Name (ARN) of the model package.
describeModelPackageResponse_modelPackageArn :: Lens.Lens' DescribeModelPackageResponse Prelude.Text
describeModelPackageResponse_modelPackageArn = Lens.lens (\DescribeModelPackageResponse' {modelPackageArn} -> modelPackageArn) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageArn = a} :: DescribeModelPackageResponse)

-- | A timestamp specifying when the model package was created.
describeModelPackageResponse_creationTime :: Lens.Lens' DescribeModelPackageResponse Prelude.UTCTime
describeModelPackageResponse_creationTime = Lens.lens (\DescribeModelPackageResponse' {creationTime} -> creationTime) (\s@DescribeModelPackageResponse' {} a -> s {creationTime = a} :: DescribeModelPackageResponse) Prelude.. Core._Time

-- | The current status of the model package.
describeModelPackageResponse_modelPackageStatus :: Lens.Lens' DescribeModelPackageResponse ModelPackageStatus
describeModelPackageResponse_modelPackageStatus = Lens.lens (\DescribeModelPackageResponse' {modelPackageStatus} -> modelPackageStatus) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageStatus = a} :: DescribeModelPackageResponse)

-- | Details about the current status of the model package.
describeModelPackageResponse_modelPackageStatusDetails :: Lens.Lens' DescribeModelPackageResponse ModelPackageStatusDetails
describeModelPackageResponse_modelPackageStatusDetails = Lens.lens (\DescribeModelPackageResponse' {modelPackageStatusDetails} -> modelPackageStatusDetails) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageStatusDetails = a} :: DescribeModelPackageResponse)

instance Prelude.NFData DescribeModelPackageResponse where
  rnf DescribeModelPackageResponse' {..} =
    Prelude.rnf modelPackageVersion
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf sourceAlgorithmSpecification
      `Prelude.seq` Prelude.rnf validationSpecification
      `Prelude.seq` Prelude.rnf certifyForMarketplace
      `Prelude.seq` Prelude.rnf inferenceSpecification
      `Prelude.seq` Prelude.rnf modelApprovalStatus
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf modelPackageDescription
      `Prelude.seq` Prelude.rnf approvalDescription
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf modelMetrics
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelPackageName
      `Prelude.seq` Prelude.rnf modelPackageArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf modelPackageStatus
      `Prelude.seq` Prelude.rnf
        modelPackageStatusDetails
