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
-- Module      : Network.AWS.SageMaker.DescribeModelPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified model package, which is used to
-- create Amazon SageMaker models or list them on AWS Marketplace.
--
-- To create models in Amazon SageMaker, buyers can subscribe to model
-- packages listed on AWS Marketplace.
module Network.AWS.SageMaker.DescribeModelPackage
  ( -- * Creating a Request
    DescribeModelPackage (..),
    newDescribeModelPackage,

    -- * Request Lenses
    describeModelPackage_modelPackageName,

    -- * Destructuring the Response
    DescribeModelPackageResponse (..),
    newDescribeModelPackageResponse,

    -- * Response Lenses
    describeModelPackageResponse_sourceAlgorithmSpecification,
    describeModelPackageResponse_modelPackageVersion,
    describeModelPackageResponse_metadataProperties,
    describeModelPackageResponse_validationSpecification,
    describeModelPackageResponse_modelMetrics,
    describeModelPackageResponse_certifyForMarketplace,
    describeModelPackageResponse_modelApprovalStatus,
    describeModelPackageResponse_approvalDescription,
    describeModelPackageResponse_lastModifiedTime,
    describeModelPackageResponse_inferenceSpecification,
    describeModelPackageResponse_modelPackageDescription,
    describeModelPackageResponse_createdBy,
    describeModelPackageResponse_lastModifiedBy,
    describeModelPackageResponse_modelPackageGroupName,
    describeModelPackageResponse_httpStatus,
    describeModelPackageResponse_modelPackageName,
    describeModelPackageResponse_modelPackageArn,
    describeModelPackageResponse_creationTime,
    describeModelPackageResponse_modelPackageStatus,
    describeModelPackageResponse_modelPackageStatusDetails,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeModelPackage' smart constructor.
data DescribeModelPackage = DescribeModelPackage'
  { -- | The name of the model package to describe.
    modelPackageName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeModelPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageName', 'describeModelPackage_modelPackageName' - The name of the model package to describe.
newDescribeModelPackage ::
  -- | 'modelPackageName'
  Core.Text ->
  DescribeModelPackage
newDescribeModelPackage pModelPackageName_ =
  DescribeModelPackage'
    { modelPackageName =
        pModelPackageName_
    }

-- | The name of the model package to describe.
describeModelPackage_modelPackageName :: Lens.Lens' DescribeModelPackage Core.Text
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
            Core.<$> (x Core..?> "SourceAlgorithmSpecification")
            Core.<*> (x Core..?> "ModelPackageVersion")
            Core.<*> (x Core..?> "MetadataProperties")
            Core.<*> (x Core..?> "ValidationSpecification")
            Core.<*> (x Core..?> "ModelMetrics")
            Core.<*> (x Core..?> "CertifyForMarketplace")
            Core.<*> (x Core..?> "ModelApprovalStatus")
            Core.<*> (x Core..?> "ApprovalDescription")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "InferenceSpecification")
            Core.<*> (x Core..?> "ModelPackageDescription")
            Core.<*> (x Core..?> "CreatedBy")
            Core.<*> (x Core..?> "LastModifiedBy")
            Core.<*> (x Core..?> "ModelPackageGroupName")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ModelPackageName")
            Core.<*> (x Core..:> "ModelPackageArn")
            Core.<*> (x Core..:> "CreationTime")
            Core.<*> (x Core..:> "ModelPackageStatus")
            Core.<*> (x Core..:> "ModelPackageStatusDetails")
      )

instance Core.Hashable DescribeModelPackage

instance Core.NFData DescribeModelPackage

instance Core.ToHeaders DescribeModelPackage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeModelPackage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeModelPackage where
  toJSON DescribeModelPackage' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ModelPackageName" Core..= modelPackageName)
          ]
      )

instance Core.ToPath DescribeModelPackage where
  toPath = Core.const "/"

instance Core.ToQuery DescribeModelPackage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeModelPackageResponse' smart constructor.
data DescribeModelPackageResponse = DescribeModelPackageResponse'
  { -- | Details about the algorithm that was used to create the model package.
    sourceAlgorithmSpecification :: Core.Maybe SourceAlgorithmSpecification,
    -- | The version of the model package.
    modelPackageVersion :: Core.Maybe Core.Natural,
    metadataProperties :: Core.Maybe MetadataProperties,
    -- | Configurations for one or more transform jobs that Amazon SageMaker runs
    -- to test the model package.
    validationSpecification :: Core.Maybe ModelPackageValidationSpecification,
    -- | Metrics for the model.
    modelMetrics :: Core.Maybe ModelMetrics,
    -- | Whether the model package is certified for listing on AWS Marketplace.
    certifyForMarketplace :: Core.Maybe Core.Bool,
    -- | The approval status of the model package.
    modelApprovalStatus :: Core.Maybe ModelApprovalStatus,
    -- | A description provided for the model approval.
    approvalDescription :: Core.Maybe Core.Text,
    -- | The last time the model package was modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | Details about inference jobs that can be run with models based on this
    -- model package.
    inferenceSpecification :: Core.Maybe InferenceSpecification,
    -- | A brief summary of the model package.
    modelPackageDescription :: Core.Maybe Core.Text,
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | If the model is a versioned model, the name of the model group that the
    -- versioned model belongs to.
    modelPackageGroupName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The name of the model package being described.
    modelPackageName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Core.Text,
    -- | A timestamp specifying when the model package was created.
    creationTime :: Core.POSIX,
    -- | The current status of the model package.
    modelPackageStatus :: ModelPackageStatus,
    -- | Details about the current status of the model package.
    modelPackageStatusDetails :: ModelPackageStatusDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeModelPackageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceAlgorithmSpecification', 'describeModelPackageResponse_sourceAlgorithmSpecification' - Details about the algorithm that was used to create the model package.
--
-- 'modelPackageVersion', 'describeModelPackageResponse_modelPackageVersion' - The version of the model package.
--
-- 'metadataProperties', 'describeModelPackageResponse_metadataProperties' - Undocumented member.
--
-- 'validationSpecification', 'describeModelPackageResponse_validationSpecification' - Configurations for one or more transform jobs that Amazon SageMaker runs
-- to test the model package.
--
-- 'modelMetrics', 'describeModelPackageResponse_modelMetrics' - Metrics for the model.
--
-- 'certifyForMarketplace', 'describeModelPackageResponse_certifyForMarketplace' - Whether the model package is certified for listing on AWS Marketplace.
--
-- 'modelApprovalStatus', 'describeModelPackageResponse_modelApprovalStatus' - The approval status of the model package.
--
-- 'approvalDescription', 'describeModelPackageResponse_approvalDescription' - A description provided for the model approval.
--
-- 'lastModifiedTime', 'describeModelPackageResponse_lastModifiedTime' - The last time the model package was modified.
--
-- 'inferenceSpecification', 'describeModelPackageResponse_inferenceSpecification' - Details about inference jobs that can be run with models based on this
-- model package.
--
-- 'modelPackageDescription', 'describeModelPackageResponse_modelPackageDescription' - A brief summary of the model package.
--
-- 'createdBy', 'describeModelPackageResponse_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'describeModelPackageResponse_lastModifiedBy' - Undocumented member.
--
-- 'modelPackageGroupName', 'describeModelPackageResponse_modelPackageGroupName' - If the model is a versioned model, the name of the model group that the
-- versioned model belongs to.
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
  Core.Int ->
  -- | 'modelPackageName'
  Core.Text ->
  -- | 'modelPackageArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
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
      { sourceAlgorithmSpecification =
          Core.Nothing,
        modelPackageVersion = Core.Nothing,
        metadataProperties = Core.Nothing,
        validationSpecification = Core.Nothing,
        modelMetrics = Core.Nothing,
        certifyForMarketplace = Core.Nothing,
        modelApprovalStatus = Core.Nothing,
        approvalDescription = Core.Nothing,
        lastModifiedTime = Core.Nothing,
        inferenceSpecification = Core.Nothing,
        modelPackageDescription = Core.Nothing,
        createdBy = Core.Nothing,
        lastModifiedBy = Core.Nothing,
        modelPackageGroupName = Core.Nothing,
        httpStatus = pHttpStatus_,
        modelPackageName = pModelPackageName_,
        modelPackageArn = pModelPackageArn_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        modelPackageStatus = pModelPackageStatus_,
        modelPackageStatusDetails =
          pModelPackageStatusDetails_
      }

-- | Details about the algorithm that was used to create the model package.
describeModelPackageResponse_sourceAlgorithmSpecification :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe SourceAlgorithmSpecification)
describeModelPackageResponse_sourceAlgorithmSpecification = Lens.lens (\DescribeModelPackageResponse' {sourceAlgorithmSpecification} -> sourceAlgorithmSpecification) (\s@DescribeModelPackageResponse' {} a -> s {sourceAlgorithmSpecification = a} :: DescribeModelPackageResponse)

-- | The version of the model package.
describeModelPackageResponse_modelPackageVersion :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Core.Natural)
describeModelPackageResponse_modelPackageVersion = Lens.lens (\DescribeModelPackageResponse' {modelPackageVersion} -> modelPackageVersion) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageVersion = a} :: DescribeModelPackageResponse)

-- | Undocumented member.
describeModelPackageResponse_metadataProperties :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe MetadataProperties)
describeModelPackageResponse_metadataProperties = Lens.lens (\DescribeModelPackageResponse' {metadataProperties} -> metadataProperties) (\s@DescribeModelPackageResponse' {} a -> s {metadataProperties = a} :: DescribeModelPackageResponse)

-- | Configurations for one or more transform jobs that Amazon SageMaker runs
-- to test the model package.
describeModelPackageResponse_validationSpecification :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe ModelPackageValidationSpecification)
describeModelPackageResponse_validationSpecification = Lens.lens (\DescribeModelPackageResponse' {validationSpecification} -> validationSpecification) (\s@DescribeModelPackageResponse' {} a -> s {validationSpecification = a} :: DescribeModelPackageResponse)

-- | Metrics for the model.
describeModelPackageResponse_modelMetrics :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe ModelMetrics)
describeModelPackageResponse_modelMetrics = Lens.lens (\DescribeModelPackageResponse' {modelMetrics} -> modelMetrics) (\s@DescribeModelPackageResponse' {} a -> s {modelMetrics = a} :: DescribeModelPackageResponse)

-- | Whether the model package is certified for listing on AWS Marketplace.
describeModelPackageResponse_certifyForMarketplace :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Core.Bool)
describeModelPackageResponse_certifyForMarketplace = Lens.lens (\DescribeModelPackageResponse' {certifyForMarketplace} -> certifyForMarketplace) (\s@DescribeModelPackageResponse' {} a -> s {certifyForMarketplace = a} :: DescribeModelPackageResponse)

-- | The approval status of the model package.
describeModelPackageResponse_modelApprovalStatus :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe ModelApprovalStatus)
describeModelPackageResponse_modelApprovalStatus = Lens.lens (\DescribeModelPackageResponse' {modelApprovalStatus} -> modelApprovalStatus) (\s@DescribeModelPackageResponse' {} a -> s {modelApprovalStatus = a} :: DescribeModelPackageResponse)

-- | A description provided for the model approval.
describeModelPackageResponse_approvalDescription :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Core.Text)
describeModelPackageResponse_approvalDescription = Lens.lens (\DescribeModelPackageResponse' {approvalDescription} -> approvalDescription) (\s@DescribeModelPackageResponse' {} a -> s {approvalDescription = a} :: DescribeModelPackageResponse)

-- | The last time the model package was modified.
describeModelPackageResponse_lastModifiedTime :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Core.UTCTime)
describeModelPackageResponse_lastModifiedTime = Lens.lens (\DescribeModelPackageResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeModelPackageResponse' {} a -> s {lastModifiedTime = a} :: DescribeModelPackageResponse) Core.. Lens.mapping Core._Time

-- | Details about inference jobs that can be run with models based on this
-- model package.
describeModelPackageResponse_inferenceSpecification :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe InferenceSpecification)
describeModelPackageResponse_inferenceSpecification = Lens.lens (\DescribeModelPackageResponse' {inferenceSpecification} -> inferenceSpecification) (\s@DescribeModelPackageResponse' {} a -> s {inferenceSpecification = a} :: DescribeModelPackageResponse)

-- | A brief summary of the model package.
describeModelPackageResponse_modelPackageDescription :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Core.Text)
describeModelPackageResponse_modelPackageDescription = Lens.lens (\DescribeModelPackageResponse' {modelPackageDescription} -> modelPackageDescription) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageDescription = a} :: DescribeModelPackageResponse)

-- | Undocumented member.
describeModelPackageResponse_createdBy :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe UserContext)
describeModelPackageResponse_createdBy = Lens.lens (\DescribeModelPackageResponse' {createdBy} -> createdBy) (\s@DescribeModelPackageResponse' {} a -> s {createdBy = a} :: DescribeModelPackageResponse)

-- | Undocumented member.
describeModelPackageResponse_lastModifiedBy :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe UserContext)
describeModelPackageResponse_lastModifiedBy = Lens.lens (\DescribeModelPackageResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeModelPackageResponse' {} a -> s {lastModifiedBy = a} :: DescribeModelPackageResponse)

-- | If the model is a versioned model, the name of the model group that the
-- versioned model belongs to.
describeModelPackageResponse_modelPackageGroupName :: Lens.Lens' DescribeModelPackageResponse (Core.Maybe Core.Text)
describeModelPackageResponse_modelPackageGroupName = Lens.lens (\DescribeModelPackageResponse' {modelPackageGroupName} -> modelPackageGroupName) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageGroupName = a} :: DescribeModelPackageResponse)

-- | The response's http status code.
describeModelPackageResponse_httpStatus :: Lens.Lens' DescribeModelPackageResponse Core.Int
describeModelPackageResponse_httpStatus = Lens.lens (\DescribeModelPackageResponse' {httpStatus} -> httpStatus) (\s@DescribeModelPackageResponse' {} a -> s {httpStatus = a} :: DescribeModelPackageResponse)

-- | The name of the model package being described.
describeModelPackageResponse_modelPackageName :: Lens.Lens' DescribeModelPackageResponse Core.Text
describeModelPackageResponse_modelPackageName = Lens.lens (\DescribeModelPackageResponse' {modelPackageName} -> modelPackageName) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageName = a} :: DescribeModelPackageResponse)

-- | The Amazon Resource Name (ARN) of the model package.
describeModelPackageResponse_modelPackageArn :: Lens.Lens' DescribeModelPackageResponse Core.Text
describeModelPackageResponse_modelPackageArn = Lens.lens (\DescribeModelPackageResponse' {modelPackageArn} -> modelPackageArn) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageArn = a} :: DescribeModelPackageResponse)

-- | A timestamp specifying when the model package was created.
describeModelPackageResponse_creationTime :: Lens.Lens' DescribeModelPackageResponse Core.UTCTime
describeModelPackageResponse_creationTime = Lens.lens (\DescribeModelPackageResponse' {creationTime} -> creationTime) (\s@DescribeModelPackageResponse' {} a -> s {creationTime = a} :: DescribeModelPackageResponse) Core.. Core._Time

-- | The current status of the model package.
describeModelPackageResponse_modelPackageStatus :: Lens.Lens' DescribeModelPackageResponse ModelPackageStatus
describeModelPackageResponse_modelPackageStatus = Lens.lens (\DescribeModelPackageResponse' {modelPackageStatus} -> modelPackageStatus) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageStatus = a} :: DescribeModelPackageResponse)

-- | Details about the current status of the model package.
describeModelPackageResponse_modelPackageStatusDetails :: Lens.Lens' DescribeModelPackageResponse ModelPackageStatusDetails
describeModelPackageResponse_modelPackageStatusDetails = Lens.lens (\DescribeModelPackageResponse' {modelPackageStatusDetails} -> modelPackageStatusDetails) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageStatusDetails = a} :: DescribeModelPackageResponse)

instance Core.NFData DescribeModelPackageResponse
