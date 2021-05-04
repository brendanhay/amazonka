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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeModelPackage' smart constructor.
data DescribeModelPackage = DescribeModelPackage'
  { -- | The name of the model package to describe.
    modelPackageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeModelPackage
newDescribeModelPackage pModelPackageName_ =
  DescribeModelPackage'
    { modelPackageName =
        pModelPackageName_
    }

-- | The name of the model package to describe.
describeModelPackage_modelPackageName :: Lens.Lens' DescribeModelPackage Prelude.Text
describeModelPackage_modelPackageName = Lens.lens (\DescribeModelPackage' {modelPackageName} -> modelPackageName) (\s@DescribeModelPackage' {} a -> s {modelPackageName = a} :: DescribeModelPackage)

instance Prelude.AWSRequest DescribeModelPackage where
  type
    Rs DescribeModelPackage =
      DescribeModelPackageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelPackageResponse'
            Prelude.<$> (x Prelude..?> "SourceAlgorithmSpecification")
            Prelude.<*> (x Prelude..?> "ModelPackageVersion")
            Prelude.<*> (x Prelude..?> "MetadataProperties")
            Prelude.<*> (x Prelude..?> "ValidationSpecification")
            Prelude.<*> (x Prelude..?> "ModelMetrics")
            Prelude.<*> (x Prelude..?> "CertifyForMarketplace")
            Prelude.<*> (x Prelude..?> "ModelApprovalStatus")
            Prelude.<*> (x Prelude..?> "ApprovalDescription")
            Prelude.<*> (x Prelude..?> "LastModifiedTime")
            Prelude.<*> (x Prelude..?> "InferenceSpecification")
            Prelude.<*> (x Prelude..?> "ModelPackageDescription")
            Prelude.<*> (x Prelude..?> "CreatedBy")
            Prelude.<*> (x Prelude..?> "LastModifiedBy")
            Prelude.<*> (x Prelude..?> "ModelPackageGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "ModelPackageName")
            Prelude.<*> (x Prelude..:> "ModelPackageArn")
            Prelude.<*> (x Prelude..:> "CreationTime")
            Prelude.<*> (x Prelude..:> "ModelPackageStatus")
            Prelude.<*> (x Prelude..:> "ModelPackageStatusDetails")
      )

instance Prelude.Hashable DescribeModelPackage

instance Prelude.NFData DescribeModelPackage

instance Prelude.ToHeaders DescribeModelPackage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DescribeModelPackage" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeModelPackage where
  toJSON DescribeModelPackage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ModelPackageName" Prelude..= modelPackageName)
          ]
      )

instance Prelude.ToPath DescribeModelPackage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeModelPackage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelPackageResponse' smart constructor.
data DescribeModelPackageResponse = DescribeModelPackageResponse'
  { -- | Details about the algorithm that was used to create the model package.
    sourceAlgorithmSpecification :: Prelude.Maybe SourceAlgorithmSpecification,
    -- | The version of the model package.
    modelPackageVersion :: Prelude.Maybe Prelude.Natural,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | Configurations for one or more transform jobs that Amazon SageMaker runs
    -- to test the model package.
    validationSpecification :: Prelude.Maybe ModelPackageValidationSpecification,
    -- | Metrics for the model.
    modelMetrics :: Prelude.Maybe ModelMetrics,
    -- | Whether the model package is certified for listing on AWS Marketplace.
    certifyForMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | The approval status of the model package.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
    -- | A description provided for the model approval.
    approvalDescription :: Prelude.Maybe Prelude.Text,
    -- | The last time the model package was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | Details about inference jobs that can be run with models based on this
    -- model package.
    inferenceSpecification :: Prelude.Maybe InferenceSpecification,
    -- | A brief summary of the model package.
    modelPackageDescription :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe UserContext,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | If the model is a versioned model, the name of the model group that the
    -- versioned model belongs to.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the model package being described.
    modelPackageName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Prelude.Text,
    -- | A timestamp specifying when the model package was created.
    creationTime :: Prelude.POSIX,
    -- | The current status of the model package.
    modelPackageStatus :: ModelPackageStatus,
    -- | Details about the current status of the model package.
    modelPackageStatusDetails :: ModelPackageStatusDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      { sourceAlgorithmSpecification =
          Prelude.Nothing,
        modelPackageVersion = Prelude.Nothing,
        metadataProperties = Prelude.Nothing,
        validationSpecification = Prelude.Nothing,
        modelMetrics = Prelude.Nothing,
        certifyForMarketplace = Prelude.Nothing,
        modelApprovalStatus = Prelude.Nothing,
        approvalDescription = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        inferenceSpecification = Prelude.Nothing,
        modelPackageDescription = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        lastModifiedBy = Prelude.Nothing,
        modelPackageGroupName = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        modelPackageName = pModelPackageName_,
        modelPackageArn = pModelPackageArn_,
        creationTime =
          Prelude._Time Lens.# pCreationTime_,
        modelPackageStatus = pModelPackageStatus_,
        modelPackageStatusDetails =
          pModelPackageStatusDetails_
      }

-- | Details about the algorithm that was used to create the model package.
describeModelPackageResponse_sourceAlgorithmSpecification :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe SourceAlgorithmSpecification)
describeModelPackageResponse_sourceAlgorithmSpecification = Lens.lens (\DescribeModelPackageResponse' {sourceAlgorithmSpecification} -> sourceAlgorithmSpecification) (\s@DescribeModelPackageResponse' {} a -> s {sourceAlgorithmSpecification = a} :: DescribeModelPackageResponse)

-- | The version of the model package.
describeModelPackageResponse_modelPackageVersion :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.Natural)
describeModelPackageResponse_modelPackageVersion = Lens.lens (\DescribeModelPackageResponse' {modelPackageVersion} -> modelPackageVersion) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageVersion = a} :: DescribeModelPackageResponse)

-- | Undocumented member.
describeModelPackageResponse_metadataProperties :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe MetadataProperties)
describeModelPackageResponse_metadataProperties = Lens.lens (\DescribeModelPackageResponse' {metadataProperties} -> metadataProperties) (\s@DescribeModelPackageResponse' {} a -> s {metadataProperties = a} :: DescribeModelPackageResponse)

-- | Configurations for one or more transform jobs that Amazon SageMaker runs
-- to test the model package.
describeModelPackageResponse_validationSpecification :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe ModelPackageValidationSpecification)
describeModelPackageResponse_validationSpecification = Lens.lens (\DescribeModelPackageResponse' {validationSpecification} -> validationSpecification) (\s@DescribeModelPackageResponse' {} a -> s {validationSpecification = a} :: DescribeModelPackageResponse)

-- | Metrics for the model.
describeModelPackageResponse_modelMetrics :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe ModelMetrics)
describeModelPackageResponse_modelMetrics = Lens.lens (\DescribeModelPackageResponse' {modelMetrics} -> modelMetrics) (\s@DescribeModelPackageResponse' {} a -> s {modelMetrics = a} :: DescribeModelPackageResponse)

-- | Whether the model package is certified for listing on AWS Marketplace.
describeModelPackageResponse_certifyForMarketplace :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.Bool)
describeModelPackageResponse_certifyForMarketplace = Lens.lens (\DescribeModelPackageResponse' {certifyForMarketplace} -> certifyForMarketplace) (\s@DescribeModelPackageResponse' {} a -> s {certifyForMarketplace = a} :: DescribeModelPackageResponse)

-- | The approval status of the model package.
describeModelPackageResponse_modelApprovalStatus :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe ModelApprovalStatus)
describeModelPackageResponse_modelApprovalStatus = Lens.lens (\DescribeModelPackageResponse' {modelApprovalStatus} -> modelApprovalStatus) (\s@DescribeModelPackageResponse' {} a -> s {modelApprovalStatus = a} :: DescribeModelPackageResponse)

-- | A description provided for the model approval.
describeModelPackageResponse_approvalDescription :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.Text)
describeModelPackageResponse_approvalDescription = Lens.lens (\DescribeModelPackageResponse' {approvalDescription} -> approvalDescription) (\s@DescribeModelPackageResponse' {} a -> s {approvalDescription = a} :: DescribeModelPackageResponse)

-- | The last time the model package was modified.
describeModelPackageResponse_lastModifiedTime :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.UTCTime)
describeModelPackageResponse_lastModifiedTime = Lens.lens (\DescribeModelPackageResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeModelPackageResponse' {} a -> s {lastModifiedTime = a} :: DescribeModelPackageResponse) Prelude.. Lens.mapping Prelude._Time

-- | Details about inference jobs that can be run with models based on this
-- model package.
describeModelPackageResponse_inferenceSpecification :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe InferenceSpecification)
describeModelPackageResponse_inferenceSpecification = Lens.lens (\DescribeModelPackageResponse' {inferenceSpecification} -> inferenceSpecification) (\s@DescribeModelPackageResponse' {} a -> s {inferenceSpecification = a} :: DescribeModelPackageResponse)

-- | A brief summary of the model package.
describeModelPackageResponse_modelPackageDescription :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.Text)
describeModelPackageResponse_modelPackageDescription = Lens.lens (\DescribeModelPackageResponse' {modelPackageDescription} -> modelPackageDescription) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageDescription = a} :: DescribeModelPackageResponse)

-- | Undocumented member.
describeModelPackageResponse_createdBy :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe UserContext)
describeModelPackageResponse_createdBy = Lens.lens (\DescribeModelPackageResponse' {createdBy} -> createdBy) (\s@DescribeModelPackageResponse' {} a -> s {createdBy = a} :: DescribeModelPackageResponse)

-- | Undocumented member.
describeModelPackageResponse_lastModifiedBy :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe UserContext)
describeModelPackageResponse_lastModifiedBy = Lens.lens (\DescribeModelPackageResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeModelPackageResponse' {} a -> s {lastModifiedBy = a} :: DescribeModelPackageResponse)

-- | If the model is a versioned model, the name of the model group that the
-- versioned model belongs to.
describeModelPackageResponse_modelPackageGroupName :: Lens.Lens' DescribeModelPackageResponse (Prelude.Maybe Prelude.Text)
describeModelPackageResponse_modelPackageGroupName = Lens.lens (\DescribeModelPackageResponse' {modelPackageGroupName} -> modelPackageGroupName) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageGroupName = a} :: DescribeModelPackageResponse)

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
describeModelPackageResponse_creationTime = Lens.lens (\DescribeModelPackageResponse' {creationTime} -> creationTime) (\s@DescribeModelPackageResponse' {} a -> s {creationTime = a} :: DescribeModelPackageResponse) Prelude.. Prelude._Time

-- | The current status of the model package.
describeModelPackageResponse_modelPackageStatus :: Lens.Lens' DescribeModelPackageResponse ModelPackageStatus
describeModelPackageResponse_modelPackageStatus = Lens.lens (\DescribeModelPackageResponse' {modelPackageStatus} -> modelPackageStatus) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageStatus = a} :: DescribeModelPackageResponse)

-- | Details about the current status of the model package.
describeModelPackageResponse_modelPackageStatusDetails :: Lens.Lens' DescribeModelPackageResponse ModelPackageStatusDetails
describeModelPackageResponse_modelPackageStatusDetails = Lens.lens (\DescribeModelPackageResponse' {modelPackageStatusDetails} -> modelPackageStatusDetails) (\s@DescribeModelPackageResponse' {} a -> s {modelPackageStatusDetails = a} :: DescribeModelPackageResponse)

instance Prelude.NFData DescribeModelPackageResponse
