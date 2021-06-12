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
-- Module      : Network.AWS.SageMaker.Types.ModelPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.InferenceSpecification
import Network.AWS.SageMaker.Types.MetadataProperties
import Network.AWS.SageMaker.Types.ModelApprovalStatus
import Network.AWS.SageMaker.Types.ModelMetrics
import Network.AWS.SageMaker.Types.ModelPackageStatus
import Network.AWS.SageMaker.Types.ModelPackageStatusDetails
import Network.AWS.SageMaker.Types.ModelPackageValidationSpecification
import Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.UserContext

-- | A versioned model that can be deployed for SageMaker inference.
--
-- /See:/ 'newModelPackage' smart constructor.
data ModelPackage = ModelPackage'
  { sourceAlgorithmSpecification :: Core.Maybe SourceAlgorithmSpecification,
    -- | The version number of a versioned model.
    modelPackageVersion :: Core.Maybe Core.Natural,
    metadataProperties :: Core.Maybe MetadataProperties,
    -- | The time that the model package was created.
    creationTime :: Core.Maybe Core.POSIX,
    validationSpecification :: Core.Maybe ModelPackageValidationSpecification,
    modelPackageStatusDetails :: Core.Maybe ModelPackageStatusDetails,
    -- | Metrics for the model.
    modelMetrics :: Core.Maybe ModelMetrics,
    -- | Whether the model package is to be certified to be listed on AWS
    -- Marketplace. For information about listing model packages on AWS
    -- Marketplace, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-mkt-list.html List Your Algorithm or Model Package on AWS Marketplace>.
    certifyForMarketplace :: Core.Maybe Core.Bool,
    -- | The name of the model.
    modelPackageName :: Core.Maybe Core.Text,
    -- | The approval status of the model. This can be one of the following
    -- values.
    --
    -- -   @APPROVED@ - The model is approved
    --
    -- -   @REJECTED@ - The model is rejected.
    --
    -- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
    --     approval.
    modelApprovalStatus :: Core.Maybe ModelApprovalStatus,
    -- | A description provided when the model approval is set.
    approvalDescription :: Core.Maybe Core.Text,
    -- | A list of the tags associated with the model package. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
    -- in the /AWS General Reference Guide/.
    tags :: Core.Maybe [Tag],
    -- | The status of the model package. This can be one of the following
    -- values.
    --
    -- -   @PENDING@ - The model package is pending being created.
    --
    -- -   @IN_PROGRESS@ - The model package is in the process of being
    --     created.
    --
    -- -   @COMPLETED@ - The model package was successfully created.
    --
    -- -   @FAILED@ - The model package failed.
    --
    -- -   @DELETING@ - The model package is in the process of being deleted.
    modelPackageStatus :: Core.Maybe ModelPackageStatus,
    -- | The last time the model package was modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    inferenceSpecification :: Core.Maybe InferenceSpecification,
    -- | The description of the model package.
    modelPackageDescription :: Core.Maybe Core.Text,
    createdBy :: Core.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Core.Maybe Core.Text,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The model group to which the model belongs.
    modelPackageGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceAlgorithmSpecification', 'modelPackage_sourceAlgorithmSpecification' - Undocumented member.
--
-- 'modelPackageVersion', 'modelPackage_modelPackageVersion' - The version number of a versioned model.
--
-- 'metadataProperties', 'modelPackage_metadataProperties' - Undocumented member.
--
-- 'creationTime', 'modelPackage_creationTime' - The time that the model package was created.
--
-- 'validationSpecification', 'modelPackage_validationSpecification' - Undocumented member.
--
-- 'modelPackageStatusDetails', 'modelPackage_modelPackageStatusDetails' - Undocumented member.
--
-- 'modelMetrics', 'modelPackage_modelMetrics' - Metrics for the model.
--
-- 'certifyForMarketplace', 'modelPackage_certifyForMarketplace' - Whether the model package is to be certified to be listed on AWS
-- Marketplace. For information about listing model packages on AWS
-- Marketplace, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-mkt-list.html List Your Algorithm or Model Package on AWS Marketplace>.
--
-- 'modelPackageName', 'modelPackage_modelPackageName' - The name of the model.
--
-- 'modelApprovalStatus', 'modelPackage_modelApprovalStatus' - The approval status of the model. This can be one of the following
-- values.
--
-- -   @APPROVED@ - The model is approved
--
-- -   @REJECTED@ - The model is rejected.
--
-- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
--     approval.
--
-- 'approvalDescription', 'modelPackage_approvalDescription' - A description provided when the model approval is set.
--
-- 'tags', 'modelPackage_tags' - A list of the tags associated with the model package. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
--
-- 'modelPackageStatus', 'modelPackage_modelPackageStatus' - The status of the model package. This can be one of the following
-- values.
--
-- -   @PENDING@ - The model package is pending being created.
--
-- -   @IN_PROGRESS@ - The model package is in the process of being
--     created.
--
-- -   @COMPLETED@ - The model package was successfully created.
--
-- -   @FAILED@ - The model package failed.
--
-- -   @DELETING@ - The model package is in the process of being deleted.
--
-- 'lastModifiedTime', 'modelPackage_lastModifiedTime' - The last time the model package was modified.
--
-- 'inferenceSpecification', 'modelPackage_inferenceSpecification' - Undocumented member.
--
-- 'modelPackageDescription', 'modelPackage_modelPackageDescription' - The description of the model package.
--
-- 'createdBy', 'modelPackage_createdBy' - Undocumented member.
--
-- 'modelPackageArn', 'modelPackage_modelPackageArn' - The Amazon Resource Name (ARN) of the model package.
--
-- 'lastModifiedBy', 'modelPackage_lastModifiedBy' - Undocumented member.
--
-- 'modelPackageGroupName', 'modelPackage_modelPackageGroupName' - The model group to which the model belongs.
newModelPackage ::
  ModelPackage
newModelPackage =
  ModelPackage'
    { sourceAlgorithmSpecification =
        Core.Nothing,
      modelPackageVersion = Core.Nothing,
      metadataProperties = Core.Nothing,
      creationTime = Core.Nothing,
      validationSpecification = Core.Nothing,
      modelPackageStatusDetails = Core.Nothing,
      modelMetrics = Core.Nothing,
      certifyForMarketplace = Core.Nothing,
      modelPackageName = Core.Nothing,
      modelApprovalStatus = Core.Nothing,
      approvalDescription = Core.Nothing,
      tags = Core.Nothing,
      modelPackageStatus = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      inferenceSpecification = Core.Nothing,
      modelPackageDescription = Core.Nothing,
      createdBy = Core.Nothing,
      modelPackageArn = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      modelPackageGroupName = Core.Nothing
    }

-- | Undocumented member.
modelPackage_sourceAlgorithmSpecification :: Lens.Lens' ModelPackage (Core.Maybe SourceAlgorithmSpecification)
modelPackage_sourceAlgorithmSpecification = Lens.lens (\ModelPackage' {sourceAlgorithmSpecification} -> sourceAlgorithmSpecification) (\s@ModelPackage' {} a -> s {sourceAlgorithmSpecification = a} :: ModelPackage)

-- | The version number of a versioned model.
modelPackage_modelPackageVersion :: Lens.Lens' ModelPackage (Core.Maybe Core.Natural)
modelPackage_modelPackageVersion = Lens.lens (\ModelPackage' {modelPackageVersion} -> modelPackageVersion) (\s@ModelPackage' {} a -> s {modelPackageVersion = a} :: ModelPackage)

-- | Undocumented member.
modelPackage_metadataProperties :: Lens.Lens' ModelPackage (Core.Maybe MetadataProperties)
modelPackage_metadataProperties = Lens.lens (\ModelPackage' {metadataProperties} -> metadataProperties) (\s@ModelPackage' {} a -> s {metadataProperties = a} :: ModelPackage)

-- | The time that the model package was created.
modelPackage_creationTime :: Lens.Lens' ModelPackage (Core.Maybe Core.UTCTime)
modelPackage_creationTime = Lens.lens (\ModelPackage' {creationTime} -> creationTime) (\s@ModelPackage' {} a -> s {creationTime = a} :: ModelPackage) Core.. Lens.mapping Core._Time

-- | Undocumented member.
modelPackage_validationSpecification :: Lens.Lens' ModelPackage (Core.Maybe ModelPackageValidationSpecification)
modelPackage_validationSpecification = Lens.lens (\ModelPackage' {validationSpecification} -> validationSpecification) (\s@ModelPackage' {} a -> s {validationSpecification = a} :: ModelPackage)

-- | Undocumented member.
modelPackage_modelPackageStatusDetails :: Lens.Lens' ModelPackage (Core.Maybe ModelPackageStatusDetails)
modelPackage_modelPackageStatusDetails = Lens.lens (\ModelPackage' {modelPackageStatusDetails} -> modelPackageStatusDetails) (\s@ModelPackage' {} a -> s {modelPackageStatusDetails = a} :: ModelPackage)

-- | Metrics for the model.
modelPackage_modelMetrics :: Lens.Lens' ModelPackage (Core.Maybe ModelMetrics)
modelPackage_modelMetrics = Lens.lens (\ModelPackage' {modelMetrics} -> modelMetrics) (\s@ModelPackage' {} a -> s {modelMetrics = a} :: ModelPackage)

-- | Whether the model package is to be certified to be listed on AWS
-- Marketplace. For information about listing model packages on AWS
-- Marketplace, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-mkt-list.html List Your Algorithm or Model Package on AWS Marketplace>.
modelPackage_certifyForMarketplace :: Lens.Lens' ModelPackage (Core.Maybe Core.Bool)
modelPackage_certifyForMarketplace = Lens.lens (\ModelPackage' {certifyForMarketplace} -> certifyForMarketplace) (\s@ModelPackage' {} a -> s {certifyForMarketplace = a} :: ModelPackage)

-- | The name of the model.
modelPackage_modelPackageName :: Lens.Lens' ModelPackage (Core.Maybe Core.Text)
modelPackage_modelPackageName = Lens.lens (\ModelPackage' {modelPackageName} -> modelPackageName) (\s@ModelPackage' {} a -> s {modelPackageName = a} :: ModelPackage)

-- | The approval status of the model. This can be one of the following
-- values.
--
-- -   @APPROVED@ - The model is approved
--
-- -   @REJECTED@ - The model is rejected.
--
-- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
--     approval.
modelPackage_modelApprovalStatus :: Lens.Lens' ModelPackage (Core.Maybe ModelApprovalStatus)
modelPackage_modelApprovalStatus = Lens.lens (\ModelPackage' {modelApprovalStatus} -> modelApprovalStatus) (\s@ModelPackage' {} a -> s {modelApprovalStatus = a} :: ModelPackage)

-- | A description provided when the model approval is set.
modelPackage_approvalDescription :: Lens.Lens' ModelPackage (Core.Maybe Core.Text)
modelPackage_approvalDescription = Lens.lens (\ModelPackage' {approvalDescription} -> approvalDescription) (\s@ModelPackage' {} a -> s {approvalDescription = a} :: ModelPackage)

-- | A list of the tags associated with the model package. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
modelPackage_tags :: Lens.Lens' ModelPackage (Core.Maybe [Tag])
modelPackage_tags = Lens.lens (\ModelPackage' {tags} -> tags) (\s@ModelPackage' {} a -> s {tags = a} :: ModelPackage) Core.. Lens.mapping Lens._Coerce

-- | The status of the model package. This can be one of the following
-- values.
--
-- -   @PENDING@ - The model package is pending being created.
--
-- -   @IN_PROGRESS@ - The model package is in the process of being
--     created.
--
-- -   @COMPLETED@ - The model package was successfully created.
--
-- -   @FAILED@ - The model package failed.
--
-- -   @DELETING@ - The model package is in the process of being deleted.
modelPackage_modelPackageStatus :: Lens.Lens' ModelPackage (Core.Maybe ModelPackageStatus)
modelPackage_modelPackageStatus = Lens.lens (\ModelPackage' {modelPackageStatus} -> modelPackageStatus) (\s@ModelPackage' {} a -> s {modelPackageStatus = a} :: ModelPackage)

-- | The last time the model package was modified.
modelPackage_lastModifiedTime :: Lens.Lens' ModelPackage (Core.Maybe Core.UTCTime)
modelPackage_lastModifiedTime = Lens.lens (\ModelPackage' {lastModifiedTime} -> lastModifiedTime) (\s@ModelPackage' {} a -> s {lastModifiedTime = a} :: ModelPackage) Core.. Lens.mapping Core._Time

-- | Undocumented member.
modelPackage_inferenceSpecification :: Lens.Lens' ModelPackage (Core.Maybe InferenceSpecification)
modelPackage_inferenceSpecification = Lens.lens (\ModelPackage' {inferenceSpecification} -> inferenceSpecification) (\s@ModelPackage' {} a -> s {inferenceSpecification = a} :: ModelPackage)

-- | The description of the model package.
modelPackage_modelPackageDescription :: Lens.Lens' ModelPackage (Core.Maybe Core.Text)
modelPackage_modelPackageDescription = Lens.lens (\ModelPackage' {modelPackageDescription} -> modelPackageDescription) (\s@ModelPackage' {} a -> s {modelPackageDescription = a} :: ModelPackage)

-- | Undocumented member.
modelPackage_createdBy :: Lens.Lens' ModelPackage (Core.Maybe UserContext)
modelPackage_createdBy = Lens.lens (\ModelPackage' {createdBy} -> createdBy) (\s@ModelPackage' {} a -> s {createdBy = a} :: ModelPackage)

-- | The Amazon Resource Name (ARN) of the model package.
modelPackage_modelPackageArn :: Lens.Lens' ModelPackage (Core.Maybe Core.Text)
modelPackage_modelPackageArn = Lens.lens (\ModelPackage' {modelPackageArn} -> modelPackageArn) (\s@ModelPackage' {} a -> s {modelPackageArn = a} :: ModelPackage)

-- | Undocumented member.
modelPackage_lastModifiedBy :: Lens.Lens' ModelPackage (Core.Maybe UserContext)
modelPackage_lastModifiedBy = Lens.lens (\ModelPackage' {lastModifiedBy} -> lastModifiedBy) (\s@ModelPackage' {} a -> s {lastModifiedBy = a} :: ModelPackage)

-- | The model group to which the model belongs.
modelPackage_modelPackageGroupName :: Lens.Lens' ModelPackage (Core.Maybe Core.Text)
modelPackage_modelPackageGroupName = Lens.lens (\ModelPackage' {modelPackageGroupName} -> modelPackageGroupName) (\s@ModelPackage' {} a -> s {modelPackageGroupName = a} :: ModelPackage)

instance Core.FromJSON ModelPackage where
  parseJSON =
    Core.withObject
      "ModelPackage"
      ( \x ->
          ModelPackage'
            Core.<$> (x Core..:? "SourceAlgorithmSpecification")
            Core.<*> (x Core..:? "ModelPackageVersion")
            Core.<*> (x Core..:? "MetadataProperties")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ValidationSpecification")
            Core.<*> (x Core..:? "ModelPackageStatusDetails")
            Core.<*> (x Core..:? "ModelMetrics")
            Core.<*> (x Core..:? "CertifyForMarketplace")
            Core.<*> (x Core..:? "ModelPackageName")
            Core.<*> (x Core..:? "ModelApprovalStatus")
            Core.<*> (x Core..:? "ApprovalDescription")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ModelPackageStatus")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "InferenceSpecification")
            Core.<*> (x Core..:? "ModelPackageDescription")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "ModelPackageArn")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "ModelPackageGroupName")
      )

instance Core.Hashable ModelPackage

instance Core.NFData ModelPackage
