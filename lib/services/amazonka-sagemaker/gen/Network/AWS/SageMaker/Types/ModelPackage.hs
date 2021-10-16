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
import qualified Network.AWS.Prelude as Prelude
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
  { sourceAlgorithmSpecification :: Prelude.Maybe SourceAlgorithmSpecification,
    -- | The version number of a versioned model.
    modelPackageVersion :: Prelude.Maybe Prelude.Natural,
    -- | The time that the model package was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    validationSpecification :: Prelude.Maybe ModelPackageValidationSpecification,
    modelPackageStatusDetails :: Prelude.Maybe ModelPackageStatusDetails,
    -- | Metrics for the model.
    modelMetrics :: Prelude.Maybe ModelMetrics,
    -- | The name of the model.
    modelPackageName :: Prelude.Maybe Prelude.Text,
    -- | Whether the model package is to be certified to be listed on Amazon Web
    -- Services Marketplace. For information about listing model packages on
    -- Amazon Web Services Marketplace, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-mkt-list.html List Your Algorithm or Model Package on Amazon Web Services Marketplace>.
    certifyForMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | The approval status of the model. This can be one of the following
    -- values.
    --
    -- -   @APPROVED@ - The model is approved
    --
    -- -   @REJECTED@ - The model is rejected.
    --
    -- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
    --     approval.
    modelApprovalStatus :: Prelude.Maybe ModelApprovalStatus,
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
    modelPackageStatus :: Prelude.Maybe ModelPackageStatus,
    -- | A list of the tags associated with the model package. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | A description provided when the model approval is set.
    approvalDescription :: Prelude.Maybe Prelude.Text,
    inferenceSpecification :: Prelude.Maybe InferenceSpecification,
    -- | The last time the model package was modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    createdBy :: Prelude.Maybe UserContext,
    -- | The description of the model package.
    modelPackageDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Prelude.Maybe Prelude.Text,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The model group to which the model belongs.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'creationTime', 'modelPackage_creationTime' - The time that the model package was created.
--
-- 'metadataProperties', 'modelPackage_metadataProperties' - Undocumented member.
--
-- 'validationSpecification', 'modelPackage_validationSpecification' - Undocumented member.
--
-- 'modelPackageStatusDetails', 'modelPackage_modelPackageStatusDetails' - Undocumented member.
--
-- 'modelMetrics', 'modelPackage_modelMetrics' - Metrics for the model.
--
-- 'modelPackageName', 'modelPackage_modelPackageName' - The name of the model.
--
-- 'certifyForMarketplace', 'modelPackage_certifyForMarketplace' - Whether the model package is to be certified to be listed on Amazon Web
-- Services Marketplace. For information about listing model packages on
-- Amazon Web Services Marketplace, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-mkt-list.html List Your Algorithm or Model Package on Amazon Web Services Marketplace>.
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
-- 'tags', 'modelPackage_tags' - A list of the tags associated with the model package. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
--
-- 'approvalDescription', 'modelPackage_approvalDescription' - A description provided when the model approval is set.
--
-- 'inferenceSpecification', 'modelPackage_inferenceSpecification' - Undocumented member.
--
-- 'lastModifiedTime', 'modelPackage_lastModifiedTime' - The last time the model package was modified.
--
-- 'createdBy', 'modelPackage_createdBy' - Undocumented member.
--
-- 'modelPackageDescription', 'modelPackage_modelPackageDescription' - The description of the model package.
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
        Prelude.Nothing,
      modelPackageVersion = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      validationSpecification = Prelude.Nothing,
      modelPackageStatusDetails = Prelude.Nothing,
      modelMetrics = Prelude.Nothing,
      modelPackageName = Prelude.Nothing,
      certifyForMarketplace = Prelude.Nothing,
      modelApprovalStatus = Prelude.Nothing,
      modelPackageStatus = Prelude.Nothing,
      tags = Prelude.Nothing,
      approvalDescription = Prelude.Nothing,
      inferenceSpecification = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      modelPackageDescription = Prelude.Nothing,
      modelPackageArn = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      modelPackageGroupName = Prelude.Nothing
    }

-- | Undocumented member.
modelPackage_sourceAlgorithmSpecification :: Lens.Lens' ModelPackage (Prelude.Maybe SourceAlgorithmSpecification)
modelPackage_sourceAlgorithmSpecification = Lens.lens (\ModelPackage' {sourceAlgorithmSpecification} -> sourceAlgorithmSpecification) (\s@ModelPackage' {} a -> s {sourceAlgorithmSpecification = a} :: ModelPackage)

-- | The version number of a versioned model.
modelPackage_modelPackageVersion :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Natural)
modelPackage_modelPackageVersion = Lens.lens (\ModelPackage' {modelPackageVersion} -> modelPackageVersion) (\s@ModelPackage' {} a -> s {modelPackageVersion = a} :: ModelPackage)

-- | The time that the model package was created.
modelPackage_creationTime :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.UTCTime)
modelPackage_creationTime = Lens.lens (\ModelPackage' {creationTime} -> creationTime) (\s@ModelPackage' {} a -> s {creationTime = a} :: ModelPackage) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
modelPackage_metadataProperties :: Lens.Lens' ModelPackage (Prelude.Maybe MetadataProperties)
modelPackage_metadataProperties = Lens.lens (\ModelPackage' {metadataProperties} -> metadataProperties) (\s@ModelPackage' {} a -> s {metadataProperties = a} :: ModelPackage)

-- | Undocumented member.
modelPackage_validationSpecification :: Lens.Lens' ModelPackage (Prelude.Maybe ModelPackageValidationSpecification)
modelPackage_validationSpecification = Lens.lens (\ModelPackage' {validationSpecification} -> validationSpecification) (\s@ModelPackage' {} a -> s {validationSpecification = a} :: ModelPackage)

-- | Undocumented member.
modelPackage_modelPackageStatusDetails :: Lens.Lens' ModelPackage (Prelude.Maybe ModelPackageStatusDetails)
modelPackage_modelPackageStatusDetails = Lens.lens (\ModelPackage' {modelPackageStatusDetails} -> modelPackageStatusDetails) (\s@ModelPackage' {} a -> s {modelPackageStatusDetails = a} :: ModelPackage)

-- | Metrics for the model.
modelPackage_modelMetrics :: Lens.Lens' ModelPackage (Prelude.Maybe ModelMetrics)
modelPackage_modelMetrics = Lens.lens (\ModelPackage' {modelMetrics} -> modelMetrics) (\s@ModelPackage' {} a -> s {modelMetrics = a} :: ModelPackage)

-- | The name of the model.
modelPackage_modelPackageName :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_modelPackageName = Lens.lens (\ModelPackage' {modelPackageName} -> modelPackageName) (\s@ModelPackage' {} a -> s {modelPackageName = a} :: ModelPackage)

-- | Whether the model package is to be certified to be listed on Amazon Web
-- Services Marketplace. For information about listing model packages on
-- Amazon Web Services Marketplace, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-mkt-list.html List Your Algorithm or Model Package on Amazon Web Services Marketplace>.
modelPackage_certifyForMarketplace :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Bool)
modelPackage_certifyForMarketplace = Lens.lens (\ModelPackage' {certifyForMarketplace} -> certifyForMarketplace) (\s@ModelPackage' {} a -> s {certifyForMarketplace = a} :: ModelPackage)

-- | The approval status of the model. This can be one of the following
-- values.
--
-- -   @APPROVED@ - The model is approved
--
-- -   @REJECTED@ - The model is rejected.
--
-- -   @PENDING_MANUAL_APPROVAL@ - The model is waiting for manual
--     approval.
modelPackage_modelApprovalStatus :: Lens.Lens' ModelPackage (Prelude.Maybe ModelApprovalStatus)
modelPackage_modelApprovalStatus = Lens.lens (\ModelPackage' {modelApprovalStatus} -> modelApprovalStatus) (\s@ModelPackage' {} a -> s {modelApprovalStatus = a} :: ModelPackage)

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
modelPackage_modelPackageStatus :: Lens.Lens' ModelPackage (Prelude.Maybe ModelPackageStatus)
modelPackage_modelPackageStatus = Lens.lens (\ModelPackage' {modelPackageStatus} -> modelPackageStatus) (\s@ModelPackage' {} a -> s {modelPackageStatus = a} :: ModelPackage)

-- | A list of the tags associated with the model package. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
modelPackage_tags :: Lens.Lens' ModelPackage (Prelude.Maybe [Tag])
modelPackage_tags = Lens.lens (\ModelPackage' {tags} -> tags) (\s@ModelPackage' {} a -> s {tags = a} :: ModelPackage) Prelude.. Lens.mapping Lens._Coerce

-- | A description provided when the model approval is set.
modelPackage_approvalDescription :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_approvalDescription = Lens.lens (\ModelPackage' {approvalDescription} -> approvalDescription) (\s@ModelPackage' {} a -> s {approvalDescription = a} :: ModelPackage)

-- | Undocumented member.
modelPackage_inferenceSpecification :: Lens.Lens' ModelPackage (Prelude.Maybe InferenceSpecification)
modelPackage_inferenceSpecification = Lens.lens (\ModelPackage' {inferenceSpecification} -> inferenceSpecification) (\s@ModelPackage' {} a -> s {inferenceSpecification = a} :: ModelPackage)

-- | The last time the model package was modified.
modelPackage_lastModifiedTime :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.UTCTime)
modelPackage_lastModifiedTime = Lens.lens (\ModelPackage' {lastModifiedTime} -> lastModifiedTime) (\s@ModelPackage' {} a -> s {lastModifiedTime = a} :: ModelPackage) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
modelPackage_createdBy :: Lens.Lens' ModelPackage (Prelude.Maybe UserContext)
modelPackage_createdBy = Lens.lens (\ModelPackage' {createdBy} -> createdBy) (\s@ModelPackage' {} a -> s {createdBy = a} :: ModelPackage)

-- | The description of the model package.
modelPackage_modelPackageDescription :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_modelPackageDescription = Lens.lens (\ModelPackage' {modelPackageDescription} -> modelPackageDescription) (\s@ModelPackage' {} a -> s {modelPackageDescription = a} :: ModelPackage)

-- | The Amazon Resource Name (ARN) of the model package.
modelPackage_modelPackageArn :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_modelPackageArn = Lens.lens (\ModelPackage' {modelPackageArn} -> modelPackageArn) (\s@ModelPackage' {} a -> s {modelPackageArn = a} :: ModelPackage)

-- | Undocumented member.
modelPackage_lastModifiedBy :: Lens.Lens' ModelPackage (Prelude.Maybe UserContext)
modelPackage_lastModifiedBy = Lens.lens (\ModelPackage' {lastModifiedBy} -> lastModifiedBy) (\s@ModelPackage' {} a -> s {lastModifiedBy = a} :: ModelPackage)

-- | The model group to which the model belongs.
modelPackage_modelPackageGroupName :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_modelPackageGroupName = Lens.lens (\ModelPackage' {modelPackageGroupName} -> modelPackageGroupName) (\s@ModelPackage' {} a -> s {modelPackageGroupName = a} :: ModelPackage)

instance Core.FromJSON ModelPackage where
  parseJSON =
    Core.withObject
      "ModelPackage"
      ( \x ->
          ModelPackage'
            Prelude.<$> (x Core..:? "SourceAlgorithmSpecification")
            Prelude.<*> (x Core..:? "ModelPackageVersion")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "MetadataProperties")
            Prelude.<*> (x Core..:? "ValidationSpecification")
            Prelude.<*> (x Core..:? "ModelPackageStatusDetails")
            Prelude.<*> (x Core..:? "ModelMetrics")
            Prelude.<*> (x Core..:? "ModelPackageName")
            Prelude.<*> (x Core..:? "CertifyForMarketplace")
            Prelude.<*> (x Core..:? "ModelApprovalStatus")
            Prelude.<*> (x Core..:? "ModelPackageStatus")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ApprovalDescription")
            Prelude.<*> (x Core..:? "InferenceSpecification")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "ModelPackageDescription")
            Prelude.<*> (x Core..:? "ModelPackageArn")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "ModelPackageGroupName")
      )

instance Prelude.Hashable ModelPackage

instance Prelude.NFData ModelPackage
