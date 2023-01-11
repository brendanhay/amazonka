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
-- Module      : Amazonka.SageMaker.Types.ModelPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AdditionalInferenceSpecificationDefinition
import Amazonka.SageMaker.Types.DriftCheckBaselines
import Amazonka.SageMaker.Types.InferenceSpecification
import Amazonka.SageMaker.Types.MetadataProperties
import Amazonka.SageMaker.Types.ModelApprovalStatus
import Amazonka.SageMaker.Types.ModelMetrics
import Amazonka.SageMaker.Types.ModelPackageStatus
import Amazonka.SageMaker.Types.ModelPackageStatusDetails
import Amazonka.SageMaker.Types.ModelPackageValidationSpecification
import Amazonka.SageMaker.Types.SourceAlgorithmSpecification
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.UserContext

-- | A versioned model that can be deployed for SageMaker inference.
--
-- /See:/ 'newModelPackage' smart constructor.
data ModelPackage = ModelPackage'
  { -- | An array of additional Inference Specification objects.
    additionalInferenceSpecifications :: Prelude.Maybe (Prelude.NonEmpty AdditionalInferenceSpecificationDefinition),
    -- | A description provided when the model approval is set.
    approvalDescription :: Prelude.Maybe Prelude.Text,
    -- | Whether the model package is to be certified to be listed on Amazon Web
    -- Services Marketplace. For information about listing model packages on
    -- Amazon Web Services Marketplace, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-mkt-list.html List Your Algorithm or Model Package on Amazon Web Services Marketplace>.
    certifyForMarketplace :: Prelude.Maybe Prelude.Bool,
    -- | Information about the user who created or modified an experiment, trial,
    -- trial component, lineage group, or project.
    createdBy :: Prelude.Maybe UserContext,
    -- | The time that the model package was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The metadata properties for the model package.
    customerMetadataProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The machine learning domain of your model package and its components.
    -- Common machine learning domains include computer vision and natural
    -- language processing.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Represents the drift check baselines that can be used when the model
    -- monitor is set using the model package.
    driftCheckBaselines :: Prelude.Maybe DriftCheckBaselines,
    -- | Defines how to perform inference generation after a training job is run.
    inferenceSpecification :: Prelude.Maybe InferenceSpecification,
    -- | Information about the user who created or modified an experiment, trial,
    -- trial component, lineage group, or project.
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The last time the model package was modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | Metadata properties of the tracking entity, trial, or trial component.
    metadataProperties :: Prelude.Maybe MetadataProperties,
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
    -- | Metrics for the model.
    modelMetrics :: Prelude.Maybe ModelMetrics,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the model package.
    modelPackageDescription :: Prelude.Maybe Prelude.Text,
    -- | The model group to which the model belongs.
    modelPackageGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the model.
    modelPackageName :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies the validation and image scan statuses of the model package.
    modelPackageStatusDetails :: Prelude.Maybe ModelPackageStatusDetails,
    -- | The version number of a versioned model.
    modelPackageVersion :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Simple Storage Service path where the sample payload are
    -- stored. This path must point to a single gzip compressed tar archive
    -- (.tar.gz suffix).
    samplePayloadUrl :: Prelude.Maybe Prelude.Text,
    -- | A list of algorithms that were used to create a model package.
    sourceAlgorithmSpecification :: Prelude.Maybe SourceAlgorithmSpecification,
    -- | A list of the tags associated with the model package. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The machine learning task your model package accomplishes. Common
    -- machine learning tasks include object detection and image
    -- classification.
    task :: Prelude.Maybe Prelude.Text,
    -- | Specifies batch transform jobs that SageMaker runs to validate your
    -- model package.
    validationSpecification :: Prelude.Maybe ModelPackageValidationSpecification
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
-- 'additionalInferenceSpecifications', 'modelPackage_additionalInferenceSpecifications' - An array of additional Inference Specification objects.
--
-- 'approvalDescription', 'modelPackage_approvalDescription' - A description provided when the model approval is set.
--
-- 'certifyForMarketplace', 'modelPackage_certifyForMarketplace' - Whether the model package is to be certified to be listed on Amazon Web
-- Services Marketplace. For information about listing model packages on
-- Amazon Web Services Marketplace, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-mkt-list.html List Your Algorithm or Model Package on Amazon Web Services Marketplace>.
--
-- 'createdBy', 'modelPackage_createdBy' - Information about the user who created or modified an experiment, trial,
-- trial component, lineage group, or project.
--
-- 'creationTime', 'modelPackage_creationTime' - The time that the model package was created.
--
-- 'customerMetadataProperties', 'modelPackage_customerMetadataProperties' - The metadata properties for the model package.
--
-- 'domain', 'modelPackage_domain' - The machine learning domain of your model package and its components.
-- Common machine learning domains include computer vision and natural
-- language processing.
--
-- 'driftCheckBaselines', 'modelPackage_driftCheckBaselines' - Represents the drift check baselines that can be used when the model
-- monitor is set using the model package.
--
-- 'inferenceSpecification', 'modelPackage_inferenceSpecification' - Defines how to perform inference generation after a training job is run.
--
-- 'lastModifiedBy', 'modelPackage_lastModifiedBy' - Information about the user who created or modified an experiment, trial,
-- trial component, lineage group, or project.
--
-- 'lastModifiedTime', 'modelPackage_lastModifiedTime' - The last time the model package was modified.
--
-- 'metadataProperties', 'modelPackage_metadataProperties' - Metadata properties of the tracking entity, trial, or trial component.
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
-- 'modelMetrics', 'modelPackage_modelMetrics' - Metrics for the model.
--
-- 'modelPackageArn', 'modelPackage_modelPackageArn' - The Amazon Resource Name (ARN) of the model package.
--
-- 'modelPackageDescription', 'modelPackage_modelPackageDescription' - The description of the model package.
--
-- 'modelPackageGroupName', 'modelPackage_modelPackageGroupName' - The model group to which the model belongs.
--
-- 'modelPackageName', 'modelPackage_modelPackageName' - The name of the model.
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
-- 'modelPackageStatusDetails', 'modelPackage_modelPackageStatusDetails' - Specifies the validation and image scan statuses of the model package.
--
-- 'modelPackageVersion', 'modelPackage_modelPackageVersion' - The version number of a versioned model.
--
-- 'samplePayloadUrl', 'modelPackage_samplePayloadUrl' - The Amazon Simple Storage Service path where the sample payload are
-- stored. This path must point to a single gzip compressed tar archive
-- (.tar.gz suffix).
--
-- 'sourceAlgorithmSpecification', 'modelPackage_sourceAlgorithmSpecification' - A list of algorithms that were used to create a model package.
--
-- 'tags', 'modelPackage_tags' - A list of the tags associated with the model package. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
--
-- 'task', 'modelPackage_task' - The machine learning task your model package accomplishes. Common
-- machine learning tasks include object detection and image
-- classification.
--
-- 'validationSpecification', 'modelPackage_validationSpecification' - Specifies batch transform jobs that SageMaker runs to validate your
-- model package.
newModelPackage ::
  ModelPackage
newModelPackage =
  ModelPackage'
    { additionalInferenceSpecifications =
        Prelude.Nothing,
      approvalDescription = Prelude.Nothing,
      certifyForMarketplace = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      customerMetadataProperties = Prelude.Nothing,
      domain = Prelude.Nothing,
      driftCheckBaselines = Prelude.Nothing,
      inferenceSpecification = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      modelApprovalStatus = Prelude.Nothing,
      modelMetrics = Prelude.Nothing,
      modelPackageArn = Prelude.Nothing,
      modelPackageDescription = Prelude.Nothing,
      modelPackageGroupName = Prelude.Nothing,
      modelPackageName = Prelude.Nothing,
      modelPackageStatus = Prelude.Nothing,
      modelPackageStatusDetails = Prelude.Nothing,
      modelPackageVersion = Prelude.Nothing,
      samplePayloadUrl = Prelude.Nothing,
      sourceAlgorithmSpecification = Prelude.Nothing,
      tags = Prelude.Nothing,
      task = Prelude.Nothing,
      validationSpecification = Prelude.Nothing
    }

-- | An array of additional Inference Specification objects.
modelPackage_additionalInferenceSpecifications :: Lens.Lens' ModelPackage (Prelude.Maybe (Prelude.NonEmpty AdditionalInferenceSpecificationDefinition))
modelPackage_additionalInferenceSpecifications = Lens.lens (\ModelPackage' {additionalInferenceSpecifications} -> additionalInferenceSpecifications) (\s@ModelPackage' {} a -> s {additionalInferenceSpecifications = a} :: ModelPackage) Prelude.. Lens.mapping Lens.coerced

-- | A description provided when the model approval is set.
modelPackage_approvalDescription :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_approvalDescription = Lens.lens (\ModelPackage' {approvalDescription} -> approvalDescription) (\s@ModelPackage' {} a -> s {approvalDescription = a} :: ModelPackage)

-- | Whether the model package is to be certified to be listed on Amazon Web
-- Services Marketplace. For information about listing model packages on
-- Amazon Web Services Marketplace, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-mkt-list.html List Your Algorithm or Model Package on Amazon Web Services Marketplace>.
modelPackage_certifyForMarketplace :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Bool)
modelPackage_certifyForMarketplace = Lens.lens (\ModelPackage' {certifyForMarketplace} -> certifyForMarketplace) (\s@ModelPackage' {} a -> s {certifyForMarketplace = a} :: ModelPackage)

-- | Information about the user who created or modified an experiment, trial,
-- trial component, lineage group, or project.
modelPackage_createdBy :: Lens.Lens' ModelPackage (Prelude.Maybe UserContext)
modelPackage_createdBy = Lens.lens (\ModelPackage' {createdBy} -> createdBy) (\s@ModelPackage' {} a -> s {createdBy = a} :: ModelPackage)

-- | The time that the model package was created.
modelPackage_creationTime :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.UTCTime)
modelPackage_creationTime = Lens.lens (\ModelPackage' {creationTime} -> creationTime) (\s@ModelPackage' {} a -> s {creationTime = a} :: ModelPackage) Prelude.. Lens.mapping Data._Time

-- | The metadata properties for the model package.
modelPackage_customerMetadataProperties :: Lens.Lens' ModelPackage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
modelPackage_customerMetadataProperties = Lens.lens (\ModelPackage' {customerMetadataProperties} -> customerMetadataProperties) (\s@ModelPackage' {} a -> s {customerMetadataProperties = a} :: ModelPackage) Prelude.. Lens.mapping Lens.coerced

-- | The machine learning domain of your model package and its components.
-- Common machine learning domains include computer vision and natural
-- language processing.
modelPackage_domain :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_domain = Lens.lens (\ModelPackage' {domain} -> domain) (\s@ModelPackage' {} a -> s {domain = a} :: ModelPackage)

-- | Represents the drift check baselines that can be used when the model
-- monitor is set using the model package.
modelPackage_driftCheckBaselines :: Lens.Lens' ModelPackage (Prelude.Maybe DriftCheckBaselines)
modelPackage_driftCheckBaselines = Lens.lens (\ModelPackage' {driftCheckBaselines} -> driftCheckBaselines) (\s@ModelPackage' {} a -> s {driftCheckBaselines = a} :: ModelPackage)

-- | Defines how to perform inference generation after a training job is run.
modelPackage_inferenceSpecification :: Lens.Lens' ModelPackage (Prelude.Maybe InferenceSpecification)
modelPackage_inferenceSpecification = Lens.lens (\ModelPackage' {inferenceSpecification} -> inferenceSpecification) (\s@ModelPackage' {} a -> s {inferenceSpecification = a} :: ModelPackage)

-- | Information about the user who created or modified an experiment, trial,
-- trial component, lineage group, or project.
modelPackage_lastModifiedBy :: Lens.Lens' ModelPackage (Prelude.Maybe UserContext)
modelPackage_lastModifiedBy = Lens.lens (\ModelPackage' {lastModifiedBy} -> lastModifiedBy) (\s@ModelPackage' {} a -> s {lastModifiedBy = a} :: ModelPackage)

-- | The last time the model package was modified.
modelPackage_lastModifiedTime :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.UTCTime)
modelPackage_lastModifiedTime = Lens.lens (\ModelPackage' {lastModifiedTime} -> lastModifiedTime) (\s@ModelPackage' {} a -> s {lastModifiedTime = a} :: ModelPackage) Prelude.. Lens.mapping Data._Time

-- | Metadata properties of the tracking entity, trial, or trial component.
modelPackage_metadataProperties :: Lens.Lens' ModelPackage (Prelude.Maybe MetadataProperties)
modelPackage_metadataProperties = Lens.lens (\ModelPackage' {metadataProperties} -> metadataProperties) (\s@ModelPackage' {} a -> s {metadataProperties = a} :: ModelPackage)

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

-- | Metrics for the model.
modelPackage_modelMetrics :: Lens.Lens' ModelPackage (Prelude.Maybe ModelMetrics)
modelPackage_modelMetrics = Lens.lens (\ModelPackage' {modelMetrics} -> modelMetrics) (\s@ModelPackage' {} a -> s {modelMetrics = a} :: ModelPackage)

-- | The Amazon Resource Name (ARN) of the model package.
modelPackage_modelPackageArn :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_modelPackageArn = Lens.lens (\ModelPackage' {modelPackageArn} -> modelPackageArn) (\s@ModelPackage' {} a -> s {modelPackageArn = a} :: ModelPackage)

-- | The description of the model package.
modelPackage_modelPackageDescription :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_modelPackageDescription = Lens.lens (\ModelPackage' {modelPackageDescription} -> modelPackageDescription) (\s@ModelPackage' {} a -> s {modelPackageDescription = a} :: ModelPackage)

-- | The model group to which the model belongs.
modelPackage_modelPackageGroupName :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_modelPackageGroupName = Lens.lens (\ModelPackage' {modelPackageGroupName} -> modelPackageGroupName) (\s@ModelPackage' {} a -> s {modelPackageGroupName = a} :: ModelPackage)

-- | The name of the model.
modelPackage_modelPackageName :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_modelPackageName = Lens.lens (\ModelPackage' {modelPackageName} -> modelPackageName) (\s@ModelPackage' {} a -> s {modelPackageName = a} :: ModelPackage)

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

-- | Specifies the validation and image scan statuses of the model package.
modelPackage_modelPackageStatusDetails :: Lens.Lens' ModelPackage (Prelude.Maybe ModelPackageStatusDetails)
modelPackage_modelPackageStatusDetails = Lens.lens (\ModelPackage' {modelPackageStatusDetails} -> modelPackageStatusDetails) (\s@ModelPackage' {} a -> s {modelPackageStatusDetails = a} :: ModelPackage)

-- | The version number of a versioned model.
modelPackage_modelPackageVersion :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Natural)
modelPackage_modelPackageVersion = Lens.lens (\ModelPackage' {modelPackageVersion} -> modelPackageVersion) (\s@ModelPackage' {} a -> s {modelPackageVersion = a} :: ModelPackage)

-- | The Amazon Simple Storage Service path where the sample payload are
-- stored. This path must point to a single gzip compressed tar archive
-- (.tar.gz suffix).
modelPackage_samplePayloadUrl :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_samplePayloadUrl = Lens.lens (\ModelPackage' {samplePayloadUrl} -> samplePayloadUrl) (\s@ModelPackage' {} a -> s {samplePayloadUrl = a} :: ModelPackage)

-- | A list of algorithms that were used to create a model package.
modelPackage_sourceAlgorithmSpecification :: Lens.Lens' ModelPackage (Prelude.Maybe SourceAlgorithmSpecification)
modelPackage_sourceAlgorithmSpecification = Lens.lens (\ModelPackage' {sourceAlgorithmSpecification} -> sourceAlgorithmSpecification) (\s@ModelPackage' {} a -> s {sourceAlgorithmSpecification = a} :: ModelPackage)

-- | A list of the tags associated with the model package. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
modelPackage_tags :: Lens.Lens' ModelPackage (Prelude.Maybe [Tag])
modelPackage_tags = Lens.lens (\ModelPackage' {tags} -> tags) (\s@ModelPackage' {} a -> s {tags = a} :: ModelPackage) Prelude.. Lens.mapping Lens.coerced

-- | The machine learning task your model package accomplishes. Common
-- machine learning tasks include object detection and image
-- classification.
modelPackage_task :: Lens.Lens' ModelPackage (Prelude.Maybe Prelude.Text)
modelPackage_task = Lens.lens (\ModelPackage' {task} -> task) (\s@ModelPackage' {} a -> s {task = a} :: ModelPackage)

-- | Specifies batch transform jobs that SageMaker runs to validate your
-- model package.
modelPackage_validationSpecification :: Lens.Lens' ModelPackage (Prelude.Maybe ModelPackageValidationSpecification)
modelPackage_validationSpecification = Lens.lens (\ModelPackage' {validationSpecification} -> validationSpecification) (\s@ModelPackage' {} a -> s {validationSpecification = a} :: ModelPackage)

instance Data.FromJSON ModelPackage where
  parseJSON =
    Data.withObject
      "ModelPackage"
      ( \x ->
          ModelPackage'
            Prelude.<$> (x Data..:? "AdditionalInferenceSpecifications")
            Prelude.<*> (x Data..:? "ApprovalDescription")
            Prelude.<*> (x Data..:? "CertifyForMarketplace")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> ( x Data..:? "CustomerMetadataProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Domain")
            Prelude.<*> (x Data..:? "DriftCheckBaselines")
            Prelude.<*> (x Data..:? "InferenceSpecification")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "MetadataProperties")
            Prelude.<*> (x Data..:? "ModelApprovalStatus")
            Prelude.<*> (x Data..:? "ModelMetrics")
            Prelude.<*> (x Data..:? "ModelPackageArn")
            Prelude.<*> (x Data..:? "ModelPackageDescription")
            Prelude.<*> (x Data..:? "ModelPackageGroupName")
            Prelude.<*> (x Data..:? "ModelPackageName")
            Prelude.<*> (x Data..:? "ModelPackageStatus")
            Prelude.<*> (x Data..:? "ModelPackageStatusDetails")
            Prelude.<*> (x Data..:? "ModelPackageVersion")
            Prelude.<*> (x Data..:? "SamplePayloadUrl")
            Prelude.<*> (x Data..:? "SourceAlgorithmSpecification")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Task")
            Prelude.<*> (x Data..:? "ValidationSpecification")
      )

instance Prelude.Hashable ModelPackage where
  hashWithSalt _salt ModelPackage' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInferenceSpecifications
      `Prelude.hashWithSalt` approvalDescription
      `Prelude.hashWithSalt` certifyForMarketplace
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` customerMetadataProperties
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` driftCheckBaselines
      `Prelude.hashWithSalt` inferenceSpecification
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` metadataProperties
      `Prelude.hashWithSalt` modelApprovalStatus
      `Prelude.hashWithSalt` modelMetrics
      `Prelude.hashWithSalt` modelPackageArn
      `Prelude.hashWithSalt` modelPackageDescription
      `Prelude.hashWithSalt` modelPackageGroupName
      `Prelude.hashWithSalt` modelPackageName
      `Prelude.hashWithSalt` modelPackageStatus
      `Prelude.hashWithSalt` modelPackageStatusDetails
      `Prelude.hashWithSalt` modelPackageVersion
      `Prelude.hashWithSalt` samplePayloadUrl
      `Prelude.hashWithSalt` sourceAlgorithmSpecification
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` task
      `Prelude.hashWithSalt` validationSpecification

instance Prelude.NFData ModelPackage where
  rnf ModelPackage' {..} =
    Prelude.rnf additionalInferenceSpecifications
      `Prelude.seq` Prelude.rnf approvalDescription
      `Prelude.seq` Prelude.rnf certifyForMarketplace
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf customerMetadataProperties
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf driftCheckBaselines
      `Prelude.seq` Prelude.rnf inferenceSpecification
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf modelApprovalStatus
      `Prelude.seq` Prelude.rnf modelMetrics
      `Prelude.seq` Prelude.rnf modelPackageArn
      `Prelude.seq` Prelude.rnf modelPackageDescription
      `Prelude.seq` Prelude.rnf modelPackageGroupName
      `Prelude.seq` Prelude.rnf modelPackageName
      `Prelude.seq` Prelude.rnf modelPackageStatus
      `Prelude.seq` Prelude.rnf
        modelPackageStatusDetails
      `Prelude.seq` Prelude.rnf
        modelPackageVersion
      `Prelude.seq` Prelude.rnf
        samplePayloadUrl
      `Prelude.seq` Prelude.rnf
        sourceAlgorithmSpecification
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf task
      `Prelude.seq` Prelude.rnf
        validationSpecification
