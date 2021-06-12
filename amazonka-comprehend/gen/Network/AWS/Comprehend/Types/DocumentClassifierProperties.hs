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
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierProperties where

import Network.AWS.Comprehend.Types.ClassifierMetadata
import Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierMode
import Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.ModelStatus
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about a document classifier.
--
-- /See:/ 'newDocumentClassifierProperties' smart constructor.
data DocumentClassifierProperties = DocumentClassifierProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your custom classifier. For
    -- more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The language code for the language of the documents that the classifier
    -- was trained on.
    languageCode :: Core.Maybe LanguageCode,
    -- | The status of the document classifier. If the status is @TRAINED@ the
    -- classifier is ready to use. If the status is @FAILED@ you can see
    -- additional information about why the classifier wasn\'t trained in the
    -- @Message@ field.
    status :: Core.Maybe ModelStatus,
    -- | The input data configuration that you supplied when you created the
    -- document classifier for training.
    inputDataConfig :: Core.Maybe DocumentClassifierInputDataConfig,
    -- | Indicates the mode in which the specific classifier was trained. This
    -- also indicates the format of input documents and the format of the
    -- confusion matrix. Each classifier can only be trained in one mode and
    -- this cannot be changed once the classifier is trained.
    mode :: Core.Maybe DocumentClassifierMode,
    -- | Additional information about the status of the classifier.
    message :: Core.Maybe Core.Text,
    -- | Provides output results configuration parameters for custom classifier
    -- jobs.
    outputDataConfig :: Core.Maybe DocumentClassifierOutputDataConfig,
    -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Core.Maybe Core.Text,
    -- | The time that training the document classifier completed.
    endTime :: Core.Maybe Core.POSIX,
    -- | Information about the document classifier, including the number of
    -- documents used for training the classifier, the number of documents used
    -- for test the classifier, and an accuracy rating.
    classifierMetadata :: Core.Maybe (Core.Sensitive ClassifierMetadata),
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that process the analysis job. The VolumeKmsKeyId can be
    -- either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Core.Maybe Core.Text,
    -- | The time that the document classifier was submitted for training.
    submitTime :: Core.Maybe Core.POSIX,
    -- | Indicates the time when the training starts on documentation
    -- classifiers. You are billed for the time interval between this time and
    -- the value of TrainingEndTime.
    trainingStartTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
    -- role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Maybe Core.Text,
    -- | The time that training of the document classifier was completed.
    -- Indicates the time when the training completes on documentation
    -- classifiers. You are billed for the time interval between this time and
    -- the value of TrainingStartTime.
    trainingEndTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentClassifierProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'documentClassifierProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your custom classifier. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'languageCode', 'documentClassifierProperties_languageCode' - The language code for the language of the documents that the classifier
-- was trained on.
--
-- 'status', 'documentClassifierProperties_status' - The status of the document classifier. If the status is @TRAINED@ the
-- classifier is ready to use. If the status is @FAILED@ you can see
-- additional information about why the classifier wasn\'t trained in the
-- @Message@ field.
--
-- 'inputDataConfig', 'documentClassifierProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- document classifier for training.
--
-- 'mode', 'documentClassifierProperties_mode' - Indicates the mode in which the specific classifier was trained. This
-- also indicates the format of input documents and the format of the
-- confusion matrix. Each classifier can only be trained in one mode and
-- this cannot be changed once the classifier is trained.
--
-- 'message', 'documentClassifierProperties_message' - Additional information about the status of the classifier.
--
-- 'outputDataConfig', 'documentClassifierProperties_outputDataConfig' - Provides output results configuration parameters for custom classifier
-- jobs.
--
-- 'documentClassifierArn', 'documentClassifierProperties_documentClassifierArn' - The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- 'endTime', 'documentClassifierProperties_endTime' - The time that training the document classifier completed.
--
-- 'classifierMetadata', 'documentClassifierProperties_classifierMetadata' - Information about the document classifier, including the number of
-- documents used for training the classifier, the number of documents used
-- for test the classifier, and an accuracy rating.
--
-- 'volumeKmsKeyId', 'documentClassifierProperties_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'submitTime', 'documentClassifierProperties_submitTime' - The time that the document classifier was submitted for training.
--
-- 'trainingStartTime', 'documentClassifierProperties_trainingStartTime' - Indicates the time when the training starts on documentation
-- classifiers. You are billed for the time interval between this time and
-- the value of TrainingEndTime.
--
-- 'dataAccessRoleArn', 'documentClassifierProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your input data.
--
-- 'trainingEndTime', 'documentClassifierProperties_trainingEndTime' - The time that training of the document classifier was completed.
-- Indicates the time when the training completes on documentation
-- classifiers. You are billed for the time interval between this time and
-- the value of TrainingStartTime.
newDocumentClassifierProperties ::
  DocumentClassifierProperties
newDocumentClassifierProperties =
  DocumentClassifierProperties'
    { vpcConfig =
        Core.Nothing,
      languageCode = Core.Nothing,
      status = Core.Nothing,
      inputDataConfig = Core.Nothing,
      mode = Core.Nothing,
      message = Core.Nothing,
      outputDataConfig = Core.Nothing,
      documentClassifierArn = Core.Nothing,
      endTime = Core.Nothing,
      classifierMetadata = Core.Nothing,
      volumeKmsKeyId = Core.Nothing,
      submitTime = Core.Nothing,
      trainingStartTime = Core.Nothing,
      dataAccessRoleArn = Core.Nothing,
      trainingEndTime = Core.Nothing
    }

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your custom classifier. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
documentClassifierProperties_vpcConfig :: Lens.Lens' DocumentClassifierProperties (Core.Maybe VpcConfig)
documentClassifierProperties_vpcConfig = Lens.lens (\DocumentClassifierProperties' {vpcConfig} -> vpcConfig) (\s@DocumentClassifierProperties' {} a -> s {vpcConfig = a} :: DocumentClassifierProperties)

-- | The language code for the language of the documents that the classifier
-- was trained on.
documentClassifierProperties_languageCode :: Lens.Lens' DocumentClassifierProperties (Core.Maybe LanguageCode)
documentClassifierProperties_languageCode = Lens.lens (\DocumentClassifierProperties' {languageCode} -> languageCode) (\s@DocumentClassifierProperties' {} a -> s {languageCode = a} :: DocumentClassifierProperties)

-- | The status of the document classifier. If the status is @TRAINED@ the
-- classifier is ready to use. If the status is @FAILED@ you can see
-- additional information about why the classifier wasn\'t trained in the
-- @Message@ field.
documentClassifierProperties_status :: Lens.Lens' DocumentClassifierProperties (Core.Maybe ModelStatus)
documentClassifierProperties_status = Lens.lens (\DocumentClassifierProperties' {status} -> status) (\s@DocumentClassifierProperties' {} a -> s {status = a} :: DocumentClassifierProperties)

-- | The input data configuration that you supplied when you created the
-- document classifier for training.
documentClassifierProperties_inputDataConfig :: Lens.Lens' DocumentClassifierProperties (Core.Maybe DocumentClassifierInputDataConfig)
documentClassifierProperties_inputDataConfig = Lens.lens (\DocumentClassifierProperties' {inputDataConfig} -> inputDataConfig) (\s@DocumentClassifierProperties' {} a -> s {inputDataConfig = a} :: DocumentClassifierProperties)

-- | Indicates the mode in which the specific classifier was trained. This
-- also indicates the format of input documents and the format of the
-- confusion matrix. Each classifier can only be trained in one mode and
-- this cannot be changed once the classifier is trained.
documentClassifierProperties_mode :: Lens.Lens' DocumentClassifierProperties (Core.Maybe DocumentClassifierMode)
documentClassifierProperties_mode = Lens.lens (\DocumentClassifierProperties' {mode} -> mode) (\s@DocumentClassifierProperties' {} a -> s {mode = a} :: DocumentClassifierProperties)

-- | Additional information about the status of the classifier.
documentClassifierProperties_message :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.Text)
documentClassifierProperties_message = Lens.lens (\DocumentClassifierProperties' {message} -> message) (\s@DocumentClassifierProperties' {} a -> s {message = a} :: DocumentClassifierProperties)

-- | Provides output results configuration parameters for custom classifier
-- jobs.
documentClassifierProperties_outputDataConfig :: Lens.Lens' DocumentClassifierProperties (Core.Maybe DocumentClassifierOutputDataConfig)
documentClassifierProperties_outputDataConfig = Lens.lens (\DocumentClassifierProperties' {outputDataConfig} -> outputDataConfig) (\s@DocumentClassifierProperties' {} a -> s {outputDataConfig = a} :: DocumentClassifierProperties)

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
documentClassifierProperties_documentClassifierArn :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.Text)
documentClassifierProperties_documentClassifierArn = Lens.lens (\DocumentClassifierProperties' {documentClassifierArn} -> documentClassifierArn) (\s@DocumentClassifierProperties' {} a -> s {documentClassifierArn = a} :: DocumentClassifierProperties)

-- | The time that training the document classifier completed.
documentClassifierProperties_endTime :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.UTCTime)
documentClassifierProperties_endTime = Lens.lens (\DocumentClassifierProperties' {endTime} -> endTime) (\s@DocumentClassifierProperties' {} a -> s {endTime = a} :: DocumentClassifierProperties) Core.. Lens.mapping Core._Time

-- | Information about the document classifier, including the number of
-- documents used for training the classifier, the number of documents used
-- for test the classifier, and an accuracy rating.
documentClassifierProperties_classifierMetadata :: Lens.Lens' DocumentClassifierProperties (Core.Maybe ClassifierMetadata)
documentClassifierProperties_classifierMetadata = Lens.lens (\DocumentClassifierProperties' {classifierMetadata} -> classifierMetadata) (\s@DocumentClassifierProperties' {} a -> s {classifierMetadata = a} :: DocumentClassifierProperties) Core.. Lens.mapping Core._Sensitive

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
documentClassifierProperties_volumeKmsKeyId :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.Text)
documentClassifierProperties_volumeKmsKeyId = Lens.lens (\DocumentClassifierProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@DocumentClassifierProperties' {} a -> s {volumeKmsKeyId = a} :: DocumentClassifierProperties)

-- | The time that the document classifier was submitted for training.
documentClassifierProperties_submitTime :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.UTCTime)
documentClassifierProperties_submitTime = Lens.lens (\DocumentClassifierProperties' {submitTime} -> submitTime) (\s@DocumentClassifierProperties' {} a -> s {submitTime = a} :: DocumentClassifierProperties) Core.. Lens.mapping Core._Time

-- | Indicates the time when the training starts on documentation
-- classifiers. You are billed for the time interval between this time and
-- the value of TrainingEndTime.
documentClassifierProperties_trainingStartTime :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.UTCTime)
documentClassifierProperties_trainingStartTime = Lens.lens (\DocumentClassifierProperties' {trainingStartTime} -> trainingStartTime) (\s@DocumentClassifierProperties' {} a -> s {trainingStartTime = a} :: DocumentClassifierProperties) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your input data.
documentClassifierProperties_dataAccessRoleArn :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.Text)
documentClassifierProperties_dataAccessRoleArn = Lens.lens (\DocumentClassifierProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@DocumentClassifierProperties' {} a -> s {dataAccessRoleArn = a} :: DocumentClassifierProperties)

-- | The time that training of the document classifier was completed.
-- Indicates the time when the training completes on documentation
-- classifiers. You are billed for the time interval between this time and
-- the value of TrainingStartTime.
documentClassifierProperties_trainingEndTime :: Lens.Lens' DocumentClassifierProperties (Core.Maybe Core.UTCTime)
documentClassifierProperties_trainingEndTime = Lens.lens (\DocumentClassifierProperties' {trainingEndTime} -> trainingEndTime) (\s@DocumentClassifierProperties' {} a -> s {trainingEndTime = a} :: DocumentClassifierProperties) Core.. Lens.mapping Core._Time

instance Core.FromJSON DocumentClassifierProperties where
  parseJSON =
    Core.withObject
      "DocumentClassifierProperties"
      ( \x ->
          DocumentClassifierProperties'
            Core.<$> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "Mode")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "OutputDataConfig")
            Core.<*> (x Core..:? "DocumentClassifierArn")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "ClassifierMetadata")
            Core.<*> (x Core..:? "VolumeKmsKeyId")
            Core.<*> (x Core..:? "SubmitTime")
            Core.<*> (x Core..:? "TrainingStartTime")
            Core.<*> (x Core..:? "DataAccessRoleArn")
            Core.<*> (x Core..:? "TrainingEndTime")
      )

instance Core.Hashable DocumentClassifierProperties

instance Core.NFData DocumentClassifierProperties
