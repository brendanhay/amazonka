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
-- Module      : Amazonka.Comprehend.Types.DocumentClassifierProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentClassifierProperties where

import Amazonka.Comprehend.Types.ClassifierMetadata
import Amazonka.Comprehend.Types.DocumentClassifierInputDataConfig
import Amazonka.Comprehend.Types.DocumentClassifierMode
import Amazonka.Comprehend.Types.DocumentClassifierOutputDataConfig
import Amazonka.Comprehend.Types.LanguageCode
import Amazonka.Comprehend.Types.ModelStatus
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a document classifier.
--
-- /See:/ 'newDocumentClassifierProperties' smart constructor.
data DocumentClassifierProperties = DocumentClassifierProperties'
  { -- | Information about the document classifier, including the number of
    -- documents used for training the classifier, the number of documents used
    -- for test the classifier, and an accuracy rating.
    classifierMetadata :: Prelude.Maybe (Data.Sensitive ClassifierMetadata),
    -- | The Amazon Resource Name (ARN) of the IAM role that grants Amazon
    -- Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Prelude.Maybe Prelude.Text,
    -- | The time that training the document classifier completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Number (ARN) of the flywheel
    flywheelArn :: Prelude.Maybe Prelude.Text,
    -- | The input data configuration that you supplied when you created the
    -- document classifier for training.
    inputDataConfig :: Prelude.Maybe DocumentClassifierInputDataConfig,
    -- | The language code for the language of the documents that the classifier
    -- was trained on.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Additional information about the status of the classifier.
    message :: Prelude.Maybe Prelude.Text,
    -- | Indicates the mode in which the specific classifier was trained. This
    -- also indicates the format of input documents and the format of the
    -- confusion matrix. Each classifier can only be trained in one mode and
    -- this cannot be changed once the classifier is trained.
    mode :: Prelude.Maybe DocumentClassifierMode,
    -- | ID for the KMS key that Amazon Comprehend uses to encrypt trained custom
    -- models. The ModelKmsKeyId can be either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    modelKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Provides output results configuration parameters for custom classifier
    -- jobs.
    outputDataConfig :: Prelude.Maybe DocumentClassifierOutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the source model. This model was
    -- imported from a different Amazon Web Services account to create the
    -- document classifier model in your Amazon Web Services account.
    sourceModelArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the document classifier. If the status is @TRAINED@ the
    -- classifier is ready to use. If the status is @TRAINED_WITH_WARNINGS@ the
    -- classifier training succeeded, but you should review the warnings
    -- returned in the @CreateDocumentClassifier@ response.
    --
    -- If the status is @FAILED@ you can see additional information about why
    -- the classifier wasn\'t trained in the @Message@ field.
    status :: Prelude.Maybe ModelStatus,
    -- | The time that the document classifier was submitted for training.
    submitTime :: Prelude.Maybe Data.POSIX,
    -- | The time that training of the document classifier was completed.
    -- Indicates the time when the training completes on documentation
    -- classifiers. You are billed for the time interval between this time and
    -- the value of TrainingStartTime.
    trainingEndTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates the time when the training starts on documentation
    -- classifiers. You are billed for the time interval between this time and
    -- the value of TrainingEndTime.
    trainingStartTime :: Prelude.Maybe Data.POSIX,
    -- | The version name that you assigned to the document classifier.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | ID for the Amazon Web Services Key Management Service (KMS) key that
    -- Amazon Comprehend uses to encrypt data on the storage volume attached to
    -- the ML compute instance(s) that process the analysis job. The
    -- VolumeKmsKeyId can be either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your custom classifier. For
    -- more information, see
    -- <https://docs.aws.amazon.com/vppc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentClassifierProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classifierMetadata', 'documentClassifierProperties_classifierMetadata' - Information about the document classifier, including the number of
-- documents used for training the classifier, the number of documents used
-- for test the classifier, and an accuracy rating.
--
-- 'dataAccessRoleArn', 'documentClassifierProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the IAM role that grants Amazon
-- Comprehend read access to your input data.
--
-- 'documentClassifierArn', 'documentClassifierProperties_documentClassifierArn' - The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- 'endTime', 'documentClassifierProperties_endTime' - The time that training the document classifier completed.
--
-- 'flywheelArn', 'documentClassifierProperties_flywheelArn' - The Amazon Resource Number (ARN) of the flywheel
--
-- 'inputDataConfig', 'documentClassifierProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- document classifier for training.
--
-- 'languageCode', 'documentClassifierProperties_languageCode' - The language code for the language of the documents that the classifier
-- was trained on.
--
-- 'message', 'documentClassifierProperties_message' - Additional information about the status of the classifier.
--
-- 'mode', 'documentClassifierProperties_mode' - Indicates the mode in which the specific classifier was trained. This
-- also indicates the format of input documents and the format of the
-- confusion matrix. Each classifier can only be trained in one mode and
-- this cannot be changed once the classifier is trained.
--
-- 'modelKmsKeyId', 'documentClassifierProperties_modelKmsKeyId' - ID for the KMS key that Amazon Comprehend uses to encrypt trained custom
-- models. The ModelKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'outputDataConfig', 'documentClassifierProperties_outputDataConfig' - Provides output results configuration parameters for custom classifier
-- jobs.
--
-- 'sourceModelArn', 'documentClassifierProperties_sourceModelArn' - The Amazon Resource Name (ARN) of the source model. This model was
-- imported from a different Amazon Web Services account to create the
-- document classifier model in your Amazon Web Services account.
--
-- 'status', 'documentClassifierProperties_status' - The status of the document classifier. If the status is @TRAINED@ the
-- classifier is ready to use. If the status is @TRAINED_WITH_WARNINGS@ the
-- classifier training succeeded, but you should review the warnings
-- returned in the @CreateDocumentClassifier@ response.
--
-- If the status is @FAILED@ you can see additional information about why
-- the classifier wasn\'t trained in the @Message@ field.
--
-- 'submitTime', 'documentClassifierProperties_submitTime' - The time that the document classifier was submitted for training.
--
-- 'trainingEndTime', 'documentClassifierProperties_trainingEndTime' - The time that training of the document classifier was completed.
-- Indicates the time when the training completes on documentation
-- classifiers. You are billed for the time interval between this time and
-- the value of TrainingStartTime.
--
-- 'trainingStartTime', 'documentClassifierProperties_trainingStartTime' - Indicates the time when the training starts on documentation
-- classifiers. You are billed for the time interval between this time and
-- the value of TrainingEndTime.
--
-- 'versionName', 'documentClassifierProperties_versionName' - The version name that you assigned to the document classifier.
--
-- 'volumeKmsKeyId', 'documentClassifierProperties_volumeKmsKeyId' - ID for the Amazon Web Services Key Management Service (KMS) key that
-- Amazon Comprehend uses to encrypt data on the storage volume attached to
-- the ML compute instance(s) that process the analysis job. The
-- VolumeKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'vpcConfig', 'documentClassifierProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your custom classifier. For
-- more information, see
-- <https://docs.aws.amazon.com/vppc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
newDocumentClassifierProperties ::
  DocumentClassifierProperties
newDocumentClassifierProperties =
  DocumentClassifierProperties'
    { classifierMetadata =
        Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      documentClassifierArn = Prelude.Nothing,
      endTime = Prelude.Nothing,
      flywheelArn = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      message = Prelude.Nothing,
      mode = Prelude.Nothing,
      modelKmsKeyId = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      sourceModelArn = Prelude.Nothing,
      status = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      trainingEndTime = Prelude.Nothing,
      trainingStartTime = Prelude.Nothing,
      versionName = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | Information about the document classifier, including the number of
-- documents used for training the classifier, the number of documents used
-- for test the classifier, and an accuracy rating.
documentClassifierProperties_classifierMetadata :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe ClassifierMetadata)
documentClassifierProperties_classifierMetadata = Lens.lens (\DocumentClassifierProperties' {classifierMetadata} -> classifierMetadata) (\s@DocumentClassifierProperties' {} a -> s {classifierMetadata = a} :: DocumentClassifierProperties) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role that grants Amazon
-- Comprehend read access to your input data.
documentClassifierProperties_dataAccessRoleArn :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.Text)
documentClassifierProperties_dataAccessRoleArn = Lens.lens (\DocumentClassifierProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@DocumentClassifierProperties' {} a -> s {dataAccessRoleArn = a} :: DocumentClassifierProperties)

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
documentClassifierProperties_documentClassifierArn :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.Text)
documentClassifierProperties_documentClassifierArn = Lens.lens (\DocumentClassifierProperties' {documentClassifierArn} -> documentClassifierArn) (\s@DocumentClassifierProperties' {} a -> s {documentClassifierArn = a} :: DocumentClassifierProperties)

-- | The time that training the document classifier completed.
documentClassifierProperties_endTime :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.UTCTime)
documentClassifierProperties_endTime = Lens.lens (\DocumentClassifierProperties' {endTime} -> endTime) (\s@DocumentClassifierProperties' {} a -> s {endTime = a} :: DocumentClassifierProperties) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Number (ARN) of the flywheel
documentClassifierProperties_flywheelArn :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.Text)
documentClassifierProperties_flywheelArn = Lens.lens (\DocumentClassifierProperties' {flywheelArn} -> flywheelArn) (\s@DocumentClassifierProperties' {} a -> s {flywheelArn = a} :: DocumentClassifierProperties)

-- | The input data configuration that you supplied when you created the
-- document classifier for training.
documentClassifierProperties_inputDataConfig :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe DocumentClassifierInputDataConfig)
documentClassifierProperties_inputDataConfig = Lens.lens (\DocumentClassifierProperties' {inputDataConfig} -> inputDataConfig) (\s@DocumentClassifierProperties' {} a -> s {inputDataConfig = a} :: DocumentClassifierProperties)

-- | The language code for the language of the documents that the classifier
-- was trained on.
documentClassifierProperties_languageCode :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe LanguageCode)
documentClassifierProperties_languageCode = Lens.lens (\DocumentClassifierProperties' {languageCode} -> languageCode) (\s@DocumentClassifierProperties' {} a -> s {languageCode = a} :: DocumentClassifierProperties)

-- | Additional information about the status of the classifier.
documentClassifierProperties_message :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.Text)
documentClassifierProperties_message = Lens.lens (\DocumentClassifierProperties' {message} -> message) (\s@DocumentClassifierProperties' {} a -> s {message = a} :: DocumentClassifierProperties)

-- | Indicates the mode in which the specific classifier was trained. This
-- also indicates the format of input documents and the format of the
-- confusion matrix. Each classifier can only be trained in one mode and
-- this cannot be changed once the classifier is trained.
documentClassifierProperties_mode :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe DocumentClassifierMode)
documentClassifierProperties_mode = Lens.lens (\DocumentClassifierProperties' {mode} -> mode) (\s@DocumentClassifierProperties' {} a -> s {mode = a} :: DocumentClassifierProperties)

-- | ID for the KMS key that Amazon Comprehend uses to encrypt trained custom
-- models. The ModelKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
documentClassifierProperties_modelKmsKeyId :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.Text)
documentClassifierProperties_modelKmsKeyId = Lens.lens (\DocumentClassifierProperties' {modelKmsKeyId} -> modelKmsKeyId) (\s@DocumentClassifierProperties' {} a -> s {modelKmsKeyId = a} :: DocumentClassifierProperties)

-- | Provides output results configuration parameters for custom classifier
-- jobs.
documentClassifierProperties_outputDataConfig :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe DocumentClassifierOutputDataConfig)
documentClassifierProperties_outputDataConfig = Lens.lens (\DocumentClassifierProperties' {outputDataConfig} -> outputDataConfig) (\s@DocumentClassifierProperties' {} a -> s {outputDataConfig = a} :: DocumentClassifierProperties)

-- | The Amazon Resource Name (ARN) of the source model. This model was
-- imported from a different Amazon Web Services account to create the
-- document classifier model in your Amazon Web Services account.
documentClassifierProperties_sourceModelArn :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.Text)
documentClassifierProperties_sourceModelArn = Lens.lens (\DocumentClassifierProperties' {sourceModelArn} -> sourceModelArn) (\s@DocumentClassifierProperties' {} a -> s {sourceModelArn = a} :: DocumentClassifierProperties)

-- | The status of the document classifier. If the status is @TRAINED@ the
-- classifier is ready to use. If the status is @TRAINED_WITH_WARNINGS@ the
-- classifier training succeeded, but you should review the warnings
-- returned in the @CreateDocumentClassifier@ response.
--
-- If the status is @FAILED@ you can see additional information about why
-- the classifier wasn\'t trained in the @Message@ field.
documentClassifierProperties_status :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe ModelStatus)
documentClassifierProperties_status = Lens.lens (\DocumentClassifierProperties' {status} -> status) (\s@DocumentClassifierProperties' {} a -> s {status = a} :: DocumentClassifierProperties)

-- | The time that the document classifier was submitted for training.
documentClassifierProperties_submitTime :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.UTCTime)
documentClassifierProperties_submitTime = Lens.lens (\DocumentClassifierProperties' {submitTime} -> submitTime) (\s@DocumentClassifierProperties' {} a -> s {submitTime = a} :: DocumentClassifierProperties) Prelude.. Lens.mapping Data._Time

-- | The time that training of the document classifier was completed.
-- Indicates the time when the training completes on documentation
-- classifiers. You are billed for the time interval between this time and
-- the value of TrainingStartTime.
documentClassifierProperties_trainingEndTime :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.UTCTime)
documentClassifierProperties_trainingEndTime = Lens.lens (\DocumentClassifierProperties' {trainingEndTime} -> trainingEndTime) (\s@DocumentClassifierProperties' {} a -> s {trainingEndTime = a} :: DocumentClassifierProperties) Prelude.. Lens.mapping Data._Time

-- | Indicates the time when the training starts on documentation
-- classifiers. You are billed for the time interval between this time and
-- the value of TrainingEndTime.
documentClassifierProperties_trainingStartTime :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.UTCTime)
documentClassifierProperties_trainingStartTime = Lens.lens (\DocumentClassifierProperties' {trainingStartTime} -> trainingStartTime) (\s@DocumentClassifierProperties' {} a -> s {trainingStartTime = a} :: DocumentClassifierProperties) Prelude.. Lens.mapping Data._Time

-- | The version name that you assigned to the document classifier.
documentClassifierProperties_versionName :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.Text)
documentClassifierProperties_versionName = Lens.lens (\DocumentClassifierProperties' {versionName} -> versionName) (\s@DocumentClassifierProperties' {} a -> s {versionName = a} :: DocumentClassifierProperties)

-- | ID for the Amazon Web Services Key Management Service (KMS) key that
-- Amazon Comprehend uses to encrypt data on the storage volume attached to
-- the ML compute instance(s) that process the analysis job. The
-- VolumeKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
documentClassifierProperties_volumeKmsKeyId :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe Prelude.Text)
documentClassifierProperties_volumeKmsKeyId = Lens.lens (\DocumentClassifierProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@DocumentClassifierProperties' {} a -> s {volumeKmsKeyId = a} :: DocumentClassifierProperties)

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your custom classifier. For
-- more information, see
-- <https://docs.aws.amazon.com/vppc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
documentClassifierProperties_vpcConfig :: Lens.Lens' DocumentClassifierProperties (Prelude.Maybe VpcConfig)
documentClassifierProperties_vpcConfig = Lens.lens (\DocumentClassifierProperties' {vpcConfig} -> vpcConfig) (\s@DocumentClassifierProperties' {} a -> s {vpcConfig = a} :: DocumentClassifierProperties)

instance Data.FromJSON DocumentClassifierProperties where
  parseJSON =
    Data.withObject
      "DocumentClassifierProperties"
      ( \x ->
          DocumentClassifierProperties'
            Prelude.<$> (x Data..:? "ClassifierMetadata")
            Prelude.<*> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "DocumentClassifierArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "FlywheelArn")
            Prelude.<*> (x Data..:? "InputDataConfig")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Mode")
            Prelude.<*> (x Data..:? "ModelKmsKeyId")
            Prelude.<*> (x Data..:? "OutputDataConfig")
            Prelude.<*> (x Data..:? "SourceModelArn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "SubmitTime")
            Prelude.<*> (x Data..:? "TrainingEndTime")
            Prelude.<*> (x Data..:? "TrainingStartTime")
            Prelude.<*> (x Data..:? "VersionName")
            Prelude.<*> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance
  Prelude.Hashable
    DocumentClassifierProperties
  where
  hashWithSalt _salt DocumentClassifierProperties' {..} =
    _salt
      `Prelude.hashWithSalt` classifierMetadata
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` documentClassifierArn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` flywheelArn
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` modelKmsKeyId
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` sourceModelArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` submitTime
      `Prelude.hashWithSalt` trainingEndTime
      `Prelude.hashWithSalt` trainingStartTime
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData DocumentClassifierProperties where
  rnf DocumentClassifierProperties' {..} =
    Prelude.rnf classifierMetadata
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf documentClassifierArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf flywheelArn
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf modelKmsKeyId
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf sourceModelArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf trainingEndTime
      `Prelude.seq` Prelude.rnf trainingStartTime
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig
