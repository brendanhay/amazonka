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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerProperties where

import Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
import Network.AWS.Comprehend.Types.EntityRecognizerMetadata
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.ModelStatus
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes information about an entity recognizer.
--
-- /See:/ 'newEntityRecognizerProperties' smart constructor.
data EntityRecognizerProperties = EntityRecognizerProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your custom entity
    -- recognizer. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The language of the input documents. All documents must be in the same
    -- language. Only English (\"en\") is currently supported.
    languageCode :: Core.Maybe LanguageCode,
    -- | Provides the status of the entity recognizer.
    status :: Core.Maybe ModelStatus,
    -- | The input data properties of an entity recognizer.
    inputDataConfig :: Core.Maybe EntityRecognizerInputDataConfig,
    -- | A description of the status of the recognizer.
    message :: Core.Maybe Core.Text,
    -- | The time that the recognizer creation completed.
    endTime :: Core.Maybe Core.POSIX,
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
    -- | The time that the recognizer was submitted for processing.
    submitTime :: Core.Maybe Core.POSIX,
    -- | Provides information about an entity recognizer.
    recognizerMetadata :: Core.Maybe (Core.Sensitive EntityRecognizerMetadata),
    -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Core.Maybe Core.Text,
    -- | The time that training of the entity recognizer started.
    trainingStartTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
    -- role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Maybe Core.Text,
    -- | The time that training of the entity recognizer was completed.
    trainingEndTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'EntityRecognizerProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'entityRecognizerProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your custom entity
-- recognizer. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'languageCode', 'entityRecognizerProperties_languageCode' - The language of the input documents. All documents must be in the same
-- language. Only English (\"en\") is currently supported.
--
-- 'status', 'entityRecognizerProperties_status' - Provides the status of the entity recognizer.
--
-- 'inputDataConfig', 'entityRecognizerProperties_inputDataConfig' - The input data properties of an entity recognizer.
--
-- 'message', 'entityRecognizerProperties_message' - A description of the status of the recognizer.
--
-- 'endTime', 'entityRecognizerProperties_endTime' - The time that the recognizer creation completed.
--
-- 'volumeKmsKeyId', 'entityRecognizerProperties_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'submitTime', 'entityRecognizerProperties_submitTime' - The time that the recognizer was submitted for processing.
--
-- 'recognizerMetadata', 'entityRecognizerProperties_recognizerMetadata' - Provides information about an entity recognizer.
--
-- 'entityRecognizerArn', 'entityRecognizerProperties_entityRecognizerArn' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- 'trainingStartTime', 'entityRecognizerProperties_trainingStartTime' - The time that training of the entity recognizer started.
--
-- 'dataAccessRoleArn', 'entityRecognizerProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your input data.
--
-- 'trainingEndTime', 'entityRecognizerProperties_trainingEndTime' - The time that training of the entity recognizer was completed.
newEntityRecognizerProperties ::
  EntityRecognizerProperties
newEntityRecognizerProperties =
  EntityRecognizerProperties'
    { vpcConfig =
        Core.Nothing,
      languageCode = Core.Nothing,
      status = Core.Nothing,
      inputDataConfig = Core.Nothing,
      message = Core.Nothing,
      endTime = Core.Nothing,
      volumeKmsKeyId = Core.Nothing,
      submitTime = Core.Nothing,
      recognizerMetadata = Core.Nothing,
      entityRecognizerArn = Core.Nothing,
      trainingStartTime = Core.Nothing,
      dataAccessRoleArn = Core.Nothing,
      trainingEndTime = Core.Nothing
    }

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your custom entity
-- recognizer. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
entityRecognizerProperties_vpcConfig :: Lens.Lens' EntityRecognizerProperties (Core.Maybe VpcConfig)
entityRecognizerProperties_vpcConfig = Lens.lens (\EntityRecognizerProperties' {vpcConfig} -> vpcConfig) (\s@EntityRecognizerProperties' {} a -> s {vpcConfig = a} :: EntityRecognizerProperties)

-- | The language of the input documents. All documents must be in the same
-- language. Only English (\"en\") is currently supported.
entityRecognizerProperties_languageCode :: Lens.Lens' EntityRecognizerProperties (Core.Maybe LanguageCode)
entityRecognizerProperties_languageCode = Lens.lens (\EntityRecognizerProperties' {languageCode} -> languageCode) (\s@EntityRecognizerProperties' {} a -> s {languageCode = a} :: EntityRecognizerProperties)

-- | Provides the status of the entity recognizer.
entityRecognizerProperties_status :: Lens.Lens' EntityRecognizerProperties (Core.Maybe ModelStatus)
entityRecognizerProperties_status = Lens.lens (\EntityRecognizerProperties' {status} -> status) (\s@EntityRecognizerProperties' {} a -> s {status = a} :: EntityRecognizerProperties)

-- | The input data properties of an entity recognizer.
entityRecognizerProperties_inputDataConfig :: Lens.Lens' EntityRecognizerProperties (Core.Maybe EntityRecognizerInputDataConfig)
entityRecognizerProperties_inputDataConfig = Lens.lens (\EntityRecognizerProperties' {inputDataConfig} -> inputDataConfig) (\s@EntityRecognizerProperties' {} a -> s {inputDataConfig = a} :: EntityRecognizerProperties)

-- | A description of the status of the recognizer.
entityRecognizerProperties_message :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.Text)
entityRecognizerProperties_message = Lens.lens (\EntityRecognizerProperties' {message} -> message) (\s@EntityRecognizerProperties' {} a -> s {message = a} :: EntityRecognizerProperties)

-- | The time that the recognizer creation completed.
entityRecognizerProperties_endTime :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.UTCTime)
entityRecognizerProperties_endTime = Lens.lens (\EntityRecognizerProperties' {endTime} -> endTime) (\s@EntityRecognizerProperties' {} a -> s {endTime = a} :: EntityRecognizerProperties) Core.. Lens.mapping Core._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
entityRecognizerProperties_volumeKmsKeyId :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.Text)
entityRecognizerProperties_volumeKmsKeyId = Lens.lens (\EntityRecognizerProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@EntityRecognizerProperties' {} a -> s {volumeKmsKeyId = a} :: EntityRecognizerProperties)

-- | The time that the recognizer was submitted for processing.
entityRecognizerProperties_submitTime :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.UTCTime)
entityRecognizerProperties_submitTime = Lens.lens (\EntityRecognizerProperties' {submitTime} -> submitTime) (\s@EntityRecognizerProperties' {} a -> s {submitTime = a} :: EntityRecognizerProperties) Core.. Lens.mapping Core._Time

-- | Provides information about an entity recognizer.
entityRecognizerProperties_recognizerMetadata :: Lens.Lens' EntityRecognizerProperties (Core.Maybe EntityRecognizerMetadata)
entityRecognizerProperties_recognizerMetadata = Lens.lens (\EntityRecognizerProperties' {recognizerMetadata} -> recognizerMetadata) (\s@EntityRecognizerProperties' {} a -> s {recognizerMetadata = a} :: EntityRecognizerProperties) Core.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
entityRecognizerProperties_entityRecognizerArn :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.Text)
entityRecognizerProperties_entityRecognizerArn = Lens.lens (\EntityRecognizerProperties' {entityRecognizerArn} -> entityRecognizerArn) (\s@EntityRecognizerProperties' {} a -> s {entityRecognizerArn = a} :: EntityRecognizerProperties)

-- | The time that training of the entity recognizer started.
entityRecognizerProperties_trainingStartTime :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.UTCTime)
entityRecognizerProperties_trainingStartTime = Lens.lens (\EntityRecognizerProperties' {trainingStartTime} -> trainingStartTime) (\s@EntityRecognizerProperties' {} a -> s {trainingStartTime = a} :: EntityRecognizerProperties) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your input data.
entityRecognizerProperties_dataAccessRoleArn :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.Text)
entityRecognizerProperties_dataAccessRoleArn = Lens.lens (\EntityRecognizerProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@EntityRecognizerProperties' {} a -> s {dataAccessRoleArn = a} :: EntityRecognizerProperties)

-- | The time that training of the entity recognizer was completed.
entityRecognizerProperties_trainingEndTime :: Lens.Lens' EntityRecognizerProperties (Core.Maybe Core.UTCTime)
entityRecognizerProperties_trainingEndTime = Lens.lens (\EntityRecognizerProperties' {trainingEndTime} -> trainingEndTime) (\s@EntityRecognizerProperties' {} a -> s {trainingEndTime = a} :: EntityRecognizerProperties) Core.. Lens.mapping Core._Time

instance Core.FromJSON EntityRecognizerProperties where
  parseJSON =
    Core.withObject
      "EntityRecognizerProperties"
      ( \x ->
          EntityRecognizerProperties'
            Core.<$> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "LanguageCode")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "VolumeKmsKeyId")
            Core.<*> (x Core..:? "SubmitTime")
            Core.<*> (x Core..:? "RecognizerMetadata")
            Core.<*> (x Core..:? "EntityRecognizerArn")
            Core.<*> (x Core..:? "TrainingStartTime")
            Core.<*> (x Core..:? "DataAccessRoleArn")
            Core.<*> (x Core..:? "TrainingEndTime")
      )

instance Core.Hashable EntityRecognizerProperties

instance Core.NFData EntityRecognizerProperties
