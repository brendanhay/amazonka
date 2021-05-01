{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes information about an entity recognizer.
--
-- /See:/ 'newEntityRecognizerProperties' smart constructor.
data EntityRecognizerProperties = EntityRecognizerProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your custom entity
    -- recognizer. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The language of the input documents. All documents must be in the same
    -- language. Only English (\"en\") is currently supported.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | Provides the status of the entity recognizer.
    status :: Prelude.Maybe ModelStatus,
    -- | The input data properties of an entity recognizer.
    inputDataConfig :: Prelude.Maybe EntityRecognizerInputDataConfig,
    -- | A description of the status of the recognizer.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time that the recognizer creation completed.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that process the analysis job. The VolumeKmsKeyId can be
    -- either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The time that the recognizer was submitted for processing.
    submitTime :: Prelude.Maybe Prelude.POSIX,
    -- | Provides information about an entity recognizer.
    recognizerMetadata :: Prelude.Maybe (Prelude.Sensitive EntityRecognizerMetadata),
    -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Prelude.Maybe Prelude.Text,
    -- | The time that training of the entity recognizer started.
    trainingStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
    -- role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time that training of the entity recognizer was completed.
    trainingEndTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      status = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      message = Prelude.Nothing,
      endTime = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      recognizerMetadata = Prelude.Nothing,
      entityRecognizerArn = Prelude.Nothing,
      trainingStartTime = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      trainingEndTime = Prelude.Nothing
    }

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your custom entity
-- recognizer. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
entityRecognizerProperties_vpcConfig :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe VpcConfig)
entityRecognizerProperties_vpcConfig = Lens.lens (\EntityRecognizerProperties' {vpcConfig} -> vpcConfig) (\s@EntityRecognizerProperties' {} a -> s {vpcConfig = a} :: EntityRecognizerProperties)

-- | The language of the input documents. All documents must be in the same
-- language. Only English (\"en\") is currently supported.
entityRecognizerProperties_languageCode :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe LanguageCode)
entityRecognizerProperties_languageCode = Lens.lens (\EntityRecognizerProperties' {languageCode} -> languageCode) (\s@EntityRecognizerProperties' {} a -> s {languageCode = a} :: EntityRecognizerProperties)

-- | Provides the status of the entity recognizer.
entityRecognizerProperties_status :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe ModelStatus)
entityRecognizerProperties_status = Lens.lens (\EntityRecognizerProperties' {status} -> status) (\s@EntityRecognizerProperties' {} a -> s {status = a} :: EntityRecognizerProperties)

-- | The input data properties of an entity recognizer.
entityRecognizerProperties_inputDataConfig :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe EntityRecognizerInputDataConfig)
entityRecognizerProperties_inputDataConfig = Lens.lens (\EntityRecognizerProperties' {inputDataConfig} -> inputDataConfig) (\s@EntityRecognizerProperties' {} a -> s {inputDataConfig = a} :: EntityRecognizerProperties)

-- | A description of the status of the recognizer.
entityRecognizerProperties_message :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe Prelude.Text)
entityRecognizerProperties_message = Lens.lens (\EntityRecognizerProperties' {message} -> message) (\s@EntityRecognizerProperties' {} a -> s {message = a} :: EntityRecognizerProperties)

-- | The time that the recognizer creation completed.
entityRecognizerProperties_endTime :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe Prelude.UTCTime)
entityRecognizerProperties_endTime = Lens.lens (\EntityRecognizerProperties' {endTime} -> endTime) (\s@EntityRecognizerProperties' {} a -> s {endTime = a} :: EntityRecognizerProperties) Prelude.. Lens.mapping Prelude._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
entityRecognizerProperties_volumeKmsKeyId :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe Prelude.Text)
entityRecognizerProperties_volumeKmsKeyId = Lens.lens (\EntityRecognizerProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@EntityRecognizerProperties' {} a -> s {volumeKmsKeyId = a} :: EntityRecognizerProperties)

-- | The time that the recognizer was submitted for processing.
entityRecognizerProperties_submitTime :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe Prelude.UTCTime)
entityRecognizerProperties_submitTime = Lens.lens (\EntityRecognizerProperties' {submitTime} -> submitTime) (\s@EntityRecognizerProperties' {} a -> s {submitTime = a} :: EntityRecognizerProperties) Prelude.. Lens.mapping Prelude._Time

-- | Provides information about an entity recognizer.
entityRecognizerProperties_recognizerMetadata :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe EntityRecognizerMetadata)
entityRecognizerProperties_recognizerMetadata = Lens.lens (\EntityRecognizerProperties' {recognizerMetadata} -> recognizerMetadata) (\s@EntityRecognizerProperties' {} a -> s {recognizerMetadata = a} :: EntityRecognizerProperties) Prelude.. Lens.mapping Prelude._Sensitive

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
entityRecognizerProperties_entityRecognizerArn :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe Prelude.Text)
entityRecognizerProperties_entityRecognizerArn = Lens.lens (\EntityRecognizerProperties' {entityRecognizerArn} -> entityRecognizerArn) (\s@EntityRecognizerProperties' {} a -> s {entityRecognizerArn = a} :: EntityRecognizerProperties)

-- | The time that training of the entity recognizer started.
entityRecognizerProperties_trainingStartTime :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe Prelude.UTCTime)
entityRecognizerProperties_trainingStartTime = Lens.lens (\EntityRecognizerProperties' {trainingStartTime} -> trainingStartTime) (\s@EntityRecognizerProperties' {} a -> s {trainingStartTime = a} :: EntityRecognizerProperties) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your input data.
entityRecognizerProperties_dataAccessRoleArn :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe Prelude.Text)
entityRecognizerProperties_dataAccessRoleArn = Lens.lens (\EntityRecognizerProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@EntityRecognizerProperties' {} a -> s {dataAccessRoleArn = a} :: EntityRecognizerProperties)

-- | The time that training of the entity recognizer was completed.
entityRecognizerProperties_trainingEndTime :: Lens.Lens' EntityRecognizerProperties (Prelude.Maybe Prelude.UTCTime)
entityRecognizerProperties_trainingEndTime = Lens.lens (\EntityRecognizerProperties' {trainingEndTime} -> trainingEndTime) (\s@EntityRecognizerProperties' {} a -> s {trainingEndTime = a} :: EntityRecognizerProperties) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON EntityRecognizerProperties where
  parseJSON =
    Prelude.withObject
      "EntityRecognizerProperties"
      ( \x ->
          EntityRecognizerProperties'
            Prelude.<$> (x Prelude..:? "VpcConfig")
            Prelude.<*> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "InputDataConfig")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "VolumeKmsKeyId")
            Prelude.<*> (x Prelude..:? "SubmitTime")
            Prelude.<*> (x Prelude..:? "RecognizerMetadata")
            Prelude.<*> (x Prelude..:? "EntityRecognizerArn")
            Prelude.<*> (x Prelude..:? "TrainingStartTime")
            Prelude.<*> (x Prelude..:? "DataAccessRoleArn")
            Prelude.<*> (x Prelude..:? "TrainingEndTime")
      )

instance Prelude.Hashable EntityRecognizerProperties

instance Prelude.NFData EntityRecognizerProperties
