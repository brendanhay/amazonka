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
-- Module      : Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about an entities detection job.
--
-- /See:/ 'newEntitiesDetectionJobProperties' smart constructor.
data EntitiesDetectionJobProperties = EntitiesDetectionJobProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your entity detection job.
    -- For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The input data configuration that you supplied when you created the
    -- entities detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the entities detection job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the
    -- entities detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the entities detection job completed
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
    -- | The time that the entities detection job was submitted for processing.
    submitTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned the entities detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the entities detection job.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EntitiesDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'entitiesDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your entity detection job.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'languageCode', 'entitiesDetectionJobProperties_languageCode' - The language code of the input documents.
--
-- 'inputDataConfig', 'entitiesDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- entities detection job.
--
-- 'message', 'entitiesDetectionJobProperties_message' - A description of the status of a job.
--
-- 'jobStatus', 'entitiesDetectionJobProperties_jobStatus' - The current status of the entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'outputDataConfig', 'entitiesDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- entities detection job.
--
-- 'endTime', 'entitiesDetectionJobProperties_endTime' - The time that the entities detection job completed
--
-- 'volumeKmsKeyId', 'entitiesDetectionJobProperties_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'submitTime', 'entitiesDetectionJobProperties_submitTime' - The time that the entities detection job was submitted for processing.
--
-- 'entityRecognizerArn', 'entitiesDetectionJobProperties_entityRecognizerArn' - The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- 'jobName', 'entitiesDetectionJobProperties_jobName' - The name that you assigned the entities detection job.
--
-- 'dataAccessRoleArn', 'entitiesDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'jobId', 'entitiesDetectionJobProperties_jobId' - The identifier assigned to the entities detection job.
newEntitiesDetectionJobProperties ::
  EntitiesDetectionJobProperties
newEntitiesDetectionJobProperties =
  EntitiesDetectionJobProperties'
    { vpcConfig =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      message = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      endTime = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      entityRecognizerArn = Prelude.Nothing,
      jobName = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your entity detection job.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
entitiesDetectionJobProperties_vpcConfig :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe VpcConfig)
entitiesDetectionJobProperties_vpcConfig = Lens.lens (\EntitiesDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@EntitiesDetectionJobProperties' {} a -> s {vpcConfig = a} :: EntitiesDetectionJobProperties)

-- | The language code of the input documents.
entitiesDetectionJobProperties_languageCode :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe LanguageCode)
entitiesDetectionJobProperties_languageCode = Lens.lens (\EntitiesDetectionJobProperties' {languageCode} -> languageCode) (\s@EntitiesDetectionJobProperties' {} a -> s {languageCode = a} :: EntitiesDetectionJobProperties)

-- | The input data configuration that you supplied when you created the
-- entities detection job.
entitiesDetectionJobProperties_inputDataConfig :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe InputDataConfig)
entitiesDetectionJobProperties_inputDataConfig = Lens.lens (\EntitiesDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@EntitiesDetectionJobProperties' {} a -> s {inputDataConfig = a} :: EntitiesDetectionJobProperties)

-- | A description of the status of a job.
entitiesDetectionJobProperties_message :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_message = Lens.lens (\EntitiesDetectionJobProperties' {message} -> message) (\s@EntitiesDetectionJobProperties' {} a -> s {message = a} :: EntitiesDetectionJobProperties)

-- | The current status of the entities detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
entitiesDetectionJobProperties_jobStatus :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe JobStatus)
entitiesDetectionJobProperties_jobStatus = Lens.lens (\EntitiesDetectionJobProperties' {jobStatus} -> jobStatus) (\s@EntitiesDetectionJobProperties' {} a -> s {jobStatus = a} :: EntitiesDetectionJobProperties)

-- | The output data configuration that you supplied when you created the
-- entities detection job.
entitiesDetectionJobProperties_outputDataConfig :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe OutputDataConfig)
entitiesDetectionJobProperties_outputDataConfig = Lens.lens (\EntitiesDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@EntitiesDetectionJobProperties' {} a -> s {outputDataConfig = a} :: EntitiesDetectionJobProperties)

-- | The time that the entities detection job completed
entitiesDetectionJobProperties_endTime :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
entitiesDetectionJobProperties_endTime = Lens.lens (\EntitiesDetectionJobProperties' {endTime} -> endTime) (\s@EntitiesDetectionJobProperties' {} a -> s {endTime = a} :: EntitiesDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
entitiesDetectionJobProperties_volumeKmsKeyId :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_volumeKmsKeyId = Lens.lens (\EntitiesDetectionJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@EntitiesDetectionJobProperties' {} a -> s {volumeKmsKeyId = a} :: EntitiesDetectionJobProperties)

-- | The time that the entities detection job was submitted for processing.
entitiesDetectionJobProperties_submitTime :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
entitiesDetectionJobProperties_submitTime = Lens.lens (\EntitiesDetectionJobProperties' {submitTime} -> submitTime) (\s@EntitiesDetectionJobProperties' {} a -> s {submitTime = a} :: EntitiesDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
entitiesDetectionJobProperties_entityRecognizerArn :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_entityRecognizerArn = Lens.lens (\EntitiesDetectionJobProperties' {entityRecognizerArn} -> entityRecognizerArn) (\s@EntitiesDetectionJobProperties' {} a -> s {entityRecognizerArn = a} :: EntitiesDetectionJobProperties)

-- | The name that you assigned the entities detection job.
entitiesDetectionJobProperties_jobName :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_jobName = Lens.lens (\EntitiesDetectionJobProperties' {jobName} -> jobName) (\s@EntitiesDetectionJobProperties' {} a -> s {jobName = a} :: EntitiesDetectionJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
entitiesDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_dataAccessRoleArn = Lens.lens (\EntitiesDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@EntitiesDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: EntitiesDetectionJobProperties)

-- | The identifier assigned to the entities detection job.
entitiesDetectionJobProperties_jobId :: Lens.Lens' EntitiesDetectionJobProperties (Prelude.Maybe Prelude.Text)
entitiesDetectionJobProperties_jobId = Lens.lens (\EntitiesDetectionJobProperties' {jobId} -> jobId) (\s@EntitiesDetectionJobProperties' {} a -> s {jobId = a} :: EntitiesDetectionJobProperties)

instance
  Prelude.FromJSON
    EntitiesDetectionJobProperties
  where
  parseJSON =
    Prelude.withObject
      "EntitiesDetectionJobProperties"
      ( \x ->
          EntitiesDetectionJobProperties'
            Prelude.<$> (x Prelude..:? "VpcConfig")
            Prelude.<*> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "InputDataConfig")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "JobStatus")
            Prelude.<*> (x Prelude..:? "OutputDataConfig")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "VolumeKmsKeyId")
            Prelude.<*> (x Prelude..:? "SubmitTime")
            Prelude.<*> (x Prelude..:? "EntityRecognizerArn")
            Prelude.<*> (x Prelude..:? "JobName")
            Prelude.<*> (x Prelude..:? "DataAccessRoleArn")
            Prelude.<*> (x Prelude..:? "JobId")
      )

instance
  Prelude.Hashable
    EntitiesDetectionJobProperties

instance
  Prelude.NFData
    EntitiesDetectionJobProperties
