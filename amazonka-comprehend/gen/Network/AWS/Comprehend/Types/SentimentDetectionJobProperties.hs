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
-- Module      : Network.AWS.Comprehend.Types.SentimentDetectionJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a sentiment detection job.
--
-- /See:/ 'newSentimentDetectionJobProperties' smart constructor.
data SentimentDetectionJobProperties = SentimentDetectionJobProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your sentiment detection job.
    -- For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The input data configuration that you supplied when you created the
    -- sentiment detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the sentiment detection job. If the status is
    -- @FAILED@, the @Messages@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the
    -- sentiment detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the sentiment detection job ended.
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
    -- | The time that the sentiment detection job was submitted for processing.
    submitTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name that you assigned to the sentiment detection job
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the sentiment detection job.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SentimentDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'sentimentDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your sentiment detection job.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'languageCode', 'sentimentDetectionJobProperties_languageCode' - The language code of the input documents.
--
-- 'inputDataConfig', 'sentimentDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- sentiment detection job.
--
-- 'message', 'sentimentDetectionJobProperties_message' - A description of the status of a job.
--
-- 'jobStatus', 'sentimentDetectionJobProperties_jobStatus' - The current status of the sentiment detection job. If the status is
-- @FAILED@, the @Messages@ field shows the reason for the failure.
--
-- 'outputDataConfig', 'sentimentDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- sentiment detection job.
--
-- 'endTime', 'sentimentDetectionJobProperties_endTime' - The time that the sentiment detection job ended.
--
-- 'volumeKmsKeyId', 'sentimentDetectionJobProperties_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'submitTime', 'sentimentDetectionJobProperties_submitTime' - The time that the sentiment detection job was submitted for processing.
--
-- 'jobName', 'sentimentDetectionJobProperties_jobName' - The name that you assigned to the sentiment detection job
--
-- 'dataAccessRoleArn', 'sentimentDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'jobId', 'sentimentDetectionJobProperties_jobId' - The identifier assigned to the sentiment detection job.
newSentimentDetectionJobProperties ::
  SentimentDetectionJobProperties
newSentimentDetectionJobProperties =
  SentimentDetectionJobProperties'
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
      jobName = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your sentiment detection job.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
sentimentDetectionJobProperties_vpcConfig :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe VpcConfig)
sentimentDetectionJobProperties_vpcConfig = Lens.lens (\SentimentDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@SentimentDetectionJobProperties' {} a -> s {vpcConfig = a} :: SentimentDetectionJobProperties)

-- | The language code of the input documents.
sentimentDetectionJobProperties_languageCode :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe LanguageCode)
sentimentDetectionJobProperties_languageCode = Lens.lens (\SentimentDetectionJobProperties' {languageCode} -> languageCode) (\s@SentimentDetectionJobProperties' {} a -> s {languageCode = a} :: SentimentDetectionJobProperties)

-- | The input data configuration that you supplied when you created the
-- sentiment detection job.
sentimentDetectionJobProperties_inputDataConfig :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe InputDataConfig)
sentimentDetectionJobProperties_inputDataConfig = Lens.lens (\SentimentDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@SentimentDetectionJobProperties' {} a -> s {inputDataConfig = a} :: SentimentDetectionJobProperties)

-- | A description of the status of a job.
sentimentDetectionJobProperties_message :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
sentimentDetectionJobProperties_message = Lens.lens (\SentimentDetectionJobProperties' {message} -> message) (\s@SentimentDetectionJobProperties' {} a -> s {message = a} :: SentimentDetectionJobProperties)

-- | The current status of the sentiment detection job. If the status is
-- @FAILED@, the @Messages@ field shows the reason for the failure.
sentimentDetectionJobProperties_jobStatus :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe JobStatus)
sentimentDetectionJobProperties_jobStatus = Lens.lens (\SentimentDetectionJobProperties' {jobStatus} -> jobStatus) (\s@SentimentDetectionJobProperties' {} a -> s {jobStatus = a} :: SentimentDetectionJobProperties)

-- | The output data configuration that you supplied when you created the
-- sentiment detection job.
sentimentDetectionJobProperties_outputDataConfig :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe OutputDataConfig)
sentimentDetectionJobProperties_outputDataConfig = Lens.lens (\SentimentDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@SentimentDetectionJobProperties' {} a -> s {outputDataConfig = a} :: SentimentDetectionJobProperties)

-- | The time that the sentiment detection job ended.
sentimentDetectionJobProperties_endTime :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
sentimentDetectionJobProperties_endTime = Lens.lens (\SentimentDetectionJobProperties' {endTime} -> endTime) (\s@SentimentDetectionJobProperties' {} a -> s {endTime = a} :: SentimentDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
sentimentDetectionJobProperties_volumeKmsKeyId :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
sentimentDetectionJobProperties_volumeKmsKeyId = Lens.lens (\SentimentDetectionJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@SentimentDetectionJobProperties' {} a -> s {volumeKmsKeyId = a} :: SentimentDetectionJobProperties)

-- | The time that the sentiment detection job was submitted for processing.
sentimentDetectionJobProperties_submitTime :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
sentimentDetectionJobProperties_submitTime = Lens.lens (\SentimentDetectionJobProperties' {submitTime} -> submitTime) (\s@SentimentDetectionJobProperties' {} a -> s {submitTime = a} :: SentimentDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | The name that you assigned to the sentiment detection job
sentimentDetectionJobProperties_jobName :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
sentimentDetectionJobProperties_jobName = Lens.lens (\SentimentDetectionJobProperties' {jobName} -> jobName) (\s@SentimentDetectionJobProperties' {} a -> s {jobName = a} :: SentimentDetectionJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
sentimentDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
sentimentDetectionJobProperties_dataAccessRoleArn = Lens.lens (\SentimentDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@SentimentDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: SentimentDetectionJobProperties)

-- | The identifier assigned to the sentiment detection job.
sentimentDetectionJobProperties_jobId :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
sentimentDetectionJobProperties_jobId = Lens.lens (\SentimentDetectionJobProperties' {jobId} -> jobId) (\s@SentimentDetectionJobProperties' {} a -> s {jobId = a} :: SentimentDetectionJobProperties)

instance
  Prelude.FromJSON
    SentimentDetectionJobProperties
  where
  parseJSON =
    Prelude.withObject
      "SentimentDetectionJobProperties"
      ( \x ->
          SentimentDetectionJobProperties'
            Prelude.<$> (x Prelude..:? "VpcConfig")
            Prelude.<*> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "InputDataConfig")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "JobStatus")
            Prelude.<*> (x Prelude..:? "OutputDataConfig")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "VolumeKmsKeyId")
            Prelude.<*> (x Prelude..:? "SubmitTime")
            Prelude.<*> (x Prelude..:? "JobName")
            Prelude.<*> (x Prelude..:? "DataAccessRoleArn")
            Prelude.<*> (x Prelude..:? "JobId")
      )

instance
  Prelude.Hashable
    SentimentDetectionJobProperties

instance
  Prelude.NFData
    SentimentDetectionJobProperties
