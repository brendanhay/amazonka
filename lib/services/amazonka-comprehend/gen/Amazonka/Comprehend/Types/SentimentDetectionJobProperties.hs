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
-- Module      : Amazonka.Comprehend.Types.SentimentDetectionJobProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.SentimentDetectionJobProperties where

import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.LanguageCode
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a sentiment detection job.
--
-- /See:/ 'newSentimentDetectionJobProperties' smart constructor.
data SentimentDetectionJobProperties = SentimentDetectionJobProperties'
  { -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the sentiment detection job ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The input data configuration that you supplied when you created the
    -- sentiment detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The Amazon Resource Name (ARN) of the sentiment detection job. It is a
    -- unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:sentiment-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the sentiment detection job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned to the sentiment detection job
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the sentiment detection job. If the status is
    -- @FAILED@, the @Messages@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The output data configuration that you supplied when you created the
    -- sentiment detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the sentiment detection job was submitted for processing.
    submitTime :: Prelude.Maybe Data.POSIX,
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
    -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your sentiment detection job.
    -- For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SentimentDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAccessRoleArn', 'sentimentDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'endTime', 'sentimentDetectionJobProperties_endTime' - The time that the sentiment detection job ended.
--
-- 'inputDataConfig', 'sentimentDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- sentiment detection job.
--
-- 'jobArn', 'sentimentDetectionJobProperties_jobArn' - The Amazon Resource Name (ARN) of the sentiment detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:sentiment-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'jobId', 'sentimentDetectionJobProperties_jobId' - The identifier assigned to the sentiment detection job.
--
-- 'jobName', 'sentimentDetectionJobProperties_jobName' - The name that you assigned to the sentiment detection job
--
-- 'jobStatus', 'sentimentDetectionJobProperties_jobStatus' - The current status of the sentiment detection job. If the status is
-- @FAILED@, the @Messages@ field shows the reason for the failure.
--
-- 'languageCode', 'sentimentDetectionJobProperties_languageCode' - The language code of the input documents.
--
-- 'message', 'sentimentDetectionJobProperties_message' - A description of the status of a job.
--
-- 'outputDataConfig', 'sentimentDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- sentiment detection job.
--
-- 'submitTime', 'sentimentDetectionJobProperties_submitTime' - The time that the sentiment detection job was submitted for processing.
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
-- 'vpcConfig', 'sentimentDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your sentiment detection job.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
newSentimentDetectionJobProperties ::
  SentimentDetectionJobProperties
newSentimentDetectionJobProperties =
  SentimentDetectionJobProperties'
    { dataAccessRoleArn =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      message = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
sentimentDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
sentimentDetectionJobProperties_dataAccessRoleArn = Lens.lens (\SentimentDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@SentimentDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: SentimentDetectionJobProperties)

-- | The time that the sentiment detection job ended.
sentimentDetectionJobProperties_endTime :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
sentimentDetectionJobProperties_endTime = Lens.lens (\SentimentDetectionJobProperties' {endTime} -> endTime) (\s@SentimentDetectionJobProperties' {} a -> s {endTime = a} :: SentimentDetectionJobProperties) Prelude.. Lens.mapping Data._Time

-- | The input data configuration that you supplied when you created the
-- sentiment detection job.
sentimentDetectionJobProperties_inputDataConfig :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe InputDataConfig)
sentimentDetectionJobProperties_inputDataConfig = Lens.lens (\SentimentDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@SentimentDetectionJobProperties' {} a -> s {inputDataConfig = a} :: SentimentDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the sentiment detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:sentiment-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
sentimentDetectionJobProperties_jobArn :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
sentimentDetectionJobProperties_jobArn = Lens.lens (\SentimentDetectionJobProperties' {jobArn} -> jobArn) (\s@SentimentDetectionJobProperties' {} a -> s {jobArn = a} :: SentimentDetectionJobProperties)

-- | The identifier assigned to the sentiment detection job.
sentimentDetectionJobProperties_jobId :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
sentimentDetectionJobProperties_jobId = Lens.lens (\SentimentDetectionJobProperties' {jobId} -> jobId) (\s@SentimentDetectionJobProperties' {} a -> s {jobId = a} :: SentimentDetectionJobProperties)

-- | The name that you assigned to the sentiment detection job
sentimentDetectionJobProperties_jobName :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
sentimentDetectionJobProperties_jobName = Lens.lens (\SentimentDetectionJobProperties' {jobName} -> jobName) (\s@SentimentDetectionJobProperties' {} a -> s {jobName = a} :: SentimentDetectionJobProperties)

-- | The current status of the sentiment detection job. If the status is
-- @FAILED@, the @Messages@ field shows the reason for the failure.
sentimentDetectionJobProperties_jobStatus :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe JobStatus)
sentimentDetectionJobProperties_jobStatus = Lens.lens (\SentimentDetectionJobProperties' {jobStatus} -> jobStatus) (\s@SentimentDetectionJobProperties' {} a -> s {jobStatus = a} :: SentimentDetectionJobProperties)

-- | The language code of the input documents.
sentimentDetectionJobProperties_languageCode :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe LanguageCode)
sentimentDetectionJobProperties_languageCode = Lens.lens (\SentimentDetectionJobProperties' {languageCode} -> languageCode) (\s@SentimentDetectionJobProperties' {} a -> s {languageCode = a} :: SentimentDetectionJobProperties)

-- | A description of the status of a job.
sentimentDetectionJobProperties_message :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
sentimentDetectionJobProperties_message = Lens.lens (\SentimentDetectionJobProperties' {message} -> message) (\s@SentimentDetectionJobProperties' {} a -> s {message = a} :: SentimentDetectionJobProperties)

-- | The output data configuration that you supplied when you created the
-- sentiment detection job.
sentimentDetectionJobProperties_outputDataConfig :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe OutputDataConfig)
sentimentDetectionJobProperties_outputDataConfig = Lens.lens (\SentimentDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@SentimentDetectionJobProperties' {} a -> s {outputDataConfig = a} :: SentimentDetectionJobProperties)

-- | The time that the sentiment detection job was submitted for processing.
sentimentDetectionJobProperties_submitTime :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
sentimentDetectionJobProperties_submitTime = Lens.lens (\SentimentDetectionJobProperties' {submitTime} -> submitTime) (\s@SentimentDetectionJobProperties' {} a -> s {submitTime = a} :: SentimentDetectionJobProperties) Prelude.. Lens.mapping Data._Time

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

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your sentiment detection job.
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
sentimentDetectionJobProperties_vpcConfig :: Lens.Lens' SentimentDetectionJobProperties (Prelude.Maybe VpcConfig)
sentimentDetectionJobProperties_vpcConfig = Lens.lens (\SentimentDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@SentimentDetectionJobProperties' {} a -> s {vpcConfig = a} :: SentimentDetectionJobProperties)

instance
  Data.FromJSON
    SentimentDetectionJobProperties
  where
  parseJSON =
    Data.withObject
      "SentimentDetectionJobProperties"
      ( \x ->
          SentimentDetectionJobProperties'
            Prelude.<$> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "InputDataConfig")
            Prelude.<*> (x Data..:? "JobArn")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobStatus")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "OutputDataConfig")
            Prelude.<*> (x Data..:? "SubmitTime")
            Prelude.<*> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance
  Prelude.Hashable
    SentimentDetectionJobProperties
  where
  hashWithSalt
    _salt
    SentimentDetectionJobProperties' {..} =
      _salt
        `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` jobArn
        `Prelude.hashWithSalt` jobId
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` languageCode
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` submitTime
        `Prelude.hashWithSalt` volumeKmsKeyId
        `Prelude.hashWithSalt` vpcConfig

instance
  Prelude.NFData
    SentimentDetectionJobProperties
  where
  rnf SentimentDetectionJobProperties' {..} =
    Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig
