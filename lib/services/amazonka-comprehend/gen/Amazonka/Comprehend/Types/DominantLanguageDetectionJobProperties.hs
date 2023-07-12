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
-- Module      : Amazonka.Comprehend.Types.DominantLanguageDetectionJobProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DominantLanguageDetectionJobProperties where

import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a dominant language detection job.
--
-- /See:/ 'newDominantLanguageDetectionJobProperties' smart constructor.
data DominantLanguageDetectionJobProperties = DominantLanguageDetectionJobProperties'
  { -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the dominant language detection job completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The input data configuration that you supplied when you created the
    -- dominant language detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The Amazon Resource Name (ARN) of the dominant language detection job.
    -- It is a unique, fully qualified identifier for the job. It includes the
    -- AWS account, Region, and the job ID. The format of the ARN is as
    -- follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:dominant-language-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:dominant-language-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the dominant language detection job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned to the dominant language detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the dominant language detection job. If the status
    -- is @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | A description for the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The output data configuration that you supplied when you created the
    -- dominant language detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the dominant language detection job was submitted for
    -- processing.
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
    -- containing the resources you are using for your dominant language
    -- detection job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DominantLanguageDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAccessRoleArn', 'dominantLanguageDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'endTime', 'dominantLanguageDetectionJobProperties_endTime' - The time that the dominant language detection job completed.
--
-- 'inputDataConfig', 'dominantLanguageDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- dominant language detection job.
--
-- 'jobArn', 'dominantLanguageDetectionJobProperties_jobArn' - The Amazon Resource Name (ARN) of the dominant language detection job.
-- It is a unique, fully qualified identifier for the job. It includes the
-- AWS account, Region, and the job ID. The format of the ARN is as
-- follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:dominant-language-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:dominant-language-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'jobId', 'dominantLanguageDetectionJobProperties_jobId' - The identifier assigned to the dominant language detection job.
--
-- 'jobName', 'dominantLanguageDetectionJobProperties_jobName' - The name that you assigned to the dominant language detection job.
--
-- 'jobStatus', 'dominantLanguageDetectionJobProperties_jobStatus' - The current status of the dominant language detection job. If the status
-- is @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'message', 'dominantLanguageDetectionJobProperties_message' - A description for the status of a job.
--
-- 'outputDataConfig', 'dominantLanguageDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- dominant language detection job.
--
-- 'submitTime', 'dominantLanguageDetectionJobProperties_submitTime' - The time that the dominant language detection job was submitted for
-- processing.
--
-- 'volumeKmsKeyId', 'dominantLanguageDetectionJobProperties_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'vpcConfig', 'dominantLanguageDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your dominant language
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
newDominantLanguageDetectionJobProperties ::
  DominantLanguageDetectionJobProperties
newDominantLanguageDetectionJobProperties =
  DominantLanguageDetectionJobProperties'
    { dataAccessRoleArn =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      message = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
dominantLanguageDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_dataAccessRoleArn = Lens.lens (\DominantLanguageDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@DominantLanguageDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: DominantLanguageDetectionJobProperties)

-- | The time that the dominant language detection job completed.
dominantLanguageDetectionJobProperties_endTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
dominantLanguageDetectionJobProperties_endTime = Lens.lens (\DominantLanguageDetectionJobProperties' {endTime} -> endTime) (\s@DominantLanguageDetectionJobProperties' {} a -> s {endTime = a} :: DominantLanguageDetectionJobProperties) Prelude.. Lens.mapping Data._Time

-- | The input data configuration that you supplied when you created the
-- dominant language detection job.
dominantLanguageDetectionJobProperties_inputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe InputDataConfig)
dominantLanguageDetectionJobProperties_inputDataConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {inputDataConfig = a} :: DominantLanguageDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the dominant language detection job.
-- It is a unique, fully qualified identifier for the job. It includes the
-- AWS account, Region, and the job ID. The format of the ARN is as
-- follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:dominant-language-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:dominant-language-detection-job\/1234abcd12ab34cd56ef1234567890ab@
dominantLanguageDetectionJobProperties_jobArn :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_jobArn = Lens.lens (\DominantLanguageDetectionJobProperties' {jobArn} -> jobArn) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobArn = a} :: DominantLanguageDetectionJobProperties)

-- | The identifier assigned to the dominant language detection job.
dominantLanguageDetectionJobProperties_jobId :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_jobId = Lens.lens (\DominantLanguageDetectionJobProperties' {jobId} -> jobId) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobId = a} :: DominantLanguageDetectionJobProperties)

-- | The name that you assigned to the dominant language detection job.
dominantLanguageDetectionJobProperties_jobName :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_jobName = Lens.lens (\DominantLanguageDetectionJobProperties' {jobName} -> jobName) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobName = a} :: DominantLanguageDetectionJobProperties)

-- | The current status of the dominant language detection job. If the status
-- is @FAILED@, the @Message@ field shows the reason for the failure.
dominantLanguageDetectionJobProperties_jobStatus :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe JobStatus)
dominantLanguageDetectionJobProperties_jobStatus = Lens.lens (\DominantLanguageDetectionJobProperties' {jobStatus} -> jobStatus) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobStatus = a} :: DominantLanguageDetectionJobProperties)

-- | A description for the status of a job.
dominantLanguageDetectionJobProperties_message :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_message = Lens.lens (\DominantLanguageDetectionJobProperties' {message} -> message) (\s@DominantLanguageDetectionJobProperties' {} a -> s {message = a} :: DominantLanguageDetectionJobProperties)

-- | The output data configuration that you supplied when you created the
-- dominant language detection job.
dominantLanguageDetectionJobProperties_outputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe OutputDataConfig)
dominantLanguageDetectionJobProperties_outputDataConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {outputDataConfig = a} :: DominantLanguageDetectionJobProperties)

-- | The time that the dominant language detection job was submitted for
-- processing.
dominantLanguageDetectionJobProperties_submitTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
dominantLanguageDetectionJobProperties_submitTime = Lens.lens (\DominantLanguageDetectionJobProperties' {submitTime} -> submitTime) (\s@DominantLanguageDetectionJobProperties' {} a -> s {submitTime = a} :: DominantLanguageDetectionJobProperties) Prelude.. Lens.mapping Data._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
dominantLanguageDetectionJobProperties_volumeKmsKeyId :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_volumeKmsKeyId = Lens.lens (\DominantLanguageDetectionJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@DominantLanguageDetectionJobProperties' {} a -> s {volumeKmsKeyId = a} :: DominantLanguageDetectionJobProperties)

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your dominant language
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
dominantLanguageDetectionJobProperties_vpcConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe VpcConfig)
dominantLanguageDetectionJobProperties_vpcConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {vpcConfig = a} :: DominantLanguageDetectionJobProperties)

instance
  Data.FromJSON
    DominantLanguageDetectionJobProperties
  where
  parseJSON =
    Data.withObject
      "DominantLanguageDetectionJobProperties"
      ( \x ->
          DominantLanguageDetectionJobProperties'
            Prelude.<$> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "InputDataConfig")
            Prelude.<*> (x Data..:? "JobArn")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobStatus")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "OutputDataConfig")
            Prelude.<*> (x Data..:? "SubmitTime")
            Prelude.<*> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance
  Prelude.Hashable
    DominantLanguageDetectionJobProperties
  where
  hashWithSalt
    _salt
    DominantLanguageDetectionJobProperties' {..} =
      _salt
        `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` jobArn
        `Prelude.hashWithSalt` jobId
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` submitTime
        `Prelude.hashWithSalt` volumeKmsKeyId
        `Prelude.hashWithSalt` vpcConfig

instance
  Prelude.NFData
    DominantLanguageDetectionJobProperties
  where
  rnf DominantLanguageDetectionJobProperties' {..} =
    Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf vpcConfig
