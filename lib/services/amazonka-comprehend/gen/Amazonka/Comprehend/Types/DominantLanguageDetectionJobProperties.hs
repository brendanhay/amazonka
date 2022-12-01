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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DominantLanguageDetectionJobProperties where

import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a dominant language detection job.
--
-- /See:/ 'newDominantLanguageDetectionJobProperties' smart constructor.
data DominantLanguageDetectionJobProperties = DominantLanguageDetectionJobProperties'
  { -- | The output data configuration that you supplied when you created the
    -- dominant language detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | A description for the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the dominant language detection job. If the status
    -- is @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your dominant language
    -- detection job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The name that you assigned to the dominant language detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The time that the dominant language detection job was submitted for
    -- processing.
    submitTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier assigned to the dominant language detection job.
    jobId :: Prelude.Maybe Prelude.Text,
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
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the dominant language detection job completed.
    endTime :: Prelude.Maybe Core.POSIX,
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
    -- | The input data configuration that you supplied when you created the
    -- dominant language detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig
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
-- 'outputDataConfig', 'dominantLanguageDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- dominant language detection job.
--
-- 'message', 'dominantLanguageDetectionJobProperties_message' - A description for the status of a job.
--
-- 'jobStatus', 'dominantLanguageDetectionJobProperties_jobStatus' - The current status of the dominant language detection job. If the status
-- is @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'vpcConfig', 'dominantLanguageDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your dominant language
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'jobName', 'dominantLanguageDetectionJobProperties_jobName' - The name that you assigned to the dominant language detection job.
--
-- 'submitTime', 'dominantLanguageDetectionJobProperties_submitTime' - The time that the dominant language detection job was submitted for
-- processing.
--
-- 'jobId', 'dominantLanguageDetectionJobProperties_jobId' - The identifier assigned to the dominant language detection job.
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
-- 'dataAccessRoleArn', 'dominantLanguageDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'endTime', 'dominantLanguageDetectionJobProperties_endTime' - The time that the dominant language detection job completed.
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
-- 'inputDataConfig', 'dominantLanguageDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- dominant language detection job.
newDominantLanguageDetectionJobProperties ::
  DominantLanguageDetectionJobProperties
newDominantLanguageDetectionJobProperties =
  DominantLanguageDetectionJobProperties'
    { outputDataConfig =
        Prelude.Nothing,
      message = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      jobName = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      jobId = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      endTime = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing
    }

-- | The output data configuration that you supplied when you created the
-- dominant language detection job.
dominantLanguageDetectionJobProperties_outputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe OutputDataConfig)
dominantLanguageDetectionJobProperties_outputDataConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {outputDataConfig = a} :: DominantLanguageDetectionJobProperties)

-- | A description for the status of a job.
dominantLanguageDetectionJobProperties_message :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_message = Lens.lens (\DominantLanguageDetectionJobProperties' {message} -> message) (\s@DominantLanguageDetectionJobProperties' {} a -> s {message = a} :: DominantLanguageDetectionJobProperties)

-- | The current status of the dominant language detection job. If the status
-- is @FAILED@, the @Message@ field shows the reason for the failure.
dominantLanguageDetectionJobProperties_jobStatus :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe JobStatus)
dominantLanguageDetectionJobProperties_jobStatus = Lens.lens (\DominantLanguageDetectionJobProperties' {jobStatus} -> jobStatus) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobStatus = a} :: DominantLanguageDetectionJobProperties)

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your dominant language
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
dominantLanguageDetectionJobProperties_vpcConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe VpcConfig)
dominantLanguageDetectionJobProperties_vpcConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {vpcConfig = a} :: DominantLanguageDetectionJobProperties)

-- | The name that you assigned to the dominant language detection job.
dominantLanguageDetectionJobProperties_jobName :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_jobName = Lens.lens (\DominantLanguageDetectionJobProperties' {jobName} -> jobName) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobName = a} :: DominantLanguageDetectionJobProperties)

-- | The time that the dominant language detection job was submitted for
-- processing.
dominantLanguageDetectionJobProperties_submitTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
dominantLanguageDetectionJobProperties_submitTime = Lens.lens (\DominantLanguageDetectionJobProperties' {submitTime} -> submitTime) (\s@DominantLanguageDetectionJobProperties' {} a -> s {submitTime = a} :: DominantLanguageDetectionJobProperties) Prelude.. Lens.mapping Core._Time

-- | The identifier assigned to the dominant language detection job.
dominantLanguageDetectionJobProperties_jobId :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_jobId = Lens.lens (\DominantLanguageDetectionJobProperties' {jobId} -> jobId) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobId = a} :: DominantLanguageDetectionJobProperties)

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

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
dominantLanguageDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_dataAccessRoleArn = Lens.lens (\DominantLanguageDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@DominantLanguageDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: DominantLanguageDetectionJobProperties)

-- | The time that the dominant language detection job completed.
dominantLanguageDetectionJobProperties_endTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
dominantLanguageDetectionJobProperties_endTime = Lens.lens (\DominantLanguageDetectionJobProperties' {endTime} -> endTime) (\s@DominantLanguageDetectionJobProperties' {} a -> s {endTime = a} :: DominantLanguageDetectionJobProperties) Prelude.. Lens.mapping Core._Time

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

-- | The input data configuration that you supplied when you created the
-- dominant language detection job.
dominantLanguageDetectionJobProperties_inputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe InputDataConfig)
dominantLanguageDetectionJobProperties_inputDataConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {inputDataConfig = a} :: DominantLanguageDetectionJobProperties)

instance
  Core.FromJSON
    DominantLanguageDetectionJobProperties
  where
  parseJSON =
    Core.withObject
      "DominantLanguageDetectionJobProperties"
      ( \x ->
          DominantLanguageDetectionJobProperties'
            Prelude.<$> (x Core..:? "OutputDataConfig")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "JobStatus")
            Prelude.<*> (x Core..:? "VpcConfig")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "SubmitTime")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "VolumeKmsKeyId")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "JobArn")
            Prelude.<*> (x Core..:? "InputDataConfig")
      )

instance
  Prelude.Hashable
    DominantLanguageDetectionJobProperties
  where
  hashWithSalt
    _salt
    DominantLanguageDetectionJobProperties' {..} =
      _salt `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` vpcConfig
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` submitTime
        `Prelude.hashWithSalt` jobId
        `Prelude.hashWithSalt` volumeKmsKeyId
        `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` jobArn
        `Prelude.hashWithSalt` inputDataConfig

instance
  Prelude.NFData
    DominantLanguageDetectionJobProperties
  where
  rnf DominantLanguageDetectionJobProperties' {..} =
    Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf inputDataConfig
