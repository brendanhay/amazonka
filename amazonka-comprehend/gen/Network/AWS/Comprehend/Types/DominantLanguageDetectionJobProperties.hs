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
-- Module      : Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a dominant language detection job.
--
-- /See:/ 'newDominantLanguageDetectionJobProperties' smart constructor.
data DominantLanguageDetectionJobProperties = DominantLanguageDetectionJobProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your dominant language
    -- detection job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The input data configuration that you supplied when you created the
    -- dominant language detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | A description for the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the dominant language detection job. If the status
    -- is @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the
    -- dominant language detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the dominant language detection job completed.
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
    -- | The time that the dominant language detection job was submitted for
    -- processing.
    submitTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name that you assigned to the dominant language detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the dominant language detection job.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DominantLanguageDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'dominantLanguageDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your dominant language
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'inputDataConfig', 'dominantLanguageDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- dominant language detection job.
--
-- 'message', 'dominantLanguageDetectionJobProperties_message' - A description for the status of a job.
--
-- 'jobStatus', 'dominantLanguageDetectionJobProperties_jobStatus' - The current status of the dominant language detection job. If the status
-- is @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'outputDataConfig', 'dominantLanguageDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- dominant language detection job.
--
-- 'endTime', 'dominantLanguageDetectionJobProperties_endTime' - The time that the dominant language detection job completed.
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
-- 'submitTime', 'dominantLanguageDetectionJobProperties_submitTime' - The time that the dominant language detection job was submitted for
-- processing.
--
-- 'jobName', 'dominantLanguageDetectionJobProperties_jobName' - The name that you assigned to the dominant language detection job.
--
-- 'dataAccessRoleArn', 'dominantLanguageDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'jobId', 'dominantLanguageDetectionJobProperties_jobId' - The identifier assigned to the dominant language detection job.
newDominantLanguageDetectionJobProperties ::
  DominantLanguageDetectionJobProperties
newDominantLanguageDetectionJobProperties =
  DominantLanguageDetectionJobProperties'
    { vpcConfig =
        Prelude.Nothing,
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
-- containing the resources you are using for your dominant language
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
dominantLanguageDetectionJobProperties_vpcConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe VpcConfig)
dominantLanguageDetectionJobProperties_vpcConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {vpcConfig = a} :: DominantLanguageDetectionJobProperties)

-- | The input data configuration that you supplied when you created the
-- dominant language detection job.
dominantLanguageDetectionJobProperties_inputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe InputDataConfig)
dominantLanguageDetectionJobProperties_inputDataConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {inputDataConfig = a} :: DominantLanguageDetectionJobProperties)

-- | A description for the status of a job.
dominantLanguageDetectionJobProperties_message :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_message = Lens.lens (\DominantLanguageDetectionJobProperties' {message} -> message) (\s@DominantLanguageDetectionJobProperties' {} a -> s {message = a} :: DominantLanguageDetectionJobProperties)

-- | The current status of the dominant language detection job. If the status
-- is @FAILED@, the @Message@ field shows the reason for the failure.
dominantLanguageDetectionJobProperties_jobStatus :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe JobStatus)
dominantLanguageDetectionJobProperties_jobStatus = Lens.lens (\DominantLanguageDetectionJobProperties' {jobStatus} -> jobStatus) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobStatus = a} :: DominantLanguageDetectionJobProperties)

-- | The output data configuration that you supplied when you created the
-- dominant language detection job.
dominantLanguageDetectionJobProperties_outputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe OutputDataConfig)
dominantLanguageDetectionJobProperties_outputDataConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {outputDataConfig = a} :: DominantLanguageDetectionJobProperties)

-- | The time that the dominant language detection job completed.
dominantLanguageDetectionJobProperties_endTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
dominantLanguageDetectionJobProperties_endTime = Lens.lens (\DominantLanguageDetectionJobProperties' {endTime} -> endTime) (\s@DominantLanguageDetectionJobProperties' {} a -> s {endTime = a} :: DominantLanguageDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

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

-- | The time that the dominant language detection job was submitted for
-- processing.
dominantLanguageDetectionJobProperties_submitTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
dominantLanguageDetectionJobProperties_submitTime = Lens.lens (\DominantLanguageDetectionJobProperties' {submitTime} -> submitTime) (\s@DominantLanguageDetectionJobProperties' {} a -> s {submitTime = a} :: DominantLanguageDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | The name that you assigned to the dominant language detection job.
dominantLanguageDetectionJobProperties_jobName :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_jobName = Lens.lens (\DominantLanguageDetectionJobProperties' {jobName} -> jobName) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobName = a} :: DominantLanguageDetectionJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
dominantLanguageDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_dataAccessRoleArn = Lens.lens (\DominantLanguageDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@DominantLanguageDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: DominantLanguageDetectionJobProperties)

-- | The identifier assigned to the dominant language detection job.
dominantLanguageDetectionJobProperties_jobId :: Lens.Lens' DominantLanguageDetectionJobProperties (Prelude.Maybe Prelude.Text)
dominantLanguageDetectionJobProperties_jobId = Lens.lens (\DominantLanguageDetectionJobProperties' {jobId} -> jobId) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobId = a} :: DominantLanguageDetectionJobProperties)

instance
  Prelude.FromJSON
    DominantLanguageDetectionJobProperties
  where
  parseJSON =
    Prelude.withObject
      "DominantLanguageDetectionJobProperties"
      ( \x ->
          DominantLanguageDetectionJobProperties'
            Prelude.<$> (x Prelude..:? "VpcConfig")
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
    DominantLanguageDetectionJobProperties

instance
  Prelude.NFData
    DominantLanguageDetectionJobProperties
