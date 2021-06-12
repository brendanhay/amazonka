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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about a dominant language detection job.
--
-- /See:/ 'newDominantLanguageDetectionJobProperties' smart constructor.
data DominantLanguageDetectionJobProperties = DominantLanguageDetectionJobProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your dominant language
    -- detection job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The input data configuration that you supplied when you created the
    -- dominant language detection job.
    inputDataConfig :: Core.Maybe InputDataConfig,
    -- | A description for the status of a job.
    message :: Core.Maybe Core.Text,
    -- | The current status of the dominant language detection job. If the status
    -- is @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Core.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the
    -- dominant language detection job.
    outputDataConfig :: Core.Maybe OutputDataConfig,
    -- | The time that the dominant language detection job completed.
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
    -- | The time that the dominant language detection job was submitted for
    -- processing.
    submitTime :: Core.Maybe Core.POSIX,
    -- | The name that you assigned to the dominant language detection job.
    jobName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Core.Maybe Core.Text,
    -- | The identifier assigned to the dominant language detection job.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      inputDataConfig = Core.Nothing,
      message = Core.Nothing,
      jobStatus = Core.Nothing,
      outputDataConfig = Core.Nothing,
      endTime = Core.Nothing,
      volumeKmsKeyId = Core.Nothing,
      submitTime = Core.Nothing,
      jobName = Core.Nothing,
      dataAccessRoleArn = Core.Nothing,
      jobId = Core.Nothing
    }

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your dominant language
-- detection job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
dominantLanguageDetectionJobProperties_vpcConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe VpcConfig)
dominantLanguageDetectionJobProperties_vpcConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {vpcConfig = a} :: DominantLanguageDetectionJobProperties)

-- | The input data configuration that you supplied when you created the
-- dominant language detection job.
dominantLanguageDetectionJobProperties_inputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe InputDataConfig)
dominantLanguageDetectionJobProperties_inputDataConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {inputDataConfig = a} :: DominantLanguageDetectionJobProperties)

-- | A description for the status of a job.
dominantLanguageDetectionJobProperties_message :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Core.Text)
dominantLanguageDetectionJobProperties_message = Lens.lens (\DominantLanguageDetectionJobProperties' {message} -> message) (\s@DominantLanguageDetectionJobProperties' {} a -> s {message = a} :: DominantLanguageDetectionJobProperties)

-- | The current status of the dominant language detection job. If the status
-- is @FAILED@, the @Message@ field shows the reason for the failure.
dominantLanguageDetectionJobProperties_jobStatus :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe JobStatus)
dominantLanguageDetectionJobProperties_jobStatus = Lens.lens (\DominantLanguageDetectionJobProperties' {jobStatus} -> jobStatus) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobStatus = a} :: DominantLanguageDetectionJobProperties)

-- | The output data configuration that you supplied when you created the
-- dominant language detection job.
dominantLanguageDetectionJobProperties_outputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe OutputDataConfig)
dominantLanguageDetectionJobProperties_outputDataConfig = Lens.lens (\DominantLanguageDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@DominantLanguageDetectionJobProperties' {} a -> s {outputDataConfig = a} :: DominantLanguageDetectionJobProperties)

-- | The time that the dominant language detection job completed.
dominantLanguageDetectionJobProperties_endTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Core.UTCTime)
dominantLanguageDetectionJobProperties_endTime = Lens.lens (\DominantLanguageDetectionJobProperties' {endTime} -> endTime) (\s@DominantLanguageDetectionJobProperties' {} a -> s {endTime = a} :: DominantLanguageDetectionJobProperties) Core.. Lens.mapping Core._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
dominantLanguageDetectionJobProperties_volumeKmsKeyId :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Core.Text)
dominantLanguageDetectionJobProperties_volumeKmsKeyId = Lens.lens (\DominantLanguageDetectionJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@DominantLanguageDetectionJobProperties' {} a -> s {volumeKmsKeyId = a} :: DominantLanguageDetectionJobProperties)

-- | The time that the dominant language detection job was submitted for
-- processing.
dominantLanguageDetectionJobProperties_submitTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Core.UTCTime)
dominantLanguageDetectionJobProperties_submitTime = Lens.lens (\DominantLanguageDetectionJobProperties' {submitTime} -> submitTime) (\s@DominantLanguageDetectionJobProperties' {} a -> s {submitTime = a} :: DominantLanguageDetectionJobProperties) Core.. Lens.mapping Core._Time

-- | The name that you assigned to the dominant language detection job.
dominantLanguageDetectionJobProperties_jobName :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Core.Text)
dominantLanguageDetectionJobProperties_jobName = Lens.lens (\DominantLanguageDetectionJobProperties' {jobName} -> jobName) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobName = a} :: DominantLanguageDetectionJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
dominantLanguageDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Core.Text)
dominantLanguageDetectionJobProperties_dataAccessRoleArn = Lens.lens (\DominantLanguageDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@DominantLanguageDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: DominantLanguageDetectionJobProperties)

-- | The identifier assigned to the dominant language detection job.
dominantLanguageDetectionJobProperties_jobId :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Core.Text)
dominantLanguageDetectionJobProperties_jobId = Lens.lens (\DominantLanguageDetectionJobProperties' {jobId} -> jobId) (\s@DominantLanguageDetectionJobProperties' {} a -> s {jobId = a} :: DominantLanguageDetectionJobProperties)

instance
  Core.FromJSON
    DominantLanguageDetectionJobProperties
  where
  parseJSON =
    Core.withObject
      "DominantLanguageDetectionJobProperties"
      ( \x ->
          DominantLanguageDetectionJobProperties'
            Core.<$> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (x Core..:? "OutputDataConfig")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "VolumeKmsKeyId")
            Core.<*> (x Core..:? "SubmitTime")
            Core.<*> (x Core..:? "JobName")
            Core.<*> (x Core..:? "DataAccessRoleArn")
            Core.<*> (x Core..:? "JobId")
      )

instance
  Core.Hashable
    DominantLanguageDetectionJobProperties

instance
  Core.NFData
    DominantLanguageDetectionJobProperties
