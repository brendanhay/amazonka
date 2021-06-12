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
-- Module      : Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about a key phrases detection job.
--
-- /See:/ 'newKeyPhrasesDetectionJobProperties' smart constructor.
data KeyPhrasesDetectionJobProperties = KeyPhrasesDetectionJobProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your key phrases detection
    -- job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The language code of the input documents.
    languageCode :: Core.Maybe LanguageCode,
    -- | The input data configuration that you supplied when you created the key
    -- phrases detection job.
    inputDataConfig :: Core.Maybe InputDataConfig,
    -- | A description of the status of a job.
    message :: Core.Maybe Core.Text,
    -- | The current status of the key phrases detection job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Core.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the key
    -- phrases detection job.
    outputDataConfig :: Core.Maybe OutputDataConfig,
    -- | The time that the key phrases detection job completed.
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
    -- | The time that the key phrases detection job was submitted for
    -- processing.
    submitTime :: Core.Maybe Core.POSIX,
    -- | The name that you assigned the key phrases detection job.
    jobName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Core.Maybe Core.Text,
    -- | The identifier assigned to the key phrases detection job.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KeyPhrasesDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'keyPhrasesDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your key phrases detection
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'languageCode', 'keyPhrasesDetectionJobProperties_languageCode' - The language code of the input documents.
--
-- 'inputDataConfig', 'keyPhrasesDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the key
-- phrases detection job.
--
-- 'message', 'keyPhrasesDetectionJobProperties_message' - A description of the status of a job.
--
-- 'jobStatus', 'keyPhrasesDetectionJobProperties_jobStatus' - The current status of the key phrases detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'outputDataConfig', 'keyPhrasesDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the key
-- phrases detection job.
--
-- 'endTime', 'keyPhrasesDetectionJobProperties_endTime' - The time that the key phrases detection job completed.
--
-- 'volumeKmsKeyId', 'keyPhrasesDetectionJobProperties_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'submitTime', 'keyPhrasesDetectionJobProperties_submitTime' - The time that the key phrases detection job was submitted for
-- processing.
--
-- 'jobName', 'keyPhrasesDetectionJobProperties_jobName' - The name that you assigned the key phrases detection job.
--
-- 'dataAccessRoleArn', 'keyPhrasesDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'jobId', 'keyPhrasesDetectionJobProperties_jobId' - The identifier assigned to the key phrases detection job.
newKeyPhrasesDetectionJobProperties ::
  KeyPhrasesDetectionJobProperties
newKeyPhrasesDetectionJobProperties =
  KeyPhrasesDetectionJobProperties'
    { vpcConfig =
        Core.Nothing,
      languageCode = Core.Nothing,
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
-- containing the resources you are using for your key phrases detection
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
keyPhrasesDetectionJobProperties_vpcConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe VpcConfig)
keyPhrasesDetectionJobProperties_vpcConfig = Lens.lens (\KeyPhrasesDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {vpcConfig = a} :: KeyPhrasesDetectionJobProperties)

-- | The language code of the input documents.
keyPhrasesDetectionJobProperties_languageCode :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe LanguageCode)
keyPhrasesDetectionJobProperties_languageCode = Lens.lens (\KeyPhrasesDetectionJobProperties' {languageCode} -> languageCode) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {languageCode = a} :: KeyPhrasesDetectionJobProperties)

-- | The input data configuration that you supplied when you created the key
-- phrases detection job.
keyPhrasesDetectionJobProperties_inputDataConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe InputDataConfig)
keyPhrasesDetectionJobProperties_inputDataConfig = Lens.lens (\KeyPhrasesDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {inputDataConfig = a} :: KeyPhrasesDetectionJobProperties)

-- | A description of the status of a job.
keyPhrasesDetectionJobProperties_message :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Core.Text)
keyPhrasesDetectionJobProperties_message = Lens.lens (\KeyPhrasesDetectionJobProperties' {message} -> message) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {message = a} :: KeyPhrasesDetectionJobProperties)

-- | The current status of the key phrases detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
keyPhrasesDetectionJobProperties_jobStatus :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe JobStatus)
keyPhrasesDetectionJobProperties_jobStatus = Lens.lens (\KeyPhrasesDetectionJobProperties' {jobStatus} -> jobStatus) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {jobStatus = a} :: KeyPhrasesDetectionJobProperties)

-- | The output data configuration that you supplied when you created the key
-- phrases detection job.
keyPhrasesDetectionJobProperties_outputDataConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe OutputDataConfig)
keyPhrasesDetectionJobProperties_outputDataConfig = Lens.lens (\KeyPhrasesDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {outputDataConfig = a} :: KeyPhrasesDetectionJobProperties)

-- | The time that the key phrases detection job completed.
keyPhrasesDetectionJobProperties_endTime :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Core.UTCTime)
keyPhrasesDetectionJobProperties_endTime = Lens.lens (\KeyPhrasesDetectionJobProperties' {endTime} -> endTime) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {endTime = a} :: KeyPhrasesDetectionJobProperties) Core.. Lens.mapping Core._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
keyPhrasesDetectionJobProperties_volumeKmsKeyId :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Core.Text)
keyPhrasesDetectionJobProperties_volumeKmsKeyId = Lens.lens (\KeyPhrasesDetectionJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {volumeKmsKeyId = a} :: KeyPhrasesDetectionJobProperties)

-- | The time that the key phrases detection job was submitted for
-- processing.
keyPhrasesDetectionJobProperties_submitTime :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Core.UTCTime)
keyPhrasesDetectionJobProperties_submitTime = Lens.lens (\KeyPhrasesDetectionJobProperties' {submitTime} -> submitTime) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {submitTime = a} :: KeyPhrasesDetectionJobProperties) Core.. Lens.mapping Core._Time

-- | The name that you assigned the key phrases detection job.
keyPhrasesDetectionJobProperties_jobName :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Core.Text)
keyPhrasesDetectionJobProperties_jobName = Lens.lens (\KeyPhrasesDetectionJobProperties' {jobName} -> jobName) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {jobName = a} :: KeyPhrasesDetectionJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
keyPhrasesDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Core.Text)
keyPhrasesDetectionJobProperties_dataAccessRoleArn = Lens.lens (\KeyPhrasesDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: KeyPhrasesDetectionJobProperties)

-- | The identifier assigned to the key phrases detection job.
keyPhrasesDetectionJobProperties_jobId :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Core.Text)
keyPhrasesDetectionJobProperties_jobId = Lens.lens (\KeyPhrasesDetectionJobProperties' {jobId} -> jobId) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {jobId = a} :: KeyPhrasesDetectionJobProperties)

instance
  Core.FromJSON
    KeyPhrasesDetectionJobProperties
  where
  parseJSON =
    Core.withObject
      "KeyPhrasesDetectionJobProperties"
      ( \x ->
          KeyPhrasesDetectionJobProperties'
            Core.<$> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "LanguageCode")
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
    KeyPhrasesDetectionJobProperties

instance Core.NFData KeyPhrasesDetectionJobProperties
