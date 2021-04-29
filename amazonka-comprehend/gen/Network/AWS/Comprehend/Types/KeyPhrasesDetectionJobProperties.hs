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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a key phrases detection job.
--
-- /See:/ 'newKeyPhrasesDetectionJobProperties' smart constructor.
data KeyPhrasesDetectionJobProperties = KeyPhrasesDetectionJobProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your key phrases detection
    -- job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The input data configuration that you supplied when you created the key
    -- phrases detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the key phrases detection job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The output data configuration that you supplied when you created the key
    -- phrases detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the key phrases detection job completed.
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
    -- | The time that the key phrases detection job was submitted for
    -- processing.
    submitTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name that you assigned the key phrases detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the key phrases detection job.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- containing the resources you are using for your key phrases detection
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
keyPhrasesDetectionJobProperties_vpcConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe VpcConfig)
keyPhrasesDetectionJobProperties_vpcConfig = Lens.lens (\KeyPhrasesDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {vpcConfig = a} :: KeyPhrasesDetectionJobProperties)

-- | The language code of the input documents.
keyPhrasesDetectionJobProperties_languageCode :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe LanguageCode)
keyPhrasesDetectionJobProperties_languageCode = Lens.lens (\KeyPhrasesDetectionJobProperties' {languageCode} -> languageCode) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {languageCode = a} :: KeyPhrasesDetectionJobProperties)

-- | The input data configuration that you supplied when you created the key
-- phrases detection job.
keyPhrasesDetectionJobProperties_inputDataConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe InputDataConfig)
keyPhrasesDetectionJobProperties_inputDataConfig = Lens.lens (\KeyPhrasesDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {inputDataConfig = a} :: KeyPhrasesDetectionJobProperties)

-- | A description of the status of a job.
keyPhrasesDetectionJobProperties_message :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.Text)
keyPhrasesDetectionJobProperties_message = Lens.lens (\KeyPhrasesDetectionJobProperties' {message} -> message) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {message = a} :: KeyPhrasesDetectionJobProperties)

-- | The current status of the key phrases detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
keyPhrasesDetectionJobProperties_jobStatus :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe JobStatus)
keyPhrasesDetectionJobProperties_jobStatus = Lens.lens (\KeyPhrasesDetectionJobProperties' {jobStatus} -> jobStatus) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {jobStatus = a} :: KeyPhrasesDetectionJobProperties)

-- | The output data configuration that you supplied when you created the key
-- phrases detection job.
keyPhrasesDetectionJobProperties_outputDataConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe OutputDataConfig)
keyPhrasesDetectionJobProperties_outputDataConfig = Lens.lens (\KeyPhrasesDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {outputDataConfig = a} :: KeyPhrasesDetectionJobProperties)

-- | The time that the key phrases detection job completed.
keyPhrasesDetectionJobProperties_endTime :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
keyPhrasesDetectionJobProperties_endTime = Lens.lens (\KeyPhrasesDetectionJobProperties' {endTime} -> endTime) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {endTime = a} :: KeyPhrasesDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
keyPhrasesDetectionJobProperties_volumeKmsKeyId :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.Text)
keyPhrasesDetectionJobProperties_volumeKmsKeyId = Lens.lens (\KeyPhrasesDetectionJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {volumeKmsKeyId = a} :: KeyPhrasesDetectionJobProperties)

-- | The time that the key phrases detection job was submitted for
-- processing.
keyPhrasesDetectionJobProperties_submitTime :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
keyPhrasesDetectionJobProperties_submitTime = Lens.lens (\KeyPhrasesDetectionJobProperties' {submitTime} -> submitTime) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {submitTime = a} :: KeyPhrasesDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | The name that you assigned the key phrases detection job.
keyPhrasesDetectionJobProperties_jobName :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.Text)
keyPhrasesDetectionJobProperties_jobName = Lens.lens (\KeyPhrasesDetectionJobProperties' {jobName} -> jobName) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {jobName = a} :: KeyPhrasesDetectionJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
keyPhrasesDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.Text)
keyPhrasesDetectionJobProperties_dataAccessRoleArn = Lens.lens (\KeyPhrasesDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: KeyPhrasesDetectionJobProperties)

-- | The identifier assigned to the key phrases detection job.
keyPhrasesDetectionJobProperties_jobId :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.Text)
keyPhrasesDetectionJobProperties_jobId = Lens.lens (\KeyPhrasesDetectionJobProperties' {jobId} -> jobId) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {jobId = a} :: KeyPhrasesDetectionJobProperties)

instance
  Prelude.FromJSON
    KeyPhrasesDetectionJobProperties
  where
  parseJSON =
    Prelude.withObject
      "KeyPhrasesDetectionJobProperties"
      ( \x ->
          KeyPhrasesDetectionJobProperties'
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
    KeyPhrasesDetectionJobProperties

instance
  Prelude.NFData
    KeyPhrasesDetectionJobProperties
