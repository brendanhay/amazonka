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
-- Module      : Amazonka.Comprehend.Types.KeyPhrasesDetectionJobProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.KeyPhrasesDetectionJobProperties where

import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.LanguageCode
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a key phrases detection job.
--
-- /See:/ 'newKeyPhrasesDetectionJobProperties' smart constructor.
data KeyPhrasesDetectionJobProperties = KeyPhrasesDetectionJobProperties'
  { -- | The output data configuration that you supplied when you created the key
    -- phrases detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the key phrases detection job. If the status is
    -- @FAILED@, the @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your key phrases detection
    -- job. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The name that you assigned the key phrases detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The time that the key phrases detection job was submitted for
    -- processing.
    submitTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier assigned to the key phrases detection job.
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
    -- | The time that the key phrases detection job completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The Amazon Resource Name (ARN) of the key phrases detection job. It is a
    -- unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:key-phrases-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:key-phrases-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The input data configuration that you supplied when you created the key
    -- phrases detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyPhrasesDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputDataConfig', 'keyPhrasesDetectionJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the key
-- phrases detection job.
--
-- 'message', 'keyPhrasesDetectionJobProperties_message' - A description of the status of a job.
--
-- 'jobStatus', 'keyPhrasesDetectionJobProperties_jobStatus' - The current status of the key phrases detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
--
-- 'vpcConfig', 'keyPhrasesDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your key phrases detection
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'jobName', 'keyPhrasesDetectionJobProperties_jobName' - The name that you assigned the key phrases detection job.
--
-- 'submitTime', 'keyPhrasesDetectionJobProperties_submitTime' - The time that the key phrases detection job was submitted for
-- processing.
--
-- 'jobId', 'keyPhrasesDetectionJobProperties_jobId' - The identifier assigned to the key phrases detection job.
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
-- 'dataAccessRoleArn', 'keyPhrasesDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'endTime', 'keyPhrasesDetectionJobProperties_endTime' - The time that the key phrases detection job completed.
--
-- 'languageCode', 'keyPhrasesDetectionJobProperties_languageCode' - The language code of the input documents.
--
-- 'jobArn', 'keyPhrasesDetectionJobProperties_jobArn' - The Amazon Resource Name (ARN) of the key phrases detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:key-phrases-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:key-phrases-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'inputDataConfig', 'keyPhrasesDetectionJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the key
-- phrases detection job.
newKeyPhrasesDetectionJobProperties ::
  KeyPhrasesDetectionJobProperties
newKeyPhrasesDetectionJobProperties =
  KeyPhrasesDetectionJobProperties'
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
      languageCode = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing
    }

-- | The output data configuration that you supplied when you created the key
-- phrases detection job.
keyPhrasesDetectionJobProperties_outputDataConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe OutputDataConfig)
keyPhrasesDetectionJobProperties_outputDataConfig = Lens.lens (\KeyPhrasesDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {outputDataConfig = a} :: KeyPhrasesDetectionJobProperties)

-- | A description of the status of a job.
keyPhrasesDetectionJobProperties_message :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.Text)
keyPhrasesDetectionJobProperties_message = Lens.lens (\KeyPhrasesDetectionJobProperties' {message} -> message) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {message = a} :: KeyPhrasesDetectionJobProperties)

-- | The current status of the key phrases detection job. If the status is
-- @FAILED@, the @Message@ field shows the reason for the failure.
keyPhrasesDetectionJobProperties_jobStatus :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe JobStatus)
keyPhrasesDetectionJobProperties_jobStatus = Lens.lens (\KeyPhrasesDetectionJobProperties' {jobStatus} -> jobStatus) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {jobStatus = a} :: KeyPhrasesDetectionJobProperties)

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your key phrases detection
-- job. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
keyPhrasesDetectionJobProperties_vpcConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe VpcConfig)
keyPhrasesDetectionJobProperties_vpcConfig = Lens.lens (\KeyPhrasesDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {vpcConfig = a} :: KeyPhrasesDetectionJobProperties)

-- | The name that you assigned the key phrases detection job.
keyPhrasesDetectionJobProperties_jobName :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.Text)
keyPhrasesDetectionJobProperties_jobName = Lens.lens (\KeyPhrasesDetectionJobProperties' {jobName} -> jobName) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {jobName = a} :: KeyPhrasesDetectionJobProperties)

-- | The time that the key phrases detection job was submitted for
-- processing.
keyPhrasesDetectionJobProperties_submitTime :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
keyPhrasesDetectionJobProperties_submitTime = Lens.lens (\KeyPhrasesDetectionJobProperties' {submitTime} -> submitTime) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {submitTime = a} :: KeyPhrasesDetectionJobProperties) Prelude.. Lens.mapping Core._Time

-- | The identifier assigned to the key phrases detection job.
keyPhrasesDetectionJobProperties_jobId :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.Text)
keyPhrasesDetectionJobProperties_jobId = Lens.lens (\KeyPhrasesDetectionJobProperties' {jobId} -> jobId) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {jobId = a} :: KeyPhrasesDetectionJobProperties)

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

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
keyPhrasesDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.Text)
keyPhrasesDetectionJobProperties_dataAccessRoleArn = Lens.lens (\KeyPhrasesDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: KeyPhrasesDetectionJobProperties)

-- | The time that the key phrases detection job completed.
keyPhrasesDetectionJobProperties_endTime :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
keyPhrasesDetectionJobProperties_endTime = Lens.lens (\KeyPhrasesDetectionJobProperties' {endTime} -> endTime) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {endTime = a} :: KeyPhrasesDetectionJobProperties) Prelude.. Lens.mapping Core._Time

-- | The language code of the input documents.
keyPhrasesDetectionJobProperties_languageCode :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe LanguageCode)
keyPhrasesDetectionJobProperties_languageCode = Lens.lens (\KeyPhrasesDetectionJobProperties' {languageCode} -> languageCode) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {languageCode = a} :: KeyPhrasesDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the key phrases detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:key-phrases-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:key-phrases-detection-job\/1234abcd12ab34cd56ef1234567890ab@
keyPhrasesDetectionJobProperties_jobArn :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe Prelude.Text)
keyPhrasesDetectionJobProperties_jobArn = Lens.lens (\KeyPhrasesDetectionJobProperties' {jobArn} -> jobArn) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {jobArn = a} :: KeyPhrasesDetectionJobProperties)

-- | The input data configuration that you supplied when you created the key
-- phrases detection job.
keyPhrasesDetectionJobProperties_inputDataConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Prelude.Maybe InputDataConfig)
keyPhrasesDetectionJobProperties_inputDataConfig = Lens.lens (\KeyPhrasesDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@KeyPhrasesDetectionJobProperties' {} a -> s {inputDataConfig = a} :: KeyPhrasesDetectionJobProperties)

instance
  Core.FromJSON
    KeyPhrasesDetectionJobProperties
  where
  parseJSON =
    Core.withObject
      "KeyPhrasesDetectionJobProperties"
      ( \x ->
          KeyPhrasesDetectionJobProperties'
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
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "JobArn")
            Prelude.<*> (x Core..:? "InputDataConfig")
      )

instance
  Prelude.Hashable
    KeyPhrasesDetectionJobProperties
  where
  hashWithSalt
    _salt
    KeyPhrasesDetectionJobProperties' {..} =
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
        `Prelude.hashWithSalt` languageCode
        `Prelude.hashWithSalt` jobArn
        `Prelude.hashWithSalt` inputDataConfig

instance
  Prelude.NFData
    KeyPhrasesDetectionJobProperties
  where
  rnf KeyPhrasesDetectionJobProperties' {..} =
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
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf inputDataConfig
