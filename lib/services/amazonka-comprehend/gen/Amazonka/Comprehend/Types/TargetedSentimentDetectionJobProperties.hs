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
-- Module      : Amazonka.Comprehend.Types.TargetedSentimentDetectionJobProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.TargetedSentimentDetectionJobProperties where

import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.LanguageCode
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a targeted sentiment detection job.
--
-- /See:/ 'newTargetedSentimentDetectionJobProperties' smart constructor.
data TargetedSentimentDetectionJobProperties = TargetedSentimentDetectionJobProperties'
  { -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
    -- to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the targeted sentiment detection job ended.
    endTime :: Prelude.Maybe Data.POSIX,
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The Amazon Resource Name (ARN) of the targeted sentiment detection job.
    -- It is a unique, fully qualified identifier for the job. It includes the
    -- AWS account, Region, and the job ID. The format of the ARN is as
    -- follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:targeted-sentiment-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:targeted-sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the targeted sentiment detection job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned to the targeted sentiment detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the targeted sentiment detection job. If the
    -- status is @FAILED@, the @Messages@ field shows the reason for the
    -- failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the targeted sentiment detection job was submitted for
    -- processing.
    submitTime :: Prelude.Maybe Data.POSIX,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that process the targeted sentiment detection job. The
    -- VolumeKmsKeyId can be either of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetedSentimentDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAccessRoleArn', 'targetedSentimentDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access
-- to your input data.
--
-- 'endTime', 'targetedSentimentDetectionJobProperties_endTime' - The time that the targeted sentiment detection job ended.
--
-- 'inputDataConfig', 'targetedSentimentDetectionJobProperties_inputDataConfig' - Undocumented member.
--
-- 'jobArn', 'targetedSentimentDetectionJobProperties_jobArn' - The Amazon Resource Name (ARN) of the targeted sentiment detection job.
-- It is a unique, fully qualified identifier for the job. It includes the
-- AWS account, Region, and the job ID. The format of the ARN is as
-- follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:targeted-sentiment-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:targeted-sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'jobId', 'targetedSentimentDetectionJobProperties_jobId' - The identifier assigned to the targeted sentiment detection job.
--
-- 'jobName', 'targetedSentimentDetectionJobProperties_jobName' - The name that you assigned to the targeted sentiment detection job.
--
-- 'jobStatus', 'targetedSentimentDetectionJobProperties_jobStatus' - The current status of the targeted sentiment detection job. If the
-- status is @FAILED@, the @Messages@ field shows the reason for the
-- failure.
--
-- 'languageCode', 'targetedSentimentDetectionJobProperties_languageCode' - The language code of the input documents.
--
-- 'message', 'targetedSentimentDetectionJobProperties_message' - A description of the status of a job.
--
-- 'outputDataConfig', 'targetedSentimentDetectionJobProperties_outputDataConfig' - Undocumented member.
--
-- 'submitTime', 'targetedSentimentDetectionJobProperties_submitTime' - The time that the targeted sentiment detection job was submitted for
-- processing.
--
-- 'volumeKmsKeyId', 'targetedSentimentDetectionJobProperties_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the targeted sentiment detection job. The
-- VolumeKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'vpcConfig', 'targetedSentimentDetectionJobProperties_vpcConfig' - Undocumented member.
newTargetedSentimentDetectionJobProperties ::
  TargetedSentimentDetectionJobProperties
newTargetedSentimentDetectionJobProperties =
  TargetedSentimentDetectionJobProperties'
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
targetedSentimentDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
targetedSentimentDetectionJobProperties_dataAccessRoleArn = Lens.lens (\TargetedSentimentDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: TargetedSentimentDetectionJobProperties)

-- | The time that the targeted sentiment detection job ended.
targetedSentimentDetectionJobProperties_endTime :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
targetedSentimentDetectionJobProperties_endTime = Lens.lens (\TargetedSentimentDetectionJobProperties' {endTime} -> endTime) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {endTime = a} :: TargetedSentimentDetectionJobProperties) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
targetedSentimentDetectionJobProperties_inputDataConfig :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe InputDataConfig)
targetedSentimentDetectionJobProperties_inputDataConfig = Lens.lens (\TargetedSentimentDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {inputDataConfig = a} :: TargetedSentimentDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the targeted sentiment detection job.
-- It is a unique, fully qualified identifier for the job. It includes the
-- AWS account, Region, and the job ID. The format of the ARN is as
-- follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:targeted-sentiment-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:targeted-sentiment-detection-job\/1234abcd12ab34cd56ef1234567890ab@
targetedSentimentDetectionJobProperties_jobArn :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
targetedSentimentDetectionJobProperties_jobArn = Lens.lens (\TargetedSentimentDetectionJobProperties' {jobArn} -> jobArn) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {jobArn = a} :: TargetedSentimentDetectionJobProperties)

-- | The identifier assigned to the targeted sentiment detection job.
targetedSentimentDetectionJobProperties_jobId :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
targetedSentimentDetectionJobProperties_jobId = Lens.lens (\TargetedSentimentDetectionJobProperties' {jobId} -> jobId) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {jobId = a} :: TargetedSentimentDetectionJobProperties)

-- | The name that you assigned to the targeted sentiment detection job.
targetedSentimentDetectionJobProperties_jobName :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
targetedSentimentDetectionJobProperties_jobName = Lens.lens (\TargetedSentimentDetectionJobProperties' {jobName} -> jobName) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {jobName = a} :: TargetedSentimentDetectionJobProperties)

-- | The current status of the targeted sentiment detection job. If the
-- status is @FAILED@, the @Messages@ field shows the reason for the
-- failure.
targetedSentimentDetectionJobProperties_jobStatus :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe JobStatus)
targetedSentimentDetectionJobProperties_jobStatus = Lens.lens (\TargetedSentimentDetectionJobProperties' {jobStatus} -> jobStatus) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {jobStatus = a} :: TargetedSentimentDetectionJobProperties)

-- | The language code of the input documents.
targetedSentimentDetectionJobProperties_languageCode :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe LanguageCode)
targetedSentimentDetectionJobProperties_languageCode = Lens.lens (\TargetedSentimentDetectionJobProperties' {languageCode} -> languageCode) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {languageCode = a} :: TargetedSentimentDetectionJobProperties)

-- | A description of the status of a job.
targetedSentimentDetectionJobProperties_message :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
targetedSentimentDetectionJobProperties_message = Lens.lens (\TargetedSentimentDetectionJobProperties' {message} -> message) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {message = a} :: TargetedSentimentDetectionJobProperties)

-- | Undocumented member.
targetedSentimentDetectionJobProperties_outputDataConfig :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe OutputDataConfig)
targetedSentimentDetectionJobProperties_outputDataConfig = Lens.lens (\TargetedSentimentDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {outputDataConfig = a} :: TargetedSentimentDetectionJobProperties)

-- | The time that the targeted sentiment detection job was submitted for
-- processing.
targetedSentimentDetectionJobProperties_submitTime :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
targetedSentimentDetectionJobProperties_submitTime = Lens.lens (\TargetedSentimentDetectionJobProperties' {submitTime} -> submitTime) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {submitTime = a} :: TargetedSentimentDetectionJobProperties) Prelude.. Lens.mapping Data._Time

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the targeted sentiment detection job. The
-- VolumeKmsKeyId can be either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
targetedSentimentDetectionJobProperties_volumeKmsKeyId :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe Prelude.Text)
targetedSentimentDetectionJobProperties_volumeKmsKeyId = Lens.lens (\TargetedSentimentDetectionJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {volumeKmsKeyId = a} :: TargetedSentimentDetectionJobProperties)

-- | Undocumented member.
targetedSentimentDetectionJobProperties_vpcConfig :: Lens.Lens' TargetedSentimentDetectionJobProperties (Prelude.Maybe VpcConfig)
targetedSentimentDetectionJobProperties_vpcConfig = Lens.lens (\TargetedSentimentDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@TargetedSentimentDetectionJobProperties' {} a -> s {vpcConfig = a} :: TargetedSentimentDetectionJobProperties)

instance
  Data.FromJSON
    TargetedSentimentDetectionJobProperties
  where
  parseJSON =
    Data.withObject
      "TargetedSentimentDetectionJobProperties"
      ( \x ->
          TargetedSentimentDetectionJobProperties'
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
    TargetedSentimentDetectionJobProperties
  where
  hashWithSalt
    _salt
    TargetedSentimentDetectionJobProperties' {..} =
      _salt `Prelude.hashWithSalt` dataAccessRoleArn
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
    TargetedSentimentDetectionJobProperties
  where
  rnf TargetedSentimentDetectionJobProperties' {..} =
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
