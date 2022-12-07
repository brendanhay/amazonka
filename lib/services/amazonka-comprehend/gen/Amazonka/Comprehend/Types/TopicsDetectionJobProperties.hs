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
-- Module      : Amazonka.Comprehend.Types.TopicsDetectionJobProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.TopicsDetectionJobProperties where

import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a topic detection job.
--
-- /See:/ 'newTopicsDetectionJobProperties' smart constructor.
data TopicsDetectionJobProperties = TopicsDetectionJobProperties'
  { -- | The output data configuration supplied when you created the topic
    -- detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | A description for the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the topic detection job. If the status is
    -- @Failed@, the reason for the failure is shown in the @Message@ field.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your topic detection job. For
    -- more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The name of the topic detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The number of topics to detect supplied when you created the topic
    -- detection job. The default is 10.
    numberOfTopics :: Prelude.Maybe Prelude.Int,
    -- | The time that the topic detection job was submitted for processing.
    submitTime :: Prelude.Maybe Data.POSIX,
    -- | The identifier assigned to the topic detection job.
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
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
    -- role that grants Amazon Comprehend read access to your job data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the topic detection job was completed.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the topics detection job. It is a
    -- unique, fully qualified identifier for the job. It includes the AWS
    -- account, Region, and the job ID. The format of the ARN is as follows:
    --
    -- @arn:\<partition>:comprehend:\<region>:\<account-id>:topics-detection-job\/\<job-id>@
    --
    -- The following is an example job ARN:
    --
    -- @arn:aws:comprehend:us-west-2:111122223333:topics-detection-job\/1234abcd12ab34cd56ef1234567890ab@
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The input data configuration supplied when you created the topic
    -- detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicsDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputDataConfig', 'topicsDetectionJobProperties_outputDataConfig' - The output data configuration supplied when you created the topic
-- detection job.
--
-- 'message', 'topicsDetectionJobProperties_message' - A description for the status of a job.
--
-- 'jobStatus', 'topicsDetectionJobProperties_jobStatus' - The current status of the topic detection job. If the status is
-- @Failed@, the reason for the failure is shown in the @Message@ field.
--
-- 'vpcConfig', 'topicsDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your topic detection job. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'jobName', 'topicsDetectionJobProperties_jobName' - The name of the topic detection job.
--
-- 'numberOfTopics', 'topicsDetectionJobProperties_numberOfTopics' - The number of topics to detect supplied when you created the topic
-- detection job. The default is 10.
--
-- 'submitTime', 'topicsDetectionJobProperties_submitTime' - The time that the topic detection job was submitted for processing.
--
-- 'jobId', 'topicsDetectionJobProperties_jobId' - The identifier assigned to the topic detection job.
--
-- 'volumeKmsKeyId', 'topicsDetectionJobProperties_volumeKmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'dataAccessRoleArn', 'topicsDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your job data.
--
-- 'endTime', 'topicsDetectionJobProperties_endTime' - The time that the topic detection job was completed.
--
-- 'jobArn', 'topicsDetectionJobProperties_jobArn' - The Amazon Resource Name (ARN) of the topics detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:topics-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:topics-detection-job\/1234abcd12ab34cd56ef1234567890ab@
--
-- 'inputDataConfig', 'topicsDetectionJobProperties_inputDataConfig' - The input data configuration supplied when you created the topic
-- detection job.
newTopicsDetectionJobProperties ::
  TopicsDetectionJobProperties
newTopicsDetectionJobProperties =
  TopicsDetectionJobProperties'
    { outputDataConfig =
        Prelude.Nothing,
      message = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      jobName = Prelude.Nothing,
      numberOfTopics = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      jobId = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      endTime = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing
    }

-- | The output data configuration supplied when you created the topic
-- detection job.
topicsDetectionJobProperties_outputDataConfig :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe OutputDataConfig)
topicsDetectionJobProperties_outputDataConfig = Lens.lens (\TopicsDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@TopicsDetectionJobProperties' {} a -> s {outputDataConfig = a} :: TopicsDetectionJobProperties)

-- | A description for the status of a job.
topicsDetectionJobProperties_message :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Text)
topicsDetectionJobProperties_message = Lens.lens (\TopicsDetectionJobProperties' {message} -> message) (\s@TopicsDetectionJobProperties' {} a -> s {message = a} :: TopicsDetectionJobProperties)

-- | The current status of the topic detection job. If the status is
-- @Failed@, the reason for the failure is shown in the @Message@ field.
topicsDetectionJobProperties_jobStatus :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe JobStatus)
topicsDetectionJobProperties_jobStatus = Lens.lens (\TopicsDetectionJobProperties' {jobStatus} -> jobStatus) (\s@TopicsDetectionJobProperties' {} a -> s {jobStatus = a} :: TopicsDetectionJobProperties)

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your topic detection job. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
topicsDetectionJobProperties_vpcConfig :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe VpcConfig)
topicsDetectionJobProperties_vpcConfig = Lens.lens (\TopicsDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@TopicsDetectionJobProperties' {} a -> s {vpcConfig = a} :: TopicsDetectionJobProperties)

-- | The name of the topic detection job.
topicsDetectionJobProperties_jobName :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Text)
topicsDetectionJobProperties_jobName = Lens.lens (\TopicsDetectionJobProperties' {jobName} -> jobName) (\s@TopicsDetectionJobProperties' {} a -> s {jobName = a} :: TopicsDetectionJobProperties)

-- | The number of topics to detect supplied when you created the topic
-- detection job. The default is 10.
topicsDetectionJobProperties_numberOfTopics :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Int)
topicsDetectionJobProperties_numberOfTopics = Lens.lens (\TopicsDetectionJobProperties' {numberOfTopics} -> numberOfTopics) (\s@TopicsDetectionJobProperties' {} a -> s {numberOfTopics = a} :: TopicsDetectionJobProperties)

-- | The time that the topic detection job was submitted for processing.
topicsDetectionJobProperties_submitTime :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
topicsDetectionJobProperties_submitTime = Lens.lens (\TopicsDetectionJobProperties' {submitTime} -> submitTime) (\s@TopicsDetectionJobProperties' {} a -> s {submitTime = a} :: TopicsDetectionJobProperties) Prelude.. Lens.mapping Data._Time

-- | The identifier assigned to the topic detection job.
topicsDetectionJobProperties_jobId :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Text)
topicsDetectionJobProperties_jobId = Lens.lens (\TopicsDetectionJobProperties' {jobId} -> jobId) (\s@TopicsDetectionJobProperties' {} a -> s {jobId = a} :: TopicsDetectionJobProperties)

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt data on the storage volume attached to the ML compute
-- instance(s) that process the analysis job. The VolumeKmsKeyId can be
-- either of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
topicsDetectionJobProperties_volumeKmsKeyId :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Text)
topicsDetectionJobProperties_volumeKmsKeyId = Lens.lens (\TopicsDetectionJobProperties' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@TopicsDetectionJobProperties' {} a -> s {volumeKmsKeyId = a} :: TopicsDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your job data.
topicsDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Text)
topicsDetectionJobProperties_dataAccessRoleArn = Lens.lens (\TopicsDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@TopicsDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: TopicsDetectionJobProperties)

-- | The time that the topic detection job was completed.
topicsDetectionJobProperties_endTime :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
topicsDetectionJobProperties_endTime = Lens.lens (\TopicsDetectionJobProperties' {endTime} -> endTime) (\s@TopicsDetectionJobProperties' {} a -> s {endTime = a} :: TopicsDetectionJobProperties) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the topics detection job. It is a
-- unique, fully qualified identifier for the job. It includes the AWS
-- account, Region, and the job ID. The format of the ARN is as follows:
--
-- @arn:\<partition>:comprehend:\<region>:\<account-id>:topics-detection-job\/\<job-id>@
--
-- The following is an example job ARN:
--
-- @arn:aws:comprehend:us-west-2:111122223333:topics-detection-job\/1234abcd12ab34cd56ef1234567890ab@
topicsDetectionJobProperties_jobArn :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Text)
topicsDetectionJobProperties_jobArn = Lens.lens (\TopicsDetectionJobProperties' {jobArn} -> jobArn) (\s@TopicsDetectionJobProperties' {} a -> s {jobArn = a} :: TopicsDetectionJobProperties)

-- | The input data configuration supplied when you created the topic
-- detection job.
topicsDetectionJobProperties_inputDataConfig :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe InputDataConfig)
topicsDetectionJobProperties_inputDataConfig = Lens.lens (\TopicsDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@TopicsDetectionJobProperties' {} a -> s {inputDataConfig = a} :: TopicsDetectionJobProperties)

instance Data.FromJSON TopicsDetectionJobProperties where
  parseJSON =
    Data.withObject
      "TopicsDetectionJobProperties"
      ( \x ->
          TopicsDetectionJobProperties'
            Prelude.<$> (x Data..:? "OutputDataConfig")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "JobStatus")
            Prelude.<*> (x Data..:? "VpcConfig")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "NumberOfTopics")
            Prelude.<*> (x Data..:? "SubmitTime")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "DataAccessRoleArn")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "JobArn")
            Prelude.<*> (x Data..:? "InputDataConfig")
      )

instance
  Prelude.Hashable
    TopicsDetectionJobProperties
  where
  hashWithSalt _salt TopicsDetectionJobProperties' {..} =
    _salt `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` numberOfTopics
      `Prelude.hashWithSalt` submitTime
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` jobArn
      `Prelude.hashWithSalt` inputDataConfig

instance Prelude.NFData TopicsDetectionJobProperties where
  rnf TopicsDetectionJobProperties' {..} =
    Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf numberOfTopics
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf inputDataConfig
