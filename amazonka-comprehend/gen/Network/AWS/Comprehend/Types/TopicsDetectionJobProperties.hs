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
-- Module      : Network.AWS.Comprehend.Types.TopicsDetectionJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.TopicsDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a topic detection job.
--
-- /See:/ 'newTopicsDetectionJobProperties' smart constructor.
data TopicsDetectionJobProperties = TopicsDetectionJobProperties'
  { -- | Configuration parameters for a private Virtual Private Cloud (VPC)
    -- containing the resources you are using for your topic detection job. For
    -- more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The input data configuration supplied when you created the topic
    -- detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | A description for the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The current status of the topic detection job. If the status is
    -- @Failed@, the reason for the failure is shown in the @Message@ field.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The output data configuration supplied when you created the topic
    -- detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The time that the topic detection job was completed.
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
    -- | The time that the topic detection job was submitted for processing.
    submitTime :: Prelude.Maybe Prelude.POSIX,
    -- | The number of topics to detect supplied when you created the topic
    -- detection job. The default is 10.
    numberOfTopics :: Prelude.Maybe Prelude.Int,
    -- | The name of the topic detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
    -- role that grants Amazon Comprehend read access to your job data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the topic detection job.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TopicsDetectionJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'topicsDetectionJobProperties_vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your topic detection job. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- 'inputDataConfig', 'topicsDetectionJobProperties_inputDataConfig' - The input data configuration supplied when you created the topic
-- detection job.
--
-- 'message', 'topicsDetectionJobProperties_message' - A description for the status of a job.
--
-- 'jobStatus', 'topicsDetectionJobProperties_jobStatus' - The current status of the topic detection job. If the status is
-- @Failed@, the reason for the failure is shown in the @Message@ field.
--
-- 'outputDataConfig', 'topicsDetectionJobProperties_outputDataConfig' - The output data configuration supplied when you created the topic
-- detection job.
--
-- 'endTime', 'topicsDetectionJobProperties_endTime' - The time that the topic detection job was completed.
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
-- 'submitTime', 'topicsDetectionJobProperties_submitTime' - The time that the topic detection job was submitted for processing.
--
-- 'numberOfTopics', 'topicsDetectionJobProperties_numberOfTopics' - The number of topics to detect supplied when you created the topic
-- detection job. The default is 10.
--
-- 'jobName', 'topicsDetectionJobProperties_jobName' - The name of the topic detection job.
--
-- 'dataAccessRoleArn', 'topicsDetectionJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your job data.
--
-- 'jobId', 'topicsDetectionJobProperties_jobId' - The identifier assigned to the topic detection job.
newTopicsDetectionJobProperties ::
  TopicsDetectionJobProperties
newTopicsDetectionJobProperties =
  TopicsDetectionJobProperties'
    { vpcConfig =
        Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      message = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      endTime = Prelude.Nothing,
      volumeKmsKeyId = Prelude.Nothing,
      submitTime = Prelude.Nothing,
      numberOfTopics = Prelude.Nothing,
      jobName = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | Configuration parameters for a private Virtual Private Cloud (VPC)
-- containing the resources you are using for your topic detection job. For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
topicsDetectionJobProperties_vpcConfig :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe VpcConfig)
topicsDetectionJobProperties_vpcConfig = Lens.lens (\TopicsDetectionJobProperties' {vpcConfig} -> vpcConfig) (\s@TopicsDetectionJobProperties' {} a -> s {vpcConfig = a} :: TopicsDetectionJobProperties)

-- | The input data configuration supplied when you created the topic
-- detection job.
topicsDetectionJobProperties_inputDataConfig :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe InputDataConfig)
topicsDetectionJobProperties_inputDataConfig = Lens.lens (\TopicsDetectionJobProperties' {inputDataConfig} -> inputDataConfig) (\s@TopicsDetectionJobProperties' {} a -> s {inputDataConfig = a} :: TopicsDetectionJobProperties)

-- | A description for the status of a job.
topicsDetectionJobProperties_message :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Text)
topicsDetectionJobProperties_message = Lens.lens (\TopicsDetectionJobProperties' {message} -> message) (\s@TopicsDetectionJobProperties' {} a -> s {message = a} :: TopicsDetectionJobProperties)

-- | The current status of the topic detection job. If the status is
-- @Failed@, the reason for the failure is shown in the @Message@ field.
topicsDetectionJobProperties_jobStatus :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe JobStatus)
topicsDetectionJobProperties_jobStatus = Lens.lens (\TopicsDetectionJobProperties' {jobStatus} -> jobStatus) (\s@TopicsDetectionJobProperties' {} a -> s {jobStatus = a} :: TopicsDetectionJobProperties)

-- | The output data configuration supplied when you created the topic
-- detection job.
topicsDetectionJobProperties_outputDataConfig :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe OutputDataConfig)
topicsDetectionJobProperties_outputDataConfig = Lens.lens (\TopicsDetectionJobProperties' {outputDataConfig} -> outputDataConfig) (\s@TopicsDetectionJobProperties' {} a -> s {outputDataConfig = a} :: TopicsDetectionJobProperties)

-- | The time that the topic detection job was completed.
topicsDetectionJobProperties_endTime :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
topicsDetectionJobProperties_endTime = Lens.lens (\TopicsDetectionJobProperties' {endTime} -> endTime) (\s@TopicsDetectionJobProperties' {} a -> s {endTime = a} :: TopicsDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

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

-- | The time that the topic detection job was submitted for processing.
topicsDetectionJobProperties_submitTime :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.UTCTime)
topicsDetectionJobProperties_submitTime = Lens.lens (\TopicsDetectionJobProperties' {submitTime} -> submitTime) (\s@TopicsDetectionJobProperties' {} a -> s {submitTime = a} :: TopicsDetectionJobProperties) Prelude.. Lens.mapping Prelude._Time

-- | The number of topics to detect supplied when you created the topic
-- detection job. The default is 10.
topicsDetectionJobProperties_numberOfTopics :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Int)
topicsDetectionJobProperties_numberOfTopics = Lens.lens (\TopicsDetectionJobProperties' {numberOfTopics} -> numberOfTopics) (\s@TopicsDetectionJobProperties' {} a -> s {numberOfTopics = a} :: TopicsDetectionJobProperties)

-- | The name of the topic detection job.
topicsDetectionJobProperties_jobName :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Text)
topicsDetectionJobProperties_jobName = Lens.lens (\TopicsDetectionJobProperties' {jobName} -> jobName) (\s@TopicsDetectionJobProperties' {} a -> s {jobName = a} :: TopicsDetectionJobProperties)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM)
-- role that grants Amazon Comprehend read access to your job data.
topicsDetectionJobProperties_dataAccessRoleArn :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Text)
topicsDetectionJobProperties_dataAccessRoleArn = Lens.lens (\TopicsDetectionJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@TopicsDetectionJobProperties' {} a -> s {dataAccessRoleArn = a} :: TopicsDetectionJobProperties)

-- | The identifier assigned to the topic detection job.
topicsDetectionJobProperties_jobId :: Lens.Lens' TopicsDetectionJobProperties (Prelude.Maybe Prelude.Text)
topicsDetectionJobProperties_jobId = Lens.lens (\TopicsDetectionJobProperties' {jobId} -> jobId) (\s@TopicsDetectionJobProperties' {} a -> s {jobId = a} :: TopicsDetectionJobProperties)

instance
  Prelude.FromJSON
    TopicsDetectionJobProperties
  where
  parseJSON =
    Prelude.withObject
      "TopicsDetectionJobProperties"
      ( \x ->
          TopicsDetectionJobProperties'
            Prelude.<$> (x Prelude..:? "VpcConfig")
            Prelude.<*> (x Prelude..:? "InputDataConfig")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "JobStatus")
            Prelude.<*> (x Prelude..:? "OutputDataConfig")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "VolumeKmsKeyId")
            Prelude.<*> (x Prelude..:? "SubmitTime")
            Prelude.<*> (x Prelude..:? "NumberOfTopics")
            Prelude.<*> (x Prelude..:? "JobName")
            Prelude.<*> (x Prelude..:? "DataAccessRoleArn")
            Prelude.<*> (x Prelude..:? "JobId")
      )

instance
  Prelude.Hashable
    TopicsDetectionJobProperties

instance Prelude.NFData TopicsDetectionJobProperties
