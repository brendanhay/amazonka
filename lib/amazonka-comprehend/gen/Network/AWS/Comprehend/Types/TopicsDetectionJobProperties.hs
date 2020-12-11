-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.TopicsDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.TopicsDetectionJobProperties
  ( TopicsDetectionJobProperties (..),

    -- * Smart constructor
    mkTopicsDetectionJobProperties,

    -- * Lenses
    tdjpJobId,
    tdjpJobName,
    tdjpInputDataConfig,
    tdjpVPCConfig,
    tdjpVolumeKMSKeyId,
    tdjpEndTime,
    tdjpOutputDataConfig,
    tdjpDataAccessRoleARN,
    tdjpNumberOfTopics,
    tdjpJobStatus,
    tdjpMessage,
    tdjpSubmitTime,
  )
where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a topic detection job.
--
-- /See:/ 'mkTopicsDetectionJobProperties' smart constructor.
data TopicsDetectionJobProperties = TopicsDetectionJobProperties'
  { jobId ::
      Lude.Maybe Lude.Text,
    jobName :: Lude.Maybe Lude.Text,
    inputDataConfig ::
      Lude.Maybe InputDataConfig,
    vpcConfig :: Lude.Maybe VPCConfig,
    volumeKMSKeyId ::
      Lude.Maybe Lude.Text,
    endTime ::
      Lude.Maybe Lude.Timestamp,
    outputDataConfig ::
      Lude.Maybe OutputDataConfig,
    dataAccessRoleARN ::
      Lude.Maybe Lude.Text,
    numberOfTopics ::
      Lude.Maybe Lude.Int,
    jobStatus :: Lude.Maybe JobStatus,
    message :: Lude.Maybe Lude.Text,
    submitTime ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TopicsDetectionJobProperties' with the minimum fields required to make a request.
--
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your job data.
-- * 'endTime' - The time that the topic detection job was completed.
-- * 'inputDataConfig' - The input data configuration supplied when you created the topic detection job.
-- * 'jobId' - The identifier assigned to the topic detection job.
-- * 'jobName' - The name of the topic detection job.
-- * 'jobStatus' - The current status of the topic detection job. If the status is @Failed@ , the reason for the failure is shown in the @Message@ field.
-- * 'message' - A description for the status of a job.
-- * 'numberOfTopics' - The number of topics to detect supplied when you created the topic detection job. The default is 10.
-- * 'outputDataConfig' - The output data configuration supplied when you created the topic detection job.
-- * 'submitTime' - The time that the topic detection job was submitted for processing.
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkTopicsDetectionJobProperties ::
  TopicsDetectionJobProperties
mkTopicsDetectionJobProperties =
  TopicsDetectionJobProperties'
    { jobId = Lude.Nothing,
      jobName = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      volumeKMSKeyId = Lude.Nothing,
      endTime = Lude.Nothing,
      outputDataConfig = Lude.Nothing,
      dataAccessRoleARN = Lude.Nothing,
      numberOfTopics = Lude.Nothing,
      jobStatus = Lude.Nothing,
      message = Lude.Nothing,
      submitTime = Lude.Nothing
    }

-- | The identifier assigned to the topic detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpJobId :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe Lude.Text)
tdjpJobId = Lens.lens (jobId :: TopicsDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name of the topic detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpJobName :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe Lude.Text)
tdjpJobName = Lens.lens (jobName :: TopicsDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The input data configuration supplied when you created the topic detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpInputDataConfig :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe InputDataConfig)
tdjpInputDataConfig = Lens.lens (inputDataConfig :: TopicsDetectionJobProperties -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpVPCConfig :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe VPCConfig)
tdjpVPCConfig = Lens.lens (vpcConfig :: TopicsDetectionJobProperties -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--
-- /Note:/ Consider using 'volumeKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpVolumeKMSKeyId :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe Lude.Text)
tdjpVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: TopicsDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The time that the topic detection job was completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpEndTime :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe Lude.Timestamp)
tdjpEndTime = Lens.lens (endTime :: TopicsDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The output data configuration supplied when you created the topic detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpOutputDataConfig :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe OutputDataConfig)
tdjpOutputDataConfig = Lens.lens (outputDataConfig :: TopicsDetectionJobProperties -> Lude.Maybe OutputDataConfig) (\s a -> s {outputDataConfig = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your job data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpDataAccessRoleARN :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe Lude.Text)
tdjpDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: TopicsDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The number of topics to detect supplied when you created the topic detection job. The default is 10.
--
-- /Note:/ Consider using 'numberOfTopics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpNumberOfTopics :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe Lude.Int)
tdjpNumberOfTopics = Lens.lens (numberOfTopics :: TopicsDetectionJobProperties -> Lude.Maybe Lude.Int) (\s a -> s {numberOfTopics = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpNumberOfTopics "Use generic-lens or generic-optics with 'numberOfTopics' instead." #-}

-- | The current status of the topic detection job. If the status is @Failed@ , the reason for the failure is shown in the @Message@ field.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpJobStatus :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe JobStatus)
tdjpJobStatus = Lens.lens (jobStatus :: TopicsDetectionJobProperties -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description for the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpMessage :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe Lude.Text)
tdjpMessage = Lens.lens (message :: TopicsDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time that the topic detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpSubmitTime :: Lens.Lens' TopicsDetectionJobProperties (Lude.Maybe Lude.Timestamp)
tdjpSubmitTime = Lens.lens (submitTime :: TopicsDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: TopicsDetectionJobProperties)
{-# DEPRECATED tdjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON TopicsDetectionJobProperties where
  parseJSON =
    Lude.withObject
      "TopicsDetectionJobProperties"
      ( \x ->
          TopicsDetectionJobProperties'
            Lude.<$> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "VolumeKmsKeyId")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "OutputDataConfig")
            Lude.<*> (x Lude..:? "DataAccessRoleArn")
            Lude.<*> (x Lude..:? "NumberOfTopics")
            Lude.<*> (x Lude..:? "JobStatus")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "SubmitTime")
      )
