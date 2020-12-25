{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    tdjpDataAccessRoleArn,
    tdjpEndTime,
    tdjpInputDataConfig,
    tdjpJobId,
    tdjpJobName,
    tdjpJobStatus,
    tdjpMessage,
    tdjpNumberOfTopics,
    tdjpOutputDataConfig,
    tdjpSubmitTime,
    tdjpVolumeKmsKeyId,
    tdjpVpcConfig,
  )
where

import qualified Network.AWS.Comprehend.Types.AnyLengthString as Types
import qualified Network.AWS.Comprehend.Types.IamRoleArn as Types
import qualified Network.AWS.Comprehend.Types.InputDataConfig as Types
import qualified Network.AWS.Comprehend.Types.JobId as Types
import qualified Network.AWS.Comprehend.Types.JobName as Types
import qualified Network.AWS.Comprehend.Types.JobStatus as Types
import qualified Network.AWS.Comprehend.Types.KmsKeyId as Types
import qualified Network.AWS.Comprehend.Types.OutputDataConfig as Types
import qualified Network.AWS.Comprehend.Types.VpcConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about a topic detection job.
--
-- /See:/ 'mkTopicsDetectionJobProperties' smart constructor.
data TopicsDetectionJobProperties = TopicsDetectionJobProperties'
  { -- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your job data.
    dataAccessRoleArn :: Core.Maybe Types.IamRoleArn,
    -- | The time that the topic detection job was completed.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The input data configuration supplied when you created the topic detection job.
    inputDataConfig :: Core.Maybe Types.InputDataConfig,
    -- | The identifier assigned to the topic detection job.
    jobId :: Core.Maybe Types.JobId,
    -- | The name of the topic detection job.
    jobName :: Core.Maybe Types.JobName,
    -- | The current status of the topic detection job. If the status is @Failed@ , the reason for the failure is shown in the @Message@ field.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | A description for the status of a job.
    message :: Core.Maybe Types.AnyLengthString,
    -- | The number of topics to detect supplied when you created the topic detection job. The default is 10.
    numberOfTopics :: Core.Maybe Core.Int,
    -- | The output data configuration supplied when you created the topic detection job.
    outputDataConfig :: Core.Maybe Types.OutputDataConfig,
    -- | The time that the topic detection job was submitted for processing.
    submitTime :: Core.Maybe Core.NominalDiffTime,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
    --
    --
    --     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    volumeKmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TopicsDetectionJobProperties' value with any optional fields omitted.
mkTopicsDetectionJobProperties ::
  TopicsDetectionJobProperties
mkTopicsDetectionJobProperties =
  TopicsDetectionJobProperties'
    { dataAccessRoleArn = Core.Nothing,
      endTime = Core.Nothing,
      inputDataConfig = Core.Nothing,
      jobId = Core.Nothing,
      jobName = Core.Nothing,
      jobStatus = Core.Nothing,
      message = Core.Nothing,
      numberOfTopics = Core.Nothing,
      outputDataConfig = Core.Nothing,
      submitTime = Core.Nothing,
      volumeKmsKeyId = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AWS Identity and Management (IAM) role that grants Amazon Comprehend read access to your job data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpDataAccessRoleArn :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Types.IamRoleArn)
tdjpDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED tdjpDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | The time that the topic detection job was completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpEndTime :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
tdjpEndTime = Lens.field @"endTime"
{-# DEPRECATED tdjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The input data configuration supplied when you created the topic detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpInputDataConfig :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Types.InputDataConfig)
tdjpInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED tdjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The identifier assigned to the topic detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpJobId :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Types.JobId)
tdjpJobId = Lens.field @"jobId"
{-# DEPRECATED tdjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name of the topic detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpJobName :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Types.JobName)
tdjpJobName = Lens.field @"jobName"
{-# DEPRECATED tdjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The current status of the topic detection job. If the status is @Failed@ , the reason for the failure is shown in the @Message@ field.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpJobStatus :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Types.JobStatus)
tdjpJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED tdjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description for the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpMessage :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Types.AnyLengthString)
tdjpMessage = Lens.field @"message"
{-# DEPRECATED tdjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The number of topics to detect supplied when you created the topic detection job. The default is 10.
--
-- /Note:/ Consider using 'numberOfTopics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpNumberOfTopics :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Core.Int)
tdjpNumberOfTopics = Lens.field @"numberOfTopics"
{-# DEPRECATED tdjpNumberOfTopics "Use generic-lens or generic-optics with 'numberOfTopics' instead." #-}

-- | The output data configuration supplied when you created the topic detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpOutputDataConfig :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Types.OutputDataConfig)
tdjpOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED tdjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The time that the topic detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpSubmitTime :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
tdjpSubmitTime = Lens.field @"submitTime"
{-# DEPRECATED tdjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

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
-- /Note:/ Consider using 'volumeKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpVolumeKmsKeyId :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Types.KmsKeyId)
tdjpVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# DEPRECATED tdjpVolumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your topic detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjpVpcConfig :: Lens.Lens' TopicsDetectionJobProperties (Core.Maybe Types.VpcConfig)
tdjpVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED tdjpVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON TopicsDetectionJobProperties where
  parseJSON =
    Core.withObject "TopicsDetectionJobProperties" Core.$
      \x ->
        TopicsDetectionJobProperties'
          Core.<$> (x Core..:? "DataAccessRoleArn")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "InputDataConfig")
          Core.<*> (x Core..:? "JobId")
          Core.<*> (x Core..:? "JobName")
          Core.<*> (x Core..:? "JobStatus")
          Core.<*> (x Core..:? "Message")
          Core.<*> (x Core..:? "NumberOfTopics")
          Core.<*> (x Core..:? "OutputDataConfig")
          Core.<*> (x Core..:? "SubmitTime")
          Core.<*> (x Core..:? "VolumeKmsKeyId")
          Core.<*> (x Core..:? "VpcConfig")
