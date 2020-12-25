{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties
  ( DominantLanguageDetectionJobProperties (..),

    -- * Smart constructor
    mkDominantLanguageDetectionJobProperties,

    -- * Lenses
    dldjpDataAccessRoleArn,
    dldjpEndTime,
    dldjpInputDataConfig,
    dldjpJobId,
    dldjpJobName,
    dldjpJobStatus,
    dldjpMessage,
    dldjpOutputDataConfig,
    dldjpSubmitTime,
    dldjpVolumeKmsKeyId,
    dldjpVpcConfig,
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

-- | Provides information about a dominant language detection job.
--
-- /See:/ 'mkDominantLanguageDetectionJobProperties' smart constructor.
data DominantLanguageDetectionJobProperties = DominantLanguageDetectionJobProperties'
  { -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Maybe Types.IamRoleArn,
    -- | The time that the dominant language detection job completed.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The input data configuration that you supplied when you created the dominant language detection job.
    inputDataConfig :: Core.Maybe Types.InputDataConfig,
    -- | The identifier assigned to the dominant language detection job.
    jobId :: Core.Maybe Types.JobId,
    -- | The name that you assigned to the dominant language detection job.
    jobName :: Core.Maybe Types.JobName,
    -- | The current status of the dominant language detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | A description for the status of a job.
    message :: Core.Maybe Types.AnyLengthString,
    -- | The output data configuration that you supplied when you created the dominant language detection job.
    outputDataConfig :: Core.Maybe Types.OutputDataConfig,
    -- | The time that the dominant language detection job was submitted for processing.
    submitTime :: Core.Maybe Core.NominalDiffTime,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
    --
    --
    --     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    volumeKmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your dominant language detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DominantLanguageDetectionJobProperties' value with any optional fields omitted.
mkDominantLanguageDetectionJobProperties ::
  DominantLanguageDetectionJobProperties
mkDominantLanguageDetectionJobProperties =
  DominantLanguageDetectionJobProperties'
    { dataAccessRoleArn =
        Core.Nothing,
      endTime = Core.Nothing,
      inputDataConfig = Core.Nothing,
      jobId = Core.Nothing,
      jobName = Core.Nothing,
      jobStatus = Core.Nothing,
      message = Core.Nothing,
      outputDataConfig = Core.Nothing,
      submitTime = Core.Nothing,
      volumeKmsKeyId = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpDataAccessRoleArn :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Types.IamRoleArn)
dldjpDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED dldjpDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | The time that the dominant language detection job completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpEndTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
dldjpEndTime = Lens.field @"endTime"
{-# DEPRECATED dldjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The input data configuration that you supplied when you created the dominant language detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpInputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Types.InputDataConfig)
dldjpInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED dldjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The identifier assigned to the dominant language detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpJobId :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Types.JobId)
dldjpJobId = Lens.field @"jobId"
{-# DEPRECATED dldjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name that you assigned to the dominant language detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpJobName :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Types.JobName)
dldjpJobName = Lens.field @"jobName"
{-# DEPRECATED dldjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The current status of the dominant language detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpJobStatus :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Types.JobStatus)
dldjpJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED dldjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description for the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpMessage :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Types.AnyLengthString)
dldjpMessage = Lens.field @"message"
{-# DEPRECATED dldjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The output data configuration that you supplied when you created the dominant language detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpOutputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Types.OutputDataConfig)
dldjpOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED dldjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The time that the dominant language detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpSubmitTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
dldjpSubmitTime = Lens.field @"submitTime"
{-# DEPRECATED dldjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

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
dldjpVolumeKmsKeyId :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Types.KmsKeyId)
dldjpVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# DEPRECATED dldjpVolumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your dominant language detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpVpcConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Core.Maybe Types.VpcConfig)
dldjpVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED dldjpVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON DominantLanguageDetectionJobProperties where
  parseJSON =
    Core.withObject "DominantLanguageDetectionJobProperties" Core.$
      \x ->
        DominantLanguageDetectionJobProperties'
          Core.<$> (x Core..:? "DataAccessRoleArn")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "InputDataConfig")
          Core.<*> (x Core..:? "JobId")
          Core.<*> (x Core..:? "JobName")
          Core.<*> (x Core..:? "JobStatus")
          Core.<*> (x Core..:? "Message")
          Core.<*> (x Core..:? "OutputDataConfig")
          Core.<*> (x Core..:? "SubmitTime")
          Core.<*> (x Core..:? "VolumeKmsKeyId")
          Core.<*> (x Core..:? "VpcConfig")
