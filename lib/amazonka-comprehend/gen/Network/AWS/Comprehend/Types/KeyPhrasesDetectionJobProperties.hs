{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties
  ( KeyPhrasesDetectionJobProperties (..),

    -- * Smart constructor
    mkKeyPhrasesDetectionJobProperties,

    -- * Lenses
    kpdjpDataAccessRoleArn,
    kpdjpEndTime,
    kpdjpInputDataConfig,
    kpdjpJobId,
    kpdjpJobName,
    kpdjpJobStatus,
    kpdjpLanguageCode,
    kpdjpMessage,
    kpdjpOutputDataConfig,
    kpdjpSubmitTime,
    kpdjpVolumeKmsKeyId,
    kpdjpVpcConfig,
  )
where

import qualified Network.AWS.Comprehend.Types.AnyLengthString as Types
import qualified Network.AWS.Comprehend.Types.IamRoleArn as Types
import qualified Network.AWS.Comprehend.Types.InputDataConfig as Types
import qualified Network.AWS.Comprehend.Types.JobId as Types
import qualified Network.AWS.Comprehend.Types.JobName as Types
import qualified Network.AWS.Comprehend.Types.JobStatus as Types
import qualified Network.AWS.Comprehend.Types.LanguageCode as Types
import qualified Network.AWS.Comprehend.Types.OutputDataConfig as Types
import qualified Network.AWS.Comprehend.Types.VolumeKmsKeyId as Types
import qualified Network.AWS.Comprehend.Types.VpcConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about a key phrases detection job.
--
-- /See:/ 'mkKeyPhrasesDetectionJobProperties' smart constructor.
data KeyPhrasesDetectionJobProperties = KeyPhrasesDetectionJobProperties'
  { -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Maybe Types.IamRoleArn,
    -- | The time that the key phrases detection job completed.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The input data configuration that you supplied when you created the key phrases detection job.
    inputDataConfig :: Core.Maybe Types.InputDataConfig,
    -- | The identifier assigned to the key phrases detection job.
    jobId :: Core.Maybe Types.JobId,
    -- | The name that you assigned the key phrases detection job.
    jobName :: Core.Maybe Types.JobName,
    -- | The current status of the key phrases detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The language code of the input documents.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | A description of the status of a job.
    message :: Core.Maybe Types.AnyLengthString,
    -- | The output data configuration that you supplied when you created the key phrases detection job.
    outputDataConfig :: Core.Maybe Types.OutputDataConfig,
    -- | The time that the key phrases detection job was submitted for processing.
    submitTime :: Core.Maybe Core.NominalDiffTime,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
    --
    --
    --     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    volumeKmsKeyId :: Core.Maybe Types.VolumeKmsKeyId,
    -- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your key phrases detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'KeyPhrasesDetectionJobProperties' value with any optional fields omitted.
mkKeyPhrasesDetectionJobProperties ::
  KeyPhrasesDetectionJobProperties
mkKeyPhrasesDetectionJobProperties =
  KeyPhrasesDetectionJobProperties'
    { dataAccessRoleArn =
        Core.Nothing,
      endTime = Core.Nothing,
      inputDataConfig = Core.Nothing,
      jobId = Core.Nothing,
      jobName = Core.Nothing,
      jobStatus = Core.Nothing,
      languageCode = Core.Nothing,
      message = Core.Nothing,
      outputDataConfig = Core.Nothing,
      submitTime = Core.Nothing,
      volumeKmsKeyId = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpDataAccessRoleArn :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Types.IamRoleArn)
kpdjpDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED kpdjpDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | The time that the key phrases detection job completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpEndTime :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
kpdjpEndTime = Lens.field @"endTime"
{-# DEPRECATED kpdjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The input data configuration that you supplied when you created the key phrases detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpInputDataConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Types.InputDataConfig)
kpdjpInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED kpdjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The identifier assigned to the key phrases detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpJobId :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Types.JobId)
kpdjpJobId = Lens.field @"jobId"
{-# DEPRECATED kpdjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name that you assigned the key phrases detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpJobName :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Types.JobName)
kpdjpJobName = Lens.field @"jobName"
{-# DEPRECATED kpdjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The current status of the key phrases detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpJobStatus :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Types.JobStatus)
kpdjpJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED kpdjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The language code of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpLanguageCode :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Types.LanguageCode)
kpdjpLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED kpdjpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | A description of the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpMessage :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Types.AnyLengthString)
kpdjpMessage = Lens.field @"message"
{-# DEPRECATED kpdjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The output data configuration that you supplied when you created the key phrases detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpOutputDataConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Types.OutputDataConfig)
kpdjpOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED kpdjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The time that the key phrases detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpSubmitTime :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
kpdjpSubmitTime = Lens.field @"submitTime"
{-# DEPRECATED kpdjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

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
kpdjpVolumeKmsKeyId :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Types.VolumeKmsKeyId)
kpdjpVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# DEPRECATED kpdjpVolumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your key phrases detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpVpcConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Core.Maybe Types.VpcConfig)
kpdjpVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED kpdjpVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON KeyPhrasesDetectionJobProperties where
  parseJSON =
    Core.withObject "KeyPhrasesDetectionJobProperties" Core.$
      \x ->
        KeyPhrasesDetectionJobProperties'
          Core.<$> (x Core..:? "DataAccessRoleArn")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "InputDataConfig")
          Core.<*> (x Core..:? "JobId")
          Core.<*> (x Core..:? "JobName")
          Core.<*> (x Core..:? "JobStatus")
          Core.<*> (x Core..:? "LanguageCode")
          Core.<*> (x Core..:? "Message")
          Core.<*> (x Core..:? "OutputDataConfig")
          Core.<*> (x Core..:? "SubmitTime")
          Core.<*> (x Core..:? "VolumeKmsKeyId")
          Core.<*> (x Core..:? "VpcConfig")
