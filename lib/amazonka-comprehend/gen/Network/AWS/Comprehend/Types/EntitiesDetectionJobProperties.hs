{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
  ( EntitiesDetectionJobProperties (..),

    -- * Smart constructor
    mkEntitiesDetectionJobProperties,

    -- * Lenses
    eDataAccessRoleArn,
    eEndTime,
    eEntityRecognizerArn,
    eInputDataConfig,
    eJobId,
    eJobName,
    eJobStatus,
    eLanguageCode,
    eMessage,
    eOutputDataConfig,
    eSubmitTime,
    eVolumeKmsKeyId,
    eVpcConfig,
  )
where

import qualified Network.AWS.Comprehend.Types.AnyLengthString as Types
import qualified Network.AWS.Comprehend.Types.EntityRecognizerArn as Types
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

-- | Provides information about an entities detection job.
--
-- /See:/ 'mkEntitiesDetectionJobProperties' smart constructor.
data EntitiesDetectionJobProperties = EntitiesDetectionJobProperties'
  { -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Maybe Types.IamRoleArn,
    -- | The time that the entities detection job completed
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
    entityRecognizerArn :: Core.Maybe Types.EntityRecognizerArn,
    -- | The input data configuration that you supplied when you created the entities detection job.
    inputDataConfig :: Core.Maybe Types.InputDataConfig,
    -- | The identifier assigned to the entities detection job.
    jobId :: Core.Maybe Types.JobId,
    -- | The name that you assigned the entities detection job.
    jobName :: Core.Maybe Types.JobName,
    -- | The current status of the entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The language code of the input documents.
    languageCode :: Core.Maybe Types.LanguageCode,
    -- | A description of the status of a job.
    message :: Core.Maybe Types.AnyLengthString,
    -- | The output data configuration that you supplied when you created the entities detection job.
    outputDataConfig :: Core.Maybe Types.OutputDataConfig,
    -- | The time that the entities detection job was submitted for processing.
    submitTime :: Core.Maybe Core.NominalDiffTime,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
    --
    --
    --     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    volumeKmsKeyId :: Core.Maybe Types.VolumeKmsKeyId,
    -- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EntitiesDetectionJobProperties' value with any optional fields omitted.
mkEntitiesDetectionJobProperties ::
  EntitiesDetectionJobProperties
mkEntitiesDetectionJobProperties =
  EntitiesDetectionJobProperties'
    { dataAccessRoleArn = Core.Nothing,
      endTime = Core.Nothing,
      entityRecognizerArn = Core.Nothing,
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
eDataAccessRoleArn :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.IamRoleArn)
eDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED eDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | The time that the entities detection job completed
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndTime :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
eEndTime = Lens.field @"endTime"
{-# DEPRECATED eEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEntityRecognizerArn :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.EntityRecognizerArn)
eEntityRecognizerArn = Lens.field @"entityRecognizerArn"
{-# DEPRECATED eEntityRecognizerArn "Use generic-lens or generic-optics with 'entityRecognizerArn' instead." #-}

-- | The input data configuration that you supplied when you created the entities detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eInputDataConfig :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.InputDataConfig)
eInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED eInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The identifier assigned to the entities detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eJobId :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.JobId)
eJobId = Lens.field @"jobId"
{-# DEPRECATED eJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name that you assigned the entities detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eJobName :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.JobName)
eJobName = Lens.field @"jobName"
{-# DEPRECATED eJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The current status of the entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eJobStatus :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.JobStatus)
eJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED eJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The language code of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLanguageCode :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.LanguageCode)
eLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED eLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | A description of the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.AnyLengthString)
eMessage = Lens.field @"message"
{-# DEPRECATED eMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The output data configuration that you supplied when you created the entities detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOutputDataConfig :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.OutputDataConfig)
eOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED eOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The time that the entities detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSubmitTime :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
eSubmitTime = Lens.field @"submitTime"
{-# DEPRECATED eSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

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
eVolumeKmsKeyId :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.VolumeKmsKeyId)
eVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# DEPRECATED eVolumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your entity detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eVpcConfig :: Lens.Lens' EntitiesDetectionJobProperties (Core.Maybe Types.VpcConfig)
eVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED eVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON EntitiesDetectionJobProperties where
  parseJSON =
    Core.withObject "EntitiesDetectionJobProperties" Core.$
      \x ->
        EntitiesDetectionJobProperties'
          Core.<$> (x Core..:? "DataAccessRoleArn")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "EntityRecognizerArn")
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
