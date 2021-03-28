{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.SentimentDetectionJobProperties
  ( SentimentDetectionJobProperties (..)
  -- * Smart constructor
  , mkSentimentDetectionJobProperties
  -- * Lenses
  , sdjpDataAccessRoleArn
  , sdjpEndTime
  , sdjpInputDataConfig
  , sdjpJobId
  , sdjpJobName
  , sdjpJobStatus
  , sdjpLanguageCode
  , sdjpMessage
  , sdjpOutputDataConfig
  , sdjpSubmitTime
  , sdjpVolumeKmsKeyId
  , sdjpVpcConfig
  ) where

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

-- | Provides information about a sentiment detection job.
--
-- /See:/ 'mkSentimentDetectionJobProperties' smart constructor.
data SentimentDetectionJobProperties = SentimentDetectionJobProperties'
  { dataAccessRoleArn :: Core.Maybe Types.IamRoleArn
    -- ^ The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the sentiment detection job ended.
  , inputDataConfig :: Core.Maybe Types.InputDataConfig
    -- ^ The input data configuration that you supplied when you created the sentiment detection job.
  , jobId :: Core.Maybe Types.JobId
    -- ^ The identifier assigned to the sentiment detection job.
  , jobName :: Core.Maybe Types.JobName
    -- ^ The name that you assigned to the sentiment detection job
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ The current status of the sentiment detection job. If the status is @FAILED@ , the @Messages@ field shows the reason for the failure.
  , languageCode :: Core.Maybe Types.LanguageCode
    -- ^ The language code of the input documents.
  , message :: Core.Maybe Types.AnyLengthString
    -- ^ A description of the status of a job.
  , outputDataConfig :: Core.Maybe Types.OutputDataConfig
    -- ^ The output data configuration that you supplied when you created the sentiment detection job.
  , submitTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the sentiment detection job was submitted for processing.
  , volumeKmsKeyId :: Core.Maybe Types.VolumeKmsKeyId
    -- ^ ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SentimentDetectionJobProperties' value with any optional fields omitted.
mkSentimentDetectionJobProperties
    :: SentimentDetectionJobProperties
mkSentimentDetectionJobProperties
  = SentimentDetectionJobProperties'{dataAccessRoleArn =
                                       Core.Nothing,
                                     endTime = Core.Nothing, inputDataConfig = Core.Nothing,
                                     jobId = Core.Nothing, jobName = Core.Nothing,
                                     jobStatus = Core.Nothing, languageCode = Core.Nothing,
                                     message = Core.Nothing, outputDataConfig = Core.Nothing,
                                     submitTime = Core.Nothing, volumeKmsKeyId = Core.Nothing,
                                     vpcConfig = Core.Nothing}

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpDataAccessRoleArn :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Types.IamRoleArn)
sdjpDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE sdjpDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | The time that the sentiment detection job ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpEndTime :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
sdjpEndTime = Lens.field @"endTime"
{-# INLINEABLE sdjpEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The input data configuration that you supplied when you created the sentiment detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpInputDataConfig :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Types.InputDataConfig)
sdjpInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE sdjpInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | The identifier assigned to the sentiment detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpJobId :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Types.JobId)
sdjpJobId = Lens.field @"jobId"
{-# INLINEABLE sdjpJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The name that you assigned to the sentiment detection job
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpJobName :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Types.JobName)
sdjpJobName = Lens.field @"jobName"
{-# INLINEABLE sdjpJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The current status of the sentiment detection job. If the status is @FAILED@ , the @Messages@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpJobStatus :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Types.JobStatus)
sdjpJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE sdjpJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | The language code of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpLanguageCode :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Types.LanguageCode)
sdjpLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE sdjpLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | A description of the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpMessage :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Types.AnyLengthString)
sdjpMessage = Lens.field @"message"
{-# INLINEABLE sdjpMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The output data configuration that you supplied when you created the sentiment detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpOutputDataConfig :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Types.OutputDataConfig)
sdjpOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE sdjpOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The time that the sentiment detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpSubmitTime :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Core.NominalDiffTime)
sdjpSubmitTime = Lens.field @"submitTime"
{-# INLINEABLE sdjpSubmitTime #-}
{-# DEPRECATED submitTime "Use generic-lens or generic-optics with 'submitTime' instead"  #-}

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
sdjpVolumeKmsKeyId :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Types.VolumeKmsKeyId)
sdjpVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE sdjpVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> . 
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpVpcConfig :: Lens.Lens' SentimentDetectionJobProperties (Core.Maybe Types.VpcConfig)
sdjpVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE sdjpVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.FromJSON SentimentDetectionJobProperties where
        parseJSON
          = Core.withObject "SentimentDetectionJobProperties" Core.$
              \ x ->
                SentimentDetectionJobProperties' Core.<$>
                  (x Core..:? "DataAccessRoleArn") Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "InputDataConfig"
                    Core.<*> x Core..:? "JobId"
                    Core.<*> x Core..:? "JobName"
                    Core.<*> x Core..:? "JobStatus"
                    Core.<*> x Core..:? "LanguageCode"
                    Core.<*> x Core..:? "Message"
                    Core.<*> x Core..:? "OutputDataConfig"
                    Core.<*> x Core..:? "SubmitTime"
                    Core.<*> x Core..:? "VolumeKmsKeyId"
                    Core.<*> x Core..:? "VpcConfig"
