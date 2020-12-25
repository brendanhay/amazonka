{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassificationJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassificationJobProperties
  ( DocumentClassificationJobProperties (..),

    -- * Smart constructor
    mkDocumentClassificationJobProperties,

    -- * Lenses
    dcjpDataAccessRoleArn,
    dcjpDocumentClassifierArn,
    dcjpEndTime,
    dcjpInputDataConfig,
    dcjpJobId,
    dcjpJobName,
    dcjpJobStatus,
    dcjpMessage,
    dcjpOutputDataConfig,
    dcjpSubmitTime,
    dcjpVolumeKmsKeyId,
    dcjpVpcConfig,
  )
where

import qualified Network.AWS.Comprehend.Types.AnyLengthString as Types
import qualified Network.AWS.Comprehend.Types.DocumentClassifierArn as Types
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

-- | Provides information about a document classification job.
--
-- /See:/ 'mkDocumentClassificationJobProperties' smart constructor.
data DocumentClassificationJobProperties = DocumentClassificationJobProperties'
  { -- | The Amazon Resource Name (ARN) of the AWS identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
    dataAccessRoleArn :: Core.Maybe Types.IamRoleArn,
    -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Core.Maybe Types.DocumentClassifierArn,
    -- | The time that the document classification job completed.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The input data configuration that you supplied when you created the document classification job.
    inputDataConfig :: Core.Maybe Types.InputDataConfig,
    -- | The identifier assigned to the document classification job.
    jobId :: Core.Maybe Types.JobId,
    -- | The name that you assigned to the document classification job.
    jobName :: Core.Maybe Types.JobName,
    -- | The current status of the document classification job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | A description of the status of the job.
    message :: Core.Maybe Types.AnyLengthString,
    -- | The output data configuration that you supplied when you created the document classification job.
    outputDataConfig :: Core.Maybe Types.OutputDataConfig,
    -- | The time that the document classification job was submitted for processing.
    submitTime :: Core.Maybe Core.NominalDiffTime,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
    --
    --
    --     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    volumeKmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DocumentClassificationJobProperties' value with any optional fields omitted.
mkDocumentClassificationJobProperties ::
  DocumentClassificationJobProperties
mkDocumentClassificationJobProperties =
  DocumentClassificationJobProperties'
    { dataAccessRoleArn =
        Core.Nothing,
      documentClassifierArn = Core.Nothing,
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

-- | The Amazon Resource Name (ARN) of the AWS identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpDataAccessRoleArn :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Types.IamRoleArn)
dcjpDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED dcjpDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- /Note:/ Consider using 'documentClassifierArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpDocumentClassifierArn :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Types.DocumentClassifierArn)
dcjpDocumentClassifierArn = Lens.field @"documentClassifierArn"
{-# DEPRECATED dcjpDocumentClassifierArn "Use generic-lens or generic-optics with 'documentClassifierArn' instead." #-}

-- | The time that the document classification job completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpEndTime :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Core.NominalDiffTime)
dcjpEndTime = Lens.field @"endTime"
{-# DEPRECATED dcjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The input data configuration that you supplied when you created the document classification job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpInputDataConfig :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Types.InputDataConfig)
dcjpInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED dcjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The identifier assigned to the document classification job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpJobId :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Types.JobId)
dcjpJobId = Lens.field @"jobId"
{-# DEPRECATED dcjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name that you assigned to the document classification job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpJobName :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Types.JobName)
dcjpJobName = Lens.field @"jobName"
{-# DEPRECATED dcjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The current status of the document classification job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpJobStatus :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Types.JobStatus)
dcjpJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED dcjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description of the status of the job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpMessage :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Types.AnyLengthString)
dcjpMessage = Lens.field @"message"
{-# DEPRECATED dcjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The output data configuration that you supplied when you created the document classification job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpOutputDataConfig :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Types.OutputDataConfig)
dcjpOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED dcjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The time that the document classification job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpSubmitTime :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Core.NominalDiffTime)
dcjpSubmitTime = Lens.field @"submitTime"
{-# DEPRECATED dcjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

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
dcjpVolumeKmsKeyId :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Types.KmsKeyId)
dcjpVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# DEPRECATED dcjpVolumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpVpcConfig :: Lens.Lens' DocumentClassificationJobProperties (Core.Maybe Types.VpcConfig)
dcjpVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED dcjpVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON DocumentClassificationJobProperties where
  parseJSON =
    Core.withObject "DocumentClassificationJobProperties" Core.$
      \x ->
        DocumentClassificationJobProperties'
          Core.<$> (x Core..:? "DataAccessRoleArn")
          Core.<*> (x Core..:? "DocumentClassifierArn")
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
