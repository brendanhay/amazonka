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
    dcjpJobId,
    dcjpDocumentClassifierARN,
    dcjpJobName,
    dcjpInputDataConfig,
    dcjpVPCConfig,
    dcjpVolumeKMSKeyId,
    dcjpEndTime,
    dcjpOutputDataConfig,
    dcjpDataAccessRoleARN,
    dcjpJobStatus,
    dcjpMessage,
    dcjpSubmitTime,
  )
where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a document classification job.
--
-- /See:/ 'mkDocumentClassificationJobProperties' smart constructor.
data DocumentClassificationJobProperties = DocumentClassificationJobProperties'
  { jobId ::
      Lude.Maybe
        Lude.Text,
    documentClassifierARN ::
      Lude.Maybe
        Lude.Text,
    jobName ::
      Lude.Maybe
        Lude.Text,
    inputDataConfig ::
      Lude.Maybe
        InputDataConfig,
    vpcConfig ::
      Lude.Maybe
        VPCConfig,
    volumeKMSKeyId ::
      Lude.Maybe
        Lude.Text,
    endTime ::
      Lude.Maybe
        Lude.Timestamp,
    outputDataConfig ::
      Lude.Maybe
        OutputDataConfig,
    dataAccessRoleARN ::
      Lude.Maybe
        Lude.Text,
    jobStatus ::
      Lude.Maybe
        JobStatus,
    message ::
      Lude.Maybe
        Lude.Text,
    submitTime ::
      Lude.Maybe
        Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentClassificationJobProperties' with the minimum fields required to make a request.
--
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
-- * 'documentClassifierARN' - The Amazon Resource Name (ARN) that identifies the document classifier.
-- * 'endTime' - The time that the document classification job completed.
-- * 'inputDataConfig' - The input data configuration that you supplied when you created the document classification job.
-- * 'jobId' - The identifier assigned to the document classification job.
-- * 'jobName' - The name that you assigned to the document classification job.
-- * 'jobStatus' - The current status of the document classification job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
-- * 'message' - A description of the status of the job.
-- * 'outputDataConfig' - The output data configuration that you supplied when you created the document classification job.
-- * 'submitTime' - The time that the document classification job was submitted for processing.
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkDocumentClassificationJobProperties ::
  DocumentClassificationJobProperties
mkDocumentClassificationJobProperties =
  DocumentClassificationJobProperties'
    { jobId = Lude.Nothing,
      documentClassifierARN = Lude.Nothing,
      jobName = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      volumeKMSKeyId = Lude.Nothing,
      endTime = Lude.Nothing,
      outputDataConfig = Lude.Nothing,
      dataAccessRoleARN = Lude.Nothing,
      jobStatus = Lude.Nothing,
      message = Lude.Nothing,
      submitTime = Lude.Nothing
    }

-- | The identifier assigned to the document classification job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpJobId :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe Lude.Text)
dcjpJobId = Lens.lens (jobId :: DocumentClassificationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- /Note:/ Consider using 'documentClassifierARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpDocumentClassifierARN :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe Lude.Text)
dcjpDocumentClassifierARN = Lens.lens (documentClassifierARN :: DocumentClassificationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {documentClassifierARN = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpDocumentClassifierARN "Use generic-lens or generic-optics with 'documentClassifierARN' instead." #-}

-- | The name that you assigned to the document classification job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpJobName :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe Lude.Text)
dcjpJobName = Lens.lens (jobName :: DocumentClassificationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The input data configuration that you supplied when you created the document classification job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpInputDataConfig :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe InputDataConfig)
dcjpInputDataConfig = Lens.lens (inputDataConfig :: DocumentClassificationJobProperties -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your document classification job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpVPCConfig :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe VPCConfig)
dcjpVPCConfig = Lens.lens (vpcConfig :: DocumentClassificationJobProperties -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
dcjpVolumeKMSKeyId :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe Lude.Text)
dcjpVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: DocumentClassificationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The time that the document classification job completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpEndTime :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe Lude.Timestamp)
dcjpEndTime = Lens.lens (endTime :: DocumentClassificationJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The output data configuration that you supplied when you created the document classification job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpOutputDataConfig :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe OutputDataConfig)
dcjpOutputDataConfig = Lens.lens (outputDataConfig :: DocumentClassificationJobProperties -> Lude.Maybe OutputDataConfig) (\s a -> s {outputDataConfig = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpDataAccessRoleARN :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe Lude.Text)
dcjpDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: DocumentClassificationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The current status of the document classification job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpJobStatus :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe JobStatus)
dcjpJobStatus = Lens.lens (jobStatus :: DocumentClassificationJobProperties -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description of the status of the job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpMessage :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe Lude.Text)
dcjpMessage = Lens.lens (message :: DocumentClassificationJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time that the document classification job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjpSubmitTime :: Lens.Lens' DocumentClassificationJobProperties (Lude.Maybe Lude.Timestamp)
dcjpSubmitTime = Lens.lens (submitTime :: DocumentClassificationJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: DocumentClassificationJobProperties)
{-# DEPRECATED dcjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON DocumentClassificationJobProperties where
  parseJSON =
    Lude.withObject
      "DocumentClassificationJobProperties"
      ( \x ->
          DocumentClassificationJobProperties'
            Lude.<$> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "DocumentClassifierArn")
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "VolumeKmsKeyId")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "OutputDataConfig")
            Lude.<*> (x Lude..:? "DataAccessRoleArn")
            Lude.<*> (x Lude..:? "JobStatus")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "SubmitTime")
      )
