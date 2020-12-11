-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentDetectionJobProperties
  ( SentimentDetectionJobProperties (..),

    -- * Smart constructor
    mkSentimentDetectionJobProperties,

    -- * Lenses
    sdjpLanguageCode,
    sdjpJobId,
    sdjpJobName,
    sdjpInputDataConfig,
    sdjpVPCConfig,
    sdjpVolumeKMSKeyId,
    sdjpEndTime,
    sdjpOutputDataConfig,
    sdjpDataAccessRoleARN,
    sdjpJobStatus,
    sdjpMessage,
    sdjpSubmitTime,
  )
where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a sentiment detection job.
--
-- /See:/ 'mkSentimentDetectionJobProperties' smart constructor.
data SentimentDetectionJobProperties = SentimentDetectionJobProperties'
  { languageCode ::
      Lude.Maybe LanguageCode,
    jobId ::
      Lude.Maybe Lude.Text,
    jobName ::
      Lude.Maybe Lude.Text,
    inputDataConfig ::
      Lude.Maybe InputDataConfig,
    vpcConfig ::
      Lude.Maybe VPCConfig,
    volumeKMSKeyId ::
      Lude.Maybe Lude.Text,
    endTime ::
      Lude.Maybe Lude.Timestamp,
    outputDataConfig ::
      Lude.Maybe OutputDataConfig,
    dataAccessRoleARN ::
      Lude.Maybe Lude.Text,
    jobStatus ::
      Lude.Maybe JobStatus,
    message ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'SentimentDetectionJobProperties' with the minimum fields required to make a request.
--
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
-- * 'endTime' - The time that the sentiment detection job ended.
-- * 'inputDataConfig' - The input data configuration that you supplied when you created the sentiment detection job.
-- * 'jobId' - The identifier assigned to the sentiment detection job.
-- * 'jobName' - The name that you assigned to the sentiment detection job
-- * 'jobStatus' - The current status of the sentiment detection job. If the status is @FAILED@ , the @Messages@ field shows the reason for the failure.
-- * 'languageCode' - The language code of the input documents.
-- * 'message' - A description of the status of a job.
-- * 'outputDataConfig' - The output data configuration that you supplied when you created the sentiment detection job.
-- * 'submitTime' - The time that the sentiment detection job was submitted for processing.
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkSentimentDetectionJobProperties ::
  SentimentDetectionJobProperties
mkSentimentDetectionJobProperties =
  SentimentDetectionJobProperties'
    { languageCode = Lude.Nothing,
      jobId = Lude.Nothing,
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

-- | The language code of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpLanguageCode :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe LanguageCode)
sdjpLanguageCode = Lens.lens (languageCode :: SentimentDetectionJobProperties -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The identifier assigned to the sentiment detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpJobId :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe Lude.Text)
sdjpJobId = Lens.lens (jobId :: SentimentDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name that you assigned to the sentiment detection job
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpJobName :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe Lude.Text)
sdjpJobName = Lens.lens (jobName :: SentimentDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The input data configuration that you supplied when you created the sentiment detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpInputDataConfig :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe InputDataConfig)
sdjpInputDataConfig = Lens.lens (inputDataConfig :: SentimentDetectionJobProperties -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your sentiment detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpVPCConfig :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe VPCConfig)
sdjpVPCConfig = Lens.lens (vpcConfig :: SentimentDetectionJobProperties -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
sdjpVolumeKMSKeyId :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe Lude.Text)
sdjpVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: SentimentDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The time that the sentiment detection job ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpEndTime :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe Lude.Timestamp)
sdjpEndTime = Lens.lens (endTime :: SentimentDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The output data configuration that you supplied when you created the sentiment detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpOutputDataConfig :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe OutputDataConfig)
sdjpOutputDataConfig = Lens.lens (outputDataConfig :: SentimentDetectionJobProperties -> Lude.Maybe OutputDataConfig) (\s a -> s {outputDataConfig = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpDataAccessRoleARN :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe Lude.Text)
sdjpDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: SentimentDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The current status of the sentiment detection job. If the status is @FAILED@ , the @Messages@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpJobStatus :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe JobStatus)
sdjpJobStatus = Lens.lens (jobStatus :: SentimentDetectionJobProperties -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description of the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpMessage :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe Lude.Text)
sdjpMessage = Lens.lens (message :: SentimentDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time that the sentiment detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjpSubmitTime :: Lens.Lens' SentimentDetectionJobProperties (Lude.Maybe Lude.Timestamp)
sdjpSubmitTime = Lens.lens (submitTime :: SentimentDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: SentimentDetectionJobProperties)
{-# DEPRECATED sdjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON SentimentDetectionJobProperties where
  parseJSON =
    Lude.withObject
      "SentimentDetectionJobProperties"
      ( \x ->
          SentimentDetectionJobProperties'
            Lude.<$> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "JobId")
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
