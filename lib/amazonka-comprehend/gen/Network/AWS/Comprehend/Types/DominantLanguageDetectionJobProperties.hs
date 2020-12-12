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
    dldjpJobId,
    dldjpJobName,
    dldjpInputDataConfig,
    dldjpVPCConfig,
    dldjpVolumeKMSKeyId,
    dldjpEndTime,
    dldjpOutputDataConfig,
    dldjpDataAccessRoleARN,
    dldjpJobStatus,
    dldjpMessage,
    dldjpSubmitTime,
  )
where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a dominant language detection job.
--
-- /See:/ 'mkDominantLanguageDetectionJobProperties' smart constructor.
data DominantLanguageDetectionJobProperties = DominantLanguageDetectionJobProperties'
  { jobId ::
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

-- | Creates a value of 'DominantLanguageDetectionJobProperties' with the minimum fields required to make a request.
--
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
-- * 'endTime' - The time that the dominant language detection job completed.
-- * 'inputDataConfig' - The input data configuration that you supplied when you created the dominant language detection job.
-- * 'jobId' - The identifier assigned to the dominant language detection job.
-- * 'jobName' - The name that you assigned to the dominant language detection job.
-- * 'jobStatus' - The current status of the dominant language detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
-- * 'message' - A description for the status of a job.
-- * 'outputDataConfig' - The output data configuration that you supplied when you created the dominant language detection job.
-- * 'submitTime' - The time that the dominant language detection job was submitted for processing.
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your dominant language detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkDominantLanguageDetectionJobProperties ::
  DominantLanguageDetectionJobProperties
mkDominantLanguageDetectionJobProperties =
  DominantLanguageDetectionJobProperties'
    { jobId = Lude.Nothing,
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

-- | The identifier assigned to the dominant language detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpJobId :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe Lude.Text)
dldjpJobId = Lens.lens (jobId :: DominantLanguageDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name that you assigned to the dominant language detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpJobName :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe Lude.Text)
dldjpJobName = Lens.lens (jobName :: DominantLanguageDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The input data configuration that you supplied when you created the dominant language detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpInputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe InputDataConfig)
dldjpInputDataConfig = Lens.lens (inputDataConfig :: DominantLanguageDetectionJobProperties -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your dominant language detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpVPCConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe VPCConfig)
dldjpVPCConfig = Lens.lens (vpcConfig :: DominantLanguageDetectionJobProperties -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
dldjpVolumeKMSKeyId :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe Lude.Text)
dldjpVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: DominantLanguageDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The time that the dominant language detection job completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpEndTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe Lude.Timestamp)
dldjpEndTime = Lens.lens (endTime :: DominantLanguageDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The output data configuration that you supplied when you created the dominant language detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpOutputDataConfig :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe OutputDataConfig)
dldjpOutputDataConfig = Lens.lens (outputDataConfig :: DominantLanguageDetectionJobProperties -> Lude.Maybe OutputDataConfig) (\s a -> s {outputDataConfig = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpDataAccessRoleARN :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe Lude.Text)
dldjpDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: DominantLanguageDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The current status of the dominant language detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpJobStatus :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe JobStatus)
dldjpJobStatus = Lens.lens (jobStatus :: DominantLanguageDetectionJobProperties -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description for the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpMessage :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe Lude.Text)
dldjpMessage = Lens.lens (message :: DominantLanguageDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time that the dominant language detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjpSubmitTime :: Lens.Lens' DominantLanguageDetectionJobProperties (Lude.Maybe Lude.Timestamp)
dldjpSubmitTime = Lens.lens (submitTime :: DominantLanguageDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: DominantLanguageDetectionJobProperties)
{-# DEPRECATED dldjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON DominantLanguageDetectionJobProperties where
  parseJSON =
    Lude.withObject
      "DominantLanguageDetectionJobProperties"
      ( \x ->
          DominantLanguageDetectionJobProperties'
            Lude.<$> (x Lude..:? "JobId")
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
