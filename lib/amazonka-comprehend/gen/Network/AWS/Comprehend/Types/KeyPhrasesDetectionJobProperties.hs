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
    kpdjpLanguageCode,
    kpdjpJobId,
    kpdjpJobName,
    kpdjpInputDataConfig,
    kpdjpVPCConfig,
    kpdjpVolumeKMSKeyId,
    kpdjpEndTime,
    kpdjpOutputDataConfig,
    kpdjpDataAccessRoleARN,
    kpdjpJobStatus,
    kpdjpMessage,
    kpdjpSubmitTime,
  )
where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a key phrases detection job.
--
-- /See:/ 'mkKeyPhrasesDetectionJobProperties' smart constructor.
data KeyPhrasesDetectionJobProperties = KeyPhrasesDetectionJobProperties'
  { languageCode ::
      Lude.Maybe LanguageCode,
    jobId ::
      Lude.Maybe Lude.Text,
    jobName ::
      Lude.Maybe Lude.Text,
    inputDataConfig ::
      Lude.Maybe
        InputDataConfig,
    vpcConfig ::
      Lude.Maybe VPCConfig,
    volumeKMSKeyId ::
      Lude.Maybe Lude.Text,
    endTime ::
      Lude.Maybe Lude.Timestamp,
    outputDataConfig ::
      Lude.Maybe
        OutputDataConfig,
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

-- | Creates a value of 'KeyPhrasesDetectionJobProperties' with the minimum fields required to make a request.
--
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
-- * 'endTime' - The time that the key phrases detection job completed.
-- * 'inputDataConfig' - The input data configuration that you supplied when you created the key phrases detection job.
-- * 'jobId' - The identifier assigned to the key phrases detection job.
-- * 'jobName' - The name that you assigned the key phrases detection job.
-- * 'jobStatus' - The current status of the key phrases detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
-- * 'languageCode' - The language code of the input documents.
-- * 'message' - A description of the status of a job.
-- * 'outputDataConfig' - The output data configuration that you supplied when you created the key phrases detection job.
-- * 'submitTime' - The time that the key phrases detection job was submitted for processing.
-- * 'volumeKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt data on the storage volume attached to the ML compute instance(s) that process the analysis job. The VolumeKmsKeyId can be either of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
-- * 'vpcConfig' - Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your key phrases detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
mkKeyPhrasesDetectionJobProperties ::
  KeyPhrasesDetectionJobProperties
mkKeyPhrasesDetectionJobProperties =
  KeyPhrasesDetectionJobProperties'
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
kpdjpLanguageCode :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe LanguageCode)
kpdjpLanguageCode = Lens.lens (languageCode :: KeyPhrasesDetectionJobProperties -> Lude.Maybe LanguageCode) (\s a -> s {languageCode = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The identifier assigned to the key phrases detection job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpJobId :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe Lude.Text)
kpdjpJobId = Lens.lens (jobId :: KeyPhrasesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name that you assigned the key phrases detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpJobName :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe Lude.Text)
kpdjpJobName = Lens.lens (jobName :: KeyPhrasesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The input data configuration that you supplied when you created the key phrases detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpInputDataConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe InputDataConfig)
kpdjpInputDataConfig = Lens.lens (inputDataConfig :: KeyPhrasesDetectionJobProperties -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Configuration parameters for a private Virtual Private Cloud (VPC) containing the resources you are using for your key phrases detection job. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC> .
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpVPCConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe VPCConfig)
kpdjpVPCConfig = Lens.lens (vpcConfig :: KeyPhrasesDetectionJobProperties -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

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
kpdjpVolumeKMSKeyId :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe Lude.Text)
kpdjpVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: KeyPhrasesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | The time that the key phrases detection job completed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpEndTime :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe Lude.Timestamp)
kpdjpEndTime = Lens.lens (endTime :: KeyPhrasesDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The output data configuration that you supplied when you created the key phrases detection job.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpOutputDataConfig :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe OutputDataConfig)
kpdjpOutputDataConfig = Lens.lens (outputDataConfig :: KeyPhrasesDetectionJobProperties -> Lude.Maybe OutputDataConfig) (\s a -> s {outputDataConfig = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpDataAccessRoleARN :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe Lude.Text)
kpdjpDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: KeyPhrasesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The current status of the key phrases detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpJobStatus :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe JobStatus)
kpdjpJobStatus = Lens.lens (jobStatus :: KeyPhrasesDetectionJobProperties -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | A description of the status of a job.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpMessage :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe Lude.Text)
kpdjpMessage = Lens.lens (message :: KeyPhrasesDetectionJobProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The time that the key phrases detection job was submitted for processing.
--
-- /Note:/ Consider using 'submitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjpSubmitTime :: Lens.Lens' KeyPhrasesDetectionJobProperties (Lude.Maybe Lude.Timestamp)
kpdjpSubmitTime = Lens.lens (submitTime :: KeyPhrasesDetectionJobProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTime = a} :: KeyPhrasesDetectionJobProperties)
{-# DEPRECATED kpdjpSubmitTime "Use generic-lens or generic-optics with 'submitTime' instead." #-}

instance Lude.FromJSON KeyPhrasesDetectionJobProperties where
  parseJSON =
    Lude.withObject
      "KeyPhrasesDetectionJobProperties"
      ( \x ->
          KeyPhrasesDetectionJobProperties'
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
